library(tidyverse)
library(readxl)

# Import data from Excel
p81_2020 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P81_2020") %>%
  select(BAUM_CODE, PLOTID, DBH20, BAUMART20, hBAUM20)  # Select columns for 2020 sheet

p82_2020 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P82_2020") %>%
  select(BAUM_CODE, PLOTID, DBH20, BAUMART20, hBAUM20)  # Select columns for 2020 sheet

p81_2024 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P81_2024") %>%
  select(BAUM_CODE, PLOTID, DBH24, BAUMART24, Wechsel24, hBAUM24)  # Select columns for 2024 sheet

p82_2024 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P82_2024") %>%
  select(BAUM_CODE, PLOTID, DBH24, BAUMART24, Wechsel24, hBAUM24)  # Select columns for 2024 sheet

# combine two years
combined_p81 <- dplyr::full_join(
  p81_2020, 
  p81_2024, 
  by = c("BAUM_CODE","PLOTID") 
)

# Add DBH20 = 0 for new trees in 2024
combined_p81 <- combined_p81 %>%
  dplyr::mutate(DBH20 = ifelse(is.na(DBH20), 0, DBH20))


# Merge 2020 and 2024 data for Plot 82
combined_p82 <- dplyr::full_join(
  p82_2020, 
  p82_2024, 
  by = c("BAUM_CODE","PLOTID") 
)

# Add DBH20 = 0 for new trees in 2024
combined_p82 <- combined_p82 %>%
  dplyr::mutate(DBH20 = ifelse(is.na(DBH20), 0, DBH20))

# joined the two plots together
plots <- bind_rows(combined_p81, combined_p82)

# remove the sp name of 2020
plots <- plots %>%
  select(-BAUMART20)

#rename variables to English
plots <- plots %>%
  dplyr::rename(Tree_ID = BAUM_CODE,
                SP = BAUMART24,
                Status = Wechsel24,
                H20 = hBAUM20,
                H24 = hBAUM24)
head(plots)

# Fill missing H24 with H20
plots <- plots %>%
  mutate(H24 = ifelse(is.na(H24), H20, H24)) %>%
  select(-H20)
 

# Fill missing DBH24 from DBH20
plots <- plots %>%
  mutate(DBH24 = ifelse(DBH24 == 0, DBH20, DBH24)) %>%
  select(-DBH20)
  
#drop NA
plots <- plots %>%
  filter(complete.cases(.))

# Exclude specified species
plots <- plots %>%
  filter(!SP %in% c("Ace_neg", "Cor_san", "Fag_syl"))

# Combine Ulm_min and Ulm_spe into Ulm_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Ulm_min", "Ulm_spe"), "Ulm_spe", SP))

# Combine Til_cor and Til_pla into Til_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Til_cor", "Til_pla"), "Til_spe", SP))

#===== CARBON STOCK CALCULATIONS ==========

#======= WOOD DENSITY MAP ===========
# wood density vals was extracted from the values corresponding to the "European" section of the 
# global wood density database

wood_density <- c(
  "Ace_pse" = 0.510,
  "Fra_exc" = 0.560, 
  "Car_bet" = 0.706,
  "Ace_pla" = 0.525, 
  "Que_rob" = 0.560, 
  "Ulm_spe" = 0.508, #got an average for all Ulm sp 
  "Til_spe" = 0.422 #both Til sp had the same value
)

plots <- plots %>%
  mutate(Wood_Density = wood_density[SP])

# Get the alllomentry values for the species from Shupe et.al.,2021 (for all species except Til spp) 
# and for Till spp used the same values as Ulm. based on justifications (see methodology)

species_params <- data.frame(
  SP = c("Car_bet", "Que_rob", "Ulm_spe", "Ace_pse", "Ace_pla", "Fra_exc", "Til_spe"),
  a = c(0.00021491, 2.00333, 1.942950, 1.89756, 1.89756, 1.95277, 1.94295),
  b = c(2.258957614, 0.85925, 1.292290, 0.97716, 0.97716, 0.77206, 1.292290),
  c = c(0.001411006, -2.86353, -4.200640, -2.94253, -2.94253, -2.48079, -4.200640) 
)

# View the species parameters
print(species_params)

# Add species-specific parameters into the dataset (join by species code)
plots <- plots %>%
  left_join(species_params, by = "SP")


#==== CARBON STOCK FUNCTION ======

# Define the carbon stock calculation function cited from Shupe et.al.,2021
calc_carbon_stocks <- function(dbh, height, a, b, c, wood_density, carbon_content = 0.47, root_shoot_ratio = 0.3) {
  # Step 1: Calculate stem volume using the allometric equation
  stem_volume <- (dbh^a) * (height^b) * exp(c)
  
  # Step 2: Calculate aboveground biomass
  agb <- stem_volume * wood_density
  
  # Step 3: Calculate aboveground carbon stock
  carbon_above <- agb * carbon_content
  
  # Step 4: Calculate belowground carbon stock
  carbon_below <- carbon_above * root_shoot_ratio
  
  # Step 5: Calculate total carbon stock
  carbon_total <- carbon_above + carbon_below
  
  return(list(carbon_above = carbon_above, carbon_below = carbon_below, carbon_total = carbon_total))
}

# Apply the function to the dataset
plots <- plots %>%
  rowwise() %>%
  mutate(
    Carbon_Above = calc_carbon_stocks(DBH24, H24, a, b, c, Wood_Density)$carbon_above,
    Carbon_Below = calc_carbon_stocks(DBH24, H24, a, b, c, Wood_Density)$carbon_below,
    Carbon_Total = calc_carbon_stocks(DBH24, H24, a, b, c, Wood_Density)$carbon_total
  )

# Reclassify Status values
plots <- plots %>%
  mutate(Status = case_when(
    Status %in% c("2 - zu THS", "4 - zu THL") ~ "Dead",  # Merge both dead categories
    Status == "1 - Nein" ~ "Alive",  # Keep as Alive
    Status == "5 - BAUM Neu" ~ "New Trees",  # Keep as New Trees
    TRUE ~ Status  # Retain any other existing categories (if applicable)
  ))

# Summarize carbon stocks for each status
summary <- plots %>%
  group_by(Status) %>%
  summarize(
    Total_Aboveground_Carbon = sum(Carbon_Above, na.rm = TRUE),
    Total_Belowground_Carbon = sum(Carbon_Below, na.rm = TRUE),
    Total_Carbon_Stock = sum(Carbon_Total, na.rm = TRUE)
  )


print(summary)


write.csv(plots, "carbon stocks.csv")
write_csv(summary, "summary of carbon stocks by status in flooded plots.csv")

# Summarize carbon stocks by species and status
summary_species_status <- plots %>%
  group_by(SP, Status) %>%
  summarize(
    Tree_count = n(),
    Total_Aboveground_Carbon = sum(Carbon_Above, na.rm = TRUE),
    Total_Belowground_Carbon = sum(Carbon_Below, na.rm = TRUE),
    Total_Carbon_Stock = sum(Carbon_Total, na.rm = TRUE)
  ) %>%
  ungroup()

# Print the summary to console
print(summary_species_status)


#write.csv(summary_species_status, "carbon_stocks_by_species_status.csv", row.names = FALSE)

readr::write_csv(summary_species_status, "carbon_stocks_by_species_status.csv")

#============= PLOTS BY SPECIES AND STATUS ===================================
library(ggplot2)

# Create a bar plot for total carbon stock per species, faceted by status
carbon_plot <- ggplot(summary_species_status, aes(x = SP, y = Total_Carbon_Stock, fill = SP)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot
  facet_wrap(~ Status, scales = "free") +  # One plot per status category
  labs(title = "Total Carbon Stock per Species by Status",
       x = "Species",
       y = "Total Carbon Stock (Mg/ha)") +
  theme_minimal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the plot
print(carbon_plot)

# Save the plot as a PNG file
ggsave("carbon_stock_per_species.png", carbon_plot, width = 10, height = 6)


#Get unique status categories
status_list <- unique(summary_species_status$Status)

# Loop through each status and create a separate plot
for (status in status_list) {
  # Filter data for the current status
  status_data <- summary_species_status %>% filter(Status == status)
  
  # Create the plot
  carbon_plot <- ggplot(status_data, aes(x = SP, y = Total_Carbon_Stock, fill = SP)) +
    geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot
    labs(title = paste("Total Carbon Stock per Species -", status),
         x = "Species",
         y = "Total Carbon Stock (Mg/ha)") +
    theme_minimal() +
    theme_bw()
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
  
  # Display the plot
  print(carbon_plot)
  
  # Save the plot as a PNG file
  ggsave(paste0("carbon_stock_", status, ".png"), carbon_plot, width = 10, height = 6)
}
 
