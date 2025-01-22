library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)

# Import data from Excel
p81_2020 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P81_2020") %>%
  select(BAUM_CODE, PLOTID, DBH20, BAUMART20)  # Select columns for 2020 sheet

p82_2020 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P82_2020") %>%
  select(BAUM_CODE, PLOTID, DBH20, BAUMART20)  # Select columns for 2020 sheet

p81_2024 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P81_2024") %>%
  select(BAUM_CODE, PLOTID, DBH24, BAUMART24, Wechsel24)  # Select columns for 2024 sheet

p82_2024 <- read_excel("Waldinventur Suedaue 2020_2024_P81_P82.xlsx", sheet = "P82_2024") %>%
  select(BAUM_CODE, PLOTID, DBH24, BAUMART24, Wechsel24)  # Select columns for 2024 sheet

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
                Status = Wechsel24)

#combine canopy layer (cl) from "Suedaue_P1_P86_with_crown_layers_MS_20241202.csv"

cl_data <- read_csv("Suedaue_P1_P86_with_crown_layers_MS_20241202.csv") %>%
  select(BAUM_CODE, PLOTID, cl)

cl_data <- cl_data %>%
  dplyr::rename(Tree_ID = BAUM_CODE)

plots <- plots %>%
  left_join(cl_data, by = c("Tree_ID", "PLOTID"))

# Exclude specified species
plots <- plots %>%
  filter(!SP %in% c("Ace_neg", "Cor_san", "Fag_syl"))

# Combine Ulm_min and Ulm_spe into Ulm_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Ulm_min", "Ulm_spe"), "Ulm_spe", SP))

# Combine Til_cor and Til_pla into Til_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Til_cor", "Til_pla"), "Til_spe", SP))

# Filter out rows where DBH20 is greater than DBH24
#plots <- plots %>%
  #filter(DBH20 < DBH24)

plots <- plots %>%
  filter(!(SP == "Que_rob" & DBH20 > DBH24))

print(plots)
write_csv(plots, "final_dataset.csv")

#+++++++++++++++calculate the demographic rates +++++++++++++++++++++

# Define constants
years_between_censuses <- 4
plot_area <- 0.5  # hectares

# 1. Filter data by canopy layer
layer1 <- plots %>% filter(cl == 1)
layer2 <- plots %>% filter(cl == 2)


# 2. Calculate Growth Rates (G1 and G2)
growth_rates <- plots %>%
  filter(Status == "1 - Nein") %>%  # Only consider alive trees
  group_by(SP, cl) %>%
  summarise(
    growth_annual = mean((DBH24 - DBH20) / years_between_censuses, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = cl, values_from = growth_annual, names_prefix = "G")

# 3. Calculate Mortality Fraction (Separate for Layer 1 and Layer 2)
mortality_fraction <- plots %>%
  filter(Status %in% c("1 - Nein", "2 - zu THS", "4 - zu THL")) %>%
  group_by(SP, cl) %>%
  summarise(
    N_start = sum(Status %in% c("1 - Nein", "2 - zu THS", "4 - zu THL")),  # Total trees
    N_dead = sum(Status %in% c("2 - zu THS", "4 - zu THL")),  # Trees that died
    m_sl = ifelse(N_start > 0, N_dead / N_start, NA),  # Mortality fraction
    .groups = "drop"
  )

# Separate by canopy layer and then join back into a single table
mortality_layer1 <- mortality_fraction %>%
  filter(cl == 1) %>%
  select(SP, m_sl1 = m_sl)

mortality_layer2 <- mortality_fraction %>%
  filter(cl == 2) %>%
  select(SP, m_sl2 = m_sl)

# Join the two layers to ensure one row per species
mortality_combined <- full_join(mortality_layer1, mortality_layer2, by = "SP")

# View the final table
print(mortality_combined)


# 4. Calculate Annual Mortality Rates (mu1 and mu2)
annual_mortality <- mortality_combined %>%
  mutate(
    mu1 = ifelse(!is.na(m_sl1), -log(1 - m_sl1) / years_between_censuses, NA),
    mu2 = ifelse(!is.na(m_sl2), -log(1 - m_sl2) / years_between_censuses, NA)
  ) %>%
  select(SP, mu1, mu2)
  
# 5. Calculate Recruitment Rates (rec_ha)
recruitment_rates <- plots %>%
  filter(Status == "5 - BAUM Neu") %>%  # Only new recruits
  group_by(SP) %>%
  summarise(
    rec_ha = n() / (plot_area * years_between_censuses),
    .groups = "drop"
  )

# 6. Combine all results
flooded <- growth_rates %>%
  full_join(annual_mortality, by = "SP") %>%
  full_join(recruitment_rates, by = "SP")

print(flooded)

write_csv(flooded, "demographic_rates_flooded.csv")
write_tsv(flooded, "demographic_rates_flooded.txt")

flooded_updated <- flooded
# import wet condition demogrpahic rates from Lucian's data
wet <- read_table("rates_13_moist_no_FAH.txt")
print(wet)

wet <- wet %>%
  dplyr::rename(SP = sp)

# Create a mapping table to standardize species names
species_mapping <- tibble(
  wet_SP = c("BAH", "GES", "HBU", "SAH", "SEI", "UL", "WLI"),
  flooded_SP = c("Ace_pse", "Fra_exc", "Car_bet","Ace_pla", "Que_rob", "Ulm_spe", "Til_spe"),
)

# Rename species in `wet` to match `flooded`
flooded_updated <- flooded_updated %>%
  left_join(species_mapping, by = c("SP" = "flooded_SP")) %>%
  mutate(SP = wet_SP) %>%  
  select(-wet_SP)    

print(flooded_updated)

#add missing columns also from 'wet'
canopy_params <- wet %>%
  select(SP, inflection, steepness, param1, param2)

flooded_updated <- flooded_updated %>%
  left_join(canopy_params, by = "SP")

# Replace missing (NA) and zero values in `flooded` with corresponding values from `wet`
flooded_updated <- flooded_updated %>%
  mutate(across(
    everything(),
    ~ ifelse(is.na(.x) | .x == 0, wet[[cur_column()]], .x)
  ))


print(wet)
print(flooded_updated)
print(flooded)

flooded_updated <- flooded_updated %>%
  dplyr::rename(sp = SP)

write_csv(flooded_updated, "demographic_rates_flooded_updated.csv")
write_tsv(flooded_updated, "demographic_rates_flooded_updated.txt")

dry <- read_table("rates_13_dry_no_FAH.txt")
intermediate <- read_table("rates_13_middle_no_FAH.txt")

#Species column rename
dry <- dry %>%
  dplyr::rename(SP = sp)
intermediate <- intermediate %>%
  dplyr::rename(SP = sp)

#PLOTS

# Add a source column to each table
flooded_updated <- flooded_updated %>% mutate(Source = "Flooded")
wet <- wet %>% mutate(Source = "Wet")
intermediate <- intermediate %>% mutate(Source = "Intermediate")
dry <- dry %>% mutate(Source = "Dry")



# Combine all tables into one
combined_data <- bind_rows(flooded_updated, wet, intermediate, dry)

# Convert data to long format for plotting
long_data <- combined_data %>%
  pivot_longer(
    cols = c(G1, G2, mu1, mu2, rec_ha, inflection, steepness, param1, param2),
    names_to = "Variable",
    values_to = "Value"
  )

# Create scatter plot
ggplot(long_data, aes(x = Variable, y = Value, color = Source)) +
  geom_point(position = position_dodge(width = 0.5), size = 2, alpha = 0.8) +  # Add scatter points
  facet_wrap(~ SP, scales = "free_y", ncol = 3) +  # Facet by species
  theme_minimal() +
  labs(
    title = "Comparison of All Variables Across Tables",
    x = "Variable",
    y = "Value",
    color = "Source"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 12),  # Adjust facet label size
    panel.grid.major = element_line(color = "grey90")  # Light grid for better readability
  )
