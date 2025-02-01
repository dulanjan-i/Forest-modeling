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
  select(-H20) %>%
  select(-DBH20)

#drop NA
plots <- plots %>%
  filter(complete.cases(.))

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

#Create a table that contain the descriptive stats of the plots
# Step 1 a panel dataset 

# Reclassify Status values
plots <- plots %>%
  mutate(Status = case_when(
    Status %in% c("2 - zu THS", "4 - zu THL") ~ "Dead",  # Merge both dead categories
    Status == "1 - Nein" ~ "Alive",  # Keep as Alive
    Status == "5 - BAUM Neu" ~ "New Trees",  # Keep as New Trees
    TRUE ~ Status  # Retain any other existing categories (if applicable)
  ))

print(plots)

# Summarize the stats as required
plots <- plots %>%
  mutate(cl = ifelse(is.na(cl) & Status == "New Trees", 2, cl))


summary_panel <- plots %>%
  group_by(SP, cl) %>%
  summarise(
    Alive_Trees = sum(Status == "Alive", na.rm = TRUE),
    Dead_Trees = sum(Status == "Dead", na.rm = TRUE),
    New_Trees = sum(Status == "New Trees", na.rm = TRUE),
    Mean_DBH = mean(DBH24, na.rm = TRUE),
    Mean_Height = mean(H24, na.rm = TRUE)
  ) %>%
  ungroup()

# Reshape the data to create two overarching canopy layer columns
summary_panel_wide <- summary_panel %>%
  pivot_wider(names_from = cl, values_from = c(Alive_Trees, Dead_Trees, New_Trees, Mean_DBH, Mean_Height), 
              names_glue = "Canopy_{cl}_{.value}")

# Reorder columns: First Canopy 1, then Canopy 2
summary_panel_wide <- summary_panel_wide %>%
  select(SP, 
         starts_with("Canopy_1"), 
         starts_with("Canopy_2"))  

write_csv(summary_panel_wide, "summary of plots.csv")
writexl::write_xlsx(summary_panel_wide, "summary panel.xls")
