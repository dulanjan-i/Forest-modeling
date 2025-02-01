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

# Exclude specified species
plots <- plots %>%
  filter(!SP %in% c("Ace_neg", "Cor_san", "Fag_syl"))

# Combine Ulm_min and Ulm_spe into Ulm_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Ulm_min", "Ulm_spe"), "Ulm_spe", SP))

# Combine Til_cor and Til_pla into Til_spe
plots <- plots %>%
  mutate(SP = ifelse(SP %in% c("Til_cor", "Til_pla"), "Til_spe", SP))

# Summarize statistics for each plot
summary_stats <- plots %>%
  mutate(Status = ifelse(is.na(Status), "1 - Nein", Status)) %>%
  group_by(PLOTID) %>%
  summarize(
    No_of_Alive_Trees = sum(Status == "1 - Nein"),
    No_of_Dead_Trees = sum(Status %in% c("2 - zu THS", "4 - zu THL")),
    No_of_New_Trees = sum(Status == "5 - BAUM Neu"),
    Max_Height = max(H24, na.rm = TRUE),
    Mean_Height = mean(H24, na.rm = TRUE),
    Min_Height = min(H24, na.rm = TRUE),
    Max_DBH = max(DBH24, na.rm = TRUE),
    Mean_DBH = mean(DBH24, na.rm = TRUE),
    Min_DBH = min(DBH24, na.rm = TRUE),
    Basal_Area = sum((DBH24 / 200)^2 * pi, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = -PLOTID,
    names_to = "Statistics",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = PLOTID,
    values_from = Value,
    names_prefix = "Plot_"
  )

# Print the summary table
print(summary_stats)
write_csv(summary_stats, "summary.csv")
