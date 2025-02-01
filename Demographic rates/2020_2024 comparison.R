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




convert_data_types <- function(df) {
  df %>%
    mutate(
      BAUM_ID = as.character(BAUM_ID),
      DBH = as.numeric(DBH),
      Height = as.numeric(Height),
      Species = as.character(Species)
    )
}

p81_2020 <- convert_data_types(p81_2020)
p82_2020 <- convert_data_types(p82_2020)
p81_2024 <- convert_data_types(p81_2024)
p82_2024 <- convert_data_types(p82_2024)


# Rename DBH columns to distinguish between years
p81_2020 <- dplyr::rename(p81_2020, DBH20 = DBH)
p82_2020 <- dplyr::rename(p82_2020, DBH20 = DBH)
p81_2024 <- dplyr::rename(p81_2024, DBH24 = DBH)
p82_2024 <- dplyr::rename(p82_2024, DBH24 = DBH)

p81_2020 <- dplyr::rename(p81_2020, Height20 = Height)
p82_2020 <- dplyr::rename(p82_2020, Height20 = Height)
p81_2024 <- dplyr::rename(p81_2024, Height24 = Height)
p82_2024 <- dplyr::rename(p82_2024, Height24 = Height)

# Merge 2020 and 2024 data for Plot 81
combined_p81 <- dplyr::full_join(
  p81_2020, 
  p81_2024, 
  by = c("Tree_ID","xxx") 
)

# Add DBH20 = 0 for new trees in 2024
combined_p81 <- combined_p81 %>%
  dplyr::mutate(DBH20 = ifelse(is.na(DBH20), 0, DBH20))


# Merge 2020 and 2024 data for Plot 82
combined_p82 <- dplyr::full_join(
  p82_2020, 
  p82_2024, 
  by = c("Tree_ID","xxx") 
)

# Add DBH20 = 0 for new trees in 2024
combined_p82 <- combined_p82 %>%
  dplyr::mutate(DBH20 = ifelse(is.na(DBH20), 0, DBH20))

# Preview the combined datasets
head(combined_p81)
head(combined_p82)

combined_p81$Species.x <- NULL
combined_p82$Species.x <- NULL

combined_p81 <- dplyr::rename(combined_p81, Species = Species.y)
combined_p82 <- dplyr::rename(combined_p82, Species = Species.y)

write_csv(combined_p81, "combined_p81.csv") 
write_csv(combined_p82, "combined_p82.csv")

combined_p81 <- combined_p81 %>%
  mutate(Growth = DBH24 - DBH20)

combined_p82 <- combined_p82 %>%
  mutate(Growth = DBH24 - DBH20)

# Grouping the trees by species and calculating growth for Plot 81
species_grouped_p81 <- combined_p81 %>%
  mutate(Growth = DBH24 - DBH20) %>%  # Calculate growth
  group_by(Species) %>%              # Group by species
  summarise(
    AvgGrowth = mean(Growth, na.rm = TRUE),   # Average growth
    SDGrowth = sd(Growth, na.rm = TRUE),     # Standard deviation
    MedianGrowth = median(Growth, na.rm = TRUE),  # Median growth
    MinGrowth = min(Growth, na.rm = TRUE),   # Minimum growth
    MaxGrowth = max(Growth, na.rm = TRUE),   # Maximum growth
    Count = n()                              # Number of trees
  )

# Grouping the trees by species and calculating growth for Plot 82
species_grouped_p82 <- combined_p82 %>%
  mutate(Growth = DBH24 - DBH20) %>%  # Calculate growth
  group_by(Species) %>%              # Group by species
  summarise(
    AvgGrowth = mean(Growth, na.rm = TRUE),   # Average growth
    SDGrowth = sd(Growth, na.rm = TRUE),     # Standard deviation
    MedianGrowth = median(Growth, na.rm = TRUE),  # Median growth
    MinGrowth = min(Growth, na.rm = TRUE),   # Minimum growth
    MaxGrowth = max(Growth, na.rm = TRUE),   # Maximum growth
    Count = n()                              # Number of trees
  )

# Grouping the trees by species and calculating growth for Plot 82
species_grouped_p82 <- combined_p82 %>%
  mutate(Growth = DBH24 - DBH20) %>%
  group_by(Species) %>%
  summarise(
    AvgGrowth = mean(Growth, na.rm = TRUE),
    SDGrowth = sd(Growth, na.rm = TRUE),
    MedianGrowth = median(Growth, na.rm = TRUE),
    MinGrowth = min(Growth, na.rm = TRUE),
    MaxGrowth = max(Growth, na.rm = TRUE),
    Count = n()
  )

# Boxplot for Plot 81
ggplot(combined_p81, aes(x = Species, y = Growth)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Growth Distribution by Species (Plot 81)",
    x = "Species",
    y = "Growth (DBH)"
  ) +
  theme_minimal()

# Boxplot for Plot 82
ggplot(combined_p82, aes(x = Species, y = Growth)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Growth Distribution by Species (Plot 82)",
    x = "Species",
    y = "Growth (DBH)"
  ) +
  theme_minimal()

