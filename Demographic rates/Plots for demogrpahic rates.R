library(tidyverse)
library(ggplot2)

# Define DTG values for each file
flooded <- read_table("demographic_rates_flooded.txt") %>% mutate(DTG = 0.8) #From Elles et al., 2025
moist <- read_table("rates_13_moist_no_FAH.txt") %>% mutate(DTG = 1.3)
intermediate <- read_table("rates_13_middle_no_FAH.txt") %>% mutate(DTG = 1.8)
dry <- read_table("rates_13_dry_no_FAH.txt") %>% mutate(DTG = 2.3)


# Combine all files into one dataset
combined_data <- bind_rows(flooded, moist, intermediate, dry)

# Ensure species names (sp) are consistent across datasets
combined_data <- combined_data %>%
  mutate(sp = factor(sp))  # Convert species to a factor

growth_data <- combined_data %>%
  select(sp, DTG, G1, G2) %>%
  pivot_longer(cols = c(G1, G2), names_to = "Layer", values_to = "GrowthRate")

mortality_data <- combined_data %>%
  select(sp, DTG, mu1, mu2) %>%
  pivot_longer(cols = c(mu1, mu2), names_to = "Layer", values_to = "MortalityRate")

recruitment_data <- combined_data %>%
  select(sp, DTG, rec_ha)

growth_layer1 <- growth_data %>%
  filter(Layer == "G1")  # Filter for canopy layer 1

ggplot(growth_layer1, aes(x = DTG, y = GrowthRate, color = sp)) +
  geom_point(size = 3) +
  geom_line(aes(group = sp), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3)) +
  labs(
    title = "Growth Rates - Canopy Layer 1",
    x = "DTG (m)",
    y = "Growth Rate (cm/year)",
    color = "Species"
  ) +
  theme_minimal()

growth_layer2 <- growth_data %>%
  filter(Layer == "G2")  # Filter for canopy layer 2

ggplot(growth_layer2, aes(x = DTG, y = GrowthRate, color = sp)) +
  geom_point(size = 3) +
  geom_line(aes(group = sp), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3)) +
  labs(
    title = "Growth Rates - Canopy Layer 2",
    x = "DTG (m)",
    y = "Growth Rate (cm/year)",
    color = "Species"
  ) +
  theme_minimal()

mortality_layer1 <- mortality_data %>%
  filter(Layer == "mu1")  # Filter for canopy layer 1

ggplot(mortality_layer1, aes(x = DTG, y = MortalityRate, color = sp)) +
  geom_point(size = 3) +
  geom_line(aes(group = sp), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3)) +
  labs(
    title = "Mortality Rates - Canopy Layer 1",
    x = "DTG (m)",
    y = "Mortality Rate (%)",
    color = "Species"
  ) +
  theme_minimal()

mortality_layer2 <- mortality_data %>%
  filter(Layer == "mu2")  # Filter for canopy layer 2

ggplot(mortality_layer2, aes(x = DTG, y = MortalityRate, color = sp)) +
  geom_point(size = 3) +
  geom_line(aes(group = sp), size = 1, alpha = 0.4) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3)) +
  labs(
    title = "Mortality Rates - Canopy Layer 2",
    x = "DTG (m)",
    y = "Mortality Rate (%)",
    color = "Species"
  ) +
  theme_minimal()

ggplot(recruitment_data, aes(x = DTG, y = rec_ha, color = sp)) +
  geom_point(size = 3) +
  geom_line(aes(group = sp), size = 1, alpha = 0.4) +
  #scale_y_log10() +  # Log scale for recruitment rates
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3)) +
  labs(
    title = "Recruitment Rates Across DTG",
    x = "DTG (m)",
    y = "Recruits per ha/year",
    color = "Species"
  ) +
  theme_minimal()

#############

# Prepare growth data
growth <- plots %>%
  filter(Status == "1 - Nein") %>%  # Only alive trees
  mutate(annual_growth = (DBH24 - DBH20) / years_between_censuses) %>%
  select(SP, cl, DBH20, annual_growth)

# Plot growth with faceting by species
ggplot(growth, aes(x = DBH20, y = annual_growth, color = factor(cl))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_color_manual(values = c("1" = "forestgreen", "2" = "orange"), name = "Canopy Layer") +
  facet_wrap(~ SP, scales = "free_y") +  # Facet by species with independent y-scales
  labs(
    title = "Annual Growth Rates by Species",
    x = "Diameter (cm)",
    y = "Annual Growth (cm)",
    color = "Canopy Layer"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)    # Rotate x-axis labels for better readability
  )

#Annual growth per species 
output_folder <- "/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/Inter_casestudy/Floodings/Annual Growth"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# List of unique species
species_list <- unique(growth$SP)

# Loop through each species and create a separate plot
for (species in species_list) {
  # Filter data for the current species
  species_data <- growth %>% filter(SP == species)
  
  # Create the plot
  p <- ggplot(species_data, aes(x = DBH20, y = annual_growth, color = factor(cl))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "loess", se = TRUE) +
    scale_color_manual(values = c("1" = "forestgreen", "2" = "orange"), name = "Canopy Layer") +
    labs(
      title = paste("Annual Growth for", species),
      x = "Diameter (cm)",
      y = "Annual Growth (cm)",
      color = "Canopy Layer"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = "white", color = "white"), # White background
      plot.background = element_rect(fill = "white", color = "white")   # White plot area
    )
  
  # Save the plot in the specified folder
  ggsave(filename = paste0(output_folder, "/growth_rates_", species, ".png"), plot = p, width = 8, height = 6)
}
