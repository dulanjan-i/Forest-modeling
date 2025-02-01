# Calculate shared y-axis range for mortality and growth rate plots
y_range <- range(c(mortality_layer1$MortalityRate, mortality_layer2$MortalityRate), na.rm = TRUE)
y_range2 <- range(c(growth_layer1$GrowthRate, growth_layer2$GrowthRate), na.rm = TRUE)

# Define the x-axis labels
dtg_labels <- c("Flooded (0.8m)", "Moist (1.3m)", "Intermediate (1.8m)", "Dry (2.3m)")

# Define shared settings for plots
common_theme <- theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = "white"),  # Panel background (plot area)
    plot.background = element_rect(fill = "white", color = "white"),  # Entire plot background
    legend.background = element_rect(fill = "white", color = "white") # Legend background
  )

# Mortality Rates - Canopy Layer 1
plot_mortality_layer1_points <- ggplot(mortality_layer1, aes(x = DTG, y = MortalityRate, color = sp)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3), labels = dtg_labels) +
  scale_y_continuous(limits = y_range) +
  labs(
    title = "Mortality Rates - Canopy Layer 1",
    x = "Soil condition and DTG (m)",
    y = "Mortality Rate (%)",
    color = "Species"
  ) +
  common_theme

plot_mortality_layer1_line <- plot_mortality_layer1_points +
  geom_line(aes(group = sp), size = 0.5, alpha = 0.3)

# Mortality Rates - Canopy Layer 2
plot_mortality_layer2_points <- ggplot(mortality_layer2, aes(x = DTG, y = MortalityRate, color = sp)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3), labels = dtg_labels) +
  scale_y_continuous(limits = y_range) +
  labs(
    title = "Mortality Rates - Canopy Layer 2",
    x = "Soil condition and DTG (m)",
    y = "Mortality Rate (%)",
    color = "Species"
  ) +
  common_theme

plot_mortality_layer2_line <- plot_mortality_layer2_points +
  geom_line(aes(group = sp), size = 0.5, alpha = 0.3)

# Growth Rates - Canopy Layer 1
plot_growth_layer1_points <- ggplot(growth_layer1, aes(x = DTG, y = GrowthRate, color = sp)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3), labels = dtg_labels) +
  scale_y_continuous(limits = y_range2) +
  labs(
    title = "Growth Rates - Canopy Layer 1",
    x = "Soil condition and DTG (m)",
    y = "Growth Rate (cm/year)",
    color = "Species"
  ) +
  common_theme

plot_growth_layer1_line <- plot_growth_layer1_points +
  geom_line(aes(group = sp), size = 0.5, alpha = 0.3)

# Growth Rates - Canopy Layer 2
plot_growth_layer2_points <- ggplot(growth_layer2, aes(x = DTG, y = GrowthRate, color = sp)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3), labels = dtg_labels) +
  scale_y_continuous(limits = y_range2) +
  labs(
    title = "Growth Rates - Canopy Layer 2",
    x = "Soil condition and DTG (m)",
    y = "Growth Rate (cm/year)",
    color = "Species"
  ) +
  common_theme

plot_growth_layer2_line <- plot_growth_layer2_points +
  geom_line(aes(group = sp), size = 0.5, alpha = 0.3)

# Recruitment Rates
plot_recruitment_points <- ggplot(recruitment_data, aes(x = DTG, y = rec_ha, color = sp)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(0.8, 1.3, 1.8, 2.3), labels = dtg_labels) +
  labs(
    title = "Recruitment Rates Across Different Soil Conditions",
    x = "Soil condition and DTG (m)",
    y = "Recruits per ha/year",
    color = "Species"
  ) +
  common_theme

plot_recruitment_line <- plot_recruitment_points +
  geom_line(aes(group = sp), size = 0.5, alpha = 0.3)

# Define the output directory
output_dir <- "/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/Inter_casestudy/Floodings/Demo rates diff GW - current conditions"

# Save plots
ggsave(file.path(output_dir, "Mortality_Rates_Layer1_Points.png"), plot_mortality_layer1_points, width = 10, height = 8)
ggsave(file.path(output_dir, "Mortality_Rates_Layer1_Line.png"), plot_mortality_layer1_line, width = 10, height = 8)

ggsave(file.path(output_dir, "Mortality_Rates_Layer2_Points.png"), plot_mortality_layer2_points, width = 10, height = 8)
ggsave(file.path(output_dir, "Mortality_Rates_Layer2_Line.png"), plot_mortality_layer2_line, width = 10, height = 8)

ggsave(file.path(output_dir, "Growth_Rates_Layer1_Points.png"), plot_growth_layer1_points, width = 10, height = 8)
ggsave(file.path(output_dir, "Growth_Rates_Layer1_Line.png"), plot_growth_layer1_line, width = 10, height = 8)

ggsave(file.path(output_dir, "Growth_Rates_Layer2_Points.png"), plot_growth_layer2_points, width = 10, height = 8)
ggsave(file.path(output_dir, "Growth_Rates_Layer2_Line.png"), plot_growth_layer2_line, width = 10, height = 8)

ggsave(file.path(output_dir, "Recruitment_Rates_Points.png"), plot_recruitment_points, width = 10, height = 8)
ggsave(file.path(output_dir, "Recruitment_Rates_Line.png"), plot_recruitment_line, width = 10, height = 8)
