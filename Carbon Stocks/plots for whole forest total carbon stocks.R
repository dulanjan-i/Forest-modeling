library(tidyverse)
library(ggplot2)

# Read total forest carbon summary table
total_carbon_table <- read.csv("total_forest_carbon_summary.csv")

# Convert data from wide to long format for ggplot
total_carbon_long <- total_carbon_table %>%
  pivot_longer(cols = -Timestep, names_to = "Condition", values_to = "Total_Carbon")

# Create the line plot
ggplot(total_carbon_long, aes(x = Timestep, y = Total_Carbon, color = Condition)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +  # Add points for better visibility
  labs(
    title = "Total Forest Carbon Stock Over Time",
    x = "Time (years)",
    y = "Total Carbon Stock (Mg/ha)",
    color = "Condition"
  ) +
  theme_minimal() +
  theme_bw() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )

ggsave("total_forest_carbon_stock.png", width = 10, height = 6, dpi = 300)


#========== stacked bar chart and line graphs for species wise ==============
#stacked bar chart

species_carbon_table <- read.csv("species_carbon_stock_summary.csv")

# Convert data from wide to long format
species_carbon_long <- species_carbon_table %>%
  pivot_longer(cols = -c(Species, Timestep), names_to = "Condition", values_to = "Total_Carbon")

# Create the stacked bar plot
species_carbon_plot <- ggplot(species_carbon_long, aes(x = factor(Timestep), y = Total_Carbon, fill = Species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Condition) +  # Separate by condition
  labs(
    title = "Species Carbon Stock Over Time",
    x = "Time (years)",
    y = "Total Carbon Stock (Mg/ha)",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )

# Save the plot as PNG
ggsave("species_carbon_stock_bar.png", plot = species_carbon_plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(species_carbon_plot)

#faceted line charts

species_carbon_plot <- ggplot(species_carbon_long, aes(x = Timestep, y = Total_Carbon, color = Species)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +  # Add points for better visibility
  facet_wrap(~Condition, scales = "free_y") +  # Separate by condition
  labs(
    title = "Species Carbon Stock Trends Over Time",
    x = "Time (years)",
    y = "Total Carbon Stock (Mg/ha)",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom"
  )

# Save the plot as PNG
ggsave("species_carbon_stock_trends.png", plot = species_carbon_plot, width = 12, height = 8, dpi = 300)

# Display the plot
print(species_carbon_plot)
