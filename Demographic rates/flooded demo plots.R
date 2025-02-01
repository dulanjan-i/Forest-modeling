library(ggplot2)
library(tidyverse)

# Load the dataset
demographic_data <- read_tsv("demographic_rates_flooded_updated.txt")

# Define the output folder path
output_folder <- "/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/Inter_casestudy/Floodings/Demo rates flooded plots"

# Define the rates to plot and their labels
demographic_rates <- c("G1", "G2", "mu1", "mu2", "rec_ha")
rate_labels <- c(
  G1 = "Growth Rate Canopy Layer 1",
  G2 = "Growth Rate Canopy Layer 2",
  mu1 = "Mortality Rate Canopy Layer 1",
  mu2 = "Mortality Rate Canopy Layer 2",
  rec_ha = "Recruitment Rate"
)

# Loop through each rate and create a plot
for (rate in demographic_rates) {
  # Create the plot
  p <- ggplot(demographic_data, aes(x = sp, y = !!sym(rate), color = sp)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(aes(group = sp), size = 1) +
    #scale_y_log10() +  # Use log scale for better visualization of wide ranges
    labs(
      title = rate_labels[[rate]],
      x = "Species",
      y = rate_labels[[rate]],
      color = "Species"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey90"),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  # Save each plot in the specified folder
  ggsave(
    filename = paste0(output_folder, "/", rate, "_plot.png"),
    plot = p,
    width = 8,
    height = 6
  )
}
