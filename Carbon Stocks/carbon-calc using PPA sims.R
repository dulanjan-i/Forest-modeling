library(tidyverse)
library(scales)  # For custom axis formatting
library(patchwork)
#=====CARBON STOCK CALCULATIONS BASED ON THE OUTPUTS OF THE PPA MODEL===========

# Load the species parameters file
species_parameters <- read.csv("species_parameters.csv")

# Load cohort files
all_cohorts_files <- list(
  "dry" = "all_cohorts_out_dry.csv",
  "flooded" = "all_cohorts_out_flooded.csv",
  "intermediate" = "all_cohorts_out_intermediate.csv",
  "moist" = "all_cohorts_out_moist.csv",
  "p81_82" = "all_cohorts_out_p81-82.csv"
)

# Define Chave et al. (2005) height estimation function
estimate_height <- function(dbh, k, p) {
  return(k * (dbh^p))
}

# Process each cohort file
process_cohort_file <- function(file_path, output_path) {
  cohort_data <- read.csv(file_path)
  
  # Merge cohort data with the species parameters file
  cohort_data <- cohort_data %>%
    left_join(species_parameters, by = "sp")  # Merge using `sp` as the key
  
  # Calculate height, stem volume, and carbon stocks
  cohort_data <- cohort_data %>%
    mutate(
      # Estimate tree height (per average tree)
      height = estimate_height(dbh, k, p),
      
      # Calculate stem volume (per average tree)
      stem_volume = (dbh^a) * (height^b) * exp(c),
      
      # Calculate aboveground biomass (per average tree)
      agb = stem_volume * Wood_Density,
      
      # Calculate carbon stocks per tree (average per cohort)
      carbon_above_per_tree = agb * 0.47,
      carbon_below_per_tree = carbon_above_per_tree * 0.3,
      carbon_total_per_tree = carbon_above_per_tree + carbon_below_per_tree,
      
      # Calculate total carbon stocks for the cohort
      total_carbon_above = carbon_above_per_tree * n,
      total_carbon_below = carbon_below_per_tree * n,
      total_carbon_total = carbon_total_per_tree * n
    )
  
  # Rename "carbon_total_per_tree" for clarity
  cohort_data <- cohort_data %>%
    rename(average_carbon_stock_per_cohort = carbon_total_per_tree)
  
  # Select and reorder columns
  cohort_data <- cohort_data %>%
    select(sp, dbh, n, cl, time, height, stem_volume, 
           average_carbon_stock_per_cohort, 
           total_carbon_total, total_carbon_above, total_carbon_below, 
           everything())
  
  # Save updated cohort file
  write.csv(cohort_data, output_path, row.names = FALSE)
}

# Process and save each cohort file
for (name in names(all_cohorts_files)) {
  input_file <- all_cohorts_files[[name]]
  output_file <- paste0("updated_", name, "_cohorts.csv")
  process_cohort_file(input_file, output_file)
}

#========== AGGREGATED TABLES FOR SPECIES AND YEAR =========================

# Load species parameters file to get species names
species_parameters <- read.csv("species_parameters.csv") %>%
  select(sp, Species_Name)  # Keep only relevant columns

# Define the list of cohort files
cohort_files <- list(
  "dry" = "updated_dry_cohorts.csv",
  "flooded" = "updated_flooded_cohorts.csv",
  "intermediate" = "updated_intermediate_cohorts.csv",
  "moist" = "updated_moist_cohorts.csv"
)

# Initialize empty lists to store summary data
species_carbon_list <- list()
total_carbon_list <- list()

# Process each cohort file to extract required summary information
for (condition in names(cohort_files)) {
  file_path <- cohort_files[[condition]]
  
  # Read cohort data
  cohort_data <- read.csv(file_path)
  
  # Summarize total carbon for each species per timestep
  species_carbon <- cohort_data %>%
    group_by(sp, time) %>%
    summarise(total_carbon = sum(total_carbon_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(condition = condition)  # Add condition name
  
  # Store species-wise data
  species_carbon_list[[condition]] <- species_carbon
  
  # Summarize total forest carbon per timestep
  total_carbon <- cohort_data %>%
    group_by(time) %>%
    summarise(total_carbon = sum(total_carbon_total, na.rm = TRUE), .groups = "drop") %>%
    mutate(condition = condition)  # Add condition name
  
  # Store total carbon data
  total_carbon_list[[condition]] <- total_carbon
}

# Combine all species-level carbon data
species_carbon_table <- bind_rows(species_carbon_list) %>%
  left_join(species_parameters, by = "sp") %>%  # Replace sp numbers with species names
  select(Species_Name, time, condition, total_carbon) %>%  # Keep relevant columns
  pivot_wider(names_from = condition, values_from = total_carbon, values_fill = 0) %>%
  rename(Species = Species_Name, Timestep = time)  # Rename for clarity

# Save species-level summary table
write.csv(species_carbon_table, "species_carbon_stock_summary.csv", row.names = FALSE)

# Combine total forest carbon data
total_carbon_table <- bind_rows(total_carbon_list) %>%
  pivot_wider(names_from = condition, values_from = total_carbon, values_fill = 0) %>%
  rename(Timestep = time)

# Save total forest carbon summary table
write.csv(total_carbon_table, "total_forest_carbon_summary.csv", row.names = FALSE)


#========PLOTS================

# List of cohort files
all_cohorts_files <- list(
  "dry" = "updated_dry_cohorts.csv",
  "flooded" = "updated_flooded_cohorts.csv",
  "intermediate" = "updated_intermediate_cohorts.csv",
  "moist" = "updated_moist_cohorts.csv"
)

# Function to process and plot carbon dynamics for a single condition
plot_cohort_condition <- function(file_path, condition_name) {
  # Load cohort data
  cohort_data <- read.csv(file_path)
  
  # Group by time and species, calculate total carbon stock per species
  cohort_data <- cohort_data %>%
    group_by(time, sp) %>% 
    summarise(total_carbon = sum(total_carbon_total, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(sp = case_when(
      sp == 1 ~ "BAH",
      sp == 2 ~ "GES",
      sp == 3 ~ "HBU",
      sp == 4 ~ "SAH",
      sp == 5 ~ "SEI",
      sp == 6 ~ "UL",
      sp == 7 ~ "WLI",
      TRUE ~ as.character(sp)
    ))
  
  # Create the plot
  p <- ggplot(cohort_data, aes(x = time, y = total_carbon, color = sp)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = c("SAH" = "#228B22",
                                  "BAH" = "#00FF00",
                                  "HBU" = "#FFA500",
                                  "GES" = "blue",
                                  "SEI" = "red",
                                  "UL" = "#17becf",
                                  "WLI" = "grey"),
                       name = "Species") +
    labs(
      title = paste("Carbon Stock Dynamics -", condition_name),
      x = "Time (years)",
      y = "Total Carbon Stock (Mg/ha)"
    ) +
    scale_y_continuous(labels = comma) +  # Format y-axis values as real numbers
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  return(p)
}

# Process and plot for each condition
plots <- list()
for (condition in names(all_cohorts_files)) {
  file_path <- all_cohorts_files[[condition]]
  p <- plot_cohort_condition(file_path, condition)
  plots[[condition]] <- p
  
  # Save the plot to a file
  ggsave(
    filename = paste0("carbon_stock_", condition, ".png"),
    plot = p,
    width = 10,
    height = 8
  )
}

# Display all plots interactively (optional)
plots

#============== FACET PLOT ==================

facet_order <- c("dry", "intermediate", "moist", "flooded")

# Load and combine all datasets into a single dataframe for faceting
facet_data <- list()

for (condition in names(all_cohorts_files)) {
  file_path <- all_cohorts_files[[condition]]
  
  # Read and process data
  cohort_data <- read.csv(file_path) %>%
    group_by(time, sp) %>% 
    summarise(total_carbon = sum(total_carbon_total, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(
      condition = factor(condition, levels = facet_order),  # Set factor levels for ordering
      sp = case_when(
        sp == 1 ~ "BAH",
        sp == 2 ~ "GES",
        sp == 3 ~ "HBU",
        sp == 4 ~ "SAH",
        sp == 5 ~ "SEI",
        sp == 6 ~ "UL",
        sp == 7 ~ "WLI",
        TRUE ~ as.character(sp)
      )
    )
  
  facet_data[[condition]] <- cohort_data
}

# Combine all data into one
facet_data_combined <- bind_rows(facet_data)

# Faceted plot with defined order
facet_plot <- ggplot(facet_data_combined, aes(x = time, y = total_carbon, color = sp)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("SAH" = "#228B22",
                                "BAH" = "#00FF00",
                                "HBU" = "#FFA500",
                                "GES" = "blue",
                                "SEI" = "red",
                                "UL" = "#17becf",
                                "WLI" = "grey"),
                     name = "Species") +
  facet_wrap(~condition, nrow = 2, ncol = 2) +  # 2x2 layout for clockwise arrangement
  labs(
    title = "Carbon Stock Dynamics Across Conditions",
    x = "Time (years)",
    y = "Total Carbon Stock (Mg/ha)"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Save and display
ggsave("faceted_carbon_stock.png", plot = facet_plot, width = 12, height = 8)
print(facet_plot)
