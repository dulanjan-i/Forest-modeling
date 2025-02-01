library(tidyverse)

# Define DBH class boundaries
dbh_breaks <- seq(5.5, 113, by = 1)
dbh_labels <- dbh_breaks 

# Load input data (PPA_initial_state_dry20.txt)
data <- read_csv("canopy_layer1.csv")

# Filter for alive trees (status = 1)
trees_alive <- data %>%
  filter(Status == "1 - Nein") %>%
  select(-Status)

# Create the DBH_class column
tree_class <- trees_alive %>%
  mutate(DBH_class = cut(
    DBH24,
    breaks = c(5, dbh_breaks),  # Add 5 as the lower bound for the first class
    labels = dbh_labels,        # Assign labels corresponding to the DBH breaks
    right = TRUE                # Include the upper bound in the current interval
  ))

# Convert DBH_class to numeric
tree_class <- tree_class %>%
  mutate(DBH_class = as.numeric(as.character(DBH_class)))

# Calculate tree density per hectare for each species and DBH class
tree_class <- tree_class %>%
  group_by(DBH_class, SP) %>%
  summarise(Density_per_ha = n(), .groups = "drop")


#species renaming
# Define species mapping
species_mapping <- c(
  "Ace_pse" = "BAH",
  "Fra_exc" = "GES",
  "Car_bet" = "HBU",
  "Ace_pla" = "SAH",
  "Que_rob" = "SEI",
  "Ulm_spe" = "UL",
  "Til_spe" = "WLI"
)

# Rename species in the dataframe
tree_class <- tree_class %>%
  mutate(SP = recode(SP, !!!species_mapping))

#write.csv(tree_class, "tree_class.csv")

tree_class <- tree_class %>%
  mutate(cl = 1)

#rearrange
tree_class <- tree_class %>%
  select(DBH_class, Density_per_ha, cl, SP)

#save for sim
write.table(tree_class, 
            file = "initial_state_flooded_p81_82.txt", 
            row.names = FALSE,  # Exclude row names
            col.names = FALSE,  # Exclude column headers
            quote = FALSE,      # Exclude quotes around character values
            sep = "\t")         # Use tab as a separator

cat("File 'initial_state_flooded_p81_82.txt' has been successfully created.\n")
