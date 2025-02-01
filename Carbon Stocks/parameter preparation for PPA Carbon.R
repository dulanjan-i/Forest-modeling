library(tidyverse)
library(ggplot2)

#====MAPPING OF SPECIES NAME, ID AND CODE TO A CONSISTENT DATAFRAME=============

# Define species map (linking species ID, species code, and species name)
species_map <- tibble(
  sp = 1:7,  # Species ID in cohort files
  SP = c("BAH", "GES", "HBU", "SAH", "SEI", "UL", "WLI"),  # Species code
  Species_Name = c("Ace_pse", "Fra_exc", "Car_bet", "Ace_pla", "Que_rob", "Ulm_spe", "Til_spe")  # Species name
)

# Define wood density for each species
wood_density <- tibble(
  Species_Name = c("Ace_pse", "Fra_exc", "Car_bet", "Ace_pla", "Que_rob", "Ulm_spe", "Til_spe"),
  Wood_Density = c(0.510, 0.560, 0.706, 0.525, 0.560, 0.508, 0.422)
)

# Load species coefficients (a, b, c)
species_coefficients <- read.csv("species coefficients.csv")
species_coefficients <- species_coefficients %>%
  dplyr::rename(Species_Name = SP)

# Load species parameters (k, p)
species_params <- read.csv("species params.csv")
species_params <- species_params %>%
  dplyr::rename(SP = sp)

wood_density <- wood_density %>%
  left_join(species_map, by = c("Species_Name" = "Species_Name")) %>%
  select(sp, SP, Species_Name, Wood_Density)  # Keep only relevant columns

# Step 2: Add species_id to species_coefficients
species_coefficients <- species_coefficients %>%
  left_join(species_map, by = c("Species_Name" = "Species_Name")) %>%
  select(sp, SP, Species_Name, k, p)  # Keep only relevant columns

# Step 3: Add species_id to species_params
species_params <- species_params %>%
  left_join(species_map, by = c("SP" = "SP")) %>%
  select(sp, SP, Species_Name, a, b, c)  # Keep only relevant columns

# Step 4: Combine all datasets into a single parameter table
species_parameters <- species_map %>%
  left_join(wood_density, by = "sp") %>%
  left_join(species_coefficients, by = "sp") %>%
  left_join(species_params, by = "sp")

#drop unwanted columns
species_parameters <- species_parameters %>%
  select(sp, Species_Name.x, SP.x, k, p, a, b, c, Wood_Density) %>%
  dplyr::rename(SP_code = SP.x) %>%
  dplyr::rename(Species_Name = Species_Name.x)

print(species_parameters)

# Step 4: Save the final combined table to a CSV file
write.csv(species_parameters, "species_parameters.csv", row.names = FALSE)

# Step 5: Print the final table to verify
print(species_parameters)
