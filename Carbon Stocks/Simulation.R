# This model uses a Simplified Diameter-Height Power Law as used by 
# Chave et al. (2005) in their study. This method has also been proven an effective model
# in the context of forest modeling and simulating tree heights based on the DBH.

library(tidyverse)
library(nls2)

carbon_stocks <- read.csv("carbon_stocks.csv")

alive_trees <- carbon_stocks %>%
  filter(Status == "1 - Nein")

# H = k * D^p model cited from Chave et al. (2005) which uses nonlinear regression 
#to estimate the tree heights based on the DBH growth rates
#Fit k and p for Each Species 
fit_height_model <- function(dbh, height) {
  # Fit the H = k * D^p model using nonlinear regression
  model <- nls(height ~ k * dbh^p, start = list(k = 1, p = 0.5))
  return(coef(model))
}

# Calculate k and p for each species
species_coefficients <- alive_trees %>%
  group_by(SP) %>%
  summarize(
    k = fit_height_model(DBH24, H24)["k"],
    p = fit_height_model(DBH24, H24)["p"]
  )

# Merge k and p back into the alive_trees dataset
alive_trees <- alive_trees %>%
  left_join(species_coefficients, by = "SP")

# Define the Chave et al. (2005) height equation
estimate_height <- function(dbh, k, p) {
  return(k * (dbh^p))
}

# Simulation function for carbon stocks
simulate_carbon_stocks <- function(data, years = 100, interval = 5) {
  results <- list()
  
  for (i in 1:nrow(data)) {
    # Extract tree-specific parameters
    tree <- data[i, ]
    dbh <- tree$DBH24
    height <- tree$H24
    k <- tree$k
    p <- tree$p
    a <- tree$a
    b <- tree$b
    c <- tree$c
    wood_density <- tree$Wood_Density
    carbon_content <- 0.47
    root_shoot_ratio <- 0.3
    growth_rate_dbh <- tree$G1  # G1 growth rate is from the dataset
    
    tree_results <- data.frame(Year = numeric(), 
                               DBH = numeric(), 
                               Height = numeric(),
                               Carbon_Above = numeric(), 
                               Carbon_Below = numeric(), 
                               Carbon_Total = numeric())
    
    for (year in seq(0, years, by = interval)) {
      # Calculate height using Chave et al. (2005)
      height <- estimate_height(dbh, k, p)
      
      # Calculate stem volume using allometric equation
      stem_volume <- (dbh^a) * (height^b) * exp(c)
      
      # Calculate aboveground biomass and carbon stock
      agb <- stem_volume * wood_density
      carbon_above <- agb * carbon_content
      
      # Calculate belowground carbon stock
      carbon_below <- carbon_above * root_shoot_ratio
      
      # Total carbon stock
      carbon_total <- carbon_above + carbon_below
      
      # Store results
      tree_results <- rbind(tree_results, data.frame(Year = year, 
                                                     DBH = dbh, 
                                                     Height = height,
                                                     Carbon_Above = carbon_above, 
                                                     Carbon_Below = carbon_below, 
                                                     Carbon_Total = carbon_total))
      
      # Update DBH for the next interval
      dbh <- dbh + (growth_rate_dbh * interval)
    }
    
    tree_results$Tree_ID <- tree$Tree_ID
    results[[i]] <- tree_results
  }
  
  # Combine results from all trees
  final_results <- do.call(rbind, results)
  return(final_results)
}

# Run the simulation for alive trees
simulation_results <- simulate_carbon_stocks(alive_trees, years = 100, interval = 5)

# View the simulation results
print(simulation_results)

# Step 7: Save the results to a CSV file
write.csv(simulation_results, "carbon_stock_simulation_results.csv", row.names = FALSE)

