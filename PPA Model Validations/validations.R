#+++++++++++++++ Temporal trends ++++++++++++++++++
library(tidyverse)
library(ggplot2)
library(lme4)  
library(car) 
#install.packages("Metrics")
library(Metrics)
library(broom)

stats_p81_82 <- read.csv("stats_out_p81-82.csv")
stats_flooded <- read.csv("stats_out_flooded.csv")
stats_dry <- read.csv("stats_out_dry.csv")
stats_inter <- read.csv("stats_out_intermediate.csv")
stats_moist <- read.csv("stats_out_moist.csv")


stats_flooded1 <- stats_flooded %>% 
  dplyr::rename(ba1 = ba) %>%
  dplyr::rename(TIMEX = time)

stats_p81_82_1 <- stats_p81_82 %>% 
  dplyr::rename(ba2 = ba) %>%
  dplyr::rename(TIMEX = time)

joined <- stats_flooded1 %>% 
  left_join(stats_p81_82_1, stats_flooded1, by = "TIMEX")

#Linear Regression for Basal Area Over Time
lm_p81 <- lm(ba ~ time, data = stats_p81_82)
lm_flooded <- lm(ba ~ time, data = stats_flooded)
lm_dry <- lm(ba ~ time, data = stats_dry)
lm_inter <- lm(ba ~ time, data = stats_inter)
lm_moist <- lm(ba ~ time, data= stats_moist)

# Summary of Trends
summary(lm_p81)
summary(lm_flooded)
summary(lm_dry)
summary(lm_inter)
summary(lm_moist)

#Linear regression for the two predicted basal areas
lm_basal <- lm(ba1 ~ ba2, data = joined)
summary(lm_basal)

regression_results <- broom::tidy(lm_basal)

print(regression_results)

write.csv(regression_results, "regression_results.csv", row.names = FALSE)

#correlation. between two predicted basal areas
correlation_ba <- cor(joined$ba1, joined$ba2, use = "complete.obs")
cat("Correlation Coefficient:", correlation_ba)

print(correlation_ba)


# Visualization
ggplot() +
  geom_line(data = stats_dry, aes(x = time, y = ba, color = "Dry")) +
  geom_line(data = stats_inter, aes(x = time, y = ba, color = "Intermediate")) +
  geom_line(data = stats_moist, aes(x = time, y = ba, color = "Moist")) +
  geom_line(data = stats_flooded, aes(x = time, y = ba, color = "Flooded")) +
  labs(title = "Basal Area Trends Under Different Groundwater Conditions",
       x = "Time (years)",
       y = "Basal Area (m²/ha)") +
  scale_color_manual(values = c("Dry" = "brown", 
                                "Intermediate" = "orange", 
                                "Moist" = "blue", 
                                "Flooded" = "darkgreen")) +
  theme_minimal()

#### RMSE VALIDATION OBSERVED VS SIMULATED

cl_data <- read.csv("canopy_layer1.csv")

observed <- cl_data %>%
  filter(Status == "1 - Nein")

#calc basal area
observed$ba0 <- pi * (observed$DBH24/200)^2 
tot_obs_ba <- sum(observed$ba0, n.rm = TRUE)

# Filter for time step 0 in simulated data
simulated_time_fl <- stats_flooded %>%
  filter(time == 10) %>%
  pull(ba)

simulated_time_pl <- stats_p81_82 %>%
  filter(time == 0) %>%
  pull(ba)

#percentage error
difference <- tot_obs_ba - simulated_time_pl
percentage_error <- (difference / tot_obs_ba)*100
print(percentage_error)
# Calculate RMSE
rmse_value <- rmse(actual = tot_obs_ba, predicted = simulated_time_pl)

cat("RMSE:", rmse_value)

# MAE (Mean Absolute Error) 
mae_value <- mae(tot_obs_ba, simulated_time_pl)
cat("MAE:", mae_value)

#for the full forest
# Calculate RMSE
rmse_value_full <- rmse(actual = tot_obs_ba, predicted = simulated_time_fl)

cat("RMSE:", rmse_value_full)

# MAE (Mean Absolute Error) 
mae_value_full <- mae(tot_obs_ba, simulated_time_fl)
cat("MAE:", mae_value_full)

#percentage error
difference_full <- tot_obs_ba - simulated_time_fl
percentage_error_full <- (difference_full / tot_obs_ba)*100
print(percentage_error_full)
