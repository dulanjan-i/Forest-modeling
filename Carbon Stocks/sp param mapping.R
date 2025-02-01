library(tidyverse)

demo <- read.csv("demographic_rates_flooded_updated.csv")

species_params <- data.frame(
  sp = c("HBU", "SEI", "UL", "BAH", "SAH", "GES", "WLI"),
  a = c(0.00021491, 2.00333, 1.942950, 1.89756, 1.89756, 1.95277, 1.94295),
  b = c(2.258957614, 0.85925, 1.292290, 0.97716, 0.97716, 0.77206, 1.292290),
  c = c(0.001411006, -2.86353, -4.200640, -2.94253, -2.94253, -2.48079, -4.200640) 
)


write_csv(species_params, "species params.csv")
#write_csv(species_coefficients, "species coefficients.csv")

demo <- demo %>%
  left_join(species_params, by = "sp")

print(demo)

species_order <- c("BAH", "GES", "HBU", "SAH", "SEI", "UL", "WLI")

# Reorder spdata to match the species order
demo <- demo %>%
  arrange(factor(sp, levels = species_order)) 
print(demo)

write_csv(demo, "demographic_rates_carbon.csv")
write_tsv(demo, "demographic_rates_carbon.txt")
