library(tidyverse)

rm(list=ls())
gc()

source("modified_PPA.R")
main_cohorts_tidy(mainfolder = "/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/Inter_casestudy/Carbon_stocks",
                  initdata_path = "initial_state_flooded_p81_82.txt",
                  spdata_path = "demographic_rates_carbon.txt")

