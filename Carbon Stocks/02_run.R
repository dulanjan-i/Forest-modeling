library(tidyverse)

rm(list=ls())
gc()

source("01_PPA_model_tidy.R")
main_cohorts_tidy(mainfolder = "/Users/dulanjanwijenayake/Library/CloudStorage/OneDrive-HiroshimaUniversity/Inter_casestudy/Flood",
                  initdata_path = "PPA_initial_state_moist13_no_FAH.txt",
                  spdata_path = "demographic_rates_flooded_updated.txt")

