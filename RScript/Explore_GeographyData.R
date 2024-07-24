##### Prepare working environment
# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, lubridate, ggmap, geosphere, stringr, 
               gender, genderdata, stringi, dplyr, tidyr, genderizeR, kableExtra, ggplot2,
               plm, fixest, stargazer, lmtest, modelsummary, tictoc, countries, stringr)

# List directories 
dir <- list()
dir$root <- here()
dir$figures <- here(dir$root, "Figures")
dir$tables <- here(dir$root, "Tables")
dir$raw.data <- here(dir$root, "Raw_data")
dir$prep.data <- here(dir$root, "Prepared_data")
# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#####
load(here(dir$prep.data, "Corpus_Country_PY.Rdata"))
countries <- unique(Country_PY_df$country)
years <- unique(Country_PY_df$PY)
df_final <- expand.grid(country = countries, PY = years)

df_final_join <- df_final %>% 
  left_join(Country_PY_df, by = c("country", "PY")) %>% 
  mutate_all(~ ifelse(is.na(.), 0, .))

#####
df_geo <- read.csv(file = here(dir$prep.data, "df_Final_merged.csv"), header = T)
