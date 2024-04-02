#===============================================================================
# Description: This RCode prepare Manon's internship data
# author: loic.henry@dauphine.psl.eu
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, lubridate, ggmap, geosphere)

# List directories 
dir <- list()
dir$root <- here()
dir$figures <- here(dir$root, "Figures")
dir$tables <- here(dir$root, "Tables")
dir$raw.data <- here(dir$root, "Raw_data")
dir$prep.data <- here(dir$root, "Prepared_data")
# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))


### This Rcode is to prepare shorter data files (less heavy).


## First with full corpus, but without citation variable
load(here(dir$raw.data,"Clean_Corpus_CC.Rdata"))

# Filter to reduce size of data: only keep Articles published after 2010
Corpus.Short = Corpus %>% 
  filter(PY > 2010)
# table(is.na(Corpus$PY))
head(Corpus.Short)

# Save the short datafile
save(Corpus.Short, df_JEL_BinaryVar_Full, file = here(dir$prep.data, "Corpus_Short.Rdata"))

## Second with Wos and Scopus data containing cited references as a variable 

# Filter to reduce size of data: only keep Articles published after 2017
# First Wos
gc()
load(here(dir$raw.data,"Clean_Wos_Econ.Rdata"))
Wos.Econ.Short = Wos_clean %>% 
  filter(PY > 2017)
save(Wos.Econ.Short, file = here(dir$prep.data, "Wos_Short.Rdata"))
#Then Scopus
gc()
load(here(dir$raw.data,"Clean_Scopus_Econ.Rdata"))
Scopus.Econ.Short = Scopus_clean %>% 
  mutate(PY = as.numeric(PY)) %>% 
  filter(PY > 2017)
save(Scopus.Econ.Short, file = here(dir$prep.data, "Scopus_Short.Rdata"))