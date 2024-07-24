#===============================================================================
# Description: This RCode prepares Corpus data to add countries' location from each affiliations
# author: loic.henry@dauphine.psl.eu
#===============================================================================


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

# Charge les données carbon tax
carbontax_df <- read.csv(file = here(dir$raw.data, "carbon_tax.csv"), header = T, sep = ";", dec = ",")

years <- c(1990:2023)

carbontax_df <- carbontax_df %>% 
  filter(Type == "National Carbon tax") %>% 
  slice(1:30) %>% 
  mutate(ImplementationYear = as.numeric(substring(Status, 16,19)))

carbontax_long_df <- carbontax_df %>% 
  slice(rep(1:n(), each = 34)) %>%
  group_by(Country) %>% 
  mutate(year = seq(from=1990, to=2023)) %>% 
  ungroup() %>% 
  mutate(is_carbontax = ifelse(year < ImplementationYear, yes = 0, no = 1)) %>% 
  select(Country, year, ImplementationYear, is_carbontax, Share.of.jurisdiction.emissions.covered, Price.on.1.April,X2024) %>% 
  arrange(Country, year) 

write.csv(x = carbontax_long_df, file = here(dir$prep.data, "carbontax.csv"), row.names = F)

# Appareiller les données
# Avant tout: créer des observations pays-années avec des valeurs de variable 0 pour les années sans publication
# Et les ajouter au tableau country py df
load(here(dir$prep.data, "Corpus_Country_PY.Rdata"))
countries <- unique(Country_PY_df$country)
years <- unique(Country_PY_df$PY)
df_final <- expand.grid(country = countries, PY = years)

df_final_join <- df_final %>% 
  left_join(Country_PY_df, by = c("country", "PY")) %>% 
  mutate_all(~ ifelse(is.na(.), 0, .))

### Stat descriptives
# Créer le tableau qui  Aggrége  les publis parpays pour toutes les années
df_aggreg_countries <- df_final_join %>% 
  group_by(country) %>% 
  summarize(N_CC_country = sum(N_CC, na.rm = T),
            mean_TC_country = mean(mean_TC, na.rm = T))

### Faire une carte
# Ajout de la colonne geometry : Créer un shapefile (une data frame géospatialisée)
# Install and load necessary packages
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Get the world country polygons
world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  mutate(ISO3 = iso_a3_eh)
world$ISO3
str(world)
# Add ISO3 variable to merge world and aggreg df country (not needed if already iso3)
temp <- countries::country_reference_list
association_table <- temp %>%
  select(Name0, ISO3, simple) %>% 
  mutate(Country.Code = ISO3) %>% 
  select(ISO3,Name0) %>% 
  rename(country=Name0)

df_aggreg_countries_ISO3 <- df_aggreg_countries %>% 
  left_join(association_table,by="country")

# Merge the data frame with the geometries
sf_aggreg_countries_ISO3 <- df_aggreg_countries_ISO3 %>% 
  left_join(world, by = "ISO3") %>% 
  st_as_sf()

ggplot(sf_aggreg_countries_ISO3) +
  geom_sf(aes(fill=N_CC_country))

