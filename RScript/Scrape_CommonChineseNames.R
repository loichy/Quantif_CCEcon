# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, rvest, stringi)

# List directories 
dir <- list()
dir$root <- here()
dir$figures <- here(dir$root, "Figures")
dir$tables <- here(dir$root, "Tables")
dir$raw.data <- here(dir$root, "Raw_data")
dir$prep.data <- here(dir$root, "Prepared_data")
# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))


# Define the URL of the webpage to scrape
url <- "https://en.wikipedia.org/wiki/Chinese_given_name"  # Replace with your actual URL

# Read the webpage content
webpage <- read_html(url)

# Get table in the html page
table <- html_element(x = webpage, ".wikitable") %>% 
  html_table() 

# Pull chinese names in pinyin and change character encoding
common_chinese_names <- table %>% 
  select(`Pinyin romanization`) %>% 
  pull() %>% 
  stri_trans_general(id = "Latin-ASCII") %>% 
  unique()

save(common_chinese_names, file = here(dir$prep.data, "Common_Chinese_Names.Rdata"))

