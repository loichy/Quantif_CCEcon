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
url <- "https://ideas.repec.org/top/top.women.html"  # Replace with your actual URL

# Read the webpage content
webpage <- read_html(url)

# Two steps: first scrape names in the tables, and then scrape names in the lists of names for the top 6%, top 7%, ..., top 10%

# First step: scrape the table of names
# Select the rows within the table with the class "shorttop"
rows <- webpage %>% 
  html_nodes("table.shorttop tr") #Use CSS selectors to locate the rows (<tr>) within the table with the class shorttop.

# Extract the content of the first <a> tag within the second <td> tag of each row
names_1 <- rows %>%
  map(~ .x %>% # iterate over each element of rows
        html_node("td:nth-child(2) a") %>% #selects all <tr> tags within the table. html_node("td:nth-child(2) a") selects the first <a> tag within the second <td> tag of each row
        html_text()) %>%
  unlist() %>%  # convert the list of extracted elements in a vector
  gsub(pattern = " +", replacement = " ") %>%  # Remove any double space or more by a single space
  gsub(pattern = "†", replacement = "") %>% # Remove the † symbol
  stri_trans_general(id = "Latin-ASCII") %>% 
  trimws() # Remove any white space in the beginning or in the end
head(names_1[!is.na(names_1)])

# Second step: get the list of names in the bottom of the page
# Select the <a> nodes using the provided XPath
lists <- webpage %>% 
  html_nodes(xpath = '//*[@id="ranking"]/a')

# Extract the text from each <a> tag and clean it
names_2 <- lists %>% 
  html_text() %>% 
  gsub(pattern = " +", replacement = " ") %>%  # Remove any double space or more by a single space
  gsub(pattern = "†", replacement = "") %>% # Remove the † symbol
  stri_trans_general(id = "Latin-ASCII") %>% # Convert to same character encoding
  trimws() # Remove any white space in the beginning or in the end
head(names_2[!is.na(names_2)])
# Combine both vectors of names and keep only unique names
top10_female_economists <- c(names_1[!is.na(names_1)], names_2) %>% 
  unique()


save(top10_female_economists, file = here(dir$prep.data, "FemaleEconomists_Names.Rdata"))
