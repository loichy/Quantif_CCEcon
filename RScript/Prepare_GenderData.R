#===============================================================================
# Description: This RCode prepares bibliographic data to analyze authorship by gender
# author: manon.eluard@dauphine.eu ; loic.henry@dauphine.psl.eu
#===============================================================================

# 0. Prepare working environment ---------------------------------------------------------------

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, lubridate, stringi, gender)

# List directories 
dir <- list()
dir$root <- here()
dir$figures <- here(dir$root, "Figures")
dir$tables <- here(dir$root, "Tables")
dir$raw.data <- here(dir$root, "Raw_data")
dir$prep.data <- here(dir$root, "Prepared_data")
# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))


# 1. Load data ---------------------------------------------------------------

# First load data: Corpus_Short
load(here(dir$raw.data,"Clean_Corpus_CC.Rdata"))

#10% of the original data to prepare cleaning code more efficiently : Corpus.Short.Small
Corpus.Short.Small <- Corpus %>% 
  # slice_sample(n = 60000) %>% 
  select(TI, AU, AB, SO, PY, DE, ID, AF, TopFive, Top25, TopField, Top10, Top30,
         TrunkJournals, C1, RP, TC, Z9, U1, U2, J9, JI, PG, CCPub_strict, 
         CCPub_large, CCPub_largebis, CCPub_narrow, CC)


# 2. Prepare variables for author names ----------------------------------------

# Creation of a new column "nb_authors" to count the number of authors per article
Corpus.Short.Small$nb_authors <- str_count(Corpus.Short.Small$AF, ";") + 1
# Display the 1000 largest values in the "nb_authors" column to see the maximum number of authors in the database. There are roughly 1500 articles that have more than 10 authors (0.2 % of the total so not very important).

# Notice that articles with more than 15 authors are very rare (less than 0.05%):
table(Corpus.Short.Small$nb_authors) # Number of articles by number of authors
sum(table(Corpus.Short.Small$nb_authors)[1:15])/length(Corpus.Short.Small$nb_authors) # Let's focus on articles with maximum 15 authors as this accounts for 99,95% of articles

# Remove articles with more than 15 authors and Creation of one column by author
Corpus.Short.Small.Filt <- Corpus.Short.Small %>%
  filter(nb_authors < 16) %>% # Only keep articles with 15 authors at max (99.95% of the corpus)
  separate(col = AF, into = paste0("author_", 1:15), sep = ";", remove = FALSE, extra = "warn") # I've just changed the extra argument with "warn", to check if there are any mistakes

# Check: number of articles by number of authors
table(Corpus.Short.Small.Filt$nb_authors)

# Then create a column with first name and another with last name
for (i in 1:15) {
  Corpus.Short.Small.Filt <- Corpus.Short.Small.Filt %>%
    separate(col = paste0("author_", i), into = c(paste0("l_name_", i), paste0("f_name_", i)), sep = ",", remove = FALSE, extra = "drop")
}

# 3. Clean character encoding ----------------------------------------
# Uniformize the newly created columns in terms of character encoding

Corpus.CleanedNames <- Corpus.Short.Small.Filt

for (i in 1:15) {
  print(i)
  # Supprimer les textes entre parenthèses :
  # Pour le prénom
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- gsub(pattern = "\\s*\\([^\\)]+\\)",
                                                      replacement = "",
                                                      x = Corpus.CleanedNames[[paste0("f_name_", i)]])
  # Pour le nom
  Corpus.CleanedNames[[paste0("l_name_", i)]] <- gsub(pattern = "\\s*\\([^\\)]+\\)",
                                                      replacement = "",
                                                      x = Corpus.CleanedNames[[paste0("l_name_", i)]])  
  
  # Supprimer les espaces supplémentaires : 
  # Pour le prénom
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- gsub(pattern = "\\s+",
                                                      replacement = " ",
                                                      x =  Corpus.CleanedNames[[paste0("f_name_", i)]])
  # Pour le nom
  Corpus.CleanedNames[[paste0("l_name_", i)]] <- gsub(pattern = "\\s+", 
                                                      replacement = " ",
                                                      x =  Corpus.CleanedNames[[paste0("l_name_", i)]])
  
  # Supprimer les espaces de début et de fin : 
  # Pour le prénom
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- trimws(Corpus.CleanedNames[[paste0("f_name_", i)]])
  # Pour le nom
  Corpus.CleanedNames[[paste0("l_name_", i)]] <- trimws(Corpus.CleanedNames[[paste0("l_name_", i)]])
  
  # Remove all accents and uniformize character encoding
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- stri_trans_general(Corpus.CleanedNames[[paste0("f_name_", i)]],
                                                                    "Latin-ASCII")
  Corpus.CleanedNames[[paste0("l_name_", i)]] <- stri_trans_general(Corpus.CleanedNames[[paste0("l_name_", i)]],
                                                                    "Latin-ASCII")
  
  # Créer une colonne fullname_i qui combine firstname et lastname avec un espace:
  
  Corpus.CleanedNames <- Corpus.CleanedNames %>% 
    mutate(!!paste0("fullname_", i) := ifelse(test = is.na(.[[paste0("f_name_", i)]]) & is.na(.[[paste0("l_name_", i)]]), 
                                              yes = NA,
                                              no = ifelse(
                                                test = is.na(.[[paste0("f_name_", i)]]),
                                                yes = .[[paste0("l_name_", i)]],
                                                no = ifelse(
                                                  test = is.na(.[[paste0("l_name_", i)]]), 
                                                  yes = .[[paste0("f_name_", i)]], 
                                                  no = paste(.[[paste0("f_name_", i)]],
                                                             .[[paste0("l_name_", i)]], 
                                                             sep = " ")
                                                )
                                              )
    )
    ) 
  
  
  # Second step cleaning: nettoyer la colonne first name pour améliorer l'identification des prénoms avec la commande gender
  # Supprimer les lettres suivies d’un point : 
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- gsub(pattern = "[A-Za-z]\\.", 
                                                      replacement = "",
                                                      x =  Corpus.CleanedNames[[paste0("f_name_", i)]])
  
  # Remplacer les ponctuations par des espaces (exemple des noms composés) : 
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- gsub(pattern = "[[:punct:]]", 
                                                      replacement = " ",
                                                      x = Corpus.CleanedNames[[paste0("f_name_", i)]])
  
  # Supprimer les espaces supplémentaires (double espaces potentiellement dûs au remplacement de la ponctuation par des espaces): 
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- gsub(pattern = "\\s+",
                                                      replacement = " ",
                                                      x =  Corpus.CleanedNames[[paste0("f_name_", i)]])
  
  # Supprimer les espaces de début et de fin : 
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- trimws(Corpus.CleanedNames[[paste0("f_name_", i)]])
  
  # If several first names only keep the first one:
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- map_chr(.x = Corpus.CleanedNames[[paste0("f_name_", i)]],
                                                         .f =  ~ strsplit(.x, " ")[[1]][1])
  
  
  # Supprimer les espaces de début et de fin (à nouveau, au cas où la dernière opération en a introduit):
  Corpus.CleanedNames[[paste0("f_name_", i)]] <- trimws(Corpus.CleanedNames[[paste0("f_name_", i)]])
  
  
}

# Check how it looks like
check <- Corpus.CleanedNames %>% 
  slice(1:20) %>% 
  select(matches("\\d$"))

# 4. Associate gender to any first names ----------------------------------------
# Create functions that compute gender probability for a given firstname
# First using ssa approach from gender package
get_gender_prob_ssa <- function(names){
  gender(names, method = "ssa")
}

#Apply function to all first names columns
gender_proba_ssa <- lapply(Corpus.CleanedNames[, grepl("^f_name_", colnames(Corpus.CleanedNames))], get_gender_prob_ssa)

# Combine results in a unique dataframe
gender_proba_ssa_df <- do.call(rbind, gender_proba_ssa)

# Create a tibble that for each unique first name give the proba for the name to be female and male
gender_proba_ssa_df_2 <- gender_proba_ssa_df %>%
  group_by(name) %>%
  slice(1)

# Second using naap approach:
get_gender_prob_napp <- function(names)
{
  gender(names, 
         method = c("napp"),
         countries = c("Canada", "United Kingdom", "Denmark", "Iceland","Norway", "Sweden")
  )
}

gender_proba_napp <- lapply(Corpus.CleanedNames[, grepl("^f_name_", colnames(Corpus.CleanedNames))], get_gender_prob_napp)

gender_proba_napp_df <- do.call(rbind, gender_proba_napp)

gender_proba_napp_df_2 <- gender_proba_napp_df %>%
  group_by(name) %>%
  slice(1) 

# Only keep names not in gender_proba_df_2 and add them:
gender_proba_napp_df_3 <- gender_proba_napp_df_2 %>%
  filter(!(name %in% gender_proba_ssa_df_2$name)) 

gender_proba_df <- gender_proba_ssa_df_2 %>% 
  bind_rows(gender_proba_napp_df_3) %>% # Add them to the first names from ssa and put that in a new object
  arrange(name)

# Final object:
names_proba_df <- gender_proba_df  # All first names that for which we have an assigned gender probability

# 5. Associate gender to all authors (gender algorithm) ----------------------------------------

# Load data of top 10% male economists, top 10% female economists and common chinese names
load(here(dir$prep.data, "FemaleEconomists_Names.Rdata")) # Generated from R script `Scrape_FemaleEconomistsName.R`
load(here(dir$prep.data, "Common_Chinese_Names.Rdata")) # Generated from R script `Scrape_CommonChineseNames.R`
load(here(dir$prep.data, "MaleEconomists_Names.Rdata")) # Generated from R script `Scrape_MaleEconomistsName.R`

# Remove `Juan` from chinese name lists (as it also refers to Juan Latin/Spanish first name given to male)
common_chinese_names <- common_chinese_names[common_chinese_names!="Juan"]

Corpus.CleanedNames.2 <- Corpus.CleanedNames
# Algorithm that create a gender variable. But it also adds for all authors their probability of male firstname, and their probability of female first name
# Gender is male if non-missing full name and in top 10% of male economists, or if non-missing first-name and probability of first name to be male is higher than 90%
# Gender is female if non-missing full name and in top 10% of female economists, or if non-missing first-name and probability of first name to be male is lower than 10%
# Gender is undecided if non-missing first name and probability of first name to be male is between 10% and 90%
# Gender is unknown if non-missing first name, and either the first-name is a common chinese first name or the first name is not found using the "gender" command
for(i in 1:15) {
  print(i)
  Corpus.CleanedNames.2 <- Corpus.CleanedNames.2 %>%
    # First create variable gender (takes value: NA, unknown, male and female)
    mutate(
      !!paste0("gender_", i) := ifelse(
        test = is.na(.[[paste0("fullname_", i)]]), # 1. if full name is missing in Corpus.CleanedNames.2 gender = NA
        yes = NA,
        no = ifelse( 
          test = .[[paste0("fullname_", i)]] %in% top10_female_economists, # 2. If full name is in the list of top 10% of female economists gender = female
          yes = "female",
          no = ifelse(
            test = .[[paste0("fullname_", i)]] %in% top10_male_economists, # 3. If full name is in the list of top 10% of male economists gender = male
            yes = "male",
            no = ifelse(
              test = is.na(.[[paste0("f_name_", i)]]), # 4. If first name is missing
              yes = NA, 
              no = ifelse(
                test = .[[paste0("f_name_", i)]] %in% common_chinese_names, # 5. if first name is in the list of common chinese first names gender = unknown
                yes = "unknown",
                no = ifelse(
                  test = !(.[[paste0("f_name_", i)]] %in% names_proba_df$name), # 6. If first name is absent from the list of first names with measured gender probability (absent from names_proba_df) gender = NA
                  yes = "unknown",
                  no = case_when(
                    names_proba_df$proportion_male[match(.[[paste0("f_name_", i)]], names_proba_df$name)] > 0.9 ~ "male",
                    names_proba_df$proportion_male[match(.[[paste0("f_name_", i)]], names_proba_df$name)] < 0.1 ~ "female",
                    names_proba_df$proportion_male[match(.[[paste0("f_name_", i)]], names_proba_df$name)] >= 0.1 & names_proba_df$proportion_male[match(.[[paste0("f_name_", i)]], names_proba_df$name)] <= 0.9 ~ "undecided"
                    )
                  )
                )
              )
            )
          )
      )
    ) %>%
    mutate(
      !!paste0("proportion_male_", i) := ifelse(
        test = is.na(.[[paste0("fullname_", i)]]), # Create the variable that gives the probability that author is male, only if gender is assigned
        yes = NA,
        no = ifelse(
          test = .[[paste0("fullname_", i)]] %in% top10_female_economists,
          yes = 0,
          no = ifelse(
            test = .[[paste0("fullname_", i)]] %in% top10_male_economists,
            yes = 1,
            no = ifelse(
              test = is.na(.[[paste0("f_name_", i)]]),
              yes = NA,
              no = ifelse(
                test = .[[paste0("f_name_", i)]] %in% common_chinese_names,
                yes = NA,
                no = ifelse(
                  test = !(.[[paste0("f_name_", i)]] %in% names_proba_df$name),
                  yes = NA,
                  no = ifelse(
                    test = .[[paste0("gender_", i)]] %in% c("male", "female"),
                    yes = names_proba_df$proportion_male[match(.[[paste0("f_name_", i)]], names_proba_df$name)],
                    no = NA
                  )
                )
              )
            )
          )
        )
      ),
      !!paste0("proportion_female_", i) := ifelse(
        test = is.na(.[[paste0("fullname_", i)]]), # Create the variable that gives the probability that author is female, only if gender is assigned
        yes = NA,
        no = ifelse(
          test = .[[paste0("fullname_", i)]] %in% top10_female_economists,
          yes = 1,
          no = ifelse(
            test = .[[paste0("fullname_", i)]] %in% top10_male_economists,
            yes = 0,
            no = ifelse(
                test = is.na(.[[paste0("f_name_", i)]]), 
                yes = NA, 
                no = ifelse(
                  test = .[[paste0("f_name_", i)]] %in% common_chinese_names,
                  yes = NA,
                  no = ifelse(
                    test = !(.[[paste0("f_name_", i)]] %in% names_proba_df$name),
                    yes = NA,
                    no = ifelse(
                      test = .[[paste0("gender_", i)]] %in% c("male", "female"),
                      yes = names_proba_df$proportion_female[match(.[[paste0("f_name_", i)]], names_proba_df$name)],
                      no = NA
                      )
                    )
                  )
                )
            )
          )
        ),
      nb_authors_gendered = rowSums(!is.na(select(., starts_with("proportion_male")))), # Count the number of authors with identified gender
      ratio_identified_gender = nb_authors_gendered / nb_authors # Among authors, the proportion of authors with identified gender
    )
}

# Check results
check <- Corpus.CleanedNames.2 %>% 
  slice(1:30) %>% 
  select(matches("\\d$"), nb_authors_gendered, nb_authors)

# 6. Create a dataframe with all individual authors and their gender ----------------------------------------
all_firstnames_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^f_name_", colnames(Corpus.CleanedNames.2))])
all_lastnames_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^l_name_", colnames(Corpus.CleanedNames.2))])
all_fullnames_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^fullname_", colnames(Corpus.CleanedNames.2))])
all_fullnames_incorpus2 <- unlist(Corpus.CleanedNames.2[, grepl("^author_", colnames(Corpus.CleanedNames.2))]) # This is the column of original author information
all_gender_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^gender_", colnames(Corpus.CleanedNames.2))])
all_proportionmale_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^proportion_male_", colnames(Corpus.CleanedNames.2))])
all_proportionfemale_incorpus <- unlist(Corpus.CleanedNames.2[, grepl("^proportion_female_", colnames(Corpus.CleanedNames.2))])

authors_df <- tibble(firstname = all_firstnames_incorpus, #base de données générale pour les auteurs
                     lastname = all_lastnames_incorpus,
                     fullname = all_fullnames_incorpus, # Clean full name
                     fullname2 = all_fullnames_incorpus2, # Raw full name
                     gender = all_gender_incorpus,
                     proportion_male = all_proportionmale_incorpus,
                     proportion_female = all_proportionfemale_incorpus) %>% 
  # filter(!is.na(firstname)) %>% 
  filter(!is.na(fullname2)) %>% # Only keep existing authors
  #mutate(fullname = trimws(fullname)) %>% 
  group_by(firstname, lastname) %>% # Consider a row full name to be a unique author (our algorithm would not distinguish authors with similar full names, so let's do so)
  mutate(n_articles = n()) %>%  # Number of articles for the authors with this full name
  slice(1) %>% #valeurs uniues
  arrange(fullname2) %>%
  ungroup()

# 7. Compute the share of gendered authorship for each article ----------------------------------------
# First, identify all columns proportion male
proportion_male_cols <- grep("^proportion_male_", colnames(Corpus.CleanedNames.2), value = TRUE)
proportion_female_cols <- grep("^proportion_female_", colnames(Corpus.CleanedNames.2), value = TRUE)

# Sum male and female probabilities of all authors in the article, only if at least one author is gendered
Corpus.CleanedNames.2 <- Corpus.CleanedNames.2 %>% 
  mutate(
    sum_gender_male = ifelse(
      test = nb_authors_gendered == 0,
      yes = NA,
      no = rowSums(Corpus.CleanedNames.2[proportion_male_cols], na.rm = TRUE)
      ),
    sum_gender_female = ifelse(
      test = nb_authors_gendered == 0,
      yes = NA,
      no = rowSums(Corpus.CleanedNames.2[proportion_female_cols], na.rm = TRUE)
    )
  )

# Derive share of male and female authors aong all authors and among only authors with identified gender
Corpus.CleanedNames.2 <- Corpus.CleanedNames.2 %>% 
  mutate(# Male
    proportion_gender_male_all = ifelse(
      test = nb_authors_gendered == 0, 
      yes = NA, 
      no = sum_gender_male / nb_authors
      ),
    proportion_gender_male_id = ifelse(
      test = nb_authors_gendered == 0,
      yes = NA,
      no =  sum_gender_male / nb_authors_gendered
      ), #Then female:
    proportion_gender_female_all = ifelse(
      test = nb_authors_gendered == 0,
      yes = NA,
      no = sum_gender_female / nb_authors
      ),
    proportion_gender_female_id = ifelse(
      test = nb_authors_gendered == 0,
      yes = NA, 
      no = sum_gender_female / nb_authors_gendered
      )
    )

# 8. Generate variable for gender type of co-authorship -----------------------
# First, create binary variable telling if there is at least one male, and one female
Corpus.CleanedNames.2 <- Corpus.CleanedNames.2 %>%
  # Distinguish case with two authors or more, with case with just one author
  mutate(
    AtLeastOneMale = ifelse(
      test = is.na(sum_gender_male),
      yes = NA,
      no = ifelse(
        test = sum_gender_male > 0.9,
        yes = 1,
        no = 0
        )
      ),
    AtLeastOneFemale = ifelse(
      test = is.na(sum_gender_female),
      yes = NA,
      no = ifelse(
        test = sum_gender_female > 0.9,
        yes = 1,
        no = 0)
      )
    )

# Then, three categories of co-authorship types: MF, MM, and FF
Corpus.CleanedNames.2 <- Corpus.CleanedNames.2 %>%
  mutate(
    category_gender = case_when(
      is.na(sum_gender_female) ~ NA,
      AtLeastOneMale == 0 & nb_authors_gendered == 1 ~ "F",
      AtLeastOneFemale == 0 & nb_authors_gendered == 1 ~ "M",
      AtLeastOneFemale == 0 & nb_authors_gendered > 1 ~ "MM",
      AtLeastOneMale == 0 & nb_authors_gendered > 1 ~ "FF",
      AtLeastOneMale == 1 & AtLeastOneFemale == 1 ~ "MF"
    )
  )

# Vérification des résultats
table(Corpus.CleanedNames.2$category_gender, useNA = "ifany")

# 9. Save objects ----------------------------------------
save(authors_df, Corpus.CleanedNames.2, file = here(dir$prep.data, "GenderedAuthors_Data.Rdata"))


