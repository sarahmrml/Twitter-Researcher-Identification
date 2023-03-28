library(openalexR)
library(tidyverse)
library(quanteda)
library(dplyr)
library(stringr)
library(gender)

####################################
####### PsycInfo (from OVID) #######
####################################
# input: DOI vector from PsycInfo (gender, location), ovid_orcid_aggr (subdiscipline)
# output: location (location_raw-RDS, location_absfreq.RDS), gender (prenames_raw.RDS, gender_prenames.RDS), subdiscipline (OVID_subdis_one.RDS, OVID_subdis_multiple.RDS)))

### prepare dataset
clean_OVID_2020_DOI_url <- na.omit(OVID_2020_DOI)
clean_OVID_2020_DOI_doi <- gsub("http://dx.doi.org/", "", clean_OVID_2020_DOI_url)

# get openalex data from doi
paper_info <- oa_fetch(entity = "works", doi = clean_OVID_2020_DOI_doi)
paper_info_unnested <- unnest(paper_info, author)
oa_id_url <- unique(paper_info_unnested$au_id)
oa_id <- gsub("https://openalex.org/", "", oa_id_url)

#get author info from oa_id, in loop because max input=50
# create dfs first row
author_info <- oa_fetch(entity = "authors", openalex = oa_id[1])
l <- length(oa_id)
#loop through vector and append as new row to df
for (i in 2:l) {
  tryCatch({
    new <- oa_fetch(entity = "authors", openalex = oa_id[i])                     
    author_info[nrow(author_info) + 1, ] <- new
  }, error = function(e){
    print(i)
    print(e)})  
}

#extract psychologists
# unnest concepts, # filter for psychologists (= psychology concept, Ebene 0, concept_score >= 50), # extract openalex id
author_info_unnested <- unnest(author_info, x_concepts, names_repair = "universal")
author_info_psych_filter <- author_info_unnested %>% filter(display_name...17 == "Psychology", level == 0, score >= 50)
oa_id_psych <- author_info_psych_filter$id...1
# extract psychologists from author_info (nested)
author_info_psych <- author_info[author_info$id %in% oa_id_psych ,]
saveRDS(author_info_psych, file = "author_info_psych.RDS")
# --> go on working with author_info_psych df (gender, location)


### gender (from prenames)
prenames <- word(author_info_psych$display_name, 1)
gender_prenames <- gender(prenames)
gender_prenames_freq <- table(gender_prenames$gender)
missed_names_gender <- length(prenames) - length(gender_prenames$name)
gender_prenames_freq$missed_names <- missed_names_gender
saveRDS(prenames, file = "prenames_raw.RDS")
saveRDS(gender_prenames, file = "gender_prenames.RDS")
saveRDS(gender_prenames_freq, file = "gender_prenames_absfreq.RDS")


### location (from affiliation country code)
loc <- author_info_psych$affiliation_country_code
loc_freq <- sort(table(loc), decreasing = TRUE)
saveRDS(loc, file = "location_raw.RDS")
saveRDS(loc_freq, file = "location_absfreq.RDS")


### subdiscipline
# import dataframe (df) of PsycInfo authors with ORCID IDs (ovid_orcid_aggr)
# df consists of 4 variables: 
# 1. "ORCID" (links to ORCID ID) 
# 2. "CC" (complete APA PsycInfo classification codes (CCs), incl. subdis-label)
# 3. "CC_code" (complete extracted CCs, excl. subdis-label, only numbers)
# 4. "CC_code_main" (only first two numbers of CCs)
ovid_orcid_aggr <- readRDS("~/ovid_orcid_aggr.RDS")

# create a document-feature matrix
DFM <- dfm(tokens(ovid_orcid_aggr$CC_code_main))

# get the unique feature names and assign them to our defined psychological subdisciplines
featnames(DFM)
# "28" "23" "36" "30" "32" "25" "29" "33" "34" "21" "22" "35" "31" "24" "39" "27" "38" "26" "37" "40" "41" "42"
# more information about CC assignment in supplemental material (table 1)

# combine columns and create new subdis_df
DFM_df <- as.data.frame(DFM)
DFM_df$clinical <- (DFM_df$`32` + DFM_df$`33`)
DFM_df$cognitive <- (DFM_df$`21` + DFM_df$`23` + DFM_df$`24` + DFM_df$`26` + DFM_df$`27` + DFM_df$`41`)
DFM_df$educational <- (DFM_df$`35`)
DFM_df$iopsych <- (DFM_df$`34` + DFM_df$`36` + DFM_df$`39`)
DFM_df$neuro <- (DFM_df$`25`)
DFM_df$social <- (DFM_df$`29` + DFM_df$`30`)
DFM_df$forensic <- (DFM_df$`38` + DFM_df$`42`)
DFM_df$statistics <- (DFM_df$`22`)
DFM_df$dev <- (DFM_df$`28`)
DFM_df$sport <- (DFM_df$`37`)
DFM_df$diff <- (DFM_df$`31`)
DFM_df$environment <- (DFM_df$`40`)

subdis_df <- data.frame(clinical = DFM_df$clinical, cognitive = DFM_df$cognitive, educational = DFM_df$educational, 
                        iopsych = DFM_df$iopsych, neuro = DFM_df$neuro, social = DFM_df$social, forensic = DFM_df$forensic, 
                        statistics = DFM_df$statistics, dev = DFM_df$dev, sport = DFM_df$sport, diff = DFM_df$diff, environment = DFM_df$environment)


## assignments
## one assignment (main analysis) 
# create new column for assignments and loop through rows to assign author (=row) to subdiscipline (as column numbers; 1=clinical, etc.)
# winner takes it all (= subdis assignment as column numbers), no clear winner -> 0
subdis_df$assignment_one <- 0

for (i in 1:nrow(subdis_df)) {
  tryCatch({
    subdis_df[i,13] <- (which(subdis_df[i,1:12] == max(subdis_df[i,1:12])))
  }, error = function(e){
    print(i)
    print(e)})
}

# get freqs and save df
sort(table(subdis_df$assignment_one), decreasing = TRUE)  
saveRDS(subdis_df, file = "OVID_subdis_one.RDS")


## multiple assignments (robustness-check)
# get subset df without assignment column and replace all values unequal zero with 1
subdis_df_multiple = subset(subdis_df, select = - assignment_one)
subdis_df_multiple[subdis_df_multiple != 0] <- 1

# get freqs and save df
sort(colSums(subdis_df_multiple), decreasing = TRUE)
saveRDS(subdis_df_multiple, file = "OVID_subdis_multiple.RDS")



####################
###### Twitter #####
####################
# input: lookups
# output: location (location_absfreq_twitter.RDS), gender (gender_distribution_twitter.RDS), subdisciplines (subdis_twitter_onematch, subdis_twitter_minone)

lookups <- readRDS("~/Twitter/lookups.RDS")

### location
loc_twitter <- lookups$country
loc_freq_twitter <- sort(table(loc_twitter), decreasing = TRUE)
saveRDS(loc, file = "location_raw_twitter.RDS")
saveRDS(loc_freq, file = "location_absfreq_twitter.RDS")


### gender
lookups_names <- data.frame("names" = (na.omit(lookups$name)))

# df names with and without title
names_title <- lookups_names %>% filter(str_detect(names, "Professor |Prof. | Prof |Dr. |Dr "))
names_notitle <- lookups_names %>% filter(!(lookups_names$names %in% names_title$names))

# extract prenames
prenames_title <- word(names_title$names, 2)
prenames_notitle <- word(names_notitle$names)

# get gender distribution
gender_1_raw <- gender(prenames_title)
gender_2_raw <- gender(prenames_notitle)

gender_1_freq <- table(gender_1_raw$gender)
gender_2_freq <- table(gender_2_raw$gender)
missing_gender <- length(lookups_names$names) - (sum(gender_1_freq[1], gender_1_freq[2], gender_2_freq[1], gender_2_freq[2]))
gender_distribution <- data.frame("female" = gender_1_freq[1] + gender_2_freq[1], "male" = gender_1_freq[2] + gender_2_freq[2], "missing" = missing_gender)

saveRDS(gender_1_raw, file = "./Twitter/gender1_raw_twitter.RDS")
saveRDS(gender_2_raw, file = "./Twitter/gender2_raw_twitter.RDS")
saveRDS(gender_distribution, file = "./Twitter/gender_distribution_twitter.RDS")


### subdisciplines
# create new df for subdis assignment
subdis_twitter <- data.frame("description" = lookups$description)
subdis_twitter$clinical <- 0
subdis_twitter$cognitive <- 0
subdis_twitter$developmental <- 0
subdis_twitter$diff <- 0
subdis_twitter$educational <- 0
subdis_twitter$environmental <- 0
subdis_twitter$forensic <- 0
subdis_twitter$history <- 0
subdis_twitter$iopsych <- 0
subdis_twitter$media <- 0
subdis_twitter$neuro <- 0
subdis_twitter$positive <- 0
subdis_twitter$social <- 0
subdis_twitter$sport <- 0
subdis_twitter$statistics <- 0

# code 1 for detection of defined search terms in description
for (i in 1:nrow(subdis_twitter)) {
  subdis_twitter$clinical[i] <- ifelse(grepl("clinical|psychotherap", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$clinical[i])
  subdis_twitter$cognitive[i] <- ifelse(grepl("cogniti|experimental", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$cognitive[i])
  subdis_twitter$developmental[i] <- ifelse(grepl("development|pediatric", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$developmental[i])
  subdis_twitter$diff[i] <- ifelse(grepl("differential", subdis_twitter$description[i], ignore.case = TRUE) |
                                     grepl("personality", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$diff[i])
  subdis_twitter$educational[i] <- ifelse(grepl("education", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$educational[i])
  subdis_twitter$environmental[i] <- ifelse(grepl("environment|climate", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$environmental[i])
  subdis_twitter$forensic[i] <- ifelse(grepl("forensic|law", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$forensic[i])
  subdis_twitter$history[i] <- ifelse(grepl("history", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$history[i])
  subdis_twitter$iopsych[i] <- ifelse(grepl("industrial|organizational|org psycholog|io psychology|iopsych", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$iopsych[i])
  subdis_twitter$media[i] <- ifelse(grepl("media", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$media[i])
  subdis_twitter$neuro[i] <- ifelse(grepl("neuro", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$neuro[i])
  subdis_twitter$positive[i] <- ifelse(grepl("positive psycho", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$positive[i])
  subdis_twitter$social[i] <- ifelse(grepl("social", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$social[i])
  subdis_twitter$sport[i] <- ifelse(grepl("sport psy", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$sport[i])
  subdis_twitter$statistics[i] <- ifelse(grepl("statistic|quantitative methods", subdis_twitter$description[i], ignore.case = TRUE), 1, subdis_twitter$statistics[i])
  print(i)
} 

# get freqs
colSums(subdis_twitter[,-1])

# save
saveRDS(subdis_twitter, file = "./Twitter/subdis_twitter.RDS")

# extract sub df with no assignments/matches 
subdis_twitter_nomatch <- subset(subdis_twitter, clinical + cognitive + developmental + diff + educational + environmental + 
                                   forensic + history + iopsych + media + neuro + positive + social + sport + statistics == 0)

# extract sub df with only one assignment (df for "only one assignment possible")
subdis_twitter_onematch <- subset(subdis_twitter, clinical + cognitive + developmental + diff + educational + environmental + 
                                   forensic + history + iopsych + media + neuro + positive + social + sport + statistics == 1)

# extract sub df with at least one assignment (df for "multiple assignments possible")
subdis_twitter_minone <- subset(subdis_twitter, clinical + cognitive + developmental + diff + educational + environmental + 
                                   forensic + history + iopsych + media + neuro + positive + social + sport + statistics > 0)

# extract sub df with more than one matc
subdis_twitter_multiplematches <- subset(subdis_twitter, clinical + cognitive + developmental + diff + educational + environmental + 
                                   forensic + history + iopsych + media + neuro + positive + social + sport + statistics > 1)


### get assignment freqs
# only one assignment possible
colSums(subdis_twitter_onematch[,-1])

# multiple assignments possible
colSums(subdis_twitter_minone[,-1])