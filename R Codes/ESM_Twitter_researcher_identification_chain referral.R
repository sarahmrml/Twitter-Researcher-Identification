# Identify psychology researchers on Twitter #
# # # # # # # # # # # # # # # # # # # # # # #

# Andre Bittermann, ZPID, Trier, abi@leibniz-psychology.org


# Snowballing psychological researchers from the German-speaking countries
# Similar to Ke et al. (2017), https://doi.org/10.1371/journal.pone.0175368
# with preferred checking of user profile descriptions



# Workflow ----------------------------------------------------------------

# Preperations:
# 1. save vector of initial seed account screen_names as "checked_users" and "all_seeds"
# 2. save timelines of initial seed accounts as "timelines_all"

# Repeat i times:
# 1. Load timelines of new seed accounts (timelines_new_seeds)
# 2. Get mentioned Twitter users in their tweets (lookups)
# 3. Use profile descriptions for researcher identification (lookups_psych_rank)
# 4. Get and save account names and timelines of all identified researchers (timelines_new_seeds)

# Resulting objects, that grow with each iteration:
# checked_users = all checked account
# all_seeds = all identified psychological researchers
# timelines_all = timelines of all_seeds


# API rate limit note:
# for memberships, only 75 requests per 15 minutes can be made
# for getting timelines, 3 minutes waiting time after 90 accounts


# Data protection note: 
# The folder "./researcher_identification" should be protected with
# password and securely stored as it contains non-anonymized data



# libraries ---------------------------------------------------------------

library(rtweet)
library(textclean)
library(quanteda)
quanteda_options("threads" = 7)



# rtweet token ------------------------------------------------------------

# token <- create_token(
#   app = "xxx",
#   consumer_key = "xxx",
#   consumer_secret = "xxx")



# objects -----------------------------------------------------------------

## Academic rank according to Ke et al. (2017), https://doi.org/10.1371/journal.pone.0175368.s001

# student: phd student, phd candidate, graduate student, grad student, doctoral student
# postdoc: postdoc, post-doc, postdoctoral
# professor: assistant professor, assistant prof, asst prof, associate professor, 
# associate prof, assoc prof, professor, prof, faculty

# modified with additional terms:

terms_student <- c("phd_student", "phd_candidate", "graduate_student", "grad_student", "doctoral_student", 
                   "doktorand", "doktorandin")
terms_postdoc <- c("postdoc", "postdoctoral", "post_doc")
terms_prof <- c("professor", "prof", "faculty", "professorin", "junprof", "jun_prof", "jprof") 

# additional personal descriptions
terms_researcher <- c("researcher", "scientist", "research_associate", "research_fellow", "lecturer", "scholar",
                      "experimental", "forscher", "forscherin", "wissenschaftler", "wissenschaftlerin", 
                      "akademiker", "akademikerin")




# functions ---------------------------------------------------------------


# lookup more than 90,000 users
lookup_many_users <- function(x, y = token){
  
  cat(paste("Looking up", length(x), "users.\n# "))
  
  if (length(x) <= 90000){            # if number of users is below rate limit
    cat("Number of users below rate limit. No waiting time.\n# ")
    return(lookup_users(x, token = y))
    
    
  } else {                            #  number of users is above rate limit
    lookups_list_many <- list()
    count <- 0
    gather_count <- 1
    
    while (count <= length(x)){
      
      cat(paste("Gather count:", gather_count, "\n# "))
      
      if ((count + 90000) > length(x)){ # length(x) is limit
        end_jjj <- length(x)
      } else {
        end_jjj <- count + 90000 # 90000 lookups at a time
      }
      
      lookups_list_many[[gather_count]] <- lookup_users(x[(count+1):end_jjj], token = y)
      
      count <- count + 90000
      gather_count <- gather_count + 1
      
      cat(paste("\n", Sys.time(), "\nIteration", count/90000, "of", ceiling(length(x)/90000)))
      
      if (end_jjj != length(x)){ # don't wait after last iteration
        Sys.sleep(15*60) # wait 15 minutes (in seconds)
        cat("\nNow waiting 15 minutes for Twitter API rate limit to reset...\n# ")
      }
      
    }
    
    # bind list entries
    
    cat("Now binding rows.\n# ")
    
    result <- lookups_list_many[[1]]
    
    for (ii in 2:length(lookups_list_many)){
      result <- rbind(result, lookups_list_many[[ii]])
    }
    
    return(result)
  }
  
}


# check if any of the rank terms are included in tokenized string
rank_check <- function(x, type){
  
  x <- tolower(x)
  x <- gsub("[[:punct:]]+", " ", x) # remove punctuation, https://stackoverflow.com/a/29099172/11752986
  
  if (type == "student"){
    x <- gsub("phd student", "phd_student", x) # keep bigrams prior to tokenization
    x <- gsub("phd candidate", "phd_candidate", x)
    x <- gsub("graduate student", "graduate_student", x)
    x <- gsub("grad student", "grad_student", x)
    x <- gsub("doctoral student", "doctoral_student", x)
    return(any(terms_student %in% unlist(strsplit(x, " "))))
  }
  if (type == "postdoc"){
    x <- gsub("post doc", "post_doc", x)
    return(any(terms_postdoc %in% unlist(strsplit(x, " "))))
  }
  if (type == "prof"){
    x <- gsub("jun prof", "jun_prof", x)
    return(any(terms_prof %in% unlist(strsplit(x, " "))))
  }
  if (type == "researcher"){
    x <- gsub("research associate", "research_associate", x)
    x <- gsub("research fellow", "research_fellow", x)
    return(any(terms_researcher %in% unlist(strsplit(x, " "))))
  }
}





# algorithm ---------------------------------------------------------------

# set number of iterations
num_iter <- 10


#### 1. load objects ---- 

# (for first run, save timelines of your seeds as timelines_news_seeds)
# (for first run, save your seeds as vector in checked_users object)
# (for first run, save screen_names of initial seed accounts as all_seeds)

load("./researcher_identification/personal.RData") # an object including timelines of seed accounts
timelines_new_seeds <- personal
checked_users <- colnames(dfm(corpus(personal$screen_name), tolower = FALSE))
all_seeds <- checked_users 
rm(personal)


starttime <- Sys.time()
for (i in 1:num_iter){
  
  
    #### 2. get mentioned Twitter users in their tweets (lookups) ----
  
  mentions <- unlist(timelines_new_seeds$mentions_screen_name)
  
  dfm <- dfm(corpus(mentions), tolower = FALSE, remove = checked_users)
  # (for first run, save your seeds as vector in checked_users object)
  
  # minimum number of mentions?
  # top_mentions <- colSums(dfm)[colSums(dfm) >= 3]
  
  
  ## LOOKUPS ##
  
  # lookups <- lookup_users(names(top_mentions), token = token)
  lookups <- lookup_many_users(colnames(dfm))
  cat(paste("Looked up", nrow(lookups), "users in iteration", i, "\n# "))
  
  lookups <- lookups[,c(1,2,3,4,74,75,32,85)] # keep only relevant cols
  
  save(lookups, file = paste0("./researcher_identification/lookups_", i,  ".RData"))
  
  
  ## CHECKED_USERS ##
  
  # add to list of checked users
  checked_users <- c(checked_users, lookups$screen_name)
  cat(paste("A total of", length(checked_users), "users have been checked."))
  
  save(checked_users, file = paste0("./researcher_identification/checked_users_", i,  ".RData"))
  
  
  
  #### 3. use profile descriptions for researcher identification (lookups_psych_rank) ----
  
  # clean text
  descriptions <- textclean::strip(lookups$description)
  
  
  ## psychology-related: "psych" in profile description, but no psychiatrists,
  # "psychiatry" only in context of psychology (e.g)
  
  lookups_psych <- lookups[(grepl("psych", descriptions) &                    # (psych
                              !grepl("psychiat", descriptions)) |             # but not psychiatry)
                             (grepl("psychiat", descriptions) &              # or (psychiatry
                                grepl("psycholog(e|in|ist)", descriptions)),] # and psychologist)
  
  # those without "psych" in profile description will be checked using lists (see below)
  lookups_other <- lookups[!((grepl("psych", descriptions) &
                                !grepl("psychiatr", descriptions)) |
                               (grepl("psychiatr", descriptions) &
                                  grepl("psycholog(e|in|ist)", descriptions))),]
  
  ## academic rank
  
  lookups_psych$student <- sapply(lookups_psych$description, rank_check, "student")
  lookups_psych$postdoc <- sapply(lookups_psych$description, rank_check, "postdoc")
  lookups_psych$prof <- sapply(lookups_psych$description, rank_check, "prof")
  lookups_psych$researcher <- sapply(lookups_psych$description, rank_check, "researcher")
  
  # "psychologist" only in context of "research" or "academic"
  # (these more general terms are not included in terms_research
  # Note: forgot to set ignore.case = TRUE in prior runs
  lookups_psych$researcher <- ifelse((grepl("psychologist", lookups_psych$description, ignore.case = TRUE) |
                                        grepl("psychologe", lookups_psych$description, ignore.case = TRUE) |
                                        grepl("psychologin", lookups_psych$description, ignore.case = TRUE)) &
                                       (grepl("\\bresearch\\b", lookups_psych$description, ignore.case = TRUE) | # \\b word boundaries
                                          grepl("\\bacademic\\b", lookups_psych$description, ignore.case = TRUE) |
                                          grepl("wissenschaft", lookups_psych$description, ignore.case = TRUE) |
                                          grepl("forschung", lookups_psych$description, ignore.case = TRUE)), 
                                     TRUE, lookups_psych$researcher)
  
  
  
  ## drop users without academic rank information ##
  
  condition <- lookups_psych$student == TRUE | lookups_psych$postdoc == TRUE | 
    lookups_psych$prof == TRUE | lookups_psych$researcher == TRUE
  
  lookups_psych_rank <- lookups_psych[condition == TRUE,]
  
  cat(paste(nrow(lookups_psych_rank), "users were found using profile descriptions.\n# "))
  
  save(lookups_psych_rank, 
       file = paste0("./researcher_identification/lookups_psych_rank_", i,  ".RData"))
  

  ## NEW SEEDS ##
  new_seeds <- lookups_psych_rank$screen_name
  
  save(new_seeds, file = paste0("./researcher_identification/new_seeds_", i,  ".RData"))
  
  
  
  
  #### 4. get and save timelines of all identified researchers ----
  
  ## Get timelines ##
  
  # limited to 90 requests, wait 3 minutes
  
  if (length(new_seeds) <= 90){
    
    timelines_new_seeds <- get_timelines(new_seeds, n = 3200, token = token)
    
    
  } else {
    
    timelines_list <- list()
    count <- 0
    gather_count_tl <- 1
    
    while (count <= length(new_seeds)){
      
      cat(paste("Gather count:", gather_count_tl, "\n# "))
      
      if ((count + 90) > length(new_seeds)){ # length(new_seeds) is limit
        end_jj <- length(new_seeds)
      } else {
        end_jj <- count + 90 # 90 users at a time
      }
      
      cat(paste(Sys.time(), "\nGathering timelines... This takes some time.
            \nIteration", (count/90)+1, "of", ceiling(length(new_seeds)/90), "\n# "))
      
      timelines_list[[gather_count_tl]] <- get_timelines(new_seeds[(count+1):end_jj], n = 3200, token = token)
      
      count <- count + 90
      gather_count_tl <- gather_count_tl + 1
      
      if (end_jj != length(new_seeds)){ # don't wait after last iteration
        cat("Now waiting 3 minutes for Twitter API rate limit to reset.\n# ")
        Sys.sleep(3*60) # wait 3 minutes (in seconds)
      }
      
    }
    
    ## Save timelines_new_seeds ##
    
    # bind list entries to one table
    
    timelines_new_seeds <- timelines_list[[1]]
    
    for (ii in 2:length(timelines_list)){
      timelines_new_seeds <- rbind(timelines_new_seeds, timelines_list[[ii]])
    }
    
    save(timelines_new_seeds, file = paste0("./researcher_identification/timelines_new_seeds_", i,  ".RData"))
    rm(timelines_list)
  }
  
  
  
  
  ## save TIMELINES_ALL ##
  
  # timelines_all <- rbind(timelines_all, timelines_new_seeds)
  # (for first run, save timelines of initial seed accounts as timelines_all)
  
  # save(timelines_all, file = "./researcher_identification/timelines_all.RData") # password protect this file
  
  
  ## save ALL_SEEDS ##
  
  all_seeds <- c(all_seeds, new_seeds)
  # (for first run, save screen_names of initial seed accounts as all_seeds)
  
  save(all_seeds, file = paste0("./researcher_identification/all_seeds_", i,  ".RData"))
}

endtime <- Sys.time()
endtime - starttime