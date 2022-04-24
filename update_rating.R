# This program update IMDB vote counts for movies where these have changed

library(dplyr)
library(tidyr)
library(rvest)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Rip individual votes for a given movie
vote_rip <- function(tconst){
  url <- paste0('https://www.imdb.com/title/',tconst,'/ratings?ref_=tt_ov_rt')
  #Reading the HTML code from the website
  webpage <- read_html(url)
  rank_html <- html_nodes(webpage,'.leftAligned')
  rank_data <- html_text(rank_html)
  ranks <- rev(as.numeric(gsub(',','',rank_data[2:11])))
  Date <- Sys.Date()
  Vote_01 <- ranks[1]
  Vote_02 <- ranks[2]
  Vote_03 <- ranks[3]
  Vote_04 <- ranks[4]
  Vote_05 <- ranks[5]
  Vote_06 <- ranks[6]
  Vote_07 <- ranks[7]
  Vote_08 <- ranks[8]
  Vote_09 <- ranks[9]
  Vote_10 <- ranks[10]
  Vote_sum <- sum(ranks)
  votes <- data.frame(tconst,Date,Vote_01,Vote_02,Vote_03,Vote_04,
                      Vote_05,Vote_06,Vote_07,Vote_08,Vote_09,Vote_10,Vote_sum)
  return(votes)
}

# Read the data
load(file=paste0(DATA_DIR,"/matched_votes.RData"))  # Read the list of matched movies
load(file=paste0(DATA_DIR,"/movie_list.RData"))  # Read the latest ratings

# Identify votes to update
vote_update <- movie_list %>% select(tconst,primaryTitle,movie_votes) %>% unique() %>%
  inner_join(matched_votes %>% select(tconst,Vote_sum) %>% 
               group_by(tconst) %>% arrange(-Vote_sum) %>%
               filter(row_number()==1) %>% ungroup()
             ,by="tconst") %>%
  filter((movie_votes>(Vote_sum+1000))|(movie_votes/Vote_sum>1.1))

updated_ids <- vote_update %>% select(tconst) %>% unique() 

while(nrow(updated_ids)>0){
  if (file.exists(paste0(DATA_DIR,"/matched_votes.RData"))){
    load(file=paste0(DATA_DIR,"/matched_votes.RData"))
  } else {
    matched_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
  }
  # Identify votes to update
  vote_update <- movie_list %>% select(tconst,primaryTitle,movie_votes) %>% unique() %>%
    inner_join(matched_votes %>% select(tconst,Vote_sum) %>% 
                 group_by(tconst) %>% arrange(-Vote_sum) %>%
                 filter(row_number()==1) %>% ungroup()
               ,by="tconst") %>%
    filter((movie_votes>(Vote_sum+1000))|(movie_votes/Vote_sum>1.1))
  updated_ids <- vote_update %>% select(tconst) %>% unique()   
#  looked_up <- matched_votes$tconst %>% unique()
#  movie_id  <- matched_votes %>% filter(tconst %in% updated_ids) %>% select(tconst) %>% unique()
  print(paste("Movies to update :",nrow((updated_ids))))
  for (i in 1:min(nrow(vote_update),100)){
    tryCatch({
      id <- vote_update$tconst[i]
      movieTitle <- vote_update$primaryTitle[i]
      print(paste(i,"Movie",id,movieTitle))
      matched_votes <- bind_rows(matched_votes,vote_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  matched_votes <- matched_votes %>% group_by(tconst) %>% arrange(Date) %>% slice_tail(n=1) %>% ungroup()  # Keep the most recent votes
  save(matched_votes,file=paste0(DATA_DIR,"/matched_votes.RData"))
}


