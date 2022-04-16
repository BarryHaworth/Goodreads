# Rip Ratings of Movies matched to books
# This program reads votes for matched movies that have not yet had their votes read.
# Updating votes will be done by a different program

library(dplyr)
library(tidyr)
library(rvest)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/Goodreads"
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
load(file=paste0(DATA_DIR,"/matched.RData"))  # Read the list of matched movies

if (file.exists(paste0(DATA_DIR,"/matched_votes.RData"))){
  load(file=paste0(DATA_DIR,"/matched_votes.RData"))
} else {
  matched_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
}

# List of Movie matched movie IDs to be looked up
movie_id <- matched %>% select(tconst,movieTitlePrimary) %>% unique()

# Get the list of IDs already looked up
vote_id <- unique(matched_votes$tconst)

movie_id <- movie_id %>% filter(!(tconst %in% vote_id))  # Filter against the ones already done

while(nrow(movie_id)>0){
  if (file.exists(paste0(DATA_DIR,"/matched_votes.RData"))){
    load(file=paste0(DATA_DIR,"/matched_votes.RData"))
  } else {
    matched_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
  }
  looked_up <- matched_votes$tconst %>% unique()
  movie_id  <- matched %>% filter(!(tconst %in% looked_up)) %>% select(tconst,movieTitlePrimary) %>% unique()
  print(paste("Movies Looked up:",length(looked_up),"Remaining:",nrow((movie_id))))
  for (i in 1:min(nrow(movie_id),100)){
    tryCatch({
      id <- movie_id$tconst[i]
      movieTitle <- movie_id$movieTitlePrimary[i]
      print(paste(i,"Movie",id,movieTitle))
      matched_votes <- bind_rows(matched_votes,vote_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  save(matched_votes,file=paste0(DATA_DIR,"/matched_votes.RData"))
}


