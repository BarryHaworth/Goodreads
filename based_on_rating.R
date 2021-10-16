# Ratings of Movies based on books

library(dplyr)
library(tidyr)
library(rvest)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/movie_based_on_book.RData"))

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
load(file=paste0(DATA_DIR,"/movie_based_on_book.RData"))

movie_id <- unique(movie_based_on_book$tconst)

if (file.exists(paste0(DATA_DIR,"/based_on_votes.RData"))){
  load(file=paste0(DATA_DIR,"/based_on_votes.RData"))
} else {
  based_on_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
}

vote_id <- unique(based_on_votes$tconst)

movie_id <- movie_id[!(movie_id %in% vote_id)]  # Filter against the ones already done

label <- "Movies based on books"
n.ids <- length(movie_id)
count <- 0
start.time <- Sys.time()

for(id in movie_id){
  count <- count + 1
  ETA <- Sys.time() + (n.ids-count) * (Sys.time() - start.time)/count
  print(paste("ID:",id,"#",count,"of",n.ids,
              "Started:",format(start.time,"%H:%M:%S"),
              "Time:",format(Sys.time(),"%H:%M:%S"),
              "ETA:",format(ETA,"%H:%M:%S")))
  based_on_votes      <- rbind(based_on_votes,vote_rip(id))
}
save(based_on_votes,file=paste0(DATA_DIR,"/based_on_votes.RData")) # Save Votes data after each step

