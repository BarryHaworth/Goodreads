# Ratings of Movies based on books
# Updated this to check for movies whose vote count has changed significantly 
# and update those counts.

library(dplyr)
library(tidyr)
library(rvest)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/movie_based_on_book.RData"))
load(file=paste0(DATA_DIR,"/movie_list.RData"))  # Read the latest ratings

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

movie_id <- movie_based_on_book %>% select(tconst) %>% unique()

if (file.exists(paste0(DATA_DIR,"/based_on_votes.RData"))){
  load(file=paste0(DATA_DIR,"/based_on_votes.RData"))
} else {
  based_on_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
}

# If the based_on_votes.RData data exists, read it. 
# and identify movies that have updated vote counts significantly

vote_update <- movie_list %>% select(tconst,primaryTitle,movie_votes) %>% unique() %>%
  inner_join(based_on_votes %>% select(tconst,Vote_sum) %>% 
             group_by(tconst) %>% arrange(-Vote_sum) %>%
               filter(row_number()==1) %>% ungroup()
               ,by="tconst") %>%
  filter((movie_votes>(Vote_sum+1000))|(movie_votes/Vote_sum>1.1))

updated_ids <- vote_update %>% select(tconst) %>% unique() 

# Get the list of IDs looked up
vote_id <- unique(based_on_votes$tconst)

movie_id <- movie_id %>% filter(!(tconst %in% vote_id))  # Filter against the ones already done

movie_id <- bind_rows(movie_id,updated_ids)  # Add the updated IDS to the movies to update counts

while(nrow(movie_id)>0){
  looked_up <- based_on_votes$tconst %>% unique()
  movie_id  <- movie_based_on_book %>% filter(!(tconst %in% looked_up)) %>% select(tconst) %>% unique()
  if (file.exists(paste0(DATA_DIR,"/based_on_votes.RData"))){
    load(file=paste0(DATA_DIR,"/based_on_votes.RData"))
  } else {
    based_on_votes <- vote_rip(movie_id[1])  # Initialise votes data frame
  }
  vote_update <- movie_list %>% select(tconst,primaryTitle,movie_votes) %>% unique() %>%
    inner_join(based_on_votes %>% select(tconst,Vote_sum) %>% 
                 group_by(tconst) %>% arrange(-Vote_sum) %>%
                 filter(row_number()==1) %>% ungroup()
               ,by="tconst") %>%
    filter((movie_votes>(Vote_sum+1000))|(movie_votes/Vote_sum>1.1))
  updated_ids <- vote_update %>% select(tconst) %>% unique() 
  vote_id <- unique(based_on_votes$tconst)
  movie_id <- bind_rows(movie_id,updated_ids)  # Add the updated IDS to the movies to update counts
  print(paste("Movies Looked up:",length(looked_up),"Remaining:",nrow((movie_id))))
  for (i in 1:min(nrow(movie_id),100)){
    tryCatch({
      id <- movie_id$tconst[i]
      print(paste(i,"Movie",id))
      based_on_votes <- bind_rows(based_on_votes,vote_rip(id))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  print("Saving Movie Votes Data Frame")
  save(based_on_votes,file=paste0(DATA_DIR,"/based_on_votes.RData"))
}


