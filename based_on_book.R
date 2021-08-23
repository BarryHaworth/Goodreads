# Movies Tagged as Based on a book
# Rip IMDB to find tags

library(dplyr)
library(rmutil)
library(tidyr)
library(rvest)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/movie_list.RData"))
sum(movie_list$movie_votes > 100000,na.rm=T)  # Movies with > 100K votes

movies_10K <- movie_list %>% filter(movie_votes > 100000) %>% select("tconst")

# Rip the keywords page
tconst <- movies_10K$tconst[1]
tconst <- "tt0120855"  # Disney Tarzan
tconst <- "tt0304141"  # Harry Potter
tconst <- "tt5687612"
url <- paste0('https://www.imdb.com/title/',tconst,'/keywords')
webpage <- read_html(url)
tag_html <- html_nodes(webpage,'.sodatext')
tags <- trimws(gsub('[\n]', '', html_text(tag_html)))
tags[grep("based on",tags)]
