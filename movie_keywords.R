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

movies <- movie_list %>% 
  filter(movie_votes> 1000) %>%
  select("tconst","movie_votes","primaryTitle") %>%
  arrange(-movie_votes) %>%
  unique()

# Rip the keywords page - test
tconst <- movies$tconst[1]
tconst <- "tt0120855"  # Disney Tarzan
tconst <- "tt0304141"  # Harry Potter
tconst <- "tt0073486"
url <- paste0('https://www.imdb.com/title/',tconst,'/keywords')
webpage <- read_html(url)
tag_html <- html_nodes(webpage,'.sodatext')
tags <- trimws(gsub('[\n]', '', html_text(tag_html)))
keywords <- data.frame("tconst"=tconst,"keywords"=tags)
tags[grep("based on",tags)]
length(grep("based on",tags))

# create a keywords dataframe
movie_keys <- function(id){
  url <- paste0('https://www.imdb.com/title/',id,'/keywords')
  webpage <- read_html(url)
  tag_html <- html_nodes(webpage,'.sodatext')
  tags <- trimws(gsub('[\n]', '', html_text(tag_html)))
  keywords <- data.frame("tconst"=id,"keywords"=tags)
  return(keywords)
}

# If the saved Movie keywords data exists, read it.
if (file.exists(paste0(DATA_DIR,"/movie_keywords.RData"))) {
  load(paste0(DATA_DIR,"/movie_keywords.RData"))
}
  
# If the local file does not exist, initialise it
if (!exists("movie_keywords")) {
  movie_keywords <- keywords(movies_10K$tconst[1])  # Initialise the keywords data frame
}

based_on_book <- c("based on novel","based on book","based on play","based on short story",
                   "based on young adult novel","based on children's book","based on a novel",
                   "based on novella","based on bestseller","based on book series")
based_on_comic <-c("based on comic book","based on comic","based on graphic novel","based on manga")


# Get the list of IDs looked up

looked_up <- movie_keywords$tconst %>% unique()
movies_notyet <- movies %>% filter(!(tconst %in% looked_up))

for (i in 1:1000){
  print(paste("Looking up keywords for movie",i,movies_notyet$primaryTitle[i]))
  keys <- movie_keys(movies_notyet$tconst[i])
  if (nrow(keys %>% filter(keywords %in% based_on_book))>0) {
    print("                  - Based on a Book")
  } else
    if (nrow(keys %>% filter(keywords %in% based_on_comic))>0)  {
      print("                  - Based on a Comic")
      
    }
  movie_keywords <- rbind(movie_keywords,keys)
  }

save(movie_keywords,file=paste0(DATA_DIR,"/movie_keywords.RData"))
