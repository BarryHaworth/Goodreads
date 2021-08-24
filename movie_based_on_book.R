# Idetify Tagged as Based on a book

library(dplyr)
library(tidyr)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/movie_list.RData"))
load(file=paste0(DATA_DIR,"/movie_keywords.RData"))

movie_based <- movie_keywords %>% filter(grepl("based on",keywords))
summary(factor(movie_based$keywords))

based_on_book <- c("based on novel","based on book","based on play","based on short story",
                   "based on young adult novel","based on children's book","based on a novel",
                   "based on novella","based on bestseller","based on book series")
based_on_comic <-c("based on comic book","based on comic","based on graphic novel","based on manga")

movie_based_on_book <- movie_based %>% group_by(tconst) 
