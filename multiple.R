# Identify authors adn books with multiple adapations

library(dplyr)
library(ggplot2)
library(scales)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
PLOT_DIR    <- paste0(PROJECT_DIR,"/plots")

# First, load the saved data

load(file=paste0(DATA_DIR,"/combined.RData"))         # Movies matched with books

# Counts of number of movies by author
author_count <- combined %>% group_by(bookWriter) %>% summarise(movie_count = n(),mean_delta=mean(delta) ) %>% arrange(-movie_count)
summary(author_count$movie_count)
table(author_count$movie_count)

head(author_count %>% arrange(-movie_count),20)
head(author_count %>% filter(movie_count>9) %>% arrange(mean_delta),20)
tail(author_count %>% filter(movie_count>9) %>% arrange(mean_delta),20)

# Counts of number of movies by title
title_count <- combined %>% group_by(bookTitle) %>% summarise(movie_count = n(),mean_delta=mean(delta) )  %>% arrange(-movie_count)
summary(title_count$movie_count)
table(title_count$movie_count)

head(title_count %>% arrange(-movie_count),20)
head(title_count %>% filter(movie_count>9) %>% arrange(mean_delta),20)
tail(title_count %>% filter(movie_count>9) %>% arrange(mean_delta),20)

# Plots of deltas for authors/titles with 10 or more movies