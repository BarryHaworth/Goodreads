# Movie vs Book Comparisons
# Taking the combined data file and performing analysis on it.

library(dplyr)
library(ggplot2)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/combined.RData"))         # Movies matched with books

summary(combined$delta)
hist(combined$delta,nclass=20)
plot(combined$bookRating,combined$movieRating)
table(combined$best)
table(combined$titleType,combined$best)
cor(combined$bookRating,combined$movieRating)

