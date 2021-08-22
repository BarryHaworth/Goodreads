# Read the IMDB data and derive a list of movies based on books

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/principals.RData"))
load(file=paste0(DATA_DIR,"/basics.RData"))
load(file=paste0(DATA_DIR,"/crew.RData"))

# Identify writers who are the writers of the novel the movie was based on.