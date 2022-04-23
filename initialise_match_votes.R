# Initialise matched movie votes file 
# using the votes already saved from the flagged movies based approach.

library(dplyr)
library(tidyr)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/matched.RData"))          # Movies matched to books
load(file=paste0(DATA_DIR,"/based_on_votes.RData"))   # Data already extracted for flag based version

matched_id <- matched$tconst %>% unique()

matched_votes <- based_on_votes %>% filter(tconst %in% matched_id)

save(matched_votes,file=paste0(DATA_DIR,"/matched_votes.RData"))
