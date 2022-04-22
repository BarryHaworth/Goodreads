# Read the Goodreads Data
#  Data files taken from Kaggle goodreads data sets
#  https://www.kaggle.com/bahramjannesarr/goodreads-book-datasets-10m 
# Note:  As at 19/04/2022 the latest Goodreads data is 03/12/2020

library(dplyr)
library(rmutil)
library(ggplot2)
library(stringr)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Get a list of the csv files

files <- list.files(DATA_DIR)

book_files <- files[grep("book",files)]           # Book files
book_files <- book_files[grep("csv",book_files)]  # Book files which are also csv

# Read the csv files.

books <- read.csv(paste0(DATA_DIR,"/",book_files[1]),stringsAsFactors = F)  # Read the first file

books <- books %>% select(c("Id","Name","RatingDist1","RatingDist2",
                            "RatingDist3","RatingDist4","RatingDist5","RatingDistTotal",
                            "PublishYear","Language","Authors","Rating") )

for(file in book_files[2:length(book_files)]){
  bookfile <- read.csv(paste0(DATA_DIR,"/",file),stringsAsFactors = F)  
  bookfile <- bookfile %>% select(c("Id","Name","RatingDist1","RatingDist2",
                                    "RatingDist3","RatingDist4","RatingDist5","RatingDistTotal",
                                    "PublishYear","Language","Authors","Rating") )
  books <- bind_rows(books,bookfile)
}

head(books)
summary(books)

# Parse book
books$rating_1 <- as.numeric(str_split_fixed(books$RatingDist1,":",2)[,2])
books$rating_2 <- as.numeric(str_split_fixed(books$RatingDist2,":",2)[,2])
books$rating_3 <- as.numeric(str_split_fixed(books$RatingDist3,":",2)[,2])
books$rating_4 <- as.numeric(str_split_fixed(books$RatingDist4,":",2)[,2])
books$rating_5 <- as.numeric(str_split_fixed(books$RatingDist5,":",2)[,2])
books$rating_total <- as.numeric(str_split_fixed(books$RatingDistTotal,":",2)[,2])

books <- books %>% 
         select(-c("RatingDist1","RatingDist2","RatingDist3","RatingDist4","RatingDist5","RatingDistTotal")) 

summary(books)

ggplot(books , aes(x=rating_total)) + geom_histogram() + labs(title="Histogram of Number of Ratings")
ggplot(books %>% filter(rating_total>10), aes(x=Rating)) + geom_histogram() + labs(title="Histogram of Rating")

save(books,file=paste0(DATA_DIR,"/books.RData"))
