# Goodread Rip
# Program to rip up to date info from Goodreads
# Pages are of form https://www.goodreads.com/book/show/nnnn
# Book data set from Kaggle has 1,850,310 records, ID from 1 t0 4,846,451
#
# Ripping all the same variables as the Kaggle dataset except:
#  Authors variable is named Author.  The Kaggle dataset only gives one author, even for books wiht multiple authors.
#  Language is omitted.  Filed is on the page, but I can't work out how to read it.
#  Seems to stop ripping after too man attempts - blocked?

library(dplyr)
library(tidyr)
library(rvest)
library(stringr)

options(timeout= 4000000)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Number of books to rip
Id = 25
book_rip <- function(Id){
  url <- paste0('https://www.goodreads.com/book/show/',format(Id,scientific = F))
  #Reading the HTML code from the website
  webpage <- read_html(url)
  name_rip <- html_nodes(webpage,'.Text__title1')
  name <- html_text(name_rip)
  Author_rip  <- html_nodes(webpage,'.ContributorLink__name')
  Author <- html_text(Author_rip)[1]
  lang_rip <- html_nodes(webpage,'.ld+json')
  Language <- "" 
  year_rip <- html_nodes(webpage,'p')
  published <- html_text(year_rip)[2]
  PublishYear <- as.numeric(word(published,-1))
  Rating_rip  <- html_nodes(webpage,'.RatingStatistics__rating')
  Rating <- as.numeric(html_text(Rating_rip)[1])
  ratings <- html_nodes(webpage,'.RatingsHistogram__labelTotal')
  ratings <- html_text(ratings)
  
  rating_1  <- as.numeric(gsub(",","",word(ratings[5])))
  rating_2  <- as.numeric(gsub(",","",word(ratings[4])))
  rating_3  <- as.numeric(gsub(",","",word(ratings[3])))
  rating_4  <- as.numeric(gsub(",","",word(ratings[2])))
  rating_5  <- as.numeric(gsub(",","",word(ratings[1])))
  rating_total <- sum(rating_1,rating_2,rating_3,rating_4,rating_5)
  
  print(paste("Book Id",Id,name,"by",Author))
  
  book <- data.frame(Id,name,PublishYear, Language, Author,Rating,rating_1,rating_2,rating_3,rating_4,rating_5,rating_total)
  return(book)
}

# Tests
# book_rip(3)
# book_rip(33)
# book_rip(333)
# book_rip(3333)
# book_rip(33333)
# book_rip(333333) 
# book_rip(3333333)
# book_rip(33333333)

if (file.exists(paste0(DATA_DIR,"/goodreads_rip.RData"))){
  load(file=paste0(DATA_DIR,"/goodreads_rip.RData"))
} else {
  goodreads_rip <- book_rip(1)  # Initialise Goodreads data frame
}
save(goodreads_rip,file=paste0(DATA_DIR,"/goodreads_rip.RData"))

max_id <- max(goodreads_rip$Id)

# Rip the next 100 books

Target_id=1000  

while(max_id < Target_id){
  if (file.exists(paste0(DATA_DIR,"/goodreads_rip.RData"))){
    load(file=paste0(DATA_DIR,"/goodreads_rip.RData"))
  } else {
    goodreads_rip <- book_rip(1)  # Initialise Goodreads data frame
  }
  max_id <- max(goodreads_rip$Id)
  print(paste("# Books Looked up:",length(goodreads_rip$Id)))
  for (i in 1:100){
    tryCatch({
      print(paste(i,"Looking up Book ID",max_id+i))
      goodreads_rip <- bind_rows(goodreads_rip,book_rip(max_id+i))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
      Sys.sleep(60)
      })
  }
  goodreads_rip <- goodreads_rip %>% unique()
  print("Saving Book Data  Frame")
  save(goodreads_rip,file=paste0(DATA_DIR,"/goodreads_rip.RData"))
}

# Fill in the Gaps

max_id <- max(goodreads_rip$Id)

all_id <- seq(1,max_id)
book_id <- goodreads_rip$Id %>% unique() 
gap_id <- all_id[!(all_id %in% book_id)]

while(length(gap_id) > 0.1*max_id){
  if (file.exists(paste0(DATA_DIR,"/goodreads_rip.RData"))){
    load(file=paste0(DATA_DIR,"/goodreads_rip.RData"))
  } else {
    goodreads_rip <- book_rip(1)  # Initialise Goodreads data frame
  }
  max_id <- max(goodreads_rip$Id)
  all_id <- seq(1,max_id)
  book_id <- goodreads_rip$Id %>% unique() 
  gap_id <- all_id[!(all_id %in% book_id)]
  print(paste("Max ID",max_id,"Scraped",length(book_id),"Gaps",length(gap_id)))
  for (i in 1:min(length(gap_id),100)){
    tryCatch({
      g<-gap_id[i]
      print(paste(i,"Looking up Book ID",g))
      goodreads_rip <- bind_rows(goodreads_rip,book_rip(g))
    }, error=function(e){
      cat("ERROR :",conditionMessage(e), "\n")
      Sys.sleep(60)
      })
  }
  goodreads_rip <- goodreads_rip %>% unique()
  print("Saving Book Data  Frame")
  save(goodreads_rip,file=paste0(DATA_DIR,"/goodreads_rip.RData"))
  max_id <- max(goodreads_rip$Id)
  all_id <- seq(1,max_id)
  book_id <- goodreads_rip$Id %>% unique() 
  gap_id <- all_id[!(all_id %in% book_id)]
}

