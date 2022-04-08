#  Match the movies based on books with the book details


library(dplyr)
library(tidyr)
library(ggplot2)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/movie_list_flagged.RData"))  # Movies based on books
load(file=paste0(DATA_DIR,"/based_on_votes.RData"))      # IMDB votes for movie based on books.
load(file=paste0(DATA_DIR,"/books.RData"))               # Books from Goodreads
#head(movie_list_flagged)
#head(based_on_votes)

# Next, format it ready for use
imdb <- movie_list_flagged %>% 
  inner_join(based_on_votes,by="tconst") %>%
  rename(movieTitle=primaryTitle,
         movieWriter=writer,
         movieRatingSum=Vote_sum) %>%
  mutate(movieRating_01=Vote_01+Vote_02,
         movieRating_02=Vote_03+Vote_04,
         movieRating_03=Vote_05+Vote_06,
         movieRating_04=Vote_07+Vote_08,
         movieRating_05=Vote_09+Vote_10,
         movieRating=(movieRating_01+2*movieRating_02+3*movieRating_03+4*movieRating_04+5*movieRating_05)/movieRatingSum ) %>%
  select(-c(originalTitle,Date,movie_rating,movie_votes,Vote_01,Vote_02,Vote_03,Vote_04,Vote_05,Vote_06,Vote_07,Vote_08,Vote_09,Vote_10))

print(head(imdb),width=200)

goodreads <- books %>% rename(bookTitle=Name,
                          bookWriter=Authors,
                          bookRating=Rating,
                          bookYear=PublishYear,
                          bookRating_01=rating_1,
                          bookRating_02=rating_2,
                          bookRating_03=rating_3,
                          bookRating_04=rating_4,
                          bookRating_05=rating_5,
                          bookRatingSum=rating_total) %>%
  select(-"Language")

# Goodreads data has multiple entries for the same book (title & author is identical, publication year & vote counts vary)
# Filter to keep one record per title/author, keeping the first year within author/title

goodreads <- goodreads %>%
  group_by(bookWriter,bookTitle) %>%
  arrange(bookYear) %>%
  filter(row_number()==1) %>%
  ungroup()

head(goodreads)

#  Next, try merging it

perfect_match <- imdb %>% inner_join(goodreads,by=c("movieTitle"="bookTitle","movieWriter"="bookWriter"))

# Now to compare

comparison <- perfect_match %>%
  select(-c(movieRating_01,movieRating_02,movieRating_03,movieRating_04,movieRating_05,
            bookRating_01,bookRating_02,bookRating_03,bookRating_04,bookRating_05)) %>%
  mutate(delta=bookRating-movieRating,
         best=case_when(delta>0 ~ "Book",TRUE ~ "Movie"))

hist(comparison$delta)
table(comparison$best)

# Merge by Author?

# Merge by Title?


'
print(movies %>% filter(tconst=="tt0241527") ,n=99,width=1000) # Harry Potter

print(movies %>% filter(tconst=="tt0241527") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0330373") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt1201607") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0304141") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0295297") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0417741") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0926084") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movies %>% filter(tconst=="tt0373889") %>% select(primaryTitle,writer),n=99) # Harry Potter


based_on_votes %>% filter(tconst=="tt0120855")  # Disney Tarzan

print(movie_list_flagged %>% filter(tconst=="tt0126029") %>% select(primaryTitle,writer),n=99) # Shrek
print(movie_list_flagged %>% filter(tconst=="tt1014759") %>% select(primaryTitle,writer),n=99) # Alice in Wonderland
'