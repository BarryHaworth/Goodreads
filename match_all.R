#  Match the movies based on books with the book details
#  This version matches on all movies regardless of whether they are flagged.
# This version still needs work. (10/04/2022)

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringdist)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/movie_list_flagged.RData"))  # Movies based on books
load(file=paste0(DATA_DIR,"/based_on_votes.RData"))      # IMDB votes for movie based on books.
load(file=paste0(DATA_DIR,"/books.RData"))               # Books from Goodreads

# Next, format it ready for use
imdb <- movie_list_flagged %>% 
  rename(movieTitle=primaryTitle,
         movieWriter=writer) %>%
  select(-c(originalTitle))

print(head(imdb),width=200)

goodreads <- books %>% 
  rename(bookTitle=Name,
         bookWriter=Authors,
         bookRating=Rating,
         bookYear=PublishYear,
         bookRating_01=rating_1,
         bookRating_02=rating_2,
         bookRating_03=rating_3,
         bookRating_04=rating_4,
         bookRating_05=rating_5,
         bookVotes=rating_total) %>%
  mutate(bookRating=(bookRating_01+2*bookRating_02+3*bookRating_03+4*bookRating_04+5*bookRating_05)/bookVotes ) %>%
  select(-"Language")

# Goodreads data has multiple editions for the same book (title & author are identical, publication year & vote counts vary)
# Filter to keep one record per title/author, keeping the edition with the largst number of votes

goodreads <- goodreads %>%
  group_by(bookWriter,bookTitle) %>%
  arrange(-bookVotes) %>%
  filter(row_number()==1) %>%
  ungroup()

head(goodreads)

#  Next, try merging it

perfect_match <- imdb %>% inner_join(goodreads,by=c("movieTitle"="bookTitle","movieWriter"="bookWriter")) 

# Merge by Author: Author is exact match, title is fuzzy

# Filter the movies:

matched_id <- perfect_match %>% select(tconst) %>% unique()

unmatched_imdb <- imdb %>% anti_join(matched_id)

fuzzy <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieWriter"="bookWriter"))

fuzzy$title_dist = stringdist(fuzzy$movieTitle,fuzzy$bookTitle,method = "jw")  

fuzzy <- fuzzy %>% arrange("tconst",-title_dist)

fuzzy_title <- fuzzy %>%
  group_by(tconst) %>%
  top_n(1, -title_dist) %>%
  ungroup() %>%
  filter(title_dist <= 0.25)

# Merge by Title: Title is exact match, author is fuzzy

matched_id <- perfect_match %>% bind_rows(fuzzy_title) %>% select(tconst) %>% unique()

unmatched_imdb <- imdb %>% anti_join(matched_id)

fuzzy <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieTitle"="bookTitle")) %>%
  select(-c(movieRating_01,movieRating_02,movieRating_03,movieRating_04,movieRating_05,
            bookRating_01,bookRating_02,bookRating_03,bookRating_04,bookRating_05,
            based_on_book,based_on_comic)) 

fuzzy$writer_dist = stringdist(fuzzy$movieWriter,fuzzy$bookWriter,method = "jw")  

fuzzy <- fuzzy %>% arrange("movieWriter","movieTitle",-writer_dist)

fuzzy_writer <- fuzzy %>%
  group_by(movieWriter,movieTitle) %>%
  top_n(1, -writer_dist) %>%
  ungroup() %>%
  filter(writer_dist <= 0.25)

# Fuzzy match: Both title and author are fuzzy

# Now to compare

perfect_match$bookTitle = perfect_match$movieTitle

comparison <- bind_rows(perfect_match %>% mutate(bookWriter=movieWriter, bookTitle=movieTitle, title_dist=0, writer_dist=0),
                        fuzzy_title   %>% mutate(bookWriter=movieWriter, writer_dist=0),
                        fuzzy_writer  %>% mutate(bookTitle=movieTitle, title_dist=0)) %>%
  mutate(delta=bookRating-movieRating,
         best=case_when(delta>0 ~ "Book",TRUE ~ "Movie"))

summary(comparison$delta)
hist(comparison$delta,nclass=20)
plot(comparison$bookRating,comparison$movieRating)
table(comparison$best)
cor(comparison$bookRating,comparison$movieRating)

save(comparison,file=paste0(DATA_DIR,"/comparison.RData"))

