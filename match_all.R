#  Match the movies based on books with the book details
#  This version matches on all movies regardless of whether they are flagged
#  and ignores votes - those are added in a later step
#  This version merges movies and books with at least 100 votes.
#  Matches:
#  perfect_primary - exact writer match, exact primary movie title to book title match
#  perfect_original - exact writer match, exact original movie title to book title match
#  perfect_aka  - exact writer match, exact AKA movie title to book title match
#  fuzzy_title  - exact writer match, fuzzy title match (best of primary or original)
#  fuzzy_writer - exact title match (to either primary or original), best fuzzy writer name match

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringdist)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/movie_list.RData"))          # Movies 
load(file=paste0(DATA_DIR,"/books.RData"))               # Books from Goodreads

# Format data ready for use
imdb <- movie_list %>% 
  rename(movieWriter=writer,
         movieVotes=movie_votes,
         movieTitlePrimary=primaryTitle,
         movieTitleOriginal=originalTitle) %>%
  filter(movieVotes>=100) %>%
  mutate(movieURL=paste0("https://www.imdb.com/title/",tconst)) %>%
  select(-c(movie_rating))

head(imdb)

goodreads <- books %>% 
  rename(bookTitle=Name,
         bookWriter=Authors,
         bookYear=PublishYear,
         bookVotes=rating_total) %>%
  filter(bookVotes>=100) %>%
  mutate(bookURL=paste0("https://www.goodreads.com/book/show/",Id)) %>%
  select(-c("Language","Rating","rating_1","rating_2","rating_3","rating_4","rating_5"))

# Goodreads data has multiple editions for the same book (title & author are identical, publication year & vote counts vary)
# Filter to keep one record per title/author, keeping the edition with the largest number of votes
# Create a count of different editions, and record the lowest year as the publication year.

goodreads <- goodreads %>%
  group_by(bookWriter,bookTitle) %>%
  mutate(pubYear = min(bookYear),
         editions = n()) %>%
  arrange(-bookVotes) %>%
  filter(row_number()==1) %>%
  ungroup()

head(goodreads)

# Edits.  
# Perform edits to selected titles etc

goodreads["bookWriter"][goodreads["bookWriter"]=="Emmuska Orczy"] <- "Baroness Emmuska Orczy"

# Matching
# Perfect Match: exact match by title(s) and author

perfect_primary <- imdb %>% 
  inner_join(goodreads,by=c("movieTitlePrimary"="bookTitle","movieWriter"="bookWriter"),keep=TRUE) %>%
  mutate(matchType="Exact Primary Title Exact Author")

# Filter the movies to remove matches:

matched_id <- perfect_primary %>% select(tconst) %>% unique()

unmatched_imdb <- imdb %>% anti_join(matched_id)

# Perfect match on original title

perfect_original <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieTitleOriginal"="bookTitle","movieWriter"="bookWriter"),keep=TRUE) %>%
  mutate(matchType="Exact Original Title Exact Author")

matched_id <- perfect_original %>% select(tconst) %>% unique()

unmatched_imdb <- unmatched_imdb %>% anti_join(matched_id)

# AKA matching
load(file=paste0(DATA_DIR,"/akas.RData"))   # Movie AKAs

akas <- akas %>% select(tconst,title) %>% unique()

unmatched_id <-unmatched_imdb %>% select(tconst) %>% unique()

akas <- akas %>% inner_join(unmatched_id)

good_aka <- goodreads %>% inner_join(akas, by=c("bookTitle"="title"))

perfect_aka <- imdb %>% 
  inner_join(good_aka,by=c("tconst","movieWriter"="bookWriter"),keep=TRUE) %>%
  select(-tconst.y) %>% rename(tconst=tconst.x) %>%
  mutate(matchType="Exact AKA Title Exact Author")

matched_id <- perfect_aka %>% select(tconst) %>% unique()

unmatched_imdb <- unmatched_imdb %>% anti_join(matched_id)

# Merge by Author: Author is exact match, Title is fuzzy

fuzzy_t <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieWriter"="bookWriter"),keep=TRUE) %>%
  rowwise() %>%
  mutate(title_dist_primary = stringdist(movieTitlePrimary ,bookTitle,method = "jw"),
         title_dist_original= stringdist(movieTitleOriginal,bookTitle,method = "jw"),
         title_dist=min(title_dist_primary,title_dist_original))

fuzzy_title <- fuzzy_t %>%
  group_by(tconst) %>%
  top_n(1, -title_dist) %>%
  ungroup() %>%
  filter(title_dist <= 0.25) %>%
  mutate(matchType="Exact Author Fuzzy Title")

matched_id <- fuzzy_title %>% select(tconst) %>% unique()

unmatched_imdb <- unmatched_imdb %>% anti_join(matched_id)

# Merge by Title: Title is exact match, writer is fuzzy

fuzzy_w_primary <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieTitlePrimary"="bookTitle"),keep=TRUE)  %>%
  rowwise() %>%
  mutate(writer_dist = stringdist(movieWriter ,bookWriter,method = "jw"))

fuzzy_w_original <- unmatched_imdb %>% 
  inner_join(goodreads,by=c("movieTitleOriginal"="bookTitle"),keep=TRUE) %>%
  rowwise() %>%
  mutate(writer_dist = stringdist(movieWriter ,bookWriter,method = "jw"))

fuzzy_w <- bind_rows(fuzzy_w_primary,fuzzy_w_original) %>% arrange("tconst",-writer_dist)

fuzzy_writer <- fuzzy_w %>%
  group_by(tconst) %>%
  top_n(1, -writer_dist) %>%
  ungroup() %>%
  filter(writer_dist <= 0.202)  %>% # Limit chosen by inspection and may be wrong.
  mutate(matchType="Fuzzy Author Exact Title")

# Fuzzy match: Exact AKA, fuzzy author.  Do later

# Fuzzy match: Both title and author are fuzzy
# Do this later. Maybe.

# Combine the matches and save them.

matched <- bind_rows(perfect_original,
                     perfect_primary,
                     perfect_aka,
                     fuzzy_title,
                     fuzzy_writer) %>%
  rowwise() %>%
  mutate(writer_dist = stringdist(movieWriter ,bookWriter,method = "jw"),
         title_dist_primary = stringdist(movieTitlePrimary ,bookTitle,method = "jw"),
         title_dist_original= stringdist(movieTitleOriginal,bookTitle,method = "jw"),
         title_dist=min(title_dist_primary,title_dist_original))

save(matched,file=paste0(DATA_DIR,"/matched.RData"))

table(matched$matchType)

#head(matched)
#summary(matched)
