# Combination of ratings
# This programs takes the matches movies&books and adds the ratings
# IMDB ratings are recoded from a 10 point to a 5 point scale
# and average ratings are calculated for both books and movies
# on this basis.
# Ratings are combined with the matched movies&books and the results saved for later analysis.

library(dplyr)
library(openxlsx)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/matched.RData"))         # Movies matched with books
load(file=paste0(DATA_DIR,"/matched_votes.RData"))   # IMDB votes for movie matched with books.
load(file=paste0(DATA_DIR,"/books.RData"))           # Books from Goodreads

#  remove unneeded columns from matched data
matched <- matched %>% select(-c(nconst,movieVotes,bookVotes))

# calculate movie ratings based on five point scale from movie vote data
movie_rating <- matched_votes %>% 
  rename(movieVotes=Vote_sum) %>%
  mutate(movieRating_01=Vote_01+Vote_02,
         movieRating_02=Vote_03+Vote_04,
         movieRating_03=Vote_05+Vote_06,
         movieRating_04=Vote_07+Vote_08,
         movieRating_05=Vote_09+Vote_10,
         movieRating=(movieRating_01+2*movieRating_02+3*movieRating_03+4*movieRating_04+5*movieRating_05)/movieVotes ) %>%
  select(c(tconst,movieVotes,movieRating_01,movieRating_02,movieRating_03,movieRating_04,movieRating_05,movieRating))
  
head(movie_rating)

# Extract book ratings from the Goodreads data
book_rating <- books %>% 
  rename(bookRating_01=rating_1,
         bookRating_02=rating_2,
         bookRating_03=rating_3,
         bookRating_04=rating_4,
         bookRating_05=rating_5,
         bookVotes=rating_total) %>%
  mutate(bookRating=(bookRating_01+2*bookRating_02+3*bookRating_03+4*bookRating_04+5*bookRating_05)/bookVotes ) %>%
  select(-c("Language",Name,Authors,Rating,PublishYear))

head(book_rating)

# Merged the matched books&movies with the ratings and calculate best flag and delta
combined <- matched %>%
  inner_join(movie_rating,by="tconst")  %>% 
  inner_join(book_rating,by="Id") %>%
  mutate(delta=bookRating-movieRating,
         best=case_when(delta>0 ~ "Book",TRUE ~ "Movie")) %>%
  mutate(across(c(best,titleType),factor)) %>%
  arrange(bookWriter,bookTitle,movieYear)

summary(combined$delta)
hist(combined$delta,nclass=20)
plot(combined$bookRating,combined$movieRating)
summary(combined$best)
table(combined$titleType,combined$best)
cor(combined$bookRating,combined$movieRating)

# Save the results
save(combined,file=paste0(DATA_DIR,"/combined.RData"))
write.xlsx(combined,file=paste0(DATA_DIR,"/combined.xlsx"))
