# Identify movies Tagged as Based on a book or a comic
# Add Book or Comic flag to movie_list and save as movie_based_on

library(dplyr)
library(tidyr)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/movie_list.RData"))
load(file=paste0(DATA_DIR,"/movie_keywords.RData"))

keyword_based <- movie_keywords %>% filter(grepl("based on",keywords))
summary(factor(keyword_based$keywords))

based_on_book <- c("based on a book",
                   "based on a novel", 
                   "based on autobiography", 
                   "based on biography", 
                   "based on book", 
                   "based on book series", 
                   "based on children's book", 
                   "based on memoir", 
                   "based on novel", 
                   "based on novella", 
                   "based on short stories", 
                   "based on short story", 
                   "based on story", 
                   "based on the bible", 
                   "based on the works of edgar allan poe",
                   "based on the works of h.g. wells", 
                   "based on the works of h.p. lovecraft", 
                   "based on the works of jules verne", 
                   "based on the works of stephen king", 
                   "based on young adult novel")
based_on_comic <-c("based on anime", 
                   "based on comic", 
                   "based on comic book", 
                   "based on comic strip", 
                   "based on franco belgian comic book", 
                   "based on graphic novel", 
                   "based on manga")

movie_based_on_book <- keyword_based %>% group_by(tconst) 

save(movie_based_on_book,file=paste0(DATA_DIR,"/movie_based_on_book.RData"))

# Create lists of ID flagged as based on a book
movie_key_based_on_book <- movie_keywords %>% 
                           filter(keywords %in% based_on_book) %>% 
                           select(tconst) %>% unique()

movie_key_based_on_book$based_on_book <-TRUE

# Create lists of ID flagged as based on a comic
movie_key_based_on_comic <- movie_keywords %>% 
                            filter(keywords %in% based_on_comic) %>% 
                            select(tconst) %>% unique()
movie_key_based_on_comic$based_on_comic <- TRUE
# Flag movies based on book or comic

movie_list_flagged <- movie_list %>% 
                      left_join(movie_key_based_on_book, by="tconst") %>%
                      left_join(movie_key_based_on_comic,by="tconst") %>%
                      replace_na(list(based_on_book=FALSE,based_on_comic=FALSE))
  
summary(movie_list_flagged)
table(movie_list_flagged$based_on_book,movie_list_flagged$based_on_comic)

save(movie_list_flagged,file=paste0(DATA_DIR,"/movie_list_flagged.RData"))

# Tests
print(movie_list_flagged %>% filter(tconst=="tt0120855") %>% select(primaryTitle,writer),n=99) # Disney Tarzan
print(movie_list_flagged %>% filter(tconst=="tt0126029") %>% select(primaryTitle,writer),n=99) # Shrek
print(movie_list_flagged %>% filter(tconst=="tt1014759") %>% select(primaryTitle,writer),n=99) # Alice in Wonderland
print(movie_list_flagged %>% filter(tconst=="tt0043274") %>% select(primaryTitle,writer),n=99) # Alice in Wonderland
print(movie_list_flagged %>% filter(tconst=="tt0164993") %>% select(primaryTitle,writer),n=99) # Alice in Wonderland
print(movie_list_flagged %>% filter(tconst=="tt2567026") %>% select(primaryTitle,writer),n=99) # Alice in Wonderland
print(movie_list_flagged %>% filter(tconst=="tt0170016") %>% select(primaryTitle,writer),n=99) # The Grinch
print(movie_list_flagged %>% filter(tconst=="tt0918940") %>% select(primaryTitle,writer),n=99) # Legend of Tarzan
print(movie_list_flagged %>% filter(tconst=="tt1343092") %>% select(primaryTitle,writer),n=99) # Great Gatsby

print(movie_list_flagged %>% filter(tconst=="tt0241527") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0330373") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt1201607") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0304141") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0295297") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0417741") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0926084") %>% select(primaryTitle,writer),n=99) # Harry Potter
print(movie_list_flagged %>% filter(tconst=="tt0373889") %>% select(primaryTitle,writer),n=99) # Harry Potter

