# Read the IMDB data and derive a list of movies based on books

# Tried to identify movies based on a book by looking at the 
# principals data set.  This has movie ID and person id (name)
# along with their category (writer) and their job
# (writer of novel/comic/manga etc)
# Problem is, the principals data set does not cover all the 
# movies - when I look for one of the Harry Potter movies
# it does not exist in that data set.
# SO - abandon this approach

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the data
load(file=paste0(DATA_DIR,"/principals.RData"))
load(file=paste0(DATA_DIR,"/crew.RData"))
load(file=paste0(DATA_DIR,"/basics.RData"))
load(file=paste0(DATA_DIR,"/names.RData"))
load(file=paste0(DATA_DIR,"/ratings.RData"))

# Harry Potter where are you
principals %>% filter(tconst=="tt0304141")
crew       %>% filter(tconst=="tt0304141")
basics     %>% filter(tconst=="tt0304141")
names      %>% filter(nconst=="nm0746830")
ratings    %>% filter(tconst=="tt0304141")


# Identify writers who are the writers of the novel the movie was based on.
head(principals)
principals %>% filter(tconst=="tt0304141")  # Harry Potter Check
#head(crew)

# Writer IDs
#head(crew %>% filter(writers != "\\N"),20) # For each movie, can list the writers.

# Identify writers from Principals
summary(principals$category)  # All Categories
summary(principals$job)       # All Jobs

writers <- principals %>% filter(category == "writer") %>% select(-c(characters,ordering))

summary(writers$job)

booktypes <- c("novel","manga","book","based on the novel by","comic","short story",
              "based on the book by"," based upon short stories by","novels")

book_writers <- writers %>% filter(job %in% booktypes)
book_writers$job <- factor(book_writers$job)  # Redefine the factor to remove unused levels
summary(book_writers$job)

# Movies Based on a Book
based_on_a_book <- book_writers %>% 
                   left_join(basics %>% select(tconst,primaryTitle,originalTitle),by="tconst") %>%
                   left_join(names %>% select(nconst,primaryName),by="nconst") %>% 
                   rename(writer = primaryName) %>%
                   left_join(ratings,by="tconst") %>% 
                   rename(movie_rating=averageRating,movie_votes=numVotes)
  
head(based_on_a_book)

save(based_on_a_book,file=paste0(DATA_DIR,"/based_on_a_book.RData"))

# Checking.  What happened to Harry Potter?

basics     %>% filter(tconst=="tt0304141")
principals %>% filter(tconst=="tt0304141")
writers    %>% filter(tconst=="tt0304141")
book_writers %>% filter(tconst=="tt0304141")
crew       %>% filter(tconst=="tt0304141")
