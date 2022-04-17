---
editor_options: 
  markdown: 
    wrap: 80
---

# Goodreads

Answering the age-old question: is the book better?

This project is intended to compare ratings of books and movies, to see which is
rated most highly.

Book ratings are taken from a data set sourced from Kaggle

Movie ratings are taken from IMDB.

Books and movies are to be matched by title and author.

The programs:

Current

**read_goodreads**.R - read the goodreads data file from Kaggle

**get_imdb**.R - download the data files from imdb

**movie_list**.R - combine the imdb files to form movie_list RData file, with
movie details (title, year, type) and writers.

**match_all**.R - match complete list of movies with complete list of books by
title and author, and saves matches

**matched_rating**.R - download the IMDB ratings for all the matched movies \<-
DONE

**update_rating**.R - check the vote counts for all saved ratings and update
those that have changed. \<- DONE

**combine**.R - combine the matched movies&books with their respective ratings.

**compare**.R - perform books and movie rating comparisons for movies matched
with books.

Superceeded (old_code):

movie_keywords.R - works through list of movies and rips keyword information for
all movies movies_based_on_books.R - reads the downloaded keyword information
and identifies movies flagged as based on books or comics

based_on_ratings.R - takes the list movies based on books & comics and rips the
rating details for these movies

match_flag.R - takes the list of movies flagged as based on a book and attempts
to match them with the book list. Has the disadvantage that not all movies based
on books are flagged as such on IMDB

movie_list_principals.R - attempts to identify movies based on books by looking
at the principals data from imdb. the principals data set does not cover all the
movies - when I look for one of the Harry Potter movies it does not exist in
that data set.
