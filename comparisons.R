# Movie vs Book Comparisons
# Taking the combined data file and performing analysis on it.

library(dplyr)
library(ggplot2)
library(scales)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# First, load the saved data

load(file=paste0(DATA_DIR,"/combined.RData"))         # Movies matched with books

summary(combined$delta)
hist(combined$delta,nclass=20)
plot(combined$bookRating,combined$movieRating)
table(combined$best)
table(combined$titleType,combined$best)
cor(combined$bookRating,combined$movieRating)

ggplot(data=combined, aes(x=movieRating, y=bookRating, color=titleType)) + 
  geom_point(size=1) +
  scale_x_continuous(limits=c(1,5)) +
  scale_y_continuous(limits=c(1,5)) +
  theme(legend.position = "bottom") +
  ggtitle("Book rating vs Movie rating") +
  geom_abline(slope=1)

ggplot(data=combined, aes(y=delta,factor(titleType))) + 
  geom_boxplot() +
  ggtitle("Delta (book-movie) by type") 

ggplot(data=combined) + geom_histogram(aes(x=delta, fill = ..x..), binwidth = 0.1) + 
  scale_fill_gradient2(low = muted("red"),mid = "white",high = muted("blue"),midpoint = 0 ) +
  ggtitle("Difference between book and movie")
