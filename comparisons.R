# Movie vs Book Comparisons
# Taking the combined data file and performing analysis on it.
# Note:  GGplot colors - http://sape.inf.usi.ch/quick-reference/ggplot2/colour

library(dplyr)
library(ggplot2)
library(scales)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
PLOT_DIR    <- paste0(PROJECT_DIR,"/plots")

# First, load the saved data

load(file=paste0(DATA_DIR,"/combined.RData"))         # Movies matched with books

summary(combined$delta)
hist(combined$delta,nclass=20)
plot(combined$bookRating,combined$movieRating)
table(combined$best)
table(combined$titleType,combined$best)
cor(combined$bookRating,combined$movieRating)

ggplot(data=combined, aes(x=movieRating, y=bookRating, color=titleType)) + 
  geom_point(size=0.1) +
  scale_x_continuous(limits=c(1,5)) +
  scale_y_continuous(limits=c(1,5)) +
  theme(legend.position = "bottom") +
  ggtitle("Book rating vs Movie rating") +
  geom_abline(slope=1)
ggsave(paste0(PLOT_DIR,"/scatter.png"))

ggplot(data=combined, aes(x=movieRating, y=bookRating, color=titleType)) + 
  geom_point(size=0.1) +
  scale_x_continuous(limits=c(1,5)) +
  scale_y_continuous(limits=c(1,5)) +
  theme(legend.position = "bottom") +
  facet_wrap(~titleType)+
  ggtitle("Book rating vs Movie rating") +
  geom_abline(slope=1)
ggsave(paste0(PLOT_DIR,"/scatter_by_type.png"))

ggplot(data=combined, aes(y=delta,factor(titleType))) + 
  geom_boxplot() +
  ggtitle("Delta (book-movie) by type") 
ggsave(paste0(PLOT_DIR,"/boxplot.png"))

ggplot(data=combined) + 
  geom_histogram(aes(x=delta),  fill="cornflowerblue" ,color="white",binwidth = 0.1) + 
  geom_vline(aes(xintercept=0))+
  ggtitle("Difference between book and movie")
ggsave(paste0(PLOT_DIR,"/histogram.png"))

ggplot(data=combined) + 
  geom_histogram(aes(x=delta),  fill="cornflowerblue" ,color="white",binwidth = 0.1) + 
  geom_vline(aes(xintercept=0))+
  facet_wrap(~titleType,scales="free_y")+
  ggtitle("Difference between book and movie by media type")
ggsave(paste0(PLOT_DIR,"/histogram_by_type.png"))

