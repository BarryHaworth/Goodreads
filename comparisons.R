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

# Stats
summary(combined$delta)
hist(combined$delta,nclass=20)
plot(combined$bookRating,combined$movieRating)
table(combined$best)
table(combined$titleType,combined$best)
cor(combined$bookRating,combined$movieRating)

# Scatter Plots
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

# Box Plots
ggplot(data=combined, aes(y=delta,factor(titleType))) + 
  geom_boxplot() +
  ggtitle("Delta (book-movie) by type") 
ggsave(paste0(PLOT_DIR,"/boxplot.png"))

# Histograms
delta_mean <- mean(combined$delta)
book_pct  <- sum(combined$best=="Book")/length(combined$best)
movie_pct <- 1-book_pct
ggplot(data=combined) + 
  geom_histogram(aes(x=delta),  fill="cornflowerblue" ,color="white",binwidth = 0.1) + 
  geom_vline(aes(xintercept=0),color="blue")+
  geom_vline(aes(xintercept=delta_mean),color="black")+
  geom_text(aes(x=Inf, y=Inf, label=paste("Book is better\n",percent(book_pct,accuracy=0.1),"of the time")),vjust="inward", hjust="inward")+
  geom_text(aes(x=-Inf, y=Inf, label=paste("Movie is better\n",percent(movie_pct,accuracy=0.1),"of the time" )),vjust="inward", hjust="inward")+
  ggtitle("Difference between book and movie (all records)",
          subtitle=paste("Mean=",round(delta_mean,3))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave(paste0(PLOT_DIR,"/histogram.png"))

hist_type <- function(type){
  temp_data <- combined %>% filter(titleType==type)
  delta_mean <- mean(temp_data$delta)
  book_pct  <- sum(temp_data$best=="Book")/length(temp_data$best)
  movie_pct <- 1-book_pct
  print(ggplot(data=temp_data) + 
    geom_histogram(aes(x=delta),  fill="cornflowerblue" ,color="white",binwidth = 0.1) + 
    geom_vline(aes(xintercept=0),color="blue")+
    geom_vline(aes(xintercept=delta_mean),color="black")+
    geom_text(aes(x=Inf, y=Inf, label=paste("Book is better\n",percent(book_pct,accuracy=0.1),"of the time")),vjust="inward", hjust="inward")+
    geom_text(aes(x=-Inf, y=Inf, label=paste(type,"is better\n",percent(movie_pct,accuracy=0.1),"of the time" )),vjust="inward", hjust="inward")+
    ggtitle(paste("Difference between Book and",type),
            subtitle=paste("Mean=",round(delta_mean,3))) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)))
  ggsave(paste0(PLOT_DIR,"/histogram_",type,".png"))
}
hist_type("movie")
hist_type("tvMovie")
hist_type("tvMiniSeries")
hist_type("tvSeries")

ggplot(data=combined) + 
  geom_histogram(aes(x=delta),  fill="cornflowerblue" ,color="white",binwidth = 0.1) + 
  geom_vline(aes(xintercept=0))+
  facet_wrap(~titleType,scales="free_y")+
  ggtitle("Difference between book and movie by media type")
ggsave(paste0(PLOT_DIR,"/histogram_by_type.png"))

time_breaks <- c(30,60,90,120,180,240,300,480,600,900,1800,1200,3000,6000,12000,24000) # Total Time Breaks for tick marks

# Delta by Length
ggplot(data=combined %>% filter(totalRuntime >= 30, totalRuntime < 30000), 
       aes(x=totalRuntime, y=delta)) + 
  geom_point(size=0.2,color="deeppink") +
  geom_smooth() +
  scale_x_log10(breaks=time_breaks) +
  theme(legend.position = "bottom") +
  ggtitle("Book vs Movie Delta by length in minutes") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0(PLOT_DIR,"/delta_all.png"))

ggplot(data=combined %>% filter(totalRuntime >= 30, totalRuntime < 30000), 
       aes(x=totalRuntime, y=delta, color=titleType)) + 
  geom_point(size=0.2) +
  geom_smooth() +
  scale_x_log10(breaks=time_breaks) +
  theme(legend.position = "bottom") +
  ggtitle("Book vs Movie Delta by length in minutes") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(paste0(PLOT_DIR,"/delta_types.png"))

length_plot <- function(type){
  temp_data <- combined %>% filter(titleType==type, totalRuntime >= 30, totalRuntime < 30000)
  print(ggplot(data=temp_data, 
               aes(x=totalRuntime, y=delta)) + 
          geom_point(size=0.2,color="deeppink") +
          geom_smooth() +
          scale_x_log10(breaks=time_breaks) +
          theme(legend.position = "bottom") +
          ggtitle(paste("Book vs Movie Delta by length in minutes for",type)) +
          theme(plot.title = element_text(hjust = 0.5)))
  ggsave(paste0(PLOT_DIR,"/delta_",type,".png"))
}
length_plot("movie")
length_plot("tvMovie")
length_plot("tvMiniSeries")
length_plot("tvSeries")

