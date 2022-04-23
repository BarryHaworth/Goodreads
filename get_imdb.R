# Get copies of files
# Files are downloaded from IMDB.
# Files are filtered to include movies & TV Movies only

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/two_goats"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
FILE_DIR    <- paste0(DATA_DIR,"/tsv")

get_title <- function(file){
  local_file <- paste0(FILE_DIR,"/",file,".tsv.gz")
  remote_file <- paste0("https://datasets.imdbws.com/",file,".tsv.gz")
  if (!file.exists(local_file) |
      as.Date(file.info(local_file)$mtime) != Sys.Date()){
    if (!file.exists(local_file)) print(paste("Downloading New File:",remote_file,"to Local file:",local_file)) 
    else print(paste("Updating Remote File:",remote_file,"to Local file:",local_file))
    download.file(remote_file,local_file)
  } else {
    print(paste("File",local_file,"Already Exists"))
  }
}

# Download the files
get_title("name.basics")
get_title("title.akas")
get_title("title.basics")
get_title("title.crew")
get_title("title.principals")
get_title("title.ratings")

basics  <- read.delim(paste0(FILE_DIR,"/title.basics.tsv.gz") ,stringsAsFactors = FALSE)
# Clean Basics
# basics <- basics[basics$titleType=="movie",]  # Only keep movies
keeptypes <- c("movie","tvMovie","tvMiniSeries","tvSeries")
basics <- basics %>% filter(titleType %in% keeptypes)  # Only keep selected types

basics <- basics[is.na(basics$runtimeMinutes)==FALSE,]  # Drop unknown runtime
basics <- basics[basics$startYear <= as.numeric(substr(Sys.Date(),1,4)),]   # drop release date after this year
# Set types for columns
basics$titleType <- as.factor(basics$titleType)
basics$isAdult   <- as.numeric(basics$isAdult)
basics$startYear <- as.numeric(basics$startYear)
basics$endYear   <- as.numeric(basics$endYear)
basics$runtimeMinutes <- as.numeric(basics$runtimeMinutes)
save(basics,file=paste0(DATA_DIR,"/basics.RData"))

#  Filter and save the results
movies_only <- basics %>% select(tconst)

# Ratings
ratings <- read.delim(paste0(FILE_DIR,"/title.ratings.tsv.gz") ,stringsAsFactors = FALSE)
ratings <- ratings %>% inner_join(movies_only,by="tconst")  # Filter on Movies only
save(ratings,file=paste0(DATA_DIR,"/ratings.RData"))

# Names (Can't filter names as they don't have tconst)
names  <- read.delim(paste0(FILE_DIR,"/name.basics.tsv.gz") ,stringsAsFactors = FALSE)
head(names)
summary(names)
save(names,file=paste0(DATA_DIR,"/names.RData"))  # Save names data frame

# Principals
principals  <- read.delim(paste0(FILE_DIR,"/title.principals.tsv.gz") ,stringsAsFactors = FALSE)
# Clean principals
principals <- principals %>% inner_join(movies_only,by="tconst")  # Filter on Movies only
principals$category <- as.factor(principals$category)
summary(principals$category)
# principals$job <- as.factor(principals$job)
# summary(principals$job)
save(principals,file=paste0(DATA_DIR,"/principals.RData"))  # Save Principals data frame

# Crew
crew  <- read.delim(paste0(FILE_DIR,"/title.crew.tsv.gz") ,stringsAsFactors = FALSE)
crew  <- crew %>% inner_join(movies_only,by="tconst")  # Filter on Movies only
save(crew,file=paste0(DATA_DIR,"/crew.RData"))   # Save Crew Data Frame


