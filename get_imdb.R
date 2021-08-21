# Get copies of files

library(dplyr)
library(rmutil)

PROJECT_DIR <- "c:/R/Goodreads"
DATA_DIR    <- paste0(PROJECT_DIR,"/data")
FILE_DIR    <- paste0(DATA_DIR,"/tsv")

get_title <- function(file){
  local_file <- paste0(FILE_DIR,"/title.",file,".tsv.gz")
  print(paste("Local file:",local_file))
  remote_file <- paste0("https://datasets.imdbws.com/title.",file,".tsv.gz")
  print(paste("Remote File:",remote_file))
  if (!file.exists(local_file) |
      as.Date(file.info(local_file)$mtime) != Sys.Date()){
    download.file(remote_file,local_file)
  }
}

get_title("basics")
get_title("crew")
get_title("principals")
