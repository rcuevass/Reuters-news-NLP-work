library(readtext)
library(stringr)
library(quanteda)



# function for substrings
get_substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# function to extract news and dump them into dataframe
get_news_to_dataFrame <- function(list.folders){
  # Intialize all lists
  news.number <- c()
  news.id <- c()
  news.date <- c()
  news.text <- c()
  news.counter <- 0
  for (folderName in list.folders) {
    # Get string corresponding to date
    aux.date.news <- get_substrRight(folderName,8)
    # Read the news
    dfAux <- readtext::readtext(file = paste0(folderName,"/*"),verbosity = 0)
    # Get number of news in folder just read
    aux.num.news <- length(dfAux$doc_id)
    # Loop over all news in folder present
    for (k in c(1:aux.num.news)) {
      # Increase counter of news
      news.counter <- news.counter + 1
      # Append news number to corresponding list
      news.number[news.counter] <- news.counter
      # Append news id to list
      news.id[news.counter] <- dfAux$doc_id[k]
      # Append news text to list
      news.text[news.counter] <- dfAux$text[k]
      # Append date to list
      news.date[news.counter] <- aux.date.news
      
    }
    
  }
  
  
  dzAllNews <- data.frame(news.number,news.id,news.text,news.date)
  
  return(dzAllNews)
  
}

# function to extract list of folders given a path
get_folders_from_path <- function(path.name){
  # Get list of folders containing news
  list.folders <-
    list.dirs(path = path.name,
              recursive = TRUE)
  # length of list
  len.list <- length(list.folders)
  # Update list without the first element
  list.folders <- list.folders[2:len.list]
  
  return(list.folders)
  
}

get_clean_string <- function(raw_string,split.string=FALSE){
  # force to string in case we receive a factor
  raw_string <- as.character(raw_string)
  # Remove "chomp"
  raw_string <- gsub("(\n|<br />)"," ",raw_string)
  # Remove strings assiciated with a webstite link
  raw_string <- raw_string %>%
    stringr::str_replace_all(c("www" = " ","http" = " ",
                               "--" = ""))
  # Change to lower case 
  raw_string <- tolower(raw_string)
  # Split string if requested by user
  if (split.string) {
    raw_string <- strsplit(raw_string, " ")
  }

  return(raw_string)
  
}



