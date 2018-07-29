# https://stringr.tidyverse.org/reference/str_replace.html

install.packages("readtext")
library(readtext)


library(stringr)
#install.packages("tidyverse")
#library(tidyverse)

install.packages("quanteda")
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



# https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html#plain-text-files-.txt

# Simple experimentation

list.folders <- get_folders_from_path(path.name = "data/ReutersNews106521")

length(list.folders)

dgAux_01 <- get_news_to_dataFrame(list.folders[1:400])
print("first batch read")
dgAux_02 <- get_news_to_dataFrame(list.folders[401:800])
print("second batch read")
dgAux_03 <- get_news_to_dataFrame(list.folders[901:1200])
print("third batch read")
dgAux_04 <- get_news_to_dataFrame(list.folders[1201:1600])
print("fourth batch read")
dgAux_05 <- get_news_to_dataFrame(list.folders[1601:2000])
print("fifth batch read")
dgAux_06 <- get_news_to_dataFrame(list.folders[2001:2400])
print("sixth batch read")
dgAux_07 <- get_news_to_dataFrame(list.folders[2401:(length(list.folders))])
print("seventh batch read")


saveRDS(dgAux_01,file="news_data_frame_batch_01.rds")
saveRDS(dgAux_02,file="news_data_frame_batch_02.rds")
saveRDS(dgAux_03,file="news_data_frame_batch_03.rds")
saveRDS(dgAux_04,file="news_data_frame_batch_04.rds")
saveRDS(dgAux_05,file="news_data_frame_batch_05.rds")
saveRDS(dgAux_06,file="news_data_frame_batch_06.rds")


textTest <- dgAux_01$news.text[1]


#======== working logic for text processing ===========
# Notes:
# 1. Text loaded from files is given as factors
# 2. Given the above, they need to be changed to characters
# as.character(textTest)

textTest <- dgAux_01$news.text[1]
textTest <- as.character(textTest)
textTest <- as.vector(textTest)
textTest
testCorpus <- quanteda::corpus(textTest)
testCorpus$documents

getwd()




textTest <- dgAux_01$news.text[1]
textTest
#textTest <- gsub("(\n|<br />|\)"," ",textTest)
textTest <- gsub("(\n|<br />)"," ",textTest)



#textTest <- stringr::str_replace(c("www","http")," ",textTest)
#textTest <- stringr::str_replace_all(textTest, "[[:punct:]]", " ")
textTest <- textTest %>%
  stringr::str_replace_all(c("www" = " ","http" = " "))
textTest <- tolower(textTest)
textTest <- strsplit(textTest, " ")
textTest
