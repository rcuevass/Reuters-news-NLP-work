library(readtext)
library(stringr)
library(quanteda)

source(file="utils/R-code/utils-docs-parsing.R")


list.folders <- get_folders_from_path(path.name = "data/input/ReutersNews106521")


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
#dgAux_07 <- get_news_to_dataFrame(list.folders[2401:(length(list.folders))])
#print("seventh batch read")


saveRDS(dgAux_01,file="data/output/news_data_frame_batch_01.rds")
saveRDS(dgAux_02,file="data/output/news_data_frame_batch_02.rds")
saveRDS(dgAux_03,file="data/output/news_data_frame_batch_03.rds")
saveRDS(dgAux_04,file="data/output/news_data_frame_batch_04.rds")
saveRDS(dgAux_05,file="data/output/news_data_frame_batch_05.rds")
saveRDS(dgAux_06,file="data/output/news_data_frame_batch_06.rds")


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
