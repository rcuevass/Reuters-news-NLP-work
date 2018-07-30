library(readtext)
library(stringr)

#install.packages("Matrix")
#install.packages("quanteda")
library(quanteda)
#install.packages("topicmodels")
library(topicmodels)



# Soruce utilities functions
source(file="utils/R-code/utils-docs-parsing.R")

# Get list of files under the selected PATH
list.folders <- get_folders_from_path(path.name = "data/input/ReutersNews106521")


# The following is a quick and dirty section to fecth news under various
# folders and dump them to RDS files
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
dgAux_06 <- get_news_to_dataFrame(list.folders[2001:2300])
print("sixth batch read")

# Save files to output data folder
saveRDS(dgAux_01,file="data/output/news_data_frame_batch_01.rds")
saveRDS(dgAux_02,file="data/output/news_data_frame_batch_02.rds")
saveRDS(dgAux_03,file="data/output/news_data_frame_batch_03.rds")
saveRDS(dgAux_04,file="data/output/news_data_frame_batch_04.rds")
saveRDS(dgAux_05,file="data/output/news_data_frame_batch_05.rds")
saveRDS(dgAux_06,file="data/output/news_data_frame_batch_06.rds")

#
# Let's experiment with one of the dataframes
# We add a column with clean text
dgAux_01$news.clean.text <- sapply(dgAux_01$news.text,
                                   function(x) get_clean_string(x))

# We check changes took effect
dim(dgAux_01)
names(dgAux_01)

# Let's take only few records of the newly created column to test
# creating a corpus
myTest <- dgAux_01$news.clean.text[1:10]
# Create corpus
myCorpus <- quanteda::corpus(myTest)

# We create list of characters to be removed; stop words are also
# removed
removed_chars <- c("-",".","/","&","xxxx","XXX00",
                   "s","u","p","35","currency","100M",
                   quanteda::stopwords("english"))
# We create document-feature matrix
my_dfm <- quanteda::dfm(myCorpus,remove=removed_chars,
                        stem=TRUE,
                        verbose = TRUE,
                        tolower=TRUE)

# Brief inspection of document-feature matrix
my_dfm[,1:10]

# We explore top features
quanteda::topfeatures(my_dfm,50)


# We can visualize top features with word cloud
set.seed(2018)
quanteda::textplot_wordcloud(my_dfm,random_order=FALSE,rotation=0.25,
                             color = RColorBrewer::brewer.pal(8,"Dark2"))


# Locating keywords in context
quanteda::kwic(myCorpus,"growth")
quanteda::kwic(myCorpus,"china")
quanteda::kwic(myCorpus,"oil")
quanteda::kwic(myCorpus,"gas")


# Let's try to quantiy how similar the descriptions are. 
# In other words, we get similarity
#  of documents based on a chosen metric.
#  We plot it as a matrix to make it more informative

description_similarity <-
  quanteda::textstat_simil(my_dfm,margin="documents",method ="cosine")

sna::plot.sociomatrix(as.matrix(description_similarity),
                      diaglab = FALSE,cex.lab = 0.5)


#
# TOPIC MODELING OF DOCUMENTS
#
quant_dfm <- quanteda::dfm_trim(my_dfm,min_termfreq = 1, max_termfreq = 5)
as.matrix(quant_dfm)[1:10,1:10]

set.seed(2018)
if(require(topicmodels)){
  myLDA <- topicmodels::LDA(convert(quant_dfm,to="topicmodels"),k=6)
  terms_Docs <- topicmodels::get_terms(myLDA,10)
  terms_Docs
}
