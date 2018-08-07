library(tm)
library(devtools)
library(LDAvis)
library(lda)
#install.packages("servr")
library(servr)

source(file = "utils/R-code/utils-LDAvis-usage.R")


df1 <- readRDS(file="data/output/news_data_frame_batch_01.rds")
df2 <- readRDS(file="data/output/news_data_frame_batch_02.rds")
df3 <- readRDS(file="data/output/news_data_frame_batch_03.rds")

df <- rbind(df1,df2,df3)
df$news.date <-as.character(df$news.date) 

# Choose news for 2007
sum( (df$news.date >= "20070101") & (df$news.date <= "20071231") )

# Subset for 2007 news
df <- df[which((df$news.date >= "20070101") &
                 (df$news.date <= "20071231")),]


news <- as.character(df$news.text)


# Collect stopwords
stop_words <- tm::stopwords("SMART")

# Clean and fixed news text
news <- get_clean_parsed_text(news)



# tokenize on space and output as a list:
###
doc.list <- strsplit(news, "[[:space:]]+")
###


# compute the table of terms:
##############
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
###############


# remove terms that are stop words or occur fewer than 5 times:
####
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
###


######
documents <- lapply(doc.list, get_terms)
#######



# LDA MODEL FITTING
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
#############

# MCMC and model tuning parameters:
##############
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02
##################



# Fit the model:
set.seed(2018)
t1 <- Sys.time()
LDAfit <- lda::lda.collapsed.gibbs.sampler(documents = documents,
                                           K = K, vocab = vocab, 
                                           num.iterations = G,
                                           alpha = alpha,
                                           eta = eta,
                                           initial = NULL,
                                           burnin = 0,
                                           compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 30 minutes on laptop

# Save model as object

t1 <- Sys.time()
save(LDAfit,file="data/output/LDAfitModel_news_2007.RData")
t2 <- Sys.time()
t2 - t1  # about 30 minutes on laptop




################
theta <- t(apply(LDAfit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(LDAfit$topics) + eta, 2, function(x) x/sum(x)))
################


##############
LDAModel <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
#################



# create the JSON object to feed the visualization:
########################
json <- createJSON(phi = LDAModel$phi, 
                   theta = LDAModel$theta, 
                   doc.length = LDAModel$doc.length, 
                   vocab = LDAModel$vocab, 
                   term.frequency = LDAModel$term.frequency)
################


#serVis(json, out.dir = 'vis', open.browser = FALSE)
serVis(json)
servr::daemon_stop("5421563144")
