library(tm)
library(devtools)
library(LDAvis)
library(lda)
#install.packages("servr")
library(servr)

# Source utiles needed
source(file="utils/R-code/utils-LDAvis-usage.R")

# Get first dataframe for testing
df1 <- readRDS(file="data/output/news_data_frame_batch_01.rds")
#
# Grab only the first 2000 records
text.data <- df1$news.text[1:20]
text.data <- as.character(text.data)

# Collect stopwords
stop_words <- tm::stopwords("SMART")

# Clean and parse list of documents
text.data <- get_clean_parsed_text(text.data)

# tokenize on space and output as a list:
doc.list <- strsplit(text.data, "[[:space:]]+")

# Get table of terms
term.table <- get_term_table(doc.list)

# remove terms that are stop words or occur fewer than 5 times:

vocab <- remove_stop_words_frequency(term.table = term.table,
                                     max.freq = 5 ) 
    
# Get documents in format requested by LDA package
documents <- lapply(doc.list, get.terms)


# LDA MODEL FITTING
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents 
D
W <- length(vocab)  # number of terms in the vocab 
W
# number of tokens per document [312, 288, 170, 436, 291, ...]
doc.length <- sapply(documents, function(x) sum(x[2, ]))  
# total number of tokens in the data 
N <- sum(doc.length)  
N
# frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
term.frequency <- as.integer(term.table)  

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02


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
save(LDAfit,file="data/output/Reuters_2000_docs.RData")


theta <- t(apply(LDAfit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(LDAfit$topics) + eta, 2, function(x) x/sum(x)))


LDAvisTool <- list(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)



#library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = LDAvisTool$phi, 
                   theta = LDAvisTool$theta, 
                   doc.length = LDAvisTool$doc.length, 
                   vocab = LDAvisTool$vocab, 
                   term.frequency = LDAvisTool$term.frequency)


#serVis(json, out.dir = 'vis', open.browser = FALSE)
serVis(json)
servr::daemon_stop("5421563144")
