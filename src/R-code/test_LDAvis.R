# testing LDAvis
# https://ldavis.cpsievert.me/reviews/reviews.html

#install.packages("tm")
library(tm)
#install.packages("devtools")
library(devtools)

install.packages("LDAvis")
library(LDAvis)

install.packages("lda")
library(lda)

# Install LDAvisData tool
#devtools::install_github("cpsievert/LDAvisData")

# Collect data
data(reviews, package = "LDAvisData")

# What is the type of data?
class(reviews)
length(reviews)
class(reviews[1])
reviews[1]

# Collect stopwords
stop_words <- tm::stopwords("SMART")
stop_words


# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews <- tolower(reviews)  # force to lowercase

class(reviews)
reviews[1]


# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")
class(doc.list)
length(doc.list)
doc.list[1]



# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
class(documents)
length(documents)
documents[1]


# LDA MODEL FITTING
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02


# Fit the model:
library(lda)
set.seed(357)
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
save(LDAfit,file="data/output/LDAfitModel.RData")


theta <- t(apply(LDAfit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(LDAfit$topics) + eta, 2, function(x) x/sum(x)))


MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)



library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)

#install.packages("servr")
library(servr)
#serVis(json, out.dir = 'vis', open.browser = FALSE)
serVis(json)
servr::daemon_stop("5421563144")
