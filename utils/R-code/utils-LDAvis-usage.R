# Function to get parsed and clean text
get_clean_parsed_text <- function(list.text) {
  
  # pre-processing:
  list.text <- gsub("'", "", list.text)  # remove apostrophes
  list.text <- gsub("[[:punct:]]", " ", list.text)  # replace punctuation with space
  list.text <- gsub("[[:cntrl:]]", " ", list.text)  # replace control characters with space
  list.text <- gsub("^[[:space:]]+", "", list.text) # remove whitespace at beginning of documents
  list.text <- gsub("[[:space:]]+$", "", list.text) # remove whitespace at end of documents
  list.text <- tolower(list.text)  # force to lowercase
  
  return(list.text)
  
}

get_term_table <- function(doc.list) {
  
  
  # compute the table of terms:
  term.table <- table(unlist(doc.list))
  term.table <- sort(term.table, decreasing = TRUE)
  
  return(term.table)
}


# function to remove stop words and words with less than 
# a given frequency
remove_stop_words_frequency <- function(term.table,max.freq) {
  
  del <- names(term.table) %in% stop_words | term.table < max.freq
  term.table <- term.table[!del]
  vocab <- names(term.table)
  
  return(vocab)
}


# Function used to put the documents into the format required
# by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
