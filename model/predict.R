load_vocabs <- function() {
  vocab = c()
  vocab.append(readRDS(file.path("data", "model", "unigrams.RDS")))
  vocab.append(readRDS(file.path("data", "model", "bigrams.RDS")))
  vocab.append(readRDS(file.path("data", "model", "trigrams.RDS")))
  vocab.append(readRDS(file.path("data", "model", "quadgrams.RDS")))
  return(vocab)
}

get_regex <- function(input) {
  return(paste(paste("^", input, sep=""), "_", sep=""))
}

find_match <- function(ngram, vocab_dt) {
  pat <- get_regex(ngram)
  matches <- vocab_dt[grep(pat, vocab_dt$terms),]
  if (nrow(matches) > 0) {
    return(matches[which.min(matches$p),])
  } else {
    return(NULL)
  }
}

count_n <- function(term) {
  return(length(strsplit(term, '_')[[1]]))
}

get_highest_ranking_term <- function() {
  # Return most frequent unigram  
}

# Given n-gram, get the n-1 n-gram up to the last word
get_sub_ngram(ngram) {
  
}

# Take n-gram up to n=3 and try to predict the next term
predict <- function(ngram) {
  n <- count_n(ngram)
  match <- find_match(ngram, vocabs[n])
  if (!is.null(match)) {
    return(match);
  } else {
    if (n == 1) {
      return(get_highest_ranking_term())
    }
    return(predict(get_sub_ngram(ngram)));
  }
  
}

