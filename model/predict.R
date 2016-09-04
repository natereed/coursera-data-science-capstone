load_vocabs <- function() {
  vocabs <- list()
  vocabs[[1]] <- readRDS(file.path("data", "model", "unigrams.RDS"))
  vocabs[[2]] <- readRDS(file.path("data", "model", "bigrams.RDS"))
  vocabs[[3]] <- readRDS(file.path("data", "model", "trigrams.RDS"))
  vocabs[[4]] <- readRDS(file.path("data", "model", "quadgrams.RDS"))
  return(vocabs)
}

get_regex <- function(input) {
  return(paste(paste("^", input, sep=""), "_", sep=""))
}

find_match <- function(ngram, vocab) {
  pat <- get_regex(ngram)
  matches <- vocab[grep(pat, vocab$terms),]
  if (nrow(matches) > 0) {
    return(matches[which.max(matches$p),])
  } else {
    return(NULL)
  }
}

count_n <- function(ngram) {
  return(length(strsplit(ngram, '_')[[1]]))
}

get_highest_ranking_term <- function() {
  return(unigrams[which.max(unigrams$p)])  
}

# Given n-gram, the n-gram that starts with the second word
get_sub_ngram <- function(ngram, n) {
  return(paste(strsplit(ngram, '_')[[1]][2:n], collapse="_"))
}

slice_ngram <- function(ngram, start, end) {
  return(paste(strsplit(ngram, '_')[[1]][start:end], collapse="_"))
}

# Take n-gram up to n=3 and try to predict the next term
predict <- function(ngram) {
  n <- count_n(ngram)
  if (n > 3) {
    ngram <- slice_ngram(ngram, n - 2, n)
    n <- 3
  }
  
  match <- find_match(ngram, vocabs[[n+1]])
  if (!is.null(match)) {
    return(match);
  } else {
    if (n == 1) {
      return(get_highest_ranking_term())
    }
    return(predict(get_sub_ngram(ngram, n)));
  }
}

vocabs <- load_vocabs();
