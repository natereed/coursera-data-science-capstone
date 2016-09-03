get_regex <- function(input) {
  return(paste(paste("^", input, sep=""), "_", sep=""))
}

find_matches <- function(ngram, vocab_dt) {
  pat <- get_regex(ngram)
  matches <- vocab_dt[grep(pat, vocab_dt$terms),]
  if (nrow(matches) > 0) {
    return(matches[which.min(matches$p),])
  } else {
    return(NULL)
  }
}

