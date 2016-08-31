#load("trie.data")

predict <- function(trie, ngram) {
  matches <- names(get(trie, ngram))
  f <- function(match) {
    print(paste("Looking up", match))
    if (match != '$p') {
      p <- trie[[ngram]][[match]]$`$p`
      print(p)
      return(p)
    }
  }
  p <- sapply(matches, f)
  arrange(p)
  
}

