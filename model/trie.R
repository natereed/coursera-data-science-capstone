insert <- function(trie, ngram) {
  term <- ngram$terms
  p <- ngram$p
  tokens <- strsplit(term, '_')[[1]]  
  n <- length(tokens)
  
  node <- trie
  for (i in 1:n) {
    token <- tokens[i]
    #print(paste("Token: ", token))
    entry <- node[[token]]
    if (is.null(entry)) {
      entry <- new.env()
      node[[token]] <- entry
    } 
    node <- entry
    
    # Last entry?
    if (i == n) {
      entry[['p']] = p
    }
  }
}

get <- function(trie, term) {
  tokens <- strsplit(term, '_')[[1]]  
  n <- length(tokens)
  node <- trie
  for (i in 1:n) {
    token <- tokens[i]
    entry <- node[[token]]
    if (is.null(entry)) {
      return(NULL)
    }
    if (i == n) {
      return(entry)
    }
    node <- entry
  }
}

print_trie <- function(trie, level=1) {
  names <- names(trie)
  indent <- paste(rep('    ', level - 1), sep="", collapse="")
  for (name in names) {
    print(paste(indent, name, sep=""))
    val <- trie[[name]]
    if (typeof(val) == 'environment') {
      print_trie(val, level=level+1)
    } else {
      print(paste(indent, name, ": ", val, sep=""))
    }
  }
}

