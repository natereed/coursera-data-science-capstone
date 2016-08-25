library(text2vec)
library(data.table)

trie <- new.env()

input <- 'The cat jumped over the hat.'
it <- itoken(input, 
             preprocess_function = tolower, 
             tokenizer = word_tokenizer);

vocab <- create_vocabulary(it, ngram=c(1L, 4L))[[1]];
vocab <- data.table(terms=vocab$terms,
                    term_count = vocab$terms_counts,
                    doc_count=vocab$doc_count)

for (index in 1:nrow(vocab)) {
  row <- vocab[index,]
  print(paste("Inserting", row$terms))
  insert(trie, row)
}

# Utility function to map a regex to a row in the vocab:
get_row <- function(vocab, pat) {
  vocab[grep(pat, vocab$terms),]
}

#the_cat <- get_row(vocab, '^the_cat$')
#insert(trie, the_cat)

# Simple case - inserted first word?
stopifnot(!is.null(names(get(trie, 'the'))))

# 'the' should be linked to 'cat' in its environment
stopifnot('cat' %in% names(get(trie, 'the')))

# last word of n-gram should have a 'p'
stopifnot('p' %in% names(get(trie, 'over_the')))

# longer than 2 words:
stopifnot('hat' %in% names(get(trie, 'over_the')))



