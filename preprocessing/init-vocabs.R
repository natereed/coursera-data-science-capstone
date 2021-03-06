library(text2vec)
library(tm)
library(data.table)

t <- proc.time()

# Load data
setwd( file.path("C:/", "Users", "Owner", "Projects", "Coursera", "Data Science Capstone"))

dir <- file.path("final", "en_US")
news_path <- file.path(dir, "en_US.news.txt")
blogs_path <- file.path(dir, "en_US.blogs.txt")
twitter_path <- file.path(dir, "en_US.twitter.txt")

news_data <- readLines(news_path, encoding='UTF-8')
blogs_data  <- readLines(blogs_path, encoding='UTF-8')
twitter_data    <- readLines(twitter_path, encoding='UTF-8')

# Subset
sample_size <- 0.30
set.seed(1234)
news_subset <- sample(news_data, length(news_data) * sample_size)
rm(news_data)

blogs_subset <- sample(blogs_data, length(blogs_data) * sample_size)
rm(blogs_data)

twitter_subset <- sample(twitter_data, length(twitter_data) * sample_size)
rm(twitter_data)

combined_doc <- sample(c(news_subset, blogs_subset, twitter_subset))
rm(news_subset, blogs_subset, twitter_subset)

# Clean
combined_doc <- iconv(combined_doc, 'utf-8', 'ascii', sub='')

# Create test and train subsets
# Split into test/train
split = 0.9
train_ind <- sample(length(combined_doc),
                    size=split * length(combined_doc))
train_subset <- combined_doc[train_ind]
test_subset <- combined_doc[-train_ind]
rm(train_ind)
rm(combined_doc)

if (!file.exists("swearwords.txt")) {
  download.file(url="https://s3.amazonaws.com/natereed-coursera/swearwords.txt", 
                destfile="swearwords.txt",
                method="curl")
}
swear_words <- readLines("swearwords.txt")

clean_twitter <- function(t) {
  # Remove hashes
  t <- gsub('#', '', t)
  
  # Remove email addresses
  t <- gsub("[[:alnum:]]+\\@[[:alpha:]]+\\.com", '', t)
  
  # Remove url's
  t <- gsub('http\\S+\\s*', '', t)
  
  # Translate some common "textese" (a.k.a. "SMS language")
  # The list of such words is very long. TBD: Add comprehensive translation.
  t <- gsub('\\br\\b', 'are', t)
  t <- gsub('\\b@\\b', 'at', t)
  t <- gsub('\\bjst\\b', 'just', t)
  t <- gsub('\\bluv\\b', 'love', t)
  t <- gsub('\\bu\\b', 'you', t)
  return(t)
}

strip_profanity_from_entry <- function(v) {
  expr <- do.call(paste, c(as.list(swear_words), sep="|"))
  return(v[grep(expr, v, invert=TRUE)])
}

strip_profanity <- function(vector_list) {
  return(lapply(vector_list, strip_profanity_from_entry))
}

# Chain functions for tokenizing and cleaning:
cleaning_tokenizer <- function(v) {
  v %>% 
    clean_twitter %>%
    removeNumbers %>%
    removePunctuation %>%
    word_tokenizer %>%
    strip_profanity
} 

calc_probabilities <- function(vocab) {
  vocab$p <- vocab$terms_counts / sum(vocab$terms_counts)
  return(vocab)
}

build_vocab <- function(n, text_vec) {
  it <- itoken(text_vec, 
               preprocess_function = tolower, 
               tokenizer = cleaning_tokenizer);

  # Create vocab from iterator
  print(paste("Initializing vocabulary for n=", n))
  vocab <- create_vocabulary(it, ngram=c(n, n));
  vocab <- vocab[[1]]
  return(calc_probabilities(vocab))
}

unigrams <- build_vocab(1, train_subset)
bigrams <- build_vocab(2, train_subset)
trigrams <- build_vocab(3, train_subset)
quadgrams <- build_vocab(4, train_subset)
vocabs <- list(unigrams, bigrams, trigrams, quadgrams)

test_vocab <- build_vocab(4, test_subset)

rm(train_subset)
rm(test_subset)
rm(swear_words)

library(feather)
# Store terms and term counts on disk
store_vocab <- function(vocab, filename) {
  print("Converting to data.table...")
  vocab_dt <- data.table(terms=vocab$terms,
                       term_count = vocab$terms_counts,
                       doc_count=vocab$doc_count,
                       p=vocab$p)
  write_feather(vocab, filename)
}

dir.create(file.path(getwd(), "data", "model"), showWarnings = FALSE)
dir.create(file.path(getwd(), "data", "test"), showWarnings = FALSE)

saveRDS(unigrams, "data/model/unigrams.RDS")
saveRDS(bigrams, "data/model/bigrams.RDS")
saveRDS(trigrams, "data/model/trigrams.RDS")
saveRDS(quadgrams, "data/model/quadgrams.RDS")

saveRDS(test_vocab, "data/test.RDS")
t <- proc.time() - t
print(paste(t[['elapsed']], "seconds."))

