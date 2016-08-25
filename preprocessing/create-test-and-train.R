library(text2vec)
library(tm)
library(data.table)

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
sample_size <- 0.05
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

build_vocab <- function(n) {
  it <- itoken(combined_doc, 
               preprocess_function = tolower, 
               tokenizer = cleaning_tokenizer);

  # Create vocab from iterator
  print(paste("Initializing vocabulary for n=", n))
  t <- proc.time()
  vocab <- create_vocabulary(it, ngram=c(n, n));
  t <- proc.time() - t
  print(paste(t[['elapsed']], "seconds."))
  return(vocab[[1]])
}

calc_probabilities <- function(vocab) {
  vocab$p <- vocab$terms_counts / sum(vocab$terms_counts)
  return(vocab)
}

v <- lapply(seq(1,4), build_vocab)
vocab <- rbindlist(lapply(v, calc_probabilities))
rm(v)
rm(combined_doc)
rm(swear_words)

# Store terms and term counts on disk
print("Converting to data.table...")
t <- proc.time()
vocab_dt <- data.table(terms=vocab$terms,
                       term_count = vocab$terms_counts,
                       doc_count=vocab$doc_count,
                       p=vocab$p)
t <- proc.time() - t
print(paste(t[['elapsed']], "seconds."))

rm(vocab)

# Split into test/train
split = 0.70
train_ind <- sample(nrow(vocab_dt),
                    size=split * nrow(vocab_dt))
train_subset <- vocab_dt[train_ind,]
test_subset <- vocab_dt[-train_ind]
rm(train_ind)
rm(vocab_dt)

library(feather)
write_feather(train_subset, "train.feather")
write_feather(test_subset, "test.feather")
