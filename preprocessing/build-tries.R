print("Building trie...")

num_records_to_load <- 100000
chunk_size <- 50000
records_to_load <- train_subset[1:num_records_to_load,]
#rm(train_subset)

chunk_apply <- function(chunkable, chunk_size, size_of_fun, chunk_fun) {
  num_chunks <- ceiling(size_of_fun(chunkable) / chunk_size)
  pb <- txtProgressBar(style=3)
  for (i in seq(1, num_chunks)) {
    print(paste("Processing chunk #", i))
    start_ind <- (i - 1) * chunk_size + 1
    end_ind <- start_ind + chunk_size - 1
    print(paste(start_ind, end_ind, sep=":"))
    if (end_ind > size_of_fun(chunkable)) {
      end_ind <- size_of_fun(chunkable)
    }
    chunk <- chunkable[start_ind:end_ind,]
    chunk_fun(chunk)
    setTxtProgressBar(pb, size_of_fun(chunk) / size_of_fun(chunkable))
  }
}

trie <- new.env()

load_records <- function(records_to_load) {
  print(paste("Loading", nrow(records_to_load), "records"))
  num_loaded <- 0
  pb <- txtProgressBar(style=3)
  for (i in 1:nrow(records_to_load)) {
    insert(trie, records_to_load[i,])
    num_loaded <- num_loaded + 1
    setTxtProgressBar(pb, num_loaded / num_records_to_load)
  }
  t <- proc.time() - t
  print(paste(t[['elapsed']], "seconds."))
}

t <- proc.time()
chunk_apply(records_to_load, chunk_size=chunk_size, size_of_fun=nrow, chunk_fun=load_records)
t <- proc.time() - t

#save(trie, file="trie.data")
