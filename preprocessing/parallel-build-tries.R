num_records_to_load <- 100000 #nrow(train_subset)
records_to_load <- train_subset[1:num_records_to_load]
#rm(train_subset)

load_records <- function(records_to_load) {
  trie <- new.env()
  print(paste("Loading", nrow(records_to_load), "records"))
  num_loaded <- 0

  #pb <- txtProgressBar(style=3)
  for (i in 1:nrow(records_to_load)) {
    insert(trie, records_to_load[i,])
    #num_loaded <- num_loaded + 1
    #setTxtProgressBar(pb, num_loaded / nrow(records_to_load))
  }
  return(trie)
}

# Create subsets based on num cores
# Hard-coded for now
s1 <- records_to_load[grep('^[a-e]', records_to_load$terms)]
s2 <- records_to_load[grep('^[f-n]', records_to_load$terms)]
s3 <- records_to_load[grep('^[o-z]', records_to_load$terms)]

library(foreach)
library(doParallel)

# This is the proper way to determine how many cores are available: no_cores <- detectCores() - 1
cl <- makeCluster(3) # Hard-code for now
clusterExport(cl, c("insert")) # Part of parallel package. use with parLapply.

print("OK -- Entering parallel shit")
subsets <- list(s1,s2,s3)
t <- proc.time()
tries <- foreach(i=1:length(subsets)) %dopar% {
  load_records(subsets[[i]])
}

t <- proc.time() - t
print(paste(t[['elapsed']], "seconds."))
