# Problem 2.2 ----

# Original loop:

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 

# Following is the answer for problem 2.2:

# Run after rewrite the lines to use parallel computing

# Load "doParallel" and "foreach"
library("doParallel")
library("foreach")

# First we compute how many cores we have -> detectCores
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores - make them work
cl <- makeCluster(Cores)

# Next - register the cluster:
registerDoParallel(cl)

# Now we can take the time (tictoc) with small adj.

tic(paste0("Test_2", Cores, " cores"))
res <- foreach(i = 1:nrow(df), 
               .combine = rbind, 
               # Add packages needed to run the parallel
               .packages = c('magrittr', 'dplyr', 'tweedie')
  ) %dopar%
    tibble(
     # Add N and M first as it's used in the MTweedieTests function as input. 
     N = df$N[i],
      M = df$M[i],
     share_reject = 
        MTweedieTests(
         N = df$N[i], 
         M = df$M[i], 
         sig = 0.05)
  )

# Stop the cluster
stopCluster(cl)

toc(log = TRUE)

# Store the result:
TicTocLog() %>%
  knitr::kable()

# Test 2: 28.113 sec elapsed
