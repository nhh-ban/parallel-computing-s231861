# Assignment 1:  
library(tweedie) 
library(ggplot2)

library(tictoc) # Added for solving problem
library(tidyverse)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


# Problem 2.1 ---- 

# Run the code as it is and take the time

# To capture the time elapsed, need to make a function
# that can then print out the result as a data frame
TicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>%
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

# Clear the tic/toc log
tic.clearlog()

# By using the tic - toc we can calc. the time to run the code
tic("Test_1") # Name the test
  for(i in 1:nrow(df)){ 
    df$share_reject[i] <-  
      MTweedieTests( 
       N=df$N[i], 
       M=df$M[i], 
       sig=.05) 
  } 
# log = T -> Push the result in a list
toc(log = TRUE)

# To store the result:
TicTocLog() %>%
  knitr::kable()

# Test 1: 49.617 sec elapsed


# Problem 2.2 ----

# Run after rewrite the lines to use parallel computing

# Load "doParallel"
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

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 


toc(log = TRUE)
## Assignemnt 4 
   
# This is one way of solving it - maybe you have a better idea? 
# First, write a function for simulating data, where the "type" 
# argument controls the distribution. We also need to ensure 
# that the mean "mu" is the same for both distributions. This 
# argument will also be needed in the t-test for the null 
# hypothesis. Therefore, if we hard code in a value here 
# we may later have an inconsistency between the mean of the 
# distributions and the t-test. So, we add it as an explicit 
# argument:  


library(magrittr)
library(tidyverse)

simDat <-
  function(N, type, mu) {
    if (type == "tweedie") {
      return(rtweedie(
        N,
        mu = mu,
        phi = 100,
        power = 1.9
      ))
    }
    if (type == "normal") {
      return(rnorm(N, mean = mu))
    }
    else{
      stop("invalid distribution")
    }
  }


# Next, the test. Note, we use mu two places:
# both for the data simulation and as the null.
simTest <-
  function(N, type, mu) {
    t.test(simDat(N = N,
                  type = type,
                  mu = mu),
           mu = mu)$p.value
  }


# Running many tests is almost the same as before.
# Here the mean is hard coded in, as we're not
# going to change it.
MTests <-
  function(N, M, type, sig) {
    sum(replicate(M,
                  simTest(
                    N = N,
                    type =
                      type,
                    mu =
                      10000
                  )) < sig) / M
  }


# We can now repeat the same analysis as before,
# but for both the tweedie and the normal:
df <-
  expand.grid(
    N = c(10, 100, 1000, 5000),
    M = 1000,
    type = c("tweedie", "normal"),
    share_reject = NA
  ) %>%
  as_tibble()


for (i in 1:nrow(df)) {
  print(i)
  df$share_reject[i] <-
    MTests(df$N[i],
           df$M[i],
           df$type[i],
           .05)
}

# As you see, with normally distributed data, N can
# be very small and the t-test is fine. With a tweedie,
# "large enough" can be many thousands. If we try
# different distributions or parameterizations, we might
# also get different results.
df %>%
  ggplot2::ggplot(aes(x = log(N), y = share_reject, col = type)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw() 
