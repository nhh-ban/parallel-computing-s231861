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

# Clear the tic/toc log when this is the first value!!!!
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

