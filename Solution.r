
# Loadning pachages ----
library(tweedie) 
library(ggplot2)
library(lubridate)
library(purrr)
library(forcats)
library(tictoc)
library(tidyverse)
library(doParallel)
library(magrittr)
library(dplyr)
library(foreach)
library(furrr)
library(tweedie)

# Given functions from homework 4 BAN420 to edit ----
simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 



# Create a log/ table here we can save all test results
# Defining the log name
TicTocLog <- 
  # Empty function
  function() {
    # Use functions from tictoc to time the function
    tic.log() %>%
      # Simplifying the list into a vector
      unlist %>%
      # Converting the vector into a tibble
      tibble(logvals = .) %>%
      # Separating the table into to columns
      separate(logvals,
               sep = ":",
               into = c("Function type", "log")) %>%
      # Modifying log column by removing unnecessary
      mutate(log = str_trim(log)) %>%
      # Separating the column log into seconds
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }


# When fresh start, use clear log
tic.clearlog()

# Problem 2.1 ----
# Time the original solution script:
test_1 <- source("scripts/test_1.r")



# Problem 2.2 ----
# Time the script after rewrite line 29-35 (final loop)
test_2 <- source("scripts/test_2.r")


# Problem 2.3 ----
# Time the script after rewriting MTweedieTests
test_3 <- source("scripts/test_3.r")

# Print tictoc log into a nice table ----
TicTocLog() |>
  knitr::kable()

# This test shows that method 3 used on test 3 is the fastest.
# This might be because that it forces the machine to use several cores to 
# calc. the lines in parallel. 