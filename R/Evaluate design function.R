source("./R/Unit tests.R")

prob_y <- function(y, n, a, b) {
  # Calculate the probability of observing y responses in n trials
  # under a Beta-binomial model with Beta(a, b) prior.
  
  choose(n, y) * beta(y + a, n - y + b) / beta(a, b)
}

test_prob_y()

expected_sample_size <- function(lambda, gamma, n1, n2, a, b) {
  
  # Calculate the expected sample size of a design defined by its 
  # decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2), along with its standard error.
  
  # Threshold to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  
  # Vector of possible stage 1 outcomes.
  y_1s <- 0:n1
  
  # Vector of corresponding progression decisions.
  stops <- pbeta(0.5, y_1s + a, n1 - y_1s + b) > C1
  
  # For each outcome, calculate its probability.
  y_1_probs <- prob_y(y_1s, n1, a, b)
  
  #print(c(lambda, gamma, n1, n2))
  return(sum(n1 * stops * y_1_probs + n2 * (!stops) * y_1_probs))
}
