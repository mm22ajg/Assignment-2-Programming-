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

prob_y1_and_y2 <- function(y_1, y_2, n1, n2, theta) {
  
  # Compute the probability of observing y_1 and y_2 given theta.
  dbinom(y_1, n1, theta)*dbinom(y_2, n2 - n1, theta)
}

error_rates <- function(lambda, gamma, n1, n2, a, b) {
  
  # Calculate the type I and type II error rates of a design defined by 
  # its decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2).
  
  # Thresholds to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  C2 <- 1 - lambda * (n2 / n2)^gamma
  
  # Matrix of possible successes in stage 1 and 2.
  ys <- expand.grid(y_1 = 0:n1, y_2 = 0:(n2 - n1))
  
  # Vector of corresponding progression decisions at stage 1.
  go_1 <- pbeta(0.5, ys$y_1 + a, n1 - ys$y_1 + b) < C1
  
  # Vector of corresponding progression decisions at stage 2.
  go_2 <- pbeta(0.5, ys$y_1 + ys$y_2 + a, n2 - ys$y_1 - ys$y_2 + b) < C2
  
  # Vector of overall progression decisions using the "and" operator "&"
  go_tI <- go_1 & go_2
  go_tII <- !go_1 | !go_2
  
  # For each outcome, calculate its probability.
  probs_tI <- prob_y1_and_y2(ys$y_1, ys$y_2, n1, n2, 0.5)
  probs_tII <- prob_y1_and_y2(ys$y_1, ys$y_2, n1, n2, 0.7)
  
  type_I <- sum(go_tI*probs_tI)
  type_II <- sum(go_tII*probs_tII)
  
  return(c(type_I,type_II))
}

to_eval <- expand.grid(lambda = seq(0,1,0.05), gamma = seq(0.05,2,0.05), n1 = seq(4, 20, 4), n2 = seq(5, 80, 5))
to_eval <- to_eval[to_eval$n1<=to_eval$n2,]

res <- c(50,0,0)

ptm <- proc.time()
for(i in 1:nrow(to_eval)){
  exp_s_s <- expected_sample_size(to_eval[i,1],to_eval[i,2],to_eval[i,3],to_eval[i,4], a = 0.5, b = 0.5)
  err_rts <- error_rates(to_eval[i,1],to_eval[i,2],to_eval[i,3],to_eval[i,4], a = 0.5, b = 0.5)
  if(exp_s_s < res[1] & err_rts[1] <= 0.05 & err_rts[2] <= 0.2){
    res <- c(exp_s_s,err_rts[1],err_rts[2])
    val <- to_eval[i,]
  }
}
proc.time() - ptm

print(res)
print(val)