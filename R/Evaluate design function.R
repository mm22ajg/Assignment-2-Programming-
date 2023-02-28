prob_y <- function(y, n){
  # Calculates the probability of observing y responses in n trials
  # under a Beta-binomial model with Beta(a = 0.5, b = 0.5) prior.
  
  choose(n, y) * beta(y + 0.5, n - y + 0.5) / beta(0.5, 0.5)
}

evaluate_design <- function(lambda, gamma, n1, n2, theta) {
  
  # Estimate the type I error rate, type II error rate, and the expected 
  # sample size of a design defined by its decision rule parameters 
  # (lambda, gamma) and sample size parameters (n1, n2).
    
  # Thresholds at each stage to determine progression, based on the 
  # decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  C2 <- 1 - lambda * (n2 / n2)^gamma
    
  # Vectors of possible stage 1 and stage 2 outcomes.
  y1 <- 0:n1
  y2 <- 0:n2
  
  # Vectors of corresponding progression decisions.
  stops1 <- pbeta(0.5, y1 + 0.5, n1 - y1 + 0.5) < C1
  stops2 <- pbeta(0.5, y2 + 0.5, n2 - y2 + 0.5) < C2
  
  # For each outcome, calculate its probability.
  y1_probs <- prob_y(y1, n1)
  y2_probs <- prob_y(y2, n2)
  
  # Estimate the expected sample size.
  ESS <- sum(n1 * stops1 * y1_probs + n2 * !stops1 * y1_probs)
  
  # Estimate the type I and type II error rates.
  type_I_error <- sum((!stops1 * y1_probs) & (!stops2 * y2_probs))
  type_II_error <- sum((stops1 * y1_probs) | (stops2 * y2_probs))
  
  # Return the estimated expected sample size, type I error rate, and 
  # type II error rate.
  return(c(ESS, type_I_error, type_II_error))
}
