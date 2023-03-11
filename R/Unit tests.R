test_prob_y <- function(a, b) {
  # Check that the probabilities sum to 1.
  n <- 30
  s <- sum(prob_y(0:n, n, a, b))
  return(all.equal(s, 1))
}