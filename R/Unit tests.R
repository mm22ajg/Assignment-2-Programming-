test_prob_y1 <- function() {
  # Check that the probabilities sum to 1.
  n1 <- 30
  s <- sum(prob_y1(0:n1, n1))
  return(all.equal(s, 1))
}