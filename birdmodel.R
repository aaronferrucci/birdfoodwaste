# Bird waste, in grams, per day
# The mean bird weight is 400 grams; the baseline random
# variable for waste averages one tenth of the bird weight.
# A filter is used to model the effect of the bird crop; little
# waste on day n makes more waste likely on day n+1.
birdwaste <- function(n) {
  weight <- rnorm(1, 400, 40)
  waste <- rep(0, n)
  mean <- weight * 0.1
  # "bimodality": if 1, you'll get a bell curve.
  # lower values spread the two peaks apart.
  bimodality <- .8
  mean0 <- mean * bimodality
  mean1 <- mean / bimodality
  waste[1] <- rnorm(1, mean, mean * .1)
  for (i in 2:n) {
    waste[i] <- ifelse(
      waste[i - 1] > mean,
      rnorm(1, mean0, mean * .1),
      rnorm(1, mean1, mean * .1)
    )
  }
  # Negative waste makes no sense - just reflect around y=0
  waste <- abs(waste)
  return(data.frame(day=1:n, waste=waste))
}
