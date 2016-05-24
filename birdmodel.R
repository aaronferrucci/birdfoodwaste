# Bird waste, in grams, per day
# The mean bird weight is 400 grams; the baseline mean 
# value for waste is one tenth of the bird weight.
# Two normal distributions are created: a low-mean distribution for use
# when the previous day had high waste, and a high-mean distribution for
# use when the previous day had low waste.

birdweight <- function() {
  weight <- rnorm(1, 400, 40)
  return(weight) 
}

next_weight <- function(cur_weight, baseline_mean, low_mean, high_mean) {
  nw <- ifelse(
    cur_weight > baseline_mean,
    rnorm(1, low_mean, baseline_mean * .3),
    rnorm(1, high_mean, baseline_mean * .3)
  )
  # Negative waste makes no sense - just limit to 0
  if (nw < 0) nw <- 0;
  return(nw)
}

birdwaste <- function(num_days) {
  weight <- birdweight()
  baseline_mean <- weight * 0.1
  waste <- rep(0, num_days)

  # "bimodality": if 1, you'll get a bell curve.
  # As the value goes below one, two peaks emerge, centered around the baseline mean.
  bimodality <- 0.6
  low_mean <- baseline_mean * bimodality
  high_mean <- baseline_mean / bimodality
  waste[1] <- rnorm(1, baseline_mean, baseline_mean * .1)
  for (i in 2:num_days) {
    waste[i] <- next_weight(waste[i - 1], baseline_mean, low_mean, high_mean)
  }
  return(data.frame(day=1:num_days, waste=waste))
}

multi_means <- function(num_days, num_birds) {
  means <- numeric(0)
  for (i in 1:num_birds) {
    w <- birdwaste(num_days)
    means <- append(means, mean(w$waste))

  }
  return(means)
}
