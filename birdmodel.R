# Bird waste, in grams, per day
# The mean bird weight is 400 grams; the baseline random
# variable for waste averages one tenth of the bird weight.
# A filter is used to model the effect of the bird crop; little
# waste on day n makes more waste likely on day n+1.
birdwaste <- function(n) {
  weight <- rnorm(1, 400, 40)
  rand <- rnorm(n, weight * 0.1, .1)
  filt <- filter(rand, filter=c(1, -1, 1, -1, 1), circular=T)
  return(filt)
}

plot(birdwaste(100))