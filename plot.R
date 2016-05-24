library(ggplot2)
bw_hist.old <- function(w) {
  mean <- mean(w$waste)
  p <- ggplot(data=w, aes(waste)) +
    geom_histogram(binwidth=1) +
    xlab("Waste (g)") +
    geom_vline(xintercept = mean, color = "red", size = 1)
  
  return(p)
}

bw_hist <- function(w) {
  mean <- mean(w$waste)
  p <- ggplot() +
    geom_histogram(data=w, aes(waste), binwidth=1) +
    xlab("Waste (g)") +
    geom_vline(xintercept = mean, color = "red", size = 1)
  
  return(p)
}

bw_line <- function(w) {
  p <- ggplot(data=w, aes(day, waste)) +
    geom_line() +
    xlab("day") +
    ylab("waste (g)")
  
  return(p)
}

multi_hist <- function(num_days, num_plots, alphahist = 0.05, alphamean = 0.05, meanpoints = FALSE) {
  means <- numeric(0)
  p <- ggplot() + xlab("Waste (g)")
  for (i in 1:num_plots) {
    w <- birdwaste(num_days)
    means <- append(means, mean(w$waste))
    p <- p + geom_histogram(data=w, aes(waste), binwidth=1, alpha = alphahist)
  }
  
  # plot the mean lines last.
  for (m in means) {
    p <- p + 
      geom_vline(xintercept = m, color = "red", size = 1, alpha = alphamean)
  }
  
  if (meanpoints) {
    meandf <- data.frame(x=means, y=4)
    p <- p + 
      geom_point(data=meandf, aes(x, y), alpha = alphamean * 2, color = "red", position = "jitter")
  }
  
  return(list(plot = p, means = means))    
}

clt_hist <- function(num_days, num_birds) {
  means <- data.frame(waste = multi_means(num_days, num_birds))
  p <- ggplot() + xlab("Waste (g)")
  p <- p + geom_histogram(data=means, aes(waste), binwidth=0.5)
}
