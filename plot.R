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

multi_hist <- function(n) {
  
}
