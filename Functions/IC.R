# Function to facilitate the calculation confidence interval (default conf.level = 0.95)
IC <- function(X, conf.level = 0.95){
  m <- mean(X)
  s <- sd(X)
  tails <- (1 - conf.level)/2
  ic <- as.numeric(quantile(X, probs = c(tails, 1 - tails)))
  res <- c(mean = m, sd = s, lower = ic[1], upper = ic[2])
  return(res)
}
