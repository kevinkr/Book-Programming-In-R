#########
#Chapter 8 - Creating Confidence Intervals
#########
#
# A confindence interval is a range of possible values to estimate the true value
# of a population parameter. We associate each confidence level with some level of 
# confidence such as 90%, 95%, or 99%. The confidence level is the complement of the 
# alpha level, so these three confidence levels correspond to alpha levels 0f .10, .05, and .01.
load("./Data/weights.rda")
weights

#CI for the mean using the Normal Distribution
#estimate sample size for required CI
sampsize.est <- function(E, sigma, alpha = 0.05) {
  #E is the desired margin of error
  n <- ((qnorm(alpha/2)*sigma)/E)^2
  estsize <- ceiling(n)
  cat("for a desired margin of error of:", E, " the required sample size is: ", estsize, "\n")
}

#>sampsize.est(5, sd(weights))

#CI for the mean using the t Distribution
# When we don't know the population standard deviation, we the sample standard deviation
# as a reasonable estimate. We use the t distribution instead of the normal distribution.
confint.mean <- function(x, alpha = .05, two.tailed = TRUE) {
  cat("\t", "Confidence Interval for the Mean", "\n")
  cat("Mean: ", mean(x), "\n")
  df <- length(x) -1
  stderr <- sd(x)/sqrt(length(x))
  
}