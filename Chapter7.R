################
# Chapter 7 Computing Normal Probabilities
################
#
#Standard normal distribution, also called "unit" normal distribution
xaxis <- seq(0, 40, .5)
xaxis
y1 <- dnorm(xaxis, 20, 6)
y2 <- dnorm(xaxis, 20, 3)
plot(xaxis, y2, type = "l", main="Comparing two normal distributions")
points(xaxis, y1, type = "l", col = "red")

#A z-score is a measure of how many standard deviations below 
#or above the population mean a raw score is. A z-score is 
#also known as a standard score and it can be placed on a normal distribution 

#Standard normal distribution has a mean of 0 and a standard deviation of 1

#Critical value of z for a 95% confidence interval is z +/- 1.96
# Area betwenn -1.96 and +1.96 is equal to 95% of the standard normal distribution

#pnorm finds a left-tail probabilility
pnorm(1.96)

#area between two zscores
pnorm(1.96) - pnorm(-1.96)
#0.95

#1 - pnorm finds right-tailed proability
1 - pnorm(1.96)

#Using rnorm ot generate random samples
samples <- rnorm(50, 100, 15)
samples
hist(samples) #samples don't appear very normal
#increasing sample size to 1000 makes it appear more "normal"
samples <- rnorm(1000, 100, 15)
hist(samples)

