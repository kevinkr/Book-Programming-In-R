#####
#Chapter 6: Discrete Probability Distributions
#########
#
#Discrete probability must satisfy the following conditions
#-The sum of probabilities is equal to 1
#-Probability of any particular outcome is between 0 and 1
#-The list of simple outcomes is exhaustive (there are no other possible outcomes)
#- and mutually exclusive (no outcome can be in more than one category)
saturday_sales <- data.frame(numsold = integer(), prob = numeric())
saturday_sales <- edit(saturday_sales)
attach(saturday_sales)
#mean of discrete probability distribution
mu <- sum(numsold*prob)
mu
#variance of discrete probability distribution
variance <- sum((numsold-mu)^2 * prob)
variance

########
#Bernoulli processes
########
#A Bernoulii process is one in which there are a series of independent trials,
#with the result of each being one of two outcomes
##Think flip of a coin
# p = probability of success
# q = 1 - p is probability of failure
