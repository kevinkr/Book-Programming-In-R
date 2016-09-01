#Chapter 3
load("./Data/dataset.rda")
attach(dataset)
mad(Age)

##########
#FUNCTIONS
###########
#
#Function for calculation mean absolute deviation
MeanAbsDev <- function(x)
{
  AbsDev <- abs(x - mean(x))
  MAD <- mean(AbsDev)
  cat("The mean absolute deviation is: ",MAD,"\n")
}

MeanAbsDev(Age)

#Confidence Interval function
#x is the data vector, default alpha = 0.05
confint <- function(x, alpha = .05) {
  conflevel = (1 - alpha)*100 #confidence level
  stderr <- sd(x)/sqrt(length(x)) #std error of the mean
  tcrit <- qt(1 - alpha/2, length(x)-1) #critical value of t
  margin <- stderr * tcrit #margin of error
  lower <- mean(x) - margin
  upper <- mean(x) + margin
  cat(conflevel,"Percent Confidence Level","\n")
  cat("Mean:", mean(x), "Std. Error:", stderr, "\n")
  cat("Lower Limit:", lower,"\n")
  cat("Upper Limit:", upper, "\n")
}


##########
#VECTORIZED OPERATIONS
###############
#
#Function to create prime numbers
#Formula is x^2 - x + 41
#looped operation
TryIt <- function(x)
  flush.console()
for (n in x) {
  result <- n^2 - n + 41
  cat("For x =",n, "Result is ",result,"\n")
}

#loop be replaced with 
x <- 0:50
y <- x^2 - x + 41
y

#ifelse
x <- -5:5
sqrt(ifelse(x>0, x, NA))

ifelse(x>=0, sqrt(x), NA)

#More powerful functions
primes <- read.csv("./Data/primes1000.csv", header=T)
head(primes)
primes$n <- NULL
