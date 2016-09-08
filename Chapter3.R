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

#use default alpha level
confint(Age)

#change alpha level
confint(Age, 0.10)

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

#generates NaNs
ifelse(x>=0, sqrt(x), NA)

#avoids geenrationg NaNs
sqrt(ifelse(x>0, x, NA))

#More powerful functions
primes <- read.csv("./Data/primes1000.csv", header=T)
head(primes)
primes$n <- NULL
summary(primes)
str(primes)

#primality function to test if number is prime
primality <- function (x) {
  stopifnot(x>=2)
  limit <- trunc(sqrt(x) + 1) #find sq rt of x adn round up by 1
  testvec <- 2:limit
  results <- x %% testvec #dvide vector by list of primes
  check <- any(results == 0)
  outcome <- "Yes."
  if (check == TRUE) outcome <- "No."
  if (x == 2)  outcome <- "Yes."
  cat("Is ", x, " prime? ", outcome, "\n" )
}

#########
#Makng Functions more useful
##########
#
#quartile function
qt
?qt

t.test
?t.test

#load weights data
load("./Data/weights.rda")
weights
confint(weights, alpha = .10)

t.test(weights, mu = 191, conf.level = .90)

confint

#new conf int with flow control to determine if there is a one-tailed or two-tailed test
confint.1 <- function(x, alpha = .05, two.tailed = TRUE) {
  cat("Mean: ", mean(x), "\n")
  df <- length(x) - 1
  stderr <- sd(x)/sqrt(length(x))
  cat("Standard error of the mean: ", stderr, "\n")
  conflevel <- 1 - alpha/2
  if (two.tailed == FALSE) {
    conflevel <- 1 - alpha
  }
  tcrit <- qt(conflevel, df)
  margin <- stderr * tcrit
  LL <- mean(x) - margin
  UL <- mean(x) + margin
  if (two.tailed == FALSE) {
    cat("You are doing a one-tailed test.","\n")
    cat("If your test is left-tailed,","\n")
    cat("the lower bound is negative infinity.","\n")
    cat("If your test is right-tailed,","\n")
    cat("the upper bound is positive infinity.","\n")
    cat("Either add the margin",margin,"to or subract it from","\n")
    cat("the sample mean as appropriate.","\n")
    cat("For a left-tailed test, the upper bound is",LL,"\n")
    cat("For a right-tailed test, the lower bound is",UL,"\n")
  }
  cat("upper bound:",LL,"\n")
  cat("lower bound:",UL,"\n")
}

confint.1(weights)

t.test(weights)

#not specifying two tailed conf int
confint.1(weights, two.tailed = FALSE)

#compare conf int to alpha = .10
t.test(weights, conf.level = .90)
