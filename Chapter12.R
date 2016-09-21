#########
# Chapter 12 - Correlation and Regression
#########
#
# We are dealing (at least initially) with the determination of the degree (if any) of
# linear relationship between two variables measured at the interval or ratio level.
#
# x is the predictor (or independent) variable
# y is the criterion (or dependent) variable
#
# When two variables have a positive covariance, increases in x are associated  with increases in y
# Negative covariance - increases in x are associated with decreases in y
#
# Covariance is affected by the units of measurement of x and y
# Dividing the covariance by the product of the standard deviations of x and y produces
# a "scaleless" quantity (known as the Pearson product-moment correlation coefficient)
# This coefficient ranges from -1 (perfect negative) through 0 (no relationship) 
# to +1 (perfect positive).
load("./Data/weights.rda")

# Example find heights in inches of the 40 adult men who exercise regularly
# use rnorm() function to generate a vector of heights with a mean of 70 and sd of 6 inches
# Use heights to predict weights
# height is x variable
# weight is y variable
y <- sort(weights)
y
x <- sort(rnorm(40, 70, 6))
x
matrix <- cbind(x, y)
head(matrix)

#covariance (expect to be high based on how we created the data)
cov(x, y)

#correlation
cor(x, y)

#examine scatterplot
plot(x, y, xlab = "height", ylab = "weight", main = "weights and heights")
#add regression line
abline(lm(y ~ x))

# When there are three or more variables, we get a variance-covariance matrix
# Assume we add a z vector, which represents the resting pulse rate of the same 40 men
# used the rnorm function to generate the hypothetical data
z <- rnorm(40, 80, 10)
z <- sort(z)
z

matrix <- cbind(x, y, z)
cov(matrix)
cor(matrix)

######
# Regression
######
load("./Data/hours.rda")
attach(hours)

#correlation between grades and GPA
cor(Hours, GPA)
# 0.82

#linear model
lm(GPA ~ Hours)

#Coefficients:
#(Intercept)        Hours  
#1.3728            0.1489 

# we can test the significanceof the regression coefficient and or the correlation coefficient
results <- lm(GPA ~ Hours)
summary(results)

#Call:
#lm(formula = GPA ~ Hours)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.52668 -0.17079  0.03171  0.12698  0.46796 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.37276    0.33329   4.119 0.000645 ***
#  Hours        0.14893    0.02473   6.022 1.08e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.24 on 18 degrees of freedom
#Multiple R-squared:  0.6683,	Adjusted R-squared:  0.6498 
#F-statistic: 36.26 on 1 and 18 DF,  p-value: 1.078e-05

#Interpretation: A student who didn't study would have estimated GPA of 1.37
# and that for every 1-hour increase in study time, the estimated
# GPA would increase by 0.14

# calculate predicted scores
predicted <- predict(results)
predicted
cbind(predicted)

# compare observed GPAs, predicted GPAs, and the residuals (diff between y(GPA) and predicted values)
resid <- residuals(results)
cbind (GPA, predicted, resid)

# Perform an ANOVA on the results of the regression analysis
anova(results)

##
# Example: Predicting the price of gasoline
##
# Examine linear relationship between annual city average price of gasoline and the year
gas_prices <- read.csv("Data/gas_prices.csv", header = TRUE)
gas_prices
attach(gas_prices)
plot(index,Average)
abline(lm(Average ~ index))
#linear fit is not the best model

#Examine the linear relationship
results <- lm(Average ~ index)
summary(results)

# TRy fitting a quadratic model to the same data
# calculate a new vector, the square of the index
# create vector of squared values and use cbind to add to the values to gas_prices data frame
indexsq <- index ^ 2
indexsq
gas_prices <- cbind(gas_prices, indexsq)
# Multiple R-squared:  0.7181 (percentage of variation)
# we can account for roughly 71% of the vaiablility in gas prices by knowing the year

# Examine new model
results <- lm(Average ~ index+indexsq)
summary(results)
# Multiple R-squared:  0.8532
# improved to 85%

class(results)

#plot results
plot(results)
#normal Q-Q shows the quadratic model is an excellent fit

# predict gasoline prices from quadratic model
predquad <- predict(results)
gas_prices <- cbind(gas_prices, predquad)

#plot
plot(index, Average, main = "Quadratic model")
lines(predquad)

#Determine confidence and prediction intervals for a regression model
#use t distribution for our confidence and prediction intervals
# We assume the error terms (residuals) are normally distributed and have equal
# variances at all levles of y.

# The confidence interval is an interval describing the ranges of the means of y for
# each value or x.

# The prediction interval, on the other hand, describes the ranges of the predicted 
# individual values of y for each value of x. For this reason, the prediction interval 
# will alway be wider than the confidence interval

# Determine the confidence and prediction intervals for the predicted y values for our linear
# and compares those to the confidence and prediction intervals for the predicted y values using
# the quadratic model.
results <- lm (Average ~ index)
predlin <- predict(results)
conf <- predict(results, interval = "confidence")
pred <- predict(results, interval = "prediction")

# plot the averages against the index number along with the fit line, the CI, and the 
# prediction interval.

# See newer version for updated examples
