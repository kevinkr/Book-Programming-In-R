###Chapter 4
###############
#Summary Statistics
###############
#
####
#Measuring Central Tendency
####
#
# Common measures: mean, median, mode
load("./Data/percapita.rda")
attach(percapita)

#mean gathered from summary()
summary(percapita[2:9])

#or from colMeans
colMeans(percapita[2:9])

#median is effective because outliers don't affect it
mean(Yr_2010)
median(Yr_2010)
#trimmed mean
mean(Yr_2010, trim = .5)

#median is the midpoint of a data set, corresponding to the 50% trimmed mean
#median isn't vectorized, have to use apply
apply(percapita[2:9], 2, median)

#quantile
apply(percapita[2:9], 2, quantile)

#find 90th percentile
quantile(Yr_2010, 0.90)

#Mode 
#R doesn't have built-in function for identifying modal values
sort(table(Yr_2010))

require(prettyR)
Mode(Yr_2010)

#Standard Scores
# z = deviation between a raw score and the mean divided by the standard deviation
#By definition, z scores have a mean of zero and standard deviation of 1
zYr_2010 <- scale(Yr_2010)
zYr_2010
mean(zYr_2010)
apply(zYr_2010, 2, sd)

######
#Measuring variability
#####
#
#Variance and Standard Deviation
#
var(Yr_2010)
sd(Yr_2010)

var(percapita[2:9])

#Range
#most commonly defined as the difference between the highest and low values in a data vector
range(weights)
range.diff <- function(x) max(x) - min(x)
range.diff(weights)

#Median and Mean Absolute Deviation
mad(weights)
mad(weights, center=mean(weights))
sd(weights)
hist(weights)

#Interquartile Range
#difference between the third quartile and the first quartile
IQR(weights)

#Coefficient of Variation
#measure size of standard deviation relative to the size of the mean
#CV = standard deviation/mean
CV <- function(x) sd(x)/mean(x)
CV
CV(weights)

#Covariance and Correlation
#dependent variable is y
#independent variable is x
load("./Data/sales.rda")
attach(sales)
cov(Advertising, Sales)
#3.5
#covariance is positive, which means there is a general trend for x and y to increase together
#but can be affected by units of measure.
#By dividing the covariance by the products of the two standard deviations,
#we achieve the correlation coefficient
cor(Advertising, Sales)
#0.9108064
(cov(Advertising, Sales)/(sd(Advertising) * sd(Sales)))
#0.9108064

cor(percapita[2:9])

###########
#Measuring Symmetry
#########
#
#Would like to know if data distribution is roughly symmetrical in shape
#Skewness coefficient used for examining symmetry
#Kurtosis efficient used as a index for peakedness
#
# Normal distribution has zero skew and zero kurtosis
#
load("./Data/Mileage.rda")
attach(Mileage)
head(Mileage)
require(psych)
skew(City)
skew(Hwy)
skew(Combined)
kurtosi(City)
kurtosi(Hwy)
kurtosi(Combined)
hist(City)
