############
# Chapter 13 - Multiple Regression
############
#
# Multiple regression is the linear combination of two or more predictor variables to
# optimize the relationship betweeen the observed and predicted variables.
#
# Many analysisof variance designs are based ont he assumption of independence between
# the factors. However, in data we observe in the real world, there is often a lack of 
# independence between predictors.
#
# In multiple regression the predicted value of y is a linear combination of the
# values of the predictors. We calculate and interpret residuals in mutiple regression the
# same way we do in bivariate regression.

# The multiple regression equation is an extension of the simple (bivariate) regression 
# equation to accomodate two or more predictors.
#
# ŷ = b0 + b1x1 + b2x2 + . . . + bkxk

# The regression coefficients are derived in  such a way as to minimize the sum of the 
# squares of the deviations of the observed and predicted values of y. This is called 
# "least squares" or "ordinary least squares" (OLD) regression. We caluclate the coefficient of 
# multiple correlation, which is symbolized as R, which we call "multiple R", by correlating
# the predicted and observed value of y.

# The direction fo the relationship (positive or negative)  between the individual predictors
# and each other and between the predictors and the criterion is taken into account in the 
# calculation of the regression coefficients. For that reason, R will always range between
# 0 and 1 and cannot achieve negative values. The square of R, R², is the coefficient of 
# determination and the equivalent of eta squared in the ANOVA terminology. 

# Multiple Regression Example: Predicting Job Satisfaction

#http://www3.norc.org/gss+website/

load("./Data/gssdata.rda")
attach(gssdata)
head(gssdata)

# Use job satisfaction as the dependent variable and the other variables as predictors
mulreg <- lm(satjob1 ~ rincome + age + yearsjob + jobsecok)
summary(mulreg)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-1.67405 -0.50569 -0.04992  0.40383  2.56341 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.548183   0.111874  13.839  < 2e-16 ***
#  rincome     -0.001547   0.007717  -0.200  0.84113    
#age         -0.008073   0.001746  -4.624 4.28e-06 ***
#  yearsjob    -0.008431   0.002868  -2.940  0.00336 ** 
#  jobsecok     0.331870   0.024512  13.539  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.6603 on 959 degrees of freedom
#Multiple R-squared:  0.2059,	Adjusted R-squared:  0.2026 
#F-statistic: 62.17 on 4 and 959 DF,  p-value: < 2.2e-16

# We see that the respondent's income is not a significant predictor, so we drop it from the analysis
mulreg <- lm(satjob1 ~ age + yearsjob + jobsecok)
summary(mulreg)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-1.66128 -0.50756 -0.04729  0.40303  2.56970 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.533169   0.083075  18.455  < 2e-16 ***
#  age         -0.008087   0.001744  -4.638    4e-06 ***
#  yearsjob    -0.008540   0.002814  -3.034  0.00248 ** 
#  jobsecok     0.332151   0.024459  13.580  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.6599 on 960 degrees of freedom
#Multiple R-squared:  0.2059,	Adjusted R-squared:  0.2034 
#F-statistic: 82.96 on 3 and 960 DF,  p-value: < 2.2e-16

# Test the significance of the overall regression with ANOVA, by partitioning the variance
# inro regression and residual sums of squares. We also test the significance of the intercept 
# (when this is of interest) and of the regression coefficients with t tests. 

# In the particular case, the overall regression is statistically significant - we can 
# account for roughly 20% of the variation in job satisfaction by knowing an individual's age,
# number of years on the job, and his or her sense of job security. (see multiple r-squared)

#plot the residuals against the fitted values
plot(mulreg)

# Also plot the predicted and observed values, along with a confidence interval and a
# prediction interval

# use predict() function to get the fitted values and confidence intervals
confint <- predict(mulreg, interval = "confidence")
predint <- predict(mulreg, interval = "prediction")
intervals <- cbind(confint, predint)
# change column headings to fit1 lwrconf uprconf fit2 lwrpred uprpred 
intervals <- edit(intervals)
head(intervals)
plot(fit1, satjob1)
lines(fit1)
