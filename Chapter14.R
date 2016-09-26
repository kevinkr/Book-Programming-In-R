############
# Chapter 14 - Logistic Regression
############
#
# When we have a binary outcome measure, we can treat this as a binomial process,
# with p being the proportion of 1's, and q = 1 - p being the proportion of 0's.
# 
# We can convert a porbability to odds, and we use odds in logistic regression.
# If p is the probability of success, the odds in favor of success, o, are:
# odds = p/q = p/(1-p)

# Odds can also be converted to probabilities quite easily. If the odds in favor of
# an event are 5:1, then the probability of an event is 5/6. Note that odds, unlike
# probabilities, can exceed 1. For example, if the probability of rain is .25, the odds
# in favor of rain are .25/.75 = .33, but the odds against rain are .75/.25 = 3.00.
#
# In logistic regression we work with a term call the logit, which is the natural
# logarithm of the odds. We develop a linear combination of predictors and an intercept
# term to predict the logit.
#     ln(odds) = bo + b1x1 + b2x2 + . . . + bkxk
#
# Logistic regression allows us to model the probability of a "success" (1) as a 
# function of the logistic curve, which is never less than 0 and never greater than 1.
# Because we aren't accustomed to thinking in terms of logarithms, we typically 
# convert the logit to odds.
# We convert the logit to odds as follows:
# odds = e^(b0 + b1x1 + b2x2 + . . . + bkxk)


####
#Logistic regression with one dichotomous predictor
####
#
load("./Data/bingedata.rda")
attach(bingedata)
head(bingedata)
tail(bingedata)
bingedata[,3:7] <- NULL #remove extra columns
table(bingedata)
#       gender
#binges  0  1
#   0   20 13
#   1    5 12

#chi-square test of independence
chisq.test(table(bingedata))
#Pearson's Chi-squared test with Yates' continuity correction

#data:  table(bingedata)
#X-squared = 3.2086, df = 1, p-value = 0.07325
# Chi-square test is "approaching" significance, and might be significant with a 
# larger sample.

# Let us model this problem as odds'
# Calculate proportion of men and women who are binge drinkers using our sample data
# phat1 - 12/25 = .48
# phat 0 - 5/25 = .20

# odds in favor of a man being a binge drinker = phat1 / qhat1 = .48/.52 = 0.9231

# odds in favor of a woman being a binge drinker = phat0 / qhat0 = .20/.80 = .25

# do logistic regression in R
results <- glm(binges ~ gender, family = "binomial")
summary(results)

#Call:
#  glm(formula = binges ~ gender, family = "binomial")

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1436  -1.0247  -0.6681   1.2116   1.7941  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -1.3863     0.5000  -2.773  0.00556 **
#  gender        1.3063     0.6405   2.039  0.04141 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 64.104  on 49  degrees of freedom
#Residual deviance: 59.637  on 48  degrees of freedom
#AIC: 63.637

#Number of Fisher Scoring iterations: 4

####
# Logisic regression wiht one continuous predictor
####

# Predict retention of college students from their freshman to their sophomore years
# Students who started as freshman and returned as sophomores will get a 1
# and those who did not register as a sophomore will get a 0
# We will use high school GPA (HSGPA) as our predictor variable and college 
# retention as our criterion.

load("./Data/retention.rda")
attach(retention)
head(retention)

# two test t-sample
t.test(HSGPA~Retained)

# Logistic regression
results <- glm(Retained ~ HSGPA, family = "binomial")
summary(results)

# find the predicted (fitted) values for retention and plot them against the HSGPA
results <- glm(Retained ~ HSGPA, family = "binomial")
predicted <- results$fitted.values
plot(HSGPA, predicted)
#  graph is monotonic increasing, so that the predicted probability of retention
# increases as a function of the student's GPA


#####
# Logistic regression with multiple predictors
#####
results <- glm(Retained ~ Pref + Athlete + FRHours + HSGPA + SATtot + Gender, family = "binomial")
summary(results)

# # Call:
# #   glm(formula = Retained ~ Pref + Athlete + FRHours + HSGPA + SATtot + 
# #         Gender, family = "binomial")
# # 
# # Deviance Residuals: 
# #   Min       1Q   Median       3Q      Max  
# # -2.6351  -0.9610   0.3044   0.8861   2.1750  
# # 
# # Coefficients:
# #   Estimate Std. Error z value Pr(>|z|)    
# # (Intercept) -5.9111678  0.9113405  -6.486 8.80e-11 ***
# #   Pref         1.4817399  0.2057348   7.202 5.93e-13 ***
# #   Athlete      2.9579003  0.3624216   8.161 3.31e-16 ***
# #   FRHours      0.0666602  0.0506562   1.316   0.1882    
# # HSGPA        0.7797138  0.1592518   4.896 9.78e-07 ***
# #   SATtot       0.0011058  0.0006552   1.688   0.0915 .  
# # Gender      -0.0199795  0.1671598  -0.120   0.9049    
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 
# # (Dispersion parameter for binomial family taken to be 1)
# # 
# # Null deviance: 1272.3  on 957  degrees of freedom
# # Residual deviance: 1011.2  on 951  degrees of freedom
# # AIC: 1025.2
# # 
# # Number of Fisher Scoring iterations: 5
# 
# # We see that gender, freshman hours, and total SAT scores are not significant
# # predictors, so we omit them and run the logistic regression again:

results <- glm(Retained ~ Pref + Athlete + HSGPA , family = "binomial")
summary(results)

# Call:
#   glm(formula = Retained ~ Pref + Athlete + HSGPA, family = "binomial")
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6377  -0.9712   0.2972   0.8918   2.2626  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -4.3629     0.5280  -8.263  < 2e-16 ***
#   Pref          1.5078     0.2049   7.358 1.87e-13 ***
#   Athlete       2.9732     0.3608   8.240  < 2e-16 ***
#   HSGPA         0.9326     0.1412   6.603 4.04e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1272.3  on 957  degrees of freedom
# Residual deviance: 1016.4  on 954  degrees of freedom
# AIC: 1024.4
# 
# Number of Fisher Scoring iterations: 5

# # Overall model fit is measured by the Akaike information criterion (AIC)
# which a is a relative measure of the information lost when the model is used to
# describe the outcome variable. The AIC can be used in model selection.
# All other things equal, the model with the lowest AIC should be chosen.

# Perform overall test of model fit with a chi-square test of the difference
# between the null variance (null model) and the fitted model.'

with(results, null.deviance - deviance)
# 255.9248
with(results, df.null - df.residual)
# 3
#the area under the chi-square curve with 3 degrees freedom is 
with(results, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# 3.422335e-55
# Since P-value < .05, value is highly significant indeicating our model fit is good.

# One of the objects retruned by the glm function is fitted values
head(results$fitted.values)
x1 <- results$fitted.values
cbind(retention,x1)

# You can think of fitted values as the risk assessment for each student. We can turn
# those into 0s and 1s by rounding .5 and up to 1, and less than .5 to 0. Then we can 
# determine how good our model fit is by comparing the original retention data
# with the predicted retention. 
# Of the freshman in the sample of 958, 592 or 64% returned as sophomores.

# We can determine the association between our predicted retention (risk) and the actual
# retention (and whether it's significant) by performing another chi-sqaure test.
head(retention)
results$fitted.values
# set risk values
results$risk[results$fitted.values >= 0.5] <- 1
results$risk
results$risk[results$fitted.values < 0.5] <- 0
head(results)
table(results$Retained, results$risk)
