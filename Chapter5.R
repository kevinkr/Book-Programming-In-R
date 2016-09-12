########################
#Chapter 5
#Creating Tables and Graphs
###########################
#
#Frequency Distributions and Tables
#
#Old faithful duration of eruptions and waiting time
load("./Data/faithful.rda")
attach(faithful)
head(faithful)
waiting<-faithful[,2]
#max and min of waiting times
range(waiting)
table(waiting)

#establish interval sequence
breaks <- seq(40, 100, by = 5)
breaks
#then cut waiting time table
wait_time <- cut(waiting, breaks, right = FALSE)
table(wait_time)
cbind(table(wait_time))

load("./Data/dataset.rda")
attach(dataset)
table(Sex,Age)

load("./Data/grades.rda")
attach(grades)
head(grades)
table(Gender, Grade)

####
#Pie Charts and Bar Charts
#######
#useful for nominal and ordinal data
#human eye is better able to judge linear distance than area, so barchart typically
#more informative than pie chart
Car_Colors <- data.frame(Color = factor(), Percent = numeric())
Car_Colors <- edit(Car_Colors)
attach(Car_Colors)
pie(Percent)
#Create color codes for coloring pie chart
colors <- c("#C0C0C0", "black", "white", "#696969", "red", "blue", "brown", "green", "#F0E68C")
#bind colors to df
Car_Colors <- cbind(Car_Colors, colors)
#dispaly pie chart with colors
pie(Percent, col = colors)
names(Percent) <- c("Silver","Black", "White","Gray","Red","Blue","Brown","Green","Other")
names(Percent)
pie(Percent, col = colors, main = "Pie Graph of Auto Color Preferences")

#barplot
barplot(Percent, col = colors, main = "Pie Graph of Auto Color Preferences")

attach(grades)
barplot(table(Grade), col = c("red","green","blue"), main = "Grade Level")

#############
#Boxplot
###########
#
#Boxplot can give an idea of how skewed or unskewed the data are
attach(dataset)
boxplot(Age, main = "Boxplot for Age")

load("./Data/quizzes.rda")
attach(quizzes)
quizzes
boxplot(quizzes)

#########
#Histograms
########
#
attach(faithful)
hist(waiting)

############
#Line graphs
##########
load("./Data/Min_Wage.rda")
attach(Min_Wage)
Min_Wage
plot(Value, type = "o", xlab = "Year", col = "blue", main = "Minimum wage")


##########
#ScTTERPLOTS
############
load("./Data/hours.rda")
attach(hours)
plot(hours, GPA)

#add a trendline comebine with lm model
plot(Hours, GPA, abline(lm(GPA~Hours)), main = "Scatterplot of Hour and GPA")

