#Zachary Lazerick
#STAT 231
#Homework 5 
#October 28, 2021

#Problem 5
#Ampules.csv

#Read in the Data Set
Ampules = read.csv("Ampules.csv")
attach(Ampules)

#Report Regression Line Statistics
Ampules.fit <- lm(Y~�..X, data=Ampules)
summary(Ampules.fit)

#Problem 6
#GPA.csv

#Read in the Data Set
GPA = read.csv("GPA.csv")
attach(GPA)

#Report Regression Line Statistics
GPA.fit <- lm(Y~�..X, data=GPA)
summary(GPA.fit)

#Define Variables Needed to Calculate Beta_0 and Beta_1 and MSE
mean(�..X)
mean(Y)
GPA$X2=(GPA$�..X*GPA$�..X)
GPA$XY=(GPA$Y*GPA$�..X)
GPA$XminusXbar=(GPA$�..X-mean(�..X))
GPA$XminusXbar2=(GPA$XminusXbar*GPA$XminusXbar)
GPA$YminusYbar=(GPA$Y-mean(Y))
GPA$YminusYbar2=(GPA$YminusYbar*GPA$YminusYbar)

#Summation for Variables
sum(GPA$XY)
sum(GPA$X2)
sum(GPA$XminusXbar2)
sum(GPA$YminusYbar2)
sum(GPA$�..X)
sum(GPA$Y)

#Problem 7
#Plastic.csv

#Read in the Data
Plastic = read.csv("Plastic.csv")
attach(Plastic)

#Report Regression Line Statistics
Plastic.fit <- lm(Y~�..X, data=Plastic)
summary(Plastic.fit)
mean(�..X)
mean(Y)
