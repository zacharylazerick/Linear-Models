##Create Simple Linear Models for HW4

##Problem 3
#Read in the Data in the CSV File
Pharmacy = read.csv("Pharmacy.csv")
attach(Pharmacy)

#Fit a Linear Model
Pharmacy.fit <- lm(Sales.Volume~Ingredients.Purchased.Directly, data=Pharmacy)
summary(Pharmacy.fit)

#Plot the Observed Data and the Regression Line
plot(Ingredients.Purchased.Directly, Sales.Volume)
abline(Pharmacy.fit, lwd = 3, col = "red")

##Problem 4
#Read in the Data in the CSV File
Casino = read.csv("Casino.csv")
attach(Casino)

#Fit a Linear Model
Casino.fit <- lm(Crime.Rate~Number.of.Casino.Employees, data=Casino)
summary(Casino.fit)

#Plot the Observed Data and the Regression Line
plot(Number.of.Casino.Employees, Crime.Rate)
abline(Casino.fit, lwd = 3, col="blue")