## HOW TO PERFORM SIMPLE LINEAR REGRESSION ##
## AND DIAGNOSTICS ##

## Load libraries
library(lawstat)

## Read in the data
Advertising <- read.csv("Advertising.csv")
attach(Advertising)

###############################################################################

## Fit a simple linear model with Y = sales and X = TV
lm.fit.TV = lm(sales~TV, data = Advertising)
summary(lm.fit.TV)

## Scatter plot along with linear fit
par(mfrow=c(1,1))
plot(TV,sales)
abline(lm.fit.TV, col='red', lwd=3)

## Diagnostic plots
plot(lm.fit.TV, which = c(1))
plot(lm.fit.TV, which = c(2))
plot(lm.fit.TV, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV$residuals, ylab="Residuals")
hist(lm.fit.TV$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV$residuals)
#
summary(Advertising)
Advertising$Group = rep("Group1",200)
indexTV = (TV > median(TV))
Advertising$Group[indexTV] = "Group2"
levene.test(lm.fit.TV$residuals, Advertising$Group , location = c("median"))

###############################################################################

## Remediation of issues Sales --> sqrt(Sales)
lm.fit.TV2 = lm(sqrt(sales)~TV)
summary(lm.fit.TV2)
par(mfrow=c(1,1))
plot(TV,sqrt(sales))
abline(lm.fit.TV2, col='red', lwd=3)
par(mfrow=c(1,1))
plot(lm.fit.TV2, which = c(1))
plot(lm.fit.TV2, which = c(2))
plot(lm.fit.TV2, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV2$residuals, ylab="Residuals")
hist(lm.fit.TV2$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV2$residuals)
levene.test(lm.fit.TV2$residuals,Advertising$Group , location = c("median"))

###############################################################################

## Remediation of issues Sales --> log10(Sales)
lm.fit.TV3 = lm(log10(sales)~TV)
summary(lm.fit.TV3)
par(mfrow=c(1,1))
plot(TV,log10(sales))
abline(lm.fit.TV3, col='red', lwd=3)

## Diagnostic plots
par(mfrow=c(1,1))
plot(lm.fit.TV3, which = c(1))
plot(lm.fit.TV3, which = c(2))
plot(lm.fit.TV3, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV3$residuals, ylab="Residuals")
hist(lm.fit.TV3$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV3$residuals)
levene.test(lm.fit.TV3$residuals,Advertising$Group , location = c("median"))

###############################################################################

## Remediation of issues Sales --> 1/(Sales)
lm.fit.TV4 = lm(1/(sales)~TV)
summary(lm.fit.TV4)
par(mfrow=c(1,1))
plot(TV,1/(sales))
abline(lm.fit.TV4, col='red', lwd=3)

## Diagnostic plots
par(mfrow=c(1,1))
plot(lm.fit.TV4, which = c(1))
plot(lm.fit.TV4, which = c(2))
plot(lm.fit.TV4, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV4$residuals, ylab="Residuals")
hist(lm.fit.TV4$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV4$residuals)
levene.test(lm.fit.TV4$residuals,Advertising$Group , location = c("median"))

###############################################################################

## Remediation of issues Sales --> log10(Sales), TV --> log10(TV)
lm.fit.TV5 = lm(log10(sales)~log10(TV))
summary(lm.fit.TV5)
par(mfrow=c(1,1))
plot(log10(TV),log10(sales))
abline(lm.fit.TV5, col='red', lwd=3)

## Diagnostic plots
par(mfrow=c(1,1))
plot(lm.fit.TV5, which = c(1))
plot(lm.fit.TV5, which = c(2))
plot(lm.fit.TV5, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV5$residuals, ylab="Residuals")
hist(lm.fit.TV5$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV5$residuals)
#
Advertising$Log10Group = rep("Group1",200)
indexlog10TV = (log10(TV) > median(log10(TV)))
Advertising$Log10Group[indexlog10TV] = "Group2"
levene.test(lm.fit.TV5$residuals,Advertising$Log10Group , location = c("median"))

###############################################################################

## Remediation of issues Sales --> log10(Sales), TV --> sqrt(TV)
lm.fit.TV6 = lm(log10(sales)~sqrt(TV))
summary(lm.fit.TV6)
par(mfrow=c(1,1))
plot(sqrt(TV),log10(sales))
abline(lm.fit.TV6, col='red', lwd=3)

## Diagnostic plots
par(mfrow=c(1,1))
plot(lm.fit.TV6, which = c(1))
plot(lm.fit.TV6, which = c(2))
plot(lm.fit.TV6, which = c(5))

## Check distribution of residuals
boxplot(lm.fit.TV6$residuals, ylab="Residuals")
hist(lm.fit.TV6$residuals)

# Diagnostic tests
shapiro.test(lm.fit.TV6$residuals)
#
Advertising$SqrtGroup = rep("Group1",200)
indexsqrtTV = (sqrt(TV) > median(sqrt(TV)))
Advertising$SqrtGroup[indexsqrtTV] = "Group2"
levene.test(lm.fit.TV6$residuals,Advertising$SqrtGroup , location = c("median"))

