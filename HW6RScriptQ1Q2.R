#Zachary Lazerick
#STAT 231
#Professor Soares
#November 9, 2021
#HW6 R_Script

#Load the Lawstat Library to Access the Brown-Forscythe-Levene Test
library(lawstat)

#Read in the Data
Advertising = read.csv("Advertising.csv")
attach(Advertising)

##Question 1: Building an SLR Model Sales ~ Radio 
#(Initial Results)

#Construct a Simple Linear Model with Predictor: Radio and Response: Sales
lm.fit.radio = lm(sales~radio, data = Advertising)
summary(lm.fit.radio)

#Construct a Scatter Plot with the Calculated Linear Model
#"Eye-Test" for Abnormalities (Non-linearity, etc.)
par(mfrow=c(1,1))
plot(radio, sales)
abline(lm.fit.radio, col = 'red', lwd = 3)

#Construct Plots to Check Diagnostics
# 1 --> Residuals vs. Fitted, 2 --> Q-Q Plot, 5 --> Residuals vs. Leverage
plot(lm.fit.radio, which = c(1))
plot(lm.fit.radio, which = c(2))
plot(lm.fit.radio, which = c(5))

#Check Distribution of Residuals for Obvious Deviations
boxplot(lm.fit.radio$residuals, ylab="Residuals")
hist(lm.fit.radio$residuals)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.radio$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (radio > median(radio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio$residuals, Advertising$Group, location = c("median"))

#Conclusion: From the Shapiro-Wilks Test, we conclude that the data is not from
#a normal distribution, and from the Levene Test, we conclude that the data is
#not homoscedastic, rather heteroscedastic. Therefore, we must transform the data
#in hopes of obtaining approximately normal and homoscedastic data to have 
#statistically significant data. 

###############################################################################

##Transformations

#First, we will transform our Response, Sales, to see if we can obtain 
#homoscedatic data. 

#SLR with Response (sqrt)Sales, Predictor Radio 
#(lm.fit.radio.2)

#Transform sqrtSales = sales
Advertising$sqrtSales = sqrt(sales)

#SLR Model
lm.fit.radio.2 = lm(sqrtSales~radio, data=Advertising)
summary(lm.fit.radio.2)

#Plots
par(mfrow=c(1,1))
plot(radio, sqrtSales)
abline(lm.fit.radio.2, col = 'red', lwd = 3)

# 1 --> Residuals vs. Fitted, 2 --> Q-Q Plot, 5 --> Residuals vs. Leverage
plot(lm.fit.radio.2, which = c(1))
plot(lm.fit.radio.2, which = c(2))
plot(lm.fit.radio.2, which = c(5))

boxplot(lm.fit.radio.2$residuals, ylab="Residuals")
hist(lm.fit.radio.2$residuals)

#Diagnostics
shapiro.test(lm.fit.radio.2$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (radio > median(radio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.2$residuals, Advertising$Group, location = c("median"))

###############################################################################

#SLR with Response (log10)Sales, Predictor Radio 
#(lm.fit.radio.3)

#Transform log10Sales = sales
Advertising$log10Sales = log10(sales)

#SLR Model
lm.fit.radio.3 = lm(log10Sales~radio, data=Advertising)
summary(lm.fit.radio.3)

#Diagnostics
shapiro.test(lm.fit.radio.3$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (radio > median(radio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.3$residuals, Advertising$Group, location = c("median"))

###############################################################################

#SLR with Response (1/Sales), Predictor Radio 
#(lm.fit.radio.4)

#Transform (1/sales) = sales
Advertising$NewSales = (1/sales)

#SLR Model
lm.fit.radio.4 = lm(NewSales~radio, data=Advertising)
summary(lm.fit.radio.4)

#Diagnostics
shapiro.test(lm.fit.radio.4$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (radio > median(radio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.4$residuals, Advertising$Group, location = c("median"))

###############################################################################

#Now that we have transformed our response to obtain homoscedastic data
#we will now transform x in hopes to obtain approximate normal data using our
#transformation for our response that gave us homoscedastic data

#SLR with Response (1/Sales), Predictor (sqrt)Radio 
#(lm.fit.radio.5)

#Transform sqrtRadio = radio
Advertising$sqrtRadio = sqrt(radio)

#SLR Model
lm.fit.radio.5 = lm(NewSales~Advertising$sqrtRadio, data=Advertising)
summary(lm.fit.radio.5)

#Diagnostics
shapiro.test(lm.fit.radio.5$residuals)

median(Advertising$sqrtRadio)

Advertising$Group = rep("Group1", 200)
indexRadio = (Advertising$sqrtRadio > median(Advertising$sqrtRadio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.5$residuals, Advertising$Group, location = c("median"))

###############################################################################

#SLR with Response (1/Sales), Predictor log10Radio
#(lm.fit.radio.6)

#Transform log10Radio = radio
Advertising$log10Radio = log10(radio)

#SLR Model
lm.fit.radio.6 = lm(NewSales~Advertising$log10Radio, data = Advertising)
summary(lm.fit.radio.6)

#Diagnostics
shapiro.test(lm.fit.radio.6$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (Advertising$log10Radio > median(Advertising$log10Radio))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.6$residuals, Advertising$Group, location = c("median"))

###############################################################################

#SLR with Response (1/Sales), Predictor (1/Radio)
#(lm.fit.radio.7)

#Transform 1/Radio = radio
Advertising$NewRadio = (1/radio)

#SLR Model
lm.fit.radio.7 = lm(NewSales~NewRadio, data = Advertising)
summary(lm.fit.radio.7)

#Diagnostics
shapiro.test(lm.fit.radio.7$residuals)

Advertising$Group = rep("Group1", 200)
indexRadio = (Advertising$NewSales > median(Advertising$NewSales))
Advertising$Group[indexRadio] = "Group2"
levene.test(lm.fit.radio.7$residuals, Advertising$Group, location = c("median"))

###############################################################################

#Question 2: Building an SLR Model for Sales ~ Newspaper

#Construct a Simple Linear Model with Predictor: Newspaper and Response: Sales
#(Initial Results)
lm.fit.news = lm(sales~newspaper, data = Advertising)
summary(lm.fit.news)

#Construct a Scatter Plot with the Calculated Linear Model
#"Eye-Test" for Abnormalities (Non-linearity, etc.)
par(mfrow=c(1,1))
plot(newspaper, sales)
abline(lm.fit.news, col = 'blue', lwd = 3)

#Construct Plots to Check Diagnostics
# 1 --> Residuals vs. Fitted, 2 --> Q-Q Plot, 5 --> Residuals vs. Leverage
plot(lm.fit.news, which = c(1))
plot(lm.fit.news, which = c(2))
plot(lm.fit.news, which = c(5))

#Check Distribution of Residuals for Obvious Deviations
boxplot(lm.fit.news$residuals, ylab="Residuals")
hist(lm.fit.news$residuals)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (newspaper > median(newspaper))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news$residuals, Advertising$Group, location = c("median"))

###############################################################################

#SqrtSales~Newspaper
Advertising$sqrtSales = sqrt(sales)

lm.fit.news.2 = lm(sqrtSales~newspaper, data = Advertising)
summary(lm.fit.news.2)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.2$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (newspaper > median(newspaper))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.2$residuals, Advertising$Group, location = c("median"))

################################################################################

#log10Sales~Newspaper
Advertising$log10Sales = log10(sales)

lm.fit.news.3 = lm(log10Sales~newspaper, data = Advertising)
summary(lm.fit.news.3)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.3$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (newspaper > median(newspaper))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.3$residuals, Advertising$Group, location = c("median"))

################################################################################
#(1/Sales)~Newspaper
Advertising$NewSales = (1/sales)

lm.fit.news.4 = lm(NewSales~newspaper, data = Advertising)
summary(lm.fit.news.4)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.4$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (newspaper > median(newspaper))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.4$residuals, Advertising$Group, location = c("median"))

################################################################################
#log10Sales~sqrt(Newspaper)
Advertising$sqrtNews = sqrt(newspaper)

lm.fit.news.5 = lm(log10Sales~sqrtNews, data = Advertising)
summary(lm.fit.news.5)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.5$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (Advertising$sqrtNews > median(Advertising$sqrtNews))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.5$residuals, Advertising$Group, location = c("median"))

################################################################################
#sqrt(Sales)~sqrt(Newspaper)

lm.fit.news.6 = lm(sqrtSales~sqrtNews, data = Advertising)
summary(lm.fit.news.6)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.6$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (Advertising$sqrtNews > median(Advertising$sqrtNews))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.6$residuals, Advertising$Group, location = c("median"))

################################################################################
#sqrt(Sales)~log10(Newspaper)

Advertising$log10News = log10(newspaper)
attach(Advertising)

lm.fit.news.7 = lm(sqrtSales~log10News, data = Advertising)
summary(lm.fit.news.7)

median(Advertising$log10News)
#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.7$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (log10News > median(log10News))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.7$residuals, Advertising$Group, location = c("median"))

################################################################################
#sqrt(Sales)~log10(Newspaper)

Advertising$log10News = log10(newspaper)
attach(Advertising)

lm.fit.news.8 = lm(log10Sales~log10News, data = Advertising)
summary(lm.fit.news.8)

median(Advertising$log10News)
#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.8$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (log10News > median(log10News))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.8$residuals, Advertising$Group, location = c("median"))

################################################################################
#(1/Sales)~log10(Newspaper)
Advertising$NewSales = (1/sales)

lm.fit.news.9 = lm(NewSales~log10News, data = Advertising)
summary(lm.fit.news.9)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.9$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (log10News > median(log10News))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.9$residuals, Advertising$Group, location = c("median"))

################################################################################
#(1/Sales)~sqrt(Newspaper)
Advertising$NewSales = (1/sales)

lm.fit.news.10 = lm(NewSales~sqrtNews, data = Advertising)
summary(lm.fit.news.10)

#Run Shapiro-Wilk and Brown-Forscythe-Levene Tests
shapiro.test(lm.fit.news.10$residuals)

Advertising$Group = rep("Group1", 200)
indexNews = (sqrtNews > median(sqrtNews))
Advertising$Group[indexNews] = "Group2"
levene.test(lm.fit.news.10$residuals, Advertising$Group, location = c("median"))