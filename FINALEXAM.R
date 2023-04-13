#Zachary Lazerick
#Linear Models
#Final Exam
#Professor Soares

#Question 7 --> (BODYFT.Rdata)

load(BODYFT.Rdata)
attach(BODYFT)
BODYFT = na.omit(BODYFT)

lm.fit.model1 = lm(BODYFAT~BMI, data = BODYFT)
summary(lm.fit.model1)

lm.fit.model2 = lm(BODYFAT~HEIGHT+WEIGHT, data = BODYFT)
summary(lm.fit.model2)

lm.fit.model3 = lm(BODYFAT~HEIGHT+WEIGHT+BMI, data = BODYFT)
sumamry(lm.fit.model3)

anova(lm.fit.model1, lm.fit.model3)

anova(lm.fit.model2, lm.fit.model3)