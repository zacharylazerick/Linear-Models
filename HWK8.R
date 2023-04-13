#Zachary Lazerick
#STAT 231
#Homework 8
#Professor Soares

#Problem 1

load("WAFER.Rdata")
attach(WAFER)

#(a)
plot(TEMP,FAILTIME)

#(b)
lm.fit.WA = lm(FAILTIME~TEMP+I(TEMP^2), data=WAFER)
summary(lm.fit.WA)

#Problem 2

load("INFECTION.Rdata")
attach(INFECTION)

#(a)
lm.fit.INF = lm(RATE~EST+I(EST^2), data=INFECTION)
summary(lm.fit.INF)

#(b)
plot(EST,RATE)

#(c)
INFECTION2 = INFECTION[-24,]
lm.fit.INF2 = lm(INFECTION2$RATE~INFECTION2$EST+I(INFECTION2$EST^2), data=INFECTION2)
summary(lm.fit.INF2)
plot(INFECTION2$EST,INFECTION2$RATE)

#Problem 3

load("ACCHW.Rdata")
attach(ACCHW)

#(c)
lm.fit.ACC = lm(IMPROVE~ASSIST, data=ACCHW)
summary(lm.fit.ACC)

#Problem 4

load("REPELLENT.Rdata")
attach(REPELLENT)

#(b)
lm.fit.REP = lm(COST~TYPE, data=REPELLENT)
summary(lm.fit.REP)

#(e)
lm.fit.HRS = lm(COST~HOURS, data=REPELLENT)
summary(lm.fit.HRS)

#Problem 5

load("GASTURBINE.Rdata")
attach(GASTURBINE)

#(a)
lm.fit.GASf = lm(HEATRATE~RPM+CPRATIO+RPM:CPRATIO+I(RPM^2)+I(CPRATIO^2), data=GASTURBINE)
summary(lm.fit.GASf)

#(d)
lm.fit.GASr = lm(HEATRATE~RPM+CPRATIO+RPM:CPRATIO, data=GASTURBINE)
summary(lm.fit.GASr)

#(f)
anova(lm.fit.GASr,lm.fit.GASf)
