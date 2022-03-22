#Set up
setwd("C:/Users/liux3/Desktop/JuneANOVA")
IvsO<-read.csv("IvsO.csv", header=T)
LvsH<-read.csv("LvsH.csv", header = T)
TwoWay<-read.csv("2WayANOVA.csv", header = T)
library(tidyverse)
library(ggplot2)
#install.packages('datarium')
library(datarium)


#IvsO
#check data
summary(IvsO)
#normality
IvsO$Sum=as.numeric(IvsO$Sum)
IvsO$Depth=as.factor(IvsO$Depth)
IvsO$ï..Day =as.factor(IvsO$ï..Day )
summary(IvsO$Sum)
summary(IvsO$Day)
summary(IvsO$Depth)
IvsO$ï..Day 
qqnorm(sqrt(IvsO$Sum))
qqline(sqrt(IvsO$Sum))

boxplot(sqrt(IvsO$Sum)~IvsO$Depth, las=1, ylab="SQRT Daily Sum", xlab="Depth", data=IvsO)

#model
IvsO.aov<-aov(sqrt(Sum)~factor(Depth)+Error(ï..Day),data=IvsO)
summary(IvsO.aov)
plot(IvsO.aov)
par(mfrow=c(2,2))



#LvsH

#check data
summary(LvsH)
#normality
LvsH$Ratio=as.numeric(LvsH$Ratio)
LvsH$Density=as.factor(LvsH$Density)
LvsH$ï..Day =as.factor(LvsH$ï..Day )
summary(LvsH$Ratio)
summary(LvsH$ï..Day)
summary(LvsH$Density)
LvsH$ï..Day 
qqnorm(1/(LvsH$Ratio))
qqline(1/(LvsH$Ratio))

boxplot(1/(LvsH$Ratio)~LvsH$Density, las=1, ylab="Reciprical I/O Ratio", xlab="Density", data=LvsH)

#model
LvsH.aov<-aov(1/Ratio~factor(Density)+Error(ï..Day),data=LvsH)
summary(LvsH.aov)
plot(LvsH.aov)
par(mfrow=c(2,2))
qqnorm(LvsH$Ratio)
qqline(LvsH$Ratio)
qqnorm(1/(LvsH$Ratio))
qqline(1/(LvsH$Ratio))

#2Way
#check data
summary(TwoWay)
#normality
TwoWay$Sum=as.numeric(TwoWay$Sum)
TwoWay$Depth=as.factor(TwoWay$Depth)
TwoWay$Density=as.factor(TwoWay$Density)

summary(TwoWay$Sum)
summary(TwoWay$ï..Day)
summary(TwoWay$Depth)
qqnorm(sqrt(TwoWay$Sum))
qqline(sqrt(TwoWay$Sum))

boxplot(sqrt(TwoWay$Sum)~IvsO$Depth, las=1, ylab="SQRT Daily Sum", xlab="Depth", data=IvsO)

#model
TwoWay.aov<-aov(sqrt(Sum)~factor(Depth)*factor(Density)+Error(ï..Day),data=TwoWay)
summary(TwoWay.aov)
plot(TwoWay.aov)
par(mfrow=c(1,1))

