
#####setup###########
setwd("C:/Users/liux3/Desktop/work/Data/Raw/NS")
library("openxlsx")
library(tidyverse)
require(dplyr)
library(GGally)
#install.packages("lubridate")


Jun1<-read.xlsx("VT Sap1 k values 06-2017.xlsx")
Jun1$Time <- format(strptime(sprintf("%04d", Jun1$Time), format = "%H%M"), "%H:%M")
Jun1$Date <- as.Date(as.numeric(Jun1$Date), origin = "1899-12-30") 
Jun1$Datetime <- with(Jun1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

Jun1<-Jun1[,-c(38,39)]

HC.Jun<-Jun1[-c(1:27,2812:2880),c(2,3,6:21,38)]

HCbyDay<-split(HC.Jun, cut(HC.Jun$Datetime, breaks = "1440 min"))

sum(HCbyDay$`2017-06-01 06:45:00`$HCOS1)

DailyHC<-lapply(HCbyDay, function(x) colSums(Filter(is.numeric),x))




#subset#
sap<-HC.Jun[,c(3:18)]
sapN <- HC.Jun[,c(4,5,7,9,11,13,16,18)]
sapS <- HC.Jun[,c(3,8,10,12,14,17)]
sapN$sapN<-as.numeric(sapN$sapN)
sapS$sapS<-as.numeric(sapS$sapS)

#sapI <- sapI %>%
 # filter(sapI != 0)

#sapO<-sapO %>%
 #  filter(sapO !=0)

###########################################################

###Check Dat###########################################
summary(sapN)
summary(sapS)


