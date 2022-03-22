setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")
library("ggplot2")
library(dplyr)

####Data Setup#################################################################
HAJunDay<-read.xlsx("HAJuneDay.xlsx")#HAON3,6 HAON6
#HBJunDay<-read.xlsx("HBJuneDay.xlsx")#good
#HCJunDay<-read.xlsx("HCJuneDay.xlsx")#good
HOJunDay<-read.xlsx("HOJuneDay.xlsx")#HOOS10
LAJunDay<-read.xlsx("LAJuneDay.xlsx")#LAON1,4
#LBJunDay<-read.xlsx("LBJuneDay.xlsx")#good
LCJunDay<-read.xlsx("LCJuneDay.xlsx")#LCON1
LOJunDay<-read.xlsx("LOJuneDay.xlsx")#LOON10

Syst1<-read.xlsx("VT Sap1 k values 06-2017.xlsx")
               
Syst2<-read.xlsx("VT Sap2 k values 06-2017.xlsx")
Syst3<-read.xlsx("VT Sap3 k values 06-2017.xlsx")
Syst4<-read.xlsx("VT Sap4 k values 06-2017.xlsx")


#####Fixing time and date#####################################################################
Syst1$Time <- format(strptime(sprintf("%04d", Syst1$Time), format = "%H%M"), "%H:%M")
Syst1$Date <- as.Date(as.numeric(Syst1$Date), origin = "1899-12-30") 
Syst1$Datetime <- with(Syst1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

Syst2$Time <- format(strptime(sprintf("%04d", Syst2$Time), format = "%H%M"), "%H:%M")
Syst2$Date <- as.Date(as.numeric(Syst2$Date), origin = "1899-12-30") 
Syst2$Datetime <- with(Syst1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

Syst3$Time <- format(strptime(sprintf("%04d", Syst3$Time), format = "%H%M"), "%H:%M")
Syst3$Date <- as.Date(as.numeric(Syst3$Date), origin = "1899-12-30") 
Syst3$Datetime <- with(Syst1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

Syst4$Time <- format(strptime(sprintf("%04d", Syst4$Time), format = "%H%M"), "%H:%M")
Syst4$Date <- as.Date(as.numeric(Syst4$Date), origin = "1899-12-30") 
Syst4$Datetime <- with(Syst1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

######By week#########################################################

Syst1Week<-split(Syst1, cut(strptime(Syst1$Datetime, format="%F %R"),"5 days"))
Syst2Week<-split(Syst2, cut(strptime(Syst2$Datetime, format="%F %R"),"5 days"))
Syst3Week<-split(Syst3, cut(strptime(Syst3$Datetime, format="%F %R"),"5 days"))
Syst4Week<-split(Syst4, cut(strptime(Syst4$Datetime, format="%F %R"),"5 days"))

####Plot########################################################################
LOJunGraph<-ggplot(Syst1Week$`2017-06-26`,aes(x=Datetime,y=as.numeric(LOON10)))+
                  geom_point()
                  
LOJunGraph

LCJunGraph<-ggplot(Syst2Week$`2017-06-26`,aes(x=Datetime,y=as.numeric(LCON1)))+
  geom_point()

LCJunGraph

HOJunGraph<-ggplot(Syst2Week$`2017-06-26`,aes(x=Datetime,y=as.numeric(HOOS10)))+
  geom_point()

HOJunGraph

LAJunGraph<-ggplot(Syst3Week$`2017-06-26`)+
  geom_point(aes(x=Datetime,y=as.numeric(LAON1),color="LAON1"),size=0.8)+
  geom_point(aes(x=Datetime,y=as.numeric(LAON4),color="LAON4"),size=0.8)+
  ylab("Sapflux")

#  + scale_color_manual(breaks = c("LAON1", "LAON4"),
                    # values = c("red", "blue"))


LAJunGraph

HAJunGraph<-ggplot(Syst4Week$`2017-06-26`)+
  geom_point(aes(x=Datetime,y=as.numeric(HAOS3),color="HAOS3"),size=0.8)+
  geom_point(aes(x=Datetime,y=as.numeric(HAOS6),color="HAOS6"),size=0.8)+
  geom_point(aes(x=Datetime,y=as.numeric(HAON6),color="HAON6"),size=0.8)+
  ylab("Sapflux")

#  + scale_color_manual(breaks = c("LAON1", "LAON4"),
# values = c("red", "blue"))


HAJunGraph
