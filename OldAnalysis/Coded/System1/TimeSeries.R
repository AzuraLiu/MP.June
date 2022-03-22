
#install.packages("openxlsx")

library("openxlsx")
library(tidyverse)

setwd("C:/Users/liux3/Desktop/work/Data/Raw/Coded/System1")



Jan1<-read.xlsx("VT Sap1 k values 01-2017.xlsx")
Feb1<-read.xlsx("VT Sap1 k values 02-2017.xlsx")
Mar1<-read.xlsx("VT Sap1 k values 03-2017.xlsx")
Apr1<-read.xlsx("VT Sap1 k values 04-2017.xlsx")
May1<-read.xlsx("VT Sap1 k values 05-2017.xlsx")
Jun1<-read.xlsx("VT Sap1 k values 06-2017.xlsx")
Jul1<-read.xlsx("VT Sap1 k values 07-2017.xlsx")
Aug1<-read.xlsx("VT Sap1 k values 08-2017.xlsx")
Sep1<-read.xlsx("VT Sap1 k values 09-2017.xlsx")
Nov1<-read.xlsx("VT Sap1 k values 11-2017.xlsx")
Dec1<-read.xlsx("VT Sap1 k values 12-2017.xlsx")

#time series
Syst1<-do.call("rbind",list(Jan1, Feb1, Mar1, Apr1, May1, Jun1, Jul1,Aug1, Sep1,Nov1,Dec1))
Syst1$Time <- format(strptime(sprintf("%04d", Syst1$Time), format = "%H%M"), "%H:%M")
Syst1$Date <- as.Date(as.numeric(Syst1$Date), origin = "1899-12-30") 
Syst1$Datetime <- with(Syst1, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M"))

summary(HC1)

 
#Subset
HC1<-Syst1[,c(2,3,6:21,38)]
LO1<-Syst1[,c(2,3,22:37,38)]
Weather1<-Syst1[,c(2:5,38)]
PAR1<-Syst1[,c(2:3, 5, 38)]
VPD1<-Syst1[,c(2:4, 38)]

###################weather plots#######################
#Weather1.1 <- Weather1 %>%
 # mutate(PAR = as.character(Weather1$PAR)) %>%
  #pivot_longer(col=3:4, names_to = "Weather", values_to = "val")


#Time Series
Weatherbyweek1<-split(Weather1, cut(Weather1$DOY, breaks = seq(0, 366, by = 7)))

#plot PAR
PARplot1 <- lapply(Weatherbyweek1, function(x)
  p <- ggplot(data = x, aes(x = Datetime, y = PAR)) +
    geom_point(size = 0.2) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous (labels= scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ",")))
  

PARplot1

lapply(names(PARplot1), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=PARplot1[[x]]))


######Plot VPD


Weather1 <- Weather1 %>%
  mutate(VPD = as.numeric(Weather1$VPD))
#%>%
#  mutate(VPD = !is.na(VPD))
Weatherbyweek1<-split(Weather1, cut(Weather1$DOY, breaks = seq(0, 365, by = 7)))


VPDplot1 <- lapply(Weatherbyweek1, function(x)
  p <- ggplot(data = x, aes(x = Datetime, y = VPD))+
    geom_point(size = 0.2) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d")+
    scale_y_continuous(labels= scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ".")))

VPDplot1

lapply(names(VPDplot1), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=VPDplot1[[x]]))

########################HC Plots############################

HC1.1 <- HC1 %>%
  pivot_longer(col=3:18, names_to = "HC", values_to = "val") %>%
  mutate(val = as.numeric(val))


#Time Series
HCbyweek1<-split(HC1.1, cut(HC1.1$DOY, breaks = seq(0, 365, by = 7)))

#plot
HCplot1 <- lapply(HCbyweek1, function(x)
  p <- ggplot(x, aes(x = Datetime, y =119*(val^1.231))) +
    geom_point(size = 0.2, aes(shape = HC, col = HC)) +
    scale_shape_manual(values = rep(c(0:2, 5:6, 9:10, 11:12), times = 2)) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1,
                                                      decimal.mark = ',')))
  
HCplot1
install.packages("ggpubr")
library(ggpubr)
get_legend(HCplot1)

lapply(names(HCplot1), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=HCplot1[[x]],width=12, height=6))
###################LO Plots##########################

LO1.1 <- LO1 %>%
  pivot_longer(col=3:18, names_to = "LO", values_to = "val") %>%
  mutate(val = as.numeric(val))


#Time Series
LObyweek1<-split(LO1.1, cut(LO1.1$DOY, breaks = seq(0, 365, by = 7)))

#plot
LOplot1 <- lapply(LObyweek1, function(x)
  p <- ggplot(x, aes(x = Datetime, y =119*(val^1.231))) +
    geom_point(size = 0.2, aes(shape = LO, col = LO)) +
    scale_shape_manual(values = rep(c(0:2, 5:6, 9:10, 11:12), times = 2)) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous(labels = scales::number_format(accuracy = 1,
                                                      decimal.mark = ',')))
LOplot1


lapply(names(LOplot1), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=LOplot1[[x]],width=12, height=6))
#############################################
###################Soil Moisture##################################
setwd("C:/Users/liux3/Desktop/work/Data/Raw/Coded/System1")

library(openxlsx)
SoMo1<-read.xlsx("SoMo1.xlsx")

SoMo1.1<-SoMo1[,c(1,8,9)] 

SoMo1.2<-SoMo1.1%>%
  pivot_longer(col=2:3, names_to = "SoMo", values_to = "SoilMoisture") %>%
  mutate(SoilMoisture=as.numeric(SoilMoisture))

SoMobyWeek<-split(SoMo1.2, cut(SoMo1.2$doy, breaks = seq(0, 365, by = 7)))

#plot
SoMo1plot <- lapply(SoMobyWeek, function(x)
  p <- ggplot(x, aes(x = doy, y=SoilMoisture))+
    geom_point(size=2, aes(col =SoMo))+
    scale_x_discrete(breaks=seq(min(x$doy),max(x$doy), by=1))+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.05))+
    expand_limits(y=0))

 
SoMo1plot


lapply(names(SoMo1plot), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=SoMo1plot[[x]],  
                         width=7.5, height=5, units="in", dpi=300))
##########################################################
setwd("C:/Users/liux3/Desktop/work/Data/Raw/Coded/System1")

ENVdat<-read.csv("ENVdat.csv")

RAD<-ENVdat[,c(2,3,27)]
RAD$Radiation<-RAD$SRADV.H.1..watt.
#RAD$Date<-as.numeric(RAD$Date)
RAD$Time<- format(strptime(sprintf(RAD$Time), format = "%H"), "%H")

RAD$Date <- as.Date(RAD$Date, format="%m/%d/%Y")
                    #origin = "1899/12/30") 
RAD$Datetime <- with(RAD, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H"))

RAD$Week <- format(as.Date(RAD$Date+1, format="%m/%d/%Y"), format= "%W")
RAD$Week<-as.numeric(RAD$Week)
Radbyweek<-split(RAD, cut(RAD$Week, breaks = seq(0, 53), by = 1))

RADplot <- lapply(Radbyweek, function(x)
  p <- ggplot(x, aes(x = Datetime, y=Radiation))+
    geom_point(size=1)+
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous (labels= scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ",")))

week1<-RAD[c(1:168),]



plot(week1$Datetime,week1$Radiation, width=8, height=5)



RADplot

lapply(names(RADplot), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=RADplot[[x]],width=8, height=5))


####################Temp####################

ENVdat<-read.csv("ENVdat.csv")

Temp<-ENVdat[,c(2,3,5)]
Temp$Temperature<-Temp$TOBS.I.1..degC.
#RAD$Date<-as.numeric(RAD$Date)
Temp$Time<- format(strptime(sprintf(Temp$Time), format = "%H"), "%H")

Temp$Date <- as.Date(Temp$Date, format="%m/%d/%Y")
#origin = "1899/12/30") 
Temp$Datetime <- with(Temp, as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H"))

###
  plot(x=Temp$Datetime, y=Temp$Temperature)





Temp$Week <- format(as.Date(Temp$Date+1, format="%m/%d/%Y"), format= "%W")
Temp$Week<-as.numeric(Temp$Week)
Tempbyweek<-split(Temp, cut(Temp$Week, breaks = seq(0, 52, by = 1)))

Tempplot <- lapply(Tempbyweek, function(x)
  p <- ggplot(x, aes(x = Datetime, y=Temperature))+
    geom_point(size=1)+
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
    scale_y_continuous (labels= scales::number_format(accuracy = 0.01,
                                                      decimal.mark = ",")))




Tempplot

lapply(names(Tempplot), 
       function(x)ggsave(filename=paste(x,".jpeg",sep=""), plot=Tempplot[[x]],width=8, height=5))




