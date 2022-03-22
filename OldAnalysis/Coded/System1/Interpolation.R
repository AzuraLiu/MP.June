setwd("C:/Users/liux3/Desktop/work/Data/Raw/Coded/System1")
library("openxlsx")
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


#install.packages("zoo")
library(zoo)

#Feb
g1<-Feb1[436:443, 36]
g1

na.approx(g1)


#Mar
g2<-Mar1[1573:1588,36]
g2

na.approx(g2)


#May
g3<-May1[1306:1314, 25]
g3

na.approx(g3)


g4<-May1[1483:1494, 31]
g4

na.approx(g4)

#July
g5<-Jul1[988:995, 24]
g5

na.approx(g5)

#Aug
g6<-Aug1[1398:1406, 26]
g6

na.approx(g6)


#Nov
g7<-Nov1[2098:2105, 14]
g7

na.approx(g7)


g8<-Nov1[2139:2147, 28]
g8

na.approx(g8)



