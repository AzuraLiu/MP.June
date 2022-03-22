setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")
library(tidyverse)
require(dplyr)
library(GGally)
library(tidyr)
library(ggplot2)

HCJunDay<-read.xlsx("HCJuneDay.xlsx")


sapN<-HCJunDay[,c(1,3,4,17,10,12)]
sapS<-HCJunDay[,c(1,2,16,7,9,13)]



#total
t.test(sapN,sapS)
#not sig

#by day
#transpose
sapN_T <- as.data.frame(t(sapN))
sapS_T <- as.data.frame(t(sapS))

#dist
shapiro_resultsN <- mapply(shapiro.test, x= sapN_T, SIMPLIFY = F)
shapiro_resultsS<-mapply(shapiro.test, x=sapS_T,SIMPLIFY=F)
#normal

#variance
var_results<-mapply(var.test,x=sapN_T,y=sapS_T,SIMPLIFY=F)

#t test
t.test_results <- mapply(t.test, x= sapN_T, y = sapS_T, SIMPLIFY = F)
capture.output(t.test_results, file = "HC6Day.txt")
#mostly sig

#########################

Sreads<- pivot_longer(sapS, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2))%>%
  subset(select = -c(probe1, probe2))

Nreads <- pivot_longer(sapN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2))%>%
  subset(select = -c(probe1, probe2))


NSDay<- merge(Sreads,Nreads, by=c("Tree","Date"))


ggplot(data=NSDay, aes(x=NVals, y=SVals, color=Tree))+
  geom_point() 


#-----------------------
NSplot <- function(SapN, SapS){
  
  Sreads<- pivot_longer(SapS, 2:6, names_to="Tree", values_to="SVals") %>% 
    separate(Tree, c("probe1", "probe2"), "S") %>%
    mutate(Tree = paste(probe1, probe2))%>%
    subset(select = -c(probe1, probe2))
  
  Nreads <- pivot_longer(SapN, 2:6, names_to="Tree", values_to="NVals") %>% 
    separate(Tree, c("probe1", "probe2"), "N") %>%
    mutate(Tree = paste(probe1, probe2))%>%
    subset(select = -c(probe1, probe2))
  
  
 # NSDay<- merge(Sreads,Nreads, by=c("Tree","Date"))
  
  
 # NSplot <- ggplot(data=NSDay, aes(x=NVals, y=SVals, color=Tree))+
 #   geom_point() 
  
 # return(NSplot)
}


NSplot(SapN, SapS)



#########################ANOVA#####################################

anovaDay<-aov(Vals~as.factor(Direction), data=NSDay)

summary(anovaDay)
boxplot(Vals~Direction, main="Sapflow by Direction", xlab="Direction", ylab="SapVals", data=NSDay)




