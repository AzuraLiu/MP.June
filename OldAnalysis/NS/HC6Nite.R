setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")
library(tidyverse)
require(dplyr)
library(GGally)

HCJunNite<-read.xlsx("HCJuneNite.xlsx")

sapNN<-HCJunNite[,c(3,4,17,10,12)]
sapNS<-HCJunNite[,c(2,16,7,9,13)]




################################

sapNN_T <- as.data.frame(t(sapNN))
sapNS_T <- as.data.frame(t(sapNS))
#variance
var_results<-mapply(var.test,x=sapN_T,y=sapS_T,SIMPLIFY=F)

#t test
t.test_resultsN<- mapply(t.test, x= sapNN_T, y = sapNS_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsN, file = "HC6Nite.txt")

#not sig
##################################################
#graph##########################
NSreads<- pivot_longer(sapNS, 2:6, names_to="Tree", values_to="Vals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

NNreads <- pivot_longer(sapNN, 2:6, names_to="Tree", values_to="Vals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


NSNite<-rbind(NSreads,NNreads)


NSNite<-cbind(SreadsN,NreadsN)
NSNite <- NSNite[,-4]



ggplot(data=NSNite, aes(x=NVals, y=SVals, color=Probes))+
  geom_point()


#################ANOVA##################
anovaNite<-aov(Vals~as.factor(Direction), data=NSNite)

summary(anovaNite)
boxplot(Vals~Direction, main="Sapflow by Direction", xlab="Direction", ylab="SapVals", data=NSNite)





