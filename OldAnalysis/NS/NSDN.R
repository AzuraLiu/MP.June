setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")
library(tidyverse)
require(dplyr)
library(GGally)
library(tidyr)


HBJunDay<-read.xlsx("HBJuneDay.xlsx")

sapN<-HBJunDay[,c(1,2,6,9,12,15)]
sapS<-HBJunDay[,c(1,3,7,10,13,16)]

#sapN<-LCJunDay[,c(1,2,7,9,12,15)]
#sapS<-LCJunDay[,c(1,3,6,10,13,16)]

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
t.test_results <- mapply(t.test, x= (sapN_T), y = (sapS_T), SIMPLIFY = F)

capture.output(t.test_results, file = "HB6Day.txt")
############graph
HBSreads<- pivot_longer(sapS, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HBNreads <- pivot_longer(sapN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HBNS<- merge(HBSreads,HBNreads, by=c("Tree","Date"))


HBgraph<-ggplot(data=HBNS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()
HBgraph

#####slope t test#######################################
HBO10<-subset(HBNS, Tree=="HBO 10")
HBO12<-subset(HBNS, Tree=="HBO 12")
HBO14<-subset(HBNS, Tree=="HBO 14")
HBO16<-subset(HBNS, Tree=="HBO 16")
HBO9<-subset(HBNS, Tree=="HBO 9")

HBO10t<-coef(lm(formula=HBO10$SVals~0+HBO10$NVals))
HBO12t<-coef(lm(formula=HBO12$SVals~0+HBO12$NVals))
HBO14t<-coef(lm(formula=HBO14$SVals~0+HBO14$NVals))
HBO16t<-coef(lm(formula=HBO16$SVals~0+HBO16$NVals))
HBO9t<-coef(lm(formula=HBO9$SVals~0+HBO9$NVals))


HBOSlopes<-c(HBO10t,HBO12t,HBO14t,HBO16t,HBO9t)
summary(HBOSlopes)
HBOt<-t.test(HBOSlopes, mu=1,equal=T)
HBOt
boxplot(HBOSlopes, main="HBO N/S Slopes Distribution", xlab="HBO", ylab="Slopes")

#########ANOVA#####################################



setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")
library(tidyverse)
require(dplyr)
library(GGally)

HBJunNite<-read.xlsx("HBJuneNite.xlsx")

sapNN<-HBJunNite[,c(1,2,6,9,12,15)]
sapNS<-HBJunNite[,c(1,3,7,10,13,16)]



################################

sapNN_T <- as.data.frame(t(sapNN))
sapNS_T <- as.data.frame(t(sapNS))
#variance
var_results<-mapply(var.test,x=sapN_T,y=sapS_T,SIMPLIFY=F)

#t test
t.test_resultsN<- mapply(t.test, x= sapNN_T, y = sapNS_T, SIMPLIFY = F,var.equal=T)
capture.output(t.test_resultsN, file = "HB6Nite.txt")

######Graph#######################

HBSreadsN<- pivot_longer(sapNS, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HBNreadsN <- pivot_longer(sapNN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HBNSN<- merge(HBSreadsN,HBNreadsN, by=c("Tree","Date"))


ggplot(data=HBNSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()


################################


HOJunDay<-read.xlsx("HOJuneDay.xlsx")

sapNHO<-na.omit(HOJunDay[,c(1,2,6,8,11,15,17)])
sapSHO<-na.omit(HOJunDay[,c(1,3,7,9,12,14,16)])
######Graph#######################

HOSreads<- pivot_longer(sapSHO, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HONreads <- pivot_longer(sapNHO, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HONS<- merge(HOSreads,HONreads, by=c("Tree","Date"))


ggplot(data=HONS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()


sapNHO_T <-(as.data.frame(t(sapNHO)))
sapSHO_T <- (as.data.frame(t(sapSHO)))


#t test
t.test_resultsHO<- mapply(t.test, x= sapNHO_T, y = sapSHO_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsHO, file = "HO6Day.txt")

#######slope t test#################################
HOO10<-subset(HONS, Tree=="HOO 10")
HOO12<-subset(HONS, Tree=="HOO 12")
HOO14<-subset(HONS, Tree=="HOO 14")
HOO16<-subset(HONS, Tree=="HOO 16")
HOO11<-subset(HONS, Tree=="HOO 11")

HOO10t<-coef(lm(formula=HOO10$SVals~0+HOO10$NVals))
HOO12t<-coef(lm(formula=HOO12$SVals~0+HOO12$NVals))
HOO14t<-coef(lm(formula=HOO14$SVals~0+HOO14$NVals))
HOO16t<-coef(lm(formula=HOO16$SVals~0+HOO16$NVals))
HOO11t<-coef(lm(formula=HOO11$SVals~0+HOO11$NVals))


HOOSlopes<-c(HOO10t,HOO12t,HOO14t,HOO16t,HOO11t)
summary(HOOSlopes)
t.test(HOOSlopes, mu=1,equal=T)

boxplot(HOOSlopes, main="HOO N/S Slopes Distribution", xlab="HOO", ylab="Slopes")

###################################

HOJunNite<-read.xlsx("HOJuneNite.xlsx")

sapNHON<-na.omit(HOJunNite[,c(1,2,6,8,11,15,17)])
sapSHON<-na.omit(HOJunNite[,c(1,3,7,9,12,14,16)])
######Graph#######################

HOSreadsN<- pivot_longer(sapSHON, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HONreadsN <- pivot_longer(sapNHON, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HONSN<- merge(HOSreadsN,HONreadsN, by=c("Tree","Date"))


ggplot(data=HONSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()


sapNHON_T <-(as.data.frame(t(sapNHON)))
sapSHON_T <- (as.data.frame(t(sapSHON)))


#t test
t.test_resultsHON<- mapply(t.test, x= sapNHON_T, y = sapSHON_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsHON, file = "HO6Nite.txt")



################################


HAJunDay<-read.xlsx("HAJuneDay.xlsx")


sapNHA<-na.omit(HAJunDay[,c(1,2,7,8,10,13,16)])
sapSHA<-na.omit(HAJunDay[,c(1,3,6,9,11,14,17)])

######Graph#######################

HASreads<- pivot_longer(sapSHA, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HANreads <- pivot_longer(sapNHA, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HANS<- merge(HASreads,HANreads, by=c("Tree","Date"))


ggplot(data=HANS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNHA_T <-(as.data.frame(t(sapNHA)))
sapSHA_T <- (as.data.frame(t(sapSHA)))
#########################################################
HAO1<-subset(HANS, Tree=="HAO 1")
HAO2<-subset(HANS, Tree=="HAO 2")
HAO4<-subset(HANS, Tree=="HAO 4")
HAO3<-subset(HANS, Tree=="HAO 3")
HAO6<-subset(HANS, Tree=="HAO 6")

HAO1t<-coef(lm(formula=HAO1$SVals~0+HAO1$NVals))
HAO2t<-coef(lm(formula=HAO2$SVals~0+HAO2$NVals))
HAO4t<-coef(lm(formula=HAO4$SVals~0+HAO4$NVals))
HAO3t<-coef(lm(formula=HAO3$SVals~0+HAO3$NVals))
HAO6t<-coef(lm(formula=HAO6$SVals~0+HAO6$NVals))


HAOSlopes<-c(HAO1t,HAO2t,HAO3t,HAO4t,HAO6t)
HAOSlopes
t.test(HAOSlopes, mu=1,equal=T)

boxplot(HAOSlopes, main="HAO N/S Slopes Distribution", xlab="HAO", ylab="Slopes")
################################################################

#t test
t.test_resultsHA<- mapply(t.test, x= sapNHA_T, y = sapSHA_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsHA, file = "HA6Day.txt")

###################################

HAJunNite<-read.xlsx("HAJuneNite.xlsx")

sapNHAN<-na.omit(HAJunNite[,c(1,2,7,8,10,13,16)])
sapSHAN<-na.omit(HAJunNite[,c(1,3,6,9,11,14,17)])


######Graph#######################

HASreadsN<- pivot_longer(sapSHAN, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HANreadsN <- pivot_longer(sapNHAN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HANSN<- merge(HASreadsN,HANreadsN, by=c("Tree","Date"))


ggplot(data=HANSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNHAN_T <-(as.data.frame(t(sapNHAN)))
sapSHAN_T <- (as.data.frame(t(sapSHAN)))


#t test
t.test_resultsHAN<- mapply(t.test, x= sapNHAN_T, y = sapSHAN_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsHAN, file = "HA6Nite.txt")

################################


LAJunDay<-read.xlsx("LAJuneDay.xlsx")


sapNLA<-na.omit(LAJunDay[,c(1,2,6,9,12,15)])
sapSLA<-na.omit(LAJunDay[,c(1,3,7,10,13,16)])

######Graph#######################

LASreads<- pivot_longer(sapSLA, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LANreads <- pivot_longer(sapNLA, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LANS<- merge(LASreads,LANreads, by=c("Tree","Date"))


ggplot(data=LANS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()
###############################################
LAO1<-subset(LANS, Tree=="LAO 1")
LAO2<-subset(LANS, Tree=="LAO 2")
LAO4<-subset(LANS, Tree=="LAO 4")
LAO8<-subset(LANS, Tree=="LAO 8")
LAO6<-subset(LANS, Tree=="LAO 6")

LAO1t<-coef(lm(formula=LAO1$SVals~0+LAO1$NVals))
LAO2t<-coef(lm(formula=LAO2$SVals~0+LAO2$NVals))
LAO4t<-coef(lm(formula=LAO4$SVals~0+LAO4$NVals))
LAO8t<-coef(lm(formula=LAO8$SVals~0+LAO8$NVals))
LAO6t<-coef(lm(formula=LAO6$SVals~0+LAO6$NVals))


LAOSlopes<-c(LAO1t,LAO2t,LAO8t,LAO4t,LAO6t)
LAOSlopes
t.test(LAOSlopes, mu=1,equal=T)

boxplot(LAOSlopes, main="LAO N/S Slopes Distribution", xlab="LAO", ylab="Slopes")
##################################################################

sapNLA_T <-(as.data.frame(t(sapNLA)))
sapSLA_T <- (as.data.frame(t(sapSLA)))


#t test
t.test_resultsLA<- mapply(t.test, x= sapNLA_T, y = sapSLA_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLA, file = "LA6Day.txt")

###################################

LAJunNite<-read.xlsx("LAJuneNite.xlsx")

sapNLAN<-na.omit(LAJunNite[,c(1,2,6,9,12,15)])
sapSLAN<-na.omit(LAJunNite[,c(1,3,7,10,13,16)])

######Graph#######################

LASreadsN<- pivot_longer(sapSLAN, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LANreadsN <- pivot_longer(sapNLAN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LANSN<- merge(LASreadsN,LANreadsN, by=c("Tree","Date"))


ggplot(data=LANSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNLAN_T <-(as.data.frame(t(sapNLAN)))
sapSLAN_T <- (as.data.frame(t(sapSLAN)))


#t test
t.test_resultsLAN<- mapply(t.test, x= sapNLAN_T, y = sapSLAN_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLAN, file = "LA6Nite.txt")

################################


LBJunDay<-read.xlsx("LBJuneDay.xlsx")


sapNLB<-na.omit(LBJunDay[,c(1,2,6,8,11,14)])
sapSLB<-na.omit(LBJunDay[,c(1,3,7,9,12,15)])


######Graph#######################

LBSreads<- pivot_longer(sapSLB, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LBNreads <- pivot_longer(sapNLB, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LBNS<- merge(LBSreads,LBNreads, by=c("Tree","Date"))


ggplot(data=LBNS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNLB_T <-(as.data.frame(t(sapNLB)))
sapSLB_T <- (as.data.frame(t(sapSLB)))
##########################################################
LBO11<-subset(LBNS, Tree=="LBO 11")
LBO12<-subset(LBNS, Tree=="LBO 12")
LBO14<-subset(LBNS, Tree=="LBO 14")
LBO16<-subset(LBNS, Tree=="LBO 16")

LBO11t<-coef(lm(formula=LBO11$SVals~0+LBO11$NVals))
LBO12t<-coef(lm(formula=LBO12$SVals~0+LBO12$NVals))
LBO14t<-coef(lm(formula=LBO14$SVals~0+LBO14$NVals))
LBO16t<-coef(lm(formula=LBO16$SVals~0+LBO16$NVals))


LBOSlopes<-c(LBO11t,LBO12t,LBO14t,LBO16t)
LBOSlopes
t.test(LBOSlopes, mu=1,equal=T)

boxplot(LBOSlopes, main="LBO N/S Slopes Distribution", xlab="LBO", ylab="Slopes")



#########################################################

#t test
t.test_resultsLB<- mapply(t.test, x= sapNLB_T, y = sapSLB_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLB, file = "LB6Day.txt")

###################################

LBJunNite<-read.xlsx("LBJuneNite.xlsx")

sapNLBN<-na.omit(LBJunNite[,c(1,2,6,8,11,14)])
sapSLBN<-na.omit(LBJunNite[,c(1,3,7,9,12,15)])

######Graph#######################

LBSreadsN<- pivot_longer(sapSLBN, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LBNreadsN <- pivot_longer(sapNLBN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))

LBNSN<- merge(LBSreadsN,LBNreadsN, by=c("Tree","Date"))


ggplot(data=LBNSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNLBN_T <-(as.data.frame(t(sapNLBN)))
sapSLBN_T <- (as.data.frame(t(sapSLBN)))


#t test
t.test_resultsLBN<- mapply(t.test, x= sapNLBN_T, y = sapSLBN_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLBN, file = "LB6Nite.txt")



################################


LOJunDay<-read.xlsx("LOJuneDay.xlsx")


sapNLO<-na.omit(LOJunDay[,c(1,2,6,8,10,12,14)])
sapSLO<-na.omit(LOJunDay[,c(1,3,7,9,11,13,15)])

######Graph#######################

LOSreads<- pivot_longer(sapSLO, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LONreads <- pivot_longer(sapNLO, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LONS<- merge(LOSreads,LONreads, by=c("Tree","Date"))


ggplot(data=LONS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()
###################################################
LOO10<-subset(LONS, Tree=="LOO 10")
LOO11<-subset(LONS, Tree=="LOO 11")
LOO15<-subset(LONS, Tree=="LOO 15")
LOO14<-subset(LONS, Tree=="LOO 14")
LOO17<-subset(LONS, Tree=="LOO 17")

LOO10t<-coef(lm(formula=LOO10$SVals~0+LOO10$NVals))
LOO11t<-coef(lm(formula=LOO11$SVals~0+LOO11$NVals))
LOO15t<-coef(lm(formula=LOO15$SVals~0+LOO15$NVals))
LOO14t<-coef(lm(formula=LOO14$SVals~0+LOO14$NVals))
LOO17t<-coef(lm(formula=LOO17$SVals~0+LOO17$NVals))


LOOSlopes<-c(LOO10t,LOO11t,LOO15t,LOO14t,LOO17t)
LOOSlopes
t.test(LOOSlopes, mu=1,equal=T)

boxplot(LOOSlopes, main="LOO N/S Slopes Distribution", xlab="LOO", ylab="Slopes")
##############################################################

sapNLO_T <-(as.data.frame(t(sapNLO)))
sapSLO_T <- (as.data.frame(t(sapSLO)))


#t test
t.test_resultsLO<- mapply(t.test, x= sapNLO_T, y = sapSLO_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLO, file = "LO6Day.txt")

###################################

LOJunNite<-read.xlsx("LOJuneNite.xlsx")

sapNLON<-na.omit(LOJunNite[,c(1,2,6,8,10,12,14)])
sapSLON<-na.omit(LOJunNite[,c(1,3,7,9,11,13,15)])

######Graph#######################

LOSreadsN<- pivot_longer(sapSLON, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LONreadsN <- pivot_longer(sapNLON, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LONSN<- merge(LOSreadsN,LONreadsN, by=c("Tree","Date"))


ggplot(data=LONSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()


sapNLON_T <-(as.data.frame(t(sapNLON)))
sapSLON_T <- (as.data.frame(t(sapSLON)))


#t test
t.test_resultsLON<- mapply(t.test, x= sapNLON_T, y = sapSLON_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLON, file = "LO6Nite.txt")


################################


LCJunDay<-read.xlsx("LCJuneDay.xlsx")


sapNLC<-na.omit(LCJunDay[,c(1,2,6,9,12,15)])
sapSLC<-na.omit(LCJunDay[,c(1,3,7,10,13,16)])

######Graph#######################

LCSreads<- pivot_longer(sapSLC, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LCNreads <- pivot_longer(sapNLC, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LCNS<- merge(LCSreads,LCNreads, by=c("Tree","Date"))


ggplot(data=LCNS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

##################################################
LCO1<-subset(LCNS, Tree=="LCO 1")
LCO4<-subset(LCNS, Tree=="LCO 4")
LCO6<-subset(LCNS, Tree=="LCO 6")
LCO8<-subset(LCNS, Tree=="LCO 8")

LCO1t<-coef(lm(formula=LCO1$SVals~0+LCO1$NVals))
LCO4t<-coef(lm(formula=LCO4$SVals~0+LCO4$NVals))
LCO6t<-coef(lm(formula=LCO6$SVals~0+LCO6$NVals))
LCO8t<-coef(lm(formula=LCO8$SVals~0+LCO8$NVals))


LCOSlopes<-c(LCO1t,LCO4t,LCO6t,LCO8t)
LCOSlopes
t.test(LCOSlopes, mu=1,equal=T)

boxplot(LCOSlopes, main="LCO N/S Slopes Distribution", xlab="LCO", ylab="Slopes")


##################################################
sapNLC_T <-(as.data.frame(t(sapNLC)))
sapSLC_T <- (as.data.frame(t(sapSLC)))


#t test
t.test_resultsLC<- mapply(t.test, x= sapNLC_T, y = sapSLC_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLC, file = "LC6Day.txt")

###################################

LCJunNite<-read.xlsx("LCJuneNite.xlsx")

sapNLCN<-na.omit(LCJunNite[,c(1,2,6,9,12,15)])
sapSLCN<-na.omit(LCJunNite[,c(1,3,7,10,13,16)])

######Graph#######################

LCSreadsN<- pivot_longer(sapSLCN, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

LCNreadsN <- pivot_longer(sapNLCN, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


LCNSN<- merge(LCSreadsN,LCNreadsN, by=c("Tree","Date"))


ggplot(data=LCNSN, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()

sapNLCN_T <-(as.data.frame(t(sapNLCN)))
sapSLCN_T <- (as.data.frame(t(sapSLCN)))


#t test
t.test_resultsLCN<- mapply(t.test, x= sapNLCN_T, y = sapSLCN_T, SIMPLIFY = F,var.equal=T)

capture.output(t.test_resultsLON, file = "LC6Nite.txt")



HCJunDay<-read.xlsx("HCJuneDay.xlsx")


sapNHC<-na.omit(HCJunDay[,c(1,3,4,10,12,17)])
sapSHC<-na.omit(HCJunDay[,c(1,2,7,9,13,16)])

######Graph#######################

HCSreads<- pivot_longer(sapSHC, 2:6, names_to="Tree", values_to="SVals") %>% 
  separate(Tree, c("probe1", "probe2"), "S") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction = "S") %>%
  subset(select = -c(probe1, probe2))

HCNreads <- pivot_longer(sapNHC, 2:6, names_to="Tree", values_to="NVals") %>% 
  separate(Tree, c("probe1", "probe2"), "N") %>%
  mutate(Tree = paste(probe1, probe2),
         Direction="N") %>%
  subset(select = -c(probe1, probe2))


HCNS<- merge(HCSreads,HCNreads, by=c("Tree","Date"))


ggplot(data=HCNS, aes(x=NVals, y=SVals, color=Tree))+
  geom_point()
HCO1<-subset(HCNS, Tree=="HCO 1")
HCO2<-subset(HCNS, Tree=="HCO 2")
HCO4<-subset(HCNS, Tree=="HCO 4")
HCO6<-subset(HCNS, Tree=="HCO 6")
HCO8<-subset(HCNS, Tree=="HCO 8")

HCO1t<-coef(lm(formula=HCO1$SVals~0+HCO1$NVals))
HCO2t<-coef(lm(formula=HCO2$SVals~0+HCO2$NVals))
HCO4t<-coef(lm(formula=HCO4$SVals~0+HCO4$NVals))
HCO6t<-coef(lm(formula=HCO6$SVals~0+HCO6$NVals))
HCO8t<-coef(lm(formula=HCO8$SVals~0+HCO8$NVals))


HCOSlopes<-c(HCO1t,HCO2t,HCO4t,HCO6t,HCO8t)
HCOSlopes
t.test(HCOSlopes, mu=1,equal=T)

boxplot(HCOSlopes, main="HCO N/S Slopes Distribution", xlab="HCO", ylab="Slopes")

