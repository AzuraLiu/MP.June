# Calculate sap flow (Et, transpiration) as the product 
# of sap flux and the sapwood cross-sectional area of 
# the tree at the location of the probe. First, average 
# the 15-minute sap flux data for the outer two probes 
# and then multiply by the outer 20 mm cross-sectional area.
# This will give Et in g/s (g/m2/s *m2=g/s) for the out 20mm of sapwood. 

require(dplyr)
require(readxl)

##import sap flow
setwd("/Users/lamonkey/Desktop/work/Data/Raw/VT\ SAP1\ k\ 2017/")
sap_reading <- read.csv("VT Sap1 k values 01-2017.csv")
#calculate sapflux using Fd = 119*k^1.231
sap_flux <- sap_reading
sap_flux[,c(6:37)] <- apply(sap_flux[,c(6:37)],2,function(x) 119*x^1.231)

##import ReyboldSAP_layout2.xlsx
setwd("/Users/lamonkey/Desktop/work/Data/Raw/")
layout <- as.data.frame(read_excel("ReynoldSAP_Layout2.xlsx"))
#filter prob in system 1 and deep
deep_prob <- layout %>% filter(.data$Notes == "deep" & .data$`Sap System`=='1')
deep_prob <- as.numeric(deep_prob$Sensor)
#remove deep sensor from sap_flux
sap_flux <- subset(sap_flux,select=-c(5+deep_prob))
#average 15-minute sap flux data for the outer two probes
averageResult <- data.frame(character(),double(),integer(),character())

layout.system1 <- layout %>% filter(.data$`Sap System` == "1")
for (tree in unique(layout.system1$Tree)){
  sensorsOfTree <- layout.system1 %>% filter(.data$Tree == tree)
  sensors <-sapply(sensorsOfTree$Sensor,function(x) paste("Probe_",x,sep=""))
  sum <- 0.0
  count <- 0
  for(sensor in sensors){
    count <- count + 1
    sum <- sum + sap_reading[[sensor]]
  }
  if(count < 2){
    print(paste(tree,"have less then 2 probs"))
  }else if (count > 2){
    print(paste(tree,"have more than 2 probs"))
  }
  average <- sum / count
  result<-cbind((rep(tree,length(average))), (average),sap_reading$DOY,sap_reading$Date)
  averageResult<-rbind(averageResult,result)
}
colnames(averageResult) <- c("treeId","averageSapFlux","DOY","Date")
#TODO: plot to check result


#import the outer most 20mm cross-sectional
setwd("/Users/lamonkey/Desktop/work/Data/Raw/")
daily_area <- read.csv("site1.csv")
daily_area.outmostLayer<- daily_area %>% filter(.data$increment==20)
daily_area.outmostLayer$tree <- as.numeric(daily_area.outmostLayer$tree)
##calculate sapflux * outer layer 
# ignoring sapflux before 1.13 and after 12.1 (exclusive) because cross section areas in 
# that period are missing
month = as.integer(unique(averageResult$Month))

if(month == 1){#if month is 1 take from 13-31
  days <- c(13:31)
}else if(month == 12){ #if month is 12 take only 1
  days <- c(334) #TODO: Debug the date, the last date should be 335
}else{# the all the days in that month
  days <- as.numeric(strftime(strptime(averageResult$Date,"%m/%d/%y"),format="%j")) %>% unique()
  
}
sap_flow <- averageResult
for (day in days){
  trees <- sap_flow %>% filter(as.numeric(strftime(strptime(.data$Date,"%m/%d/%y"),format="%j")) == day)
  trees <- as.integer(unique(trees$treeId))
  for (t in trees){
    area <- daily_area.outmostLayer %>% 
      filter(.data$DOY == day & .data$tree == t ) %>% 
      select(sectionArea) 
    if(nrow(area)==0){
      area <-0.0
    }else if(nrow(area)!=1){#TODO: some tree has more than one?
      print(paste("problem with tree",t))
      area <- area[[1]][1]
    }
    sap_flow$averageSapFlux <- as.numeric(sap_flow$averageSapFlux)
    sap_flow %>%
      filter(as.numeric(strftime(strptime(.data$Date,"%m/%d/%y"),format="%j")) == day & .data$treeId == tree)%>%
      select(averageSapFlux)%>%
      mutate(averageSapFlux = averageSapFlux * area)
  }
  #treesOutSectionAreaWithSameDay <- (daily_area.outmostLayer %>% filter(.data$DOY == day))
  
} 
write.csv(sap_flow,"sap_flow.csv")



