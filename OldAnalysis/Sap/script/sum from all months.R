#code to sum up all prob data between 8:00 today and 8:00 the next day and average VPD and PAR
#code could not run for now because there are missing date within some of the month.
#preprocessing or modification of this code is required 

require(dplyr)
require(readxl)
require(regex)
setwd("/Users/lamonkey/Desktop/work/Raw/")
layout <- read_excel("ReynoldSAP_Layout2.xlsx")


#combind sap reading from may to sep
#system1
setwd("/Users/lamonkey/Desktop/work/probReadings/VT\ SAP1\ k\ 2017/")
#to change selected month change this option
saps <- list.files(path=".",pattern="*.csv")
months.1 <- list(c(1:9),c(11:12))
sap_read.sys1 <- data.frame()

for(sap in saps){
  reader <- read.csv(sap)
  sap_read.sys1 <- rbind(sap_read.sys1,reader)
}
sap_read.sys1$system = 1
head(sap_read.sys1)
tail(sap_read.sys1)
#system2
setwd("/Users/lamonkey/Desktop/work/probReadings/VT\ SAP2\ k\ 2017/")
saps <- list.files(path=".",pattern="*.csv")
months.2 <- list(c(1:9),c(11:12))
sap_read.sys2 <- data.frame()
for(sap in saps){
  reader<-read.csv(sap)
  sap_read.sys2 <- rbind(sap_read.sys2,reader)
}
sap_read.sys2$system = 2
head(sap_read.sys2)
tail(sap_read.sys2)
#system3
setwd("/Users/lamonkey/Desktop/work/probReadings/VT\ SAP3\ k\ 2017/")
saps <- list.files(path=".",pattern="*.csv")
months.3 <- list(c(1:2),c(4:9),c(11:12))
sap_read.sys3 <- data.frame()
#sap_reads <- read.csv(saps[5])
for(sap in saps){
  reader <- read.csv(sap) 
  sap_read.sys3 <- rbind(sap_read.sys3,reader)
}
sap_read.sys3$system = 3
head(sap_read.sys3)
tail(sap_read.sys3)
#system4
setwd("/Users/lamonkey/Desktop/work/probReadings/VT\ SAP4\ k\ 2017/")
months.4 <- list(c(1),c(3:9),c(11:12))
sap_read.sys4 <- data.frame()
saps <- list.files(path=".",pattern="*.csv")
for(sap in saps){
  reader <- read.csv(sap) 
  sap_read.sys4 <- rbind(sap_read.sys4,reader)
}
sap_read.sys4$system = 4
head(sap_read.sys4)
tail(sap_read.sys4)
sap_read.combine <- rbind(sap_read.sys1,sap_read.sys2,sap_read.sys3,sap_read.sys4)
#change date to only month
months <- sap_read.combine$Date %>% 
  lapply(function(x)as.numeric(strsplit(x,split="[/]")[[1]][1]))
sap_read.combine$Date <- months

sap_read.combine$Date <- as.numeric(sap_read.combine$Date)                         

#check sap_read.combine
head(sap_read.combine)
tail(sap_read.combine)

#take colsums for row with same DOY in the same system
sap_read.combine.sum <- data.frame()
for(s in c(1:4)){
  if(s == 1){
    months <- months.1
  }else if(s == 2){
    months <- months.2
  }else if(s == 3){
    months <- months.3
  }else{
    months <- months.4
  }
  for (month in months){
      DOYs <- sap_read.combine %>% 
        filter(system == s)  %>%
        filter(Date >= month[1] & Date <= month[length(month)] ) %>%
        select(DOY) %>%
        unique()
      DOYs <- head(DOYs,-1)
      for(d in DOYs[[1]]){
        #sum for d from 5:00 am forward
        #prob reading takes sum, VPD and PAR take average and DOY doesn't change
        prob.sum <- sap_read.combine %>% 
          filter(system == s) %>%
          filter(DOY == d) %>%
          filter(Time >= 300) %>%
          select(paste("Probe_",c(1:32),sep="")) %>%
          apply(2,function(x)as.numeric(x)) %>%
          colSums() %>%
          as.data.frame() %>%
          t()
        
        # sum for d + 1 from 12:am to 5:00 am
        prob.sum2 <- sap_read.combine %>% 
          filter(system == s) %>%
          filter(DOY == d + 1) %>%
          filter(Time < 300) %>%
          select(paste("Probe_",c(1:32),sep="")) %>%
          apply(2,function(x)as.numeric(x)) %>%
          colSums() %>%
          as.data.frame() %>%
          t() 
        
        #average VPD and PAR
        averageVPDandPAR.1 <- sap_read.combine %>%
          filter(system == s) %>%
          filter(DOY == d + 1) %>%
          filter(Time < 300) %>%
          select(VPD,PAR)%>%
          apply(2,function(x)as.numeric(x)) %>%
          colMeans() %>%
          as.data.frame %>%
          t()
        averageVPDandPAR.2 <- sap_read.combine %>%
          filter(system == s) %>%
          filter(DOY == d) %>%
          filter(Time >= 300) %>%
          select(VPD,PAR)%>%
          apply(2,function(x)as.numeric(x)) %>%
          colMeans() %>%
          as.data.frame %>%
          t()
        averageVPDandPAR <- rbind(averageVPDandPAR.1,averageVPDandPAR.2) %>% colMeans()
        
      
    
  }
    
    # add d + 1 and d togeather
    prob.sum <- rbind(prob.sum,prob.sum2) %>% colSums()
    
    #construct a data frame
    result <- as.data.frame(t(c(system=s,DOY=d,averageVPDandPAR,prob.sum)))
    colnames(result) <- c("system","DOY","VPD","PAR",paste("Probe_",c(1:32),sep=""))
    
    sap_read.combine.sum <- rbind(sap_read.combine.sum,result)
  }
}

#check data 
columnName <- c("system","DOY","VPD","PAR",paste("Probe_",c(1:32),sep=""))
colnames(sap_read.combine.sum) <- columnName # fix columname
head(sap_read.combine.sum)
tail(sap_read.combine.sum)
#plot prob1 to check
#TODO: the result see odd, need to double check
plot(x=sap_read.combine.sum$DOY,y=119 * sap_read.combine.sum$Probe_1 ^ 1.231)

#export result to csv
write.csv(sap_read.combine.sum,"sapfluxDailySumAll.csv")
