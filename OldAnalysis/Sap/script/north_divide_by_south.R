

#select tree labeled with measurment from all system. generate a chart with result of their
# N/S based on different lighting, moisture and geno types.
require(dplyr)
require(readxl)
setwd("c:/Users/jianl/Desktop/work/Raw")
layout <- read_excel("ReynoldSAP_Layout2.xlsx") #import layout


#combind sap reading from may to sep
#system1
lowMoist = 0.15 # the threshold for moisture level
setwd("C:/Users/jianl/Desktop/work/probReadings/VT SAP1 k 2017/")

#to change selected month change this option
months <- c(6:9)
sap_read.sys1 <- data.frame()
saps <- list.files(path=".",pattern="*.csv")

for(month in months){
  reader <- read.csv(saps[month]) 
  sap_read.sys1 <- rbind(sap_read.sys1,reader)
}
sap_read.sys1$system = 1
head(sap_read.sys1)
tail(sap_read.sys1)
#system2
setwd("C:/Users/jianl/Desktop/work/probReadings/VT SAP2 k 2017/")
months <- c(6:9)
sap_read.sys2 <- data.frame()
saps <- list.files(path=".",pattern="*.csv")
#sap_reads <- read.csv(saps[5])
for(month in months){
  reader <- read.csv(saps[month]) 
  sap_read.sys2 <- rbind(sap_read.sys2,reader)
}
sap_read.sys2$system = 2
head(sap_read.sys2)
tail(sap_read.sys2)
#system3
setwd("C:/Users/jianl/Desktop/work/probReadings/VT SAP3 k 2017/")
months <- c(5:8)
sap_read.sys3 <- data.frame()
saps <- list.files(path=".",pattern="*.csv")
#sap_reads <- read.csv(saps[5])
for(month in months){
  reader <- read.csv(saps[month]) 
  sap_read.sys3 <- rbind(sap_read.sys3,reader)
}
sap_read.sys3$system = 3
head(sap_read.sys3)
tail(sap_read.sys3)
#system4
setwd("C:/Users/jianl/Desktop/work/probReadings/VT SAP4 k 2017/")
months <- c(4:7)
sap_read.sys4 <- data.frame()
saps <- list.files(path=".",pattern="*.csv")
for(month in months){
  reader <- read.csv(saps[month]) 
  sap_read.sys4 <- rbind(sap_read.sys4,reader)
}
sap_read.sys4$system = 4
head(sap_read.sys4)
tail(sap_read.sys4)
sap_read.combine <- rbind(sap_read.sys1,sap_read.sys2,sap_read.sys3,sap_read.sys4)
#calculate sap flux for all probe
sap_read.combine[,c(6:37)] <- sap_read.combine %>% 
  select(paste("Probe_",c(1:32),sep="")) %>% 
  apply(2,function(x)as.numeric(x)) %>%
  as.data.frame() %>% 
  apply(2,function(x)119*x^1.231)
#check sap_read.combine
head(sap_read.combine)
tail(sap_read.combine)
#save combined result
write.csv("sap_flux_6_9_combine.csv")
#sum all prob in each system and average VPD and PAR
#take colsums for row with same DOY in the same system
sap_read.combine.sum <- data.frame()
for(s in c(1:4)){
  DOYs <- sap_read.combine %>% 
    filter(system == s)  %>%
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
      filter(DOY == d) %>%
      filter(Time < 300) %>%
      select(paste("Probe_",c(1:32),sep="")) %>%
      apply(2,function(x)as.numeric(x)) %>%
      colSums() %>%
      as.data.frame() %>%
      t() 
    
    #average VPD and PAR
    averageVPDandPAR.1 <- sap_read.combine %>%
      filter(system == s) %>%
      filter(DOY == d) %>%
      filter(Time >= 300) %>%
      select(VPD,PAR)%>%
      apply(2,function(x)as.numeric(x))
     
    averageVPDandPAR.2 <- sap_read.combine %>%
      filter(system == s) %>%
      filter(DOY == d + 1) %>%
      filter(Time < 300) %>%
      select(VPD,PAR)%>%
      apply(2,function(x)as.numeric(x))
   averageVPDandPAR <- rbind(averageVPDandPAR.1,averageVPDandPAR.2) %>% colMeans()
    
    # add data from d + 1 to d togeather
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

plot(x=sap_read.combine.sum$DOY,y=sap_read.combine.sum$Probe_1)

#export result to csv
write.csv(sap_read.combine.sum,"sapfluxDailySumFrom6-9.csv")

#giving sensor unique name 
layout$Sensor <- c(paste(as.character(layout$`Sap System`),"-",as.character(layout$Sensor),sep=""))


#extract all geno type
genoTypes <- layout %>%
  filter(.data$`Sap System` == "1" | `Sap System` == '2' | 'Sap System' == '3' | `Sap System` == '4') %>%
  select(Clone)%>%
  unique()

#check number of tree for each geno types
#TODO genotype c4 missing 2 probs, suppose to be 8, but having 6 instead
g1.sensor <- layout %>% 
  filter(Clone == genoTypes[[1]][1]) %>%
  filter(`Direction from measurement tree` == "Measurent tree" & is.na(.data$Notes))%>%
  select(Sensor)
g1.sensor
g2.sensor <- layout %>% 
  filter(Clone == genoTypes[[1]][2]) %>%
  filter(`Direction from measurement tree` == "Measurent tree" & is.na(.data$Notes))%>%
  select(Sensor)  
g2.sensor
g3.sensor <- layout %>% 
  filter(Clone == genoTypes[[1]][3]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>%
  select(Sensor)
g3.sensor
g4.sensor <- layout %>% 
  filter(Clone == genoTypes[[1]][4]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>%
  select(Sensor)  
g4.sensor
#filter


#filter environmental factor for two geno types
##low density
g1.lowDensity <- layout %>%
  filter(Clone == genoTypes[[1]][1]) %>%
  filter(`Direction from measurement tree` == "Measurent tree" & is.na(.data$Notes))%>% 
  filter(Density == 250)
g1.lowDensity
g2.lowDensity <- layout %>%
  filter(Clone == genoTypes[[1]][2]) %>%
  filter(`Direction from measurement tree` == "Measurent tree" & is.na(.data$Notes))%>% 
  filter(Density == 250)
g2.lowDensity
g3.lowDensity <- layout %>%
  filter(Clone == genoTypes[[1]][3]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>% 
  filter(Density == 250)
g3.lowDensity
g4.lowDensity <- layout %>%
  filter(Clone == genoTypes[[1]][4]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>% 
  filter(Density == 250)
g4.lowDensity
lowDensitys <- list(g1.lowDensity,g2.lowDensity,g3.lowDensity,g4.lowDensity)

##high density
#TODO: g1.hDensity suppose to have 4 probs but only have 2 instead
g1.hDensity <- layout %>%
  filter(Clone == genoTypes[[1]][1]) %>%
  filter(`Direction from measurement tree` == "Measurent tree" & is.na(.data$Notes))%>% 
  filter(Density == 750)
g1.hDensity
g2.hDensity <- layout %>%
  filter(Clone == genoTypes[[1]][2]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>% 
  filter(Density == 750)
g2.hDensity
g3.hDensity <- layout %>%
  filter(Clone == genoTypes[[1]][3]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>% 
  filter(Density == 750)
g3.hDensity
g4.hDensity <- layout %>%
  filter(Clone == genoTypes[[1]][4]) %>%
  filter(`Direction from measurement tree` == "Measurent tree"& is.na(.data$Notes))%>% 
  filter(Density == 750)
g4.hDensity
HighDensitys <- list(g1.hDensity,g2.hDensity,g3.hDensity,g4.hDensity)
## low density + highlight + high moist


#create a data frame to store n/d result
resultChart <- data.frame("system"=character(), "Density" = character(), "Genotype" = character(), "Light" = character(), "Moist" = character(), "SapFlux" = double(), "sd" = double())
detailedResultChart <- data.frame()

## low density + high density 
for (t in c(1:2)){
  if(t == 1){
    typeOfDensity <- HighDensitys
    density <- "high"
  }else{
    typeOfDensity <- lowDensitys
    density <- "low"
  }
  
  ## four genotype within the same density
  for (g in typeOfDensity){
    #low moist + hight moist
    isHmoist = TRUE
    for(m in c(1:2)){
      #light <- "high"
      if(isHmoist){
        moist <- "high"
        result <- sap_read.combine.sum %>%
        filter(system == g$`Sap System`[[1]][1] & VPD >= lowMoist) 
      }else{
        moist <- "low"
        result <- sap_read.combine.sum %>%
        filter(system == g$`Sap System`[[1]][1] & VPD < lowMoist)
      }
      isHmoist = FALSE
      #high light and low light
      isHLight = TRUE
      for (l in c(1:2)){
        if(isHLight){
          result <- result %>%
            arrange(desc(.data$PAR))
          light <- "high"
        }else{
          result <- result %>%
            arrange(.data$PAR)
          light <- "low"
        }
        isHLight <- FALSE
       
          s <- substr(g$Sensor,1,1)
          sensors <- substr(g$Sensor,3,nchar(g$Sensor))
          result <- result %>% 
          select(system,DOY,VPD,PAR,paste("Probe_",sensors,sep="")) %>% #select first 5 without null value
          apply(2,function(x)as.numeric(x))%>%
          as.data.frame() %>%
          na.omit() %>%
          head(5)
          ## apend detailed result chart at here
        geo <- g$Clone[[1]][1]
            
            tmp <- rbind(result$system, result$DOY, result$VPD, result$PAR)
            tmp <- t(tmp)
            tmp <- cbind(tmp,rep(t(colnames(result)[5]),5))
            tmp <- cbind(tmp,rep(t(colnames(result)[6]),5))
            tmp <- cbind(tmp,rep(t(paste("D:",density,"geno:",geo,"Moist:",moist,"Light:",light)),5))
          detailedResultChart <- rbind(detailedResultChart,tmp)
       
        #south probe 
        south <- layout %>% filter(Orientation == "s" & `Sap System` == g$`Sap System`[[1]][1]) %>% 
          filter(Sensor %in% g$Sensor )
        #north probe
        north <- layout %>% filter(Orientation == "n" & `Sap System` == g$`Sap System`[[1]][1]) %>% 
          filter(Sensor %in% g$Sensor )
        #south probe reading
        south.reading <- result %>% 
          select(paste("Probe_",substr(south$Sensor,3,nchar(south$Sensor)),sep="")) 
       #north probe reading
        north.reading <- result %>% 
          select(paste("Probe_",substr(north$Sensor,3,nchar(north$Sensor)),sep="")) 
       
        #get ratio
        nDs <- north.reading / south.reading
        #check if south and north belongs to same tree
        if(south$Tree == north$Tree){
          treeName <- south$Tree
        }else{
          treeName <- "error"
        }
        if(south$`Sap System` == north$`Sap System`){
          system <- south$`Sap System`
        }else{
          system <- -1
        }
        resultChart<- rbind(resultChart,data.frame(treeName,system,density,geo,light,moist,colSums(nDs),sd(t(nDs))))
      }
      
    }
  }
}
##the following code is to get eveyr result filtered under different conditions, light, moist, 
colnames(detailedResultChart) <- c("system","DOY","VPD","PAR","prob1","prob2","condition")
head(detailedResultChart)
tail(detailedResultChart)
write.csv(detailedResultChart,"dates.csv")

rownames(resultChart) <- c(1:nrow(resultChart))
resultChart
#remove col name
write.csv(resultChart,"NdSResultChart.csv")
