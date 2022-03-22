
# title: "sapflux"
# comment: "this program calculate sap flux for every tree in the system and plot the result for every week"
# author: "Azura Liu"
# date: "10/29/2020"
# edited: "12/8/2020"
# edited: "12/26/2020: edited code so that can apply to all four system"
# output: every trees' weekly sapflux plot in folders with auto generated described names 


#Step 1: calculating sapflow & plot in weekly resolution

require("ggplot2")
require("dplyr")
require("stringr")
require(doParallel)
require(foreach)
require(paprallel)
require(grDevices)
setwd("/Users/lamonkey/Desktop/work/workplace/")
#import layout from all four systems
layout1 <- read.csv("layout1.csv",fileEncoding="UTF-16LE") 
layout2 <- read.csv("layout2.csv",fileEncoding="UTF-16LE")
layout3 <- read.csv("layout3.csv",fileEncoding="UTF-16LE")
layout4 <- read.csv("layout4.csv",fileEncoding="UTF-16LE")
ncores <- detectCores()


#plot every sensor's sap flux reading
#file, list of files to plot
#foldernanme, folder name to save plots
plotBySensor <- function(file,foldername,layout){
  
  sap_reading <- read.csv(file,head=T)
  # sap_reading[paste("Probe_",c(1:32),sep="")] <- (sap_reading %>% 
  #           select(paste("Probe_",c(1:32),sep="")) %>% 
  #           apply(2,function(x)as.numeric(x)))
 
  #take the max DOY out of the data
  num_of_day <- max(sap_reading$DOY) - min(sap_reading$DOY)
  ceiling(num_of_day/7)
  weeks <- c(1:ceiling(num_of_day/7))
  probsName <- sap_reading %>% colnames()
  probsName <- str_subset(probsName,"Probe") # extract the probe name
  #plot each prob for every week
  plot.list<-foreach (p = 1:32, .packages=c("ggplot2","dplyr","stringr")) %do%{
    prob <- probsName[p]
    probsName <- sap_reading %>% colnames()
    probsName <- str_subset(probsName,"Probe")
    treeid <- layout$Tree[which(layout$Sensor == gsub("[a-z A-Z _]","",prob))] #get tree name
    #plot with resolution of one week
    for(week in weeks){
      days_floor <- 1*week + min(sap_reading$DOY)-1
      days_ceil <- 7*week + min(sap_reading$DOY)-1
      data <- sap_reading %>% filter(DOY >= days_floor, DOY <= days_ceil)
      time <- c(1:nrow(data))
      probReading <- data[[prob]]
      #EQUATION：calc sapflow
      probReading <- as.numeric(probReading)#convert all to numeric value
      plot_data<- data.frame("time" = time, "flux" = 119 * (probReading^1.231))
      ggplot(plot_data,aes(x = time, y = flux,color="flux")) + geom_point(shape=1,show.legend = T) + labs(x="Time every 15 min",y="Flux") + labs(title=paste("tree#",treeid,prob,"week#",week,sep=""))
      if (!dir.exists(foldername)){
        dir.create(foldername)
      }
      print(paste("saving tree#",treeid,"week#",week, "prob#",prob))
      ggsave(paste(foldername,"/",paste("tree#",treeid,"week#",week,prob,".jpg",sep=""),sep=""),scale=1.5) # this line to save the plot
      
    }
  }
}
registerDoParallel(ncores) #registrate cpu for parallel 
getDoParWorkers()
# import prob reading and generate plot
dirs <- list.dirs(path=".",recursive=FALSE)
for (dir in c(1:4)){ #all directory
  if(dir == 1){
    layout <- layout1
  }else if(dir == 2){
    layout <- layout2
  }
  else if(dir == 3){
    layout <- layout3
  }
  else{
    layout <- layout4
  }
  print(paste("Proccessing system",dir))
  setwd("/Users/lamonkey/Desktop/work/workplace/")
  setwd(paste("",dirs[dir],sep=""))
  saps <- list.files(".",pattern="*.csv") #csv file from each directory
  #saps <- saps[-9]
  for (file in saps){
    #file <- saps[11]
    print(paste("processing", file))
    filename <- (gsub("[A-Z a-z .]","",file))
    plotBySensor(file,filename,layout)
  }
  
}



