##plot south and north prob of each tree on different plots
##Eight plots totoal
##One with all all tree's north and south on one plot.
require(dplyr)
require(readxl)
setwd("C:/Users/jianl/Desktop/work/NdSResult/")
sapFluxSum <- read.csv("sapfluxDailySumFrom6-9.csv")
setwd("c:/Users/jianl/Desktop/work/Raw")
layout <- read_excel("ReynoldSAP_Layout2.xlsx") %>% as.data.frame() 
setwd("C:/Users/jianl/Desktop/work/NdSResult/")
#remove null
layout$Notes <- as.character(layout$Notes)
measurementTree <- layout %>%
  filter(`Direction from measurement tree` == 'Measurent tree' & is.na(.data$Notes))
head(measurementTree)
tail(measurementTree)
#total should be 16 data entries, and seems right

#plot every tree's north and south 
i <- 1
while (i < nrow(measurementTree)){
  south <- measurementTree[c(i:(i+1)),] %>% filter(Orientation == "s") %>% as.data.frame()
  north <- measurementTree[c(i:(i+1)),] %>% filter(Orientation == "n") %>% as.data.frame()
  southProbe <- sapFluxSum %>%
    filter(system == south$`Sap System`)%>%
    select(paste("Probe_",south$Sensor,sep=""))
  northProb <- sapFluxSum %>%
    filter(system == north$`Sap System`)%>%
    select(paste("Probe_",north$Sensor,sep=""))
  jpeg(paste("system_",south$`Sap System`,"tree_",south$Tree,"geno_",south$Clone,".jpg",sep=""))
  plot(y = northProb[[1]], x = filter(sapFluxSum,system == south$`Sap System`)$DOY, pch = 1, col = "red")
  points(y = southProbe[[1]], x=filter(sapFluxSum,system == south$`Sap System`)$DOY , pch = 2, col ="blue")
  title(main = paste("system:",south$`Sap System`,"tree:",south$Tree,"geno:",south$Clone))
  abline(lm(northProb[[1]] ~ filter(sapFluxSum,system == south$`Sap System`)$DOY),col = "red")
  abline(lm(southProbe[[1]] ~ filter(sapFluxSum,system == south$`Sap System`)$DOY),col = "blue")
  legend(160,500,legend=c("n","s"),pch=c(1,2),col=c("red","blue"))
  dev.off()
  i <- i + 2
}



#plot all north and south probe on the same plot
  jpeg(paste("All tree's south and prob",".jpg",sep=""))
  i <- 1
  plot(x=c(100:400),,type="n",ylim=c(0,6000),xlim = c(80,300),main="All Tree's S and N probes")
  while( i < nrow(measurementTree)){
    south <- measurementTree[c(i:(i+1)),] %>% filter(Orientation == "s") %>% as.data.frame()
    north <- measurementTree[c(i:(i+1)),] %>% filter(Orientation == "n") %>% as.data.frame()
    southProbe <- sapFluxSum %>%
      filter(system == south$`Sap System`)%>%
      select(paste("Probe_",south$Sensor,sep=""))
    northProb <- sapFluxSum %>%
      filter(system == north$`Sap System`)%>%
      select(paste("Probe_",north$Sensor,sep=""))
  points(y = northProb[[1]], x = filter(sapFluxSum,system == south$`Sap System`)$DOY, pch = 1, col = "red")
  points(y = southProbe[[1]], x=filter(sapFluxSum,system == south$`Sap System`)$DOY , pch = 2, col = "blue")
  
  i <- i + 2
  }
  
  legend(280,5000,legend=c("n","s"),pch=c(1,2),col=c("red","blue"))
  dev.off()


