#This script plot every tree filterd using same condition on the same plot
#There should be total four plot
require(dplyr)
setwd("/Users/jianl/Desktop/work/NdSResult/")
NdS <- read.csv("NdSResultChart.csv")
NdS.hDensity <- NdS[c(1:16),]
NdS.lDensity <- NdS[c(17:32),]
#select Nds after row 4
NdS.afterRow4h <- NdS.hDensity[c(5:16),]
NdS.afterRow4l <- NdS.lDensity[c(5:16),]
maxY <- ceiling(max(NdS$colSums.nDs.))
minY <- floor(min(NdS$colSums.nDs.))
#plot and save every tree in file1 
plotTrees <- function(file1,file2){
  for (i in c(1:4)){
    row <- file1[i,]
    reading <- row$sd.nDs.
    # all tree in file 2 having same density, light and moist
    result <- file2 %>%filter(density == row$density & light == row$light & moist == row$moist  )
    result <- rbind(result,row)
    context <- c()
    jpeg(paste("D_",row$density,"L_",row$light,"M_",row$moist,".jpg",sep=""))
    plot(result$colSums.nDs.,,type="n",ylim=c(minY,maxY),xlim=c(1,10),ylab="N/S",main=paste("D:",row$density,"L:",row$light,"M:",row$moist,sep=""))
    for(k in c(1:4)){
      points(x=k,y=result[k,]$colSums.nDs.,pch=k)
      context <- c(context,paste("tree:",result[k,]$treeName,"sys:",result[k,]$system))
    }
    legend(5,15,legend=context,pch=c(1:4))

    dev.off()
  }
}
plotTrees(NdS.hDensity,NdS.afterRow4h)
plotTrees(NdS.lDensity,NdS.afterRow4l)
