##This script is for plotting the sapwood cros section area to check the result


##plot interpolation result
interpolation <- read.csv("G:/My Drive/Azura/work/Data/Raw/fullArea.csv")

for (col in c(1:77)){
  col<- interpolation[col,]
  title <- paste("system#",col[2],"tree#",col[4])
  plot(y=col[5:326],x=c(1:322),ylab="corss area(mm)",xlab="indivadual day",main=title)
}


## plot section area to check the result for the outer 20mm
area1<-read.csv("G:/My Drive/Azura/work/Data/Raw/site1.csv",head=T)
area2<-read.csv("G:/My Drive/Azura/work/Data/Raw/Cross_Section/syst2.csv",head=T)
area3<-read.csv("G:/My Drive/Azura/work/Data/Raw/Cross_Section/syst3.csv",head=T)
area4<-read.csv("G:/My Drive/Azura/work/Data/Raw/Cross_Section/syst4.csv",head=T)

tmp <- area1$sectionArea[which(area1$increment==20 & area1$tree==3)]
plot(tmp)








