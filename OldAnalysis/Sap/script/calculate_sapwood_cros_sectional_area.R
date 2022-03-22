# 1.calculating and save interpolation on diameter
# 2.calculate and save the sap wood area for entire stem
# 3.calculate and save sap wood area with 20mm increment.


require("readxl")
require("tm")
require('ggplot2')

#set working directory
setwd("/Users/lamonkey/Desktop/work/Data/Raw/")
#import file
rds<-read_excel("Reynolds Diameters.xlsx")
#convert to data frame
rds<-as.data.frame(rds)
#extract system number from the system-tree
system<-(substring(rds$`System-Tree`, 1,1))
#extract tree id from system-tree
tree <- (substring(rds$`System-Tree`,3,))
#get col with high prob
col <- which(grepl("high",tree))
#uniform the format by removing the "high" tag
tree[col] <- removeWords(tree[col],"high")
#creating label for trees
label <- rep("",length(tree))
label[col]<-"high"

#extract diameter from the table from col 2 to col 15
diameter.col<-c(2:15)
diameter.data.x <- colnames(rds[diameter.col])
diameter.data.x <- as.numeric(diameter.data.x) # convert to number for plotting graph
diameter.data.y <- rds[,diameter.col] # diameter data from col 2 to col 15

length=43070-42748
##plot all trees' diameter and the result of linear interpolation
#TODO: save plot for later analysis Analysis
for(i in c(1:17)){
  plot(diameter.data.x,diameter.data.y[i,])
  plot(approx(diameter.data.x,diameter.data.y[i,],method='linear',n=length))
}

## doing interpolation 
diameter.data.interpolated <- t(rep(NA,length))
diameter.data.interpolated <- t(approx(diameter.data.x,diameter.data.y[1,],method='linear',n=length)$y) #using linear interpolation
diameter.data.interpolated <- as.data.frame(t(rep(NA,length)))
interpolationResult <- apply(diameter.data.y,1,function(x) (approx(diameter.data.x,x,n=length,method="linear")$y))
interpolationResult <- t(interpolationResult)
interpolationResult <- as.data.frame(interpolationResult)
colnames(interpolationResult) <- t(approx(diameter.data.x,diameter.data.y[1,],method='linear',n=length)$x)
length(system)
length(tree)
nrow(interpolationResult)
data <- data.frame("system" = system, "label" = label, "tree" = tree, interpolationResult) 
write.csv(data,file="daily_cross_section_radios.csv")
#TODO: only have 322 result how to make up the result for rest of the year

## organize
# data.system.1 <- data[which(data$system==1),]
# data.system.2 <- data[which(data$system==2),]
# data.system.3 <- data[which(data$system==3),]
# data.system.4 <- data[which(data$system==4),]
datas <- list(data,system,1,data,system,2,data,system,3,data,system,4)
systems <- c(1:4)

##calculate area for entire stem
entire_stem_area <- data
entire_stem_area[,c(4:325)]<-apply(data[,c(4:325)],2,function(x) (x/2)^2*pi)
write.csv(entire_stem_area,file="daily_stem_area.csv")

##calculating sap area with 20mm increment 
calculate_sapwoodAre<-function(d){
 #TODO: figure out whats the unit. 
  r <- d*10/2 #convert to mm
  r <- r - 15 #bark
  iteration <- ceiling(r/20) #round up
  areas <- matrix(nrow=2)
  i <- 0;
  while(r >= 20){
    area <- r^2*pi - (r-20)^2*pi
    i <- i + 1
    areas<-cbind(areas,c(area,i*20))
    r <- r-20
  }
 # areas<-cbind(areas,c(i*20 + r,r^2*pi))
   areas<-cbind(areas,c(i*20 + r,r))
  
  return(areas)
}

#result <- matrix(ncol=3)
#finalresult <- matrix(ncol=4)
for(system in systems){
  result <- matrix(ncol=3)
  finalresult <- matrix(ncol=5)
  data.system <- data[which(data$system==system),]
  
  for( tree in data.system$tree){
   for(days in (which(data.system$tree == tree))){
     DOY <- 13
     for(day in (data.system[days,c(4:325)] )){
       radius <- day[[1]]
      # result <- matrix(ncol=3)
       result <- calculate_sapwoodAre(radius)[,-1]
       #result <- c(result,tree)
       if(is.null(ncol(result))){
         result <- c(result,(tree)) #add tree number
       }else{
       result <- rbind(result,rep(tree,ncol(result))) #add tree number
       }
       #result <- c(result,1)
       if(is.null(ncol(result))){
         result <- c(result,rep(tree)) #add tree number
       }else{
       result <- rbind(result,rep(system,ncol(result))) #add system
       #result <- t(result)
       }
       if(is.null(ncol(result))){ #add DOY
         result <- c(result,rep(DOY,tree))
       }else{
         result <- rbind(result,rep(DOY,ncol(result)))
         result <- t(result)
       }
       finalresult <- rbind(finalresult,result)
       DOY <- DOY + 1
       print(DOY)
     }
   }
  }
  finalresult<-finalresult[-1,]
  finalresult<-as.data.frame(finalresult)
  colnames(finalresult)<-c("sectionArea","increment","tree","system","DOY")
  write.csv(finalresult,paste("site",system,".csv",sep=""))
}
