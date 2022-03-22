setwd("/Users/lamonkey/Desktop/work/Raw/VT\ SAP4\ k\ 2017/")
sapread <- read.csv("VT\ Sap4\ k\ values\ 02-2017.csv")
#check doy
head(sapread)
tail(sapread)

colname <- colnames(sapread)
#drop column
sapread <- subset(sapread,select = -c(2))
colnames(sapread) <- head(colname,-1)
head(sapread)
tail(sapread)
write.csv(sapread,"VT\ Sap4\ k\ values\ 02-2017.csv")

dim(sapread)
