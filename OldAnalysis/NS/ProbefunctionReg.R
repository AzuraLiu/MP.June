setwd("C:/Users/liux3/Desktop/Research/Data/Raw/NS")
library("openxlsx")

HAJunDay<-read.xlsx("HAJuneDay.xlsx")
plot(x=HAJunDay$HAON4, y=HAJunDay$HAON6)
plot(x=HAJunDay$HAON4, y=HAJunDay$HAON1)
plot(x=HAJunDay$HAON4, y=HAJunDay$HAON2)
plot(x=HAJunDay$HAON4, y=HAJunDay$HAON3)

plot(x=HAJunDay$HAOS4, y=HAJunDay$HAOS1)
plot(x=HAJunDay$HAOS4, y=HAJunDay$HAOS2)
plot(x=HAJunDay$HAOS4, y=HAJunDay$HAOS3)
plot(x=HAJunDay$HAOS4, y=HAJunDay$HAOS6)

HAJunNight<-read.xlsx("HAJuneNite.xlsx")
plot(x=HAJunNight$HAON4, y=HAJunNight$HAON1)
plot(x=HAJunNight$HAON4, y=HAJunNight$HAON2)
plot(x=HAJunNight$HAON4, y=HAJunNight$HAON3)
plot(x=HAJunNight$HAON4, y=HAJunNight$HAON6)

plot(x=HAJunNight$HAOS4, y=HAJunNight$HAOS1)
plot(x=HAJunNight$HAOS4, y=HAJunNight$HAOS2)
plot(x=HAJunNight$HAOS4, y=HAJunNight$HAOS3)
plot(x=HAJunNight$HAOS4, y=HAJunNight$HAOS6)

HBJunDay<-read.xlsx("HBJuneDay.xlsx")
plot(x=HBJunDay$HBON10, y=HBJunDay$HBON9)
plot(x=HBJunDay$HBON10, y=HBJunDay$HBON12)
plot(x=HBJunDay$HBON10, y=HBJunDay$HBON14)
plot(x=HBJunDay$HBON10, y=HBJunDay$HBON16)

plot(x=HBJunDay$HBOS10, y=HBJunDay$HBOS9)
plot(x=HBJunDay$HBOS10, y=HBJunDay$HBOS12)
plot(x=HBJunDay$HBOS10, y=HBJunDay$HBOS14)
plot(x=HBJunDay$HBOS10, y=HBJunDay$HBOS16)


HBJunNight<-read.xlsx("HBJuneNite.xlsx")
plot(x=HBJunNight$HBON9, y=HBJunNight$HBON10)
plot(x=HBJunNight$HBON9, y=HBJunNight$HBON12)
plot(x=HBJunNight$HBON9, y=HBJunNight$HBON14)
plot(x=HBJunNight$HBON9, y=HBJunNight$HBON16)


plot(x=HBJunNight$HBOS9, y=HBJunNight$HBOS10)
plot(x=HBJunNight$HBOS9, y=HBJunNight$HBOS12)
plot(x=HBJunNight$HBOS9, y=HBJunNight$HBOS14)
plot(x=HBJunNight$HBOS9, y=HBJunNight$HBOS16)

HCJunDay<-read.xlsx("HCJuneDay.xlsx")
plot(x=HCJunDay$HCON4, y=HCJunDay$HCON1)
plot(x=HCJunDay$HCON4, y=HCJunDay$HCON2)
plot(x=HCJunDay$HCON4, y=HCJunDay$HCON6)
plot(x=HCJunDay$HCON4, y=HCJunDay$HCON8)

plot(x=HCJunDay$HCOS4, y=HCJunDay$HCOS1)

plot(x=HCJunDay$HCOS4, y=HCJunDay$HCOS2)
plot(x=HCJunDay$HCOS4, y=HCJunDay$HCOS6)
plot(x=HCJunDay$HCOS4, y=HCJunDay$HCOS8)

HCJunNight<-read.xlsx("HCJuneNite.xlsx")
plot(x=HAJunNight$HAON4, y=HCJunNight$HCON1)
plot(x=HAJunNight$HAON4, y=HCJunNight$HCON2)
plot(x=HAJunNight$HAON4, y=HCJunNight$HCON4)
plot(x=HAJunNight$HAON4, y=HCJunNight$HCON6)
plot(x=HAJunNight$HAON4, y=HCJunNight$HCON8)

plot(x=HBJunNight$HBOS16, y=HCJunNight$HCOS1)
plot(x=HBJunNight$HBOS16, y=HCJunNight$HCOS2)
plot(x=HBJunNight$HBOS16, y=HCJunNight$HCOS6)
plot(x=HBJunNight$HBOS16, y=HCJunNight$HCOS8)
plot(x=HBJunNight$HBOS16, y=HCJunNight$HCOS4)

HOJunDay<-read.xlsx("HOJuneDay.xlsx")

plot(x=HCJunDay$HCON4, y=HOJunDay$HOON10)
plot(x=HCJunDay$HCON4, y=HOJunDay$HOON11)

plot(x=HCJunDay$HCON4, y=HOJunDay$HOON12)
plot(x=HCJunDay$HCON4, y=HOJunDay$HOON14)
plot(x=HCJunDay$HCON4, y=HOJunDay$HOON16)

plot(x=HCJunDay$HCOS4, y=HOJunDay$HOOS10)
plot(x=HCJunDay$HCOS4, y=HOJunDay$HOOS11)
plot(x=HCJunDay$HCOS4, y=HOJunDay$HOOS12)
plot(x=HCJunDay$HCOS4, y=HOJunDay$HOOS14)
plot(x=HCJunDay$HCOS4, y=HOJunDay$HOOS16)

HOJunNight<-read.xlsx("HOJuneNite.xlsx")
plot(x=HAJunNight$HAON4, y=HOJunNight$HOON10)
plot(x=HAJunNight$HAON4, y=HOJunNight$HOON11)
plot(x=HAJunNight$HAON4, y=HOJunNight$HOON12)
plot(x=HAJunNight$HAON4, y=HOJunNight$HOON14)
plot(x=HAJunNight$HAON4, y=HOJunNight$HOON16)

plot(x=HBJunNight$HBOS16, y=HOJunNight$HOON10)
plot(x=HBJunNight$HBOS16, y=HOJunNight$HOON11)
plot(x=HBJunNight$HBOS16, y=HOJunNight$HOON12)
plot(x=HBJunNight$HBOS16, y=HOJunNight$HOON14)
plot(x=HBJunNight$HBOS16, y=HOJunNight$HOON16)

LOJunDay<-read.xlsx("LOJuneDay.xlsx")
plot(x=LOJunDay$LOON14, y=LOJunDay$LOON10)
plot(x=LOJunDay$LOON14, y=LOJunDay$LOON11)
plot(x=LOJunDay$LOON14, y=LOJunDay$LOON15)
plot(x=LOJunDay$LOON14, y=LOJunDay$LOON17)


plot(x=LOJunDay$LOOS14, y=LOJunDay$LOOS10)
plot(x=LOJunDay$LOOS14, y=LOJunDay$LOOS11)
plot(x=LOJunDay$LOOS14, y=LOJunDay$LOOS15)
plot(x=LOJunDay$LOOS14, y=LOJunDay$LOOS17)

LOJunNight<-read.xlsx("LOJuneNite.xlsx")
plot(x=LOJunNight$LOON14, y=LOJunNight$LOON10)
plot(x=LOJunNight$LOON14, y=LOJunNight$LOON11)
plot(x=LOJunNight$LOON14, y=LOJunNight$LOON15)
plot(x=LOJunNight$LOON14, y=LOJunNight$LOON17)

plot(x=LOJunNight$LOOS14, y=LOJunNight$LOOS10)

plot(x=LOJunNight$LOOS14, y=LOJunNight$LOOS11)
plot(x=LOJunNight$LOOS14, y=LOJunNight$LOOS15)
plot(x=LOJunNight$LOOS14, y=LOJunNight$LOOS17)


LCJunDay<-read.xlsx("LCJuneDay.xlsx")
plot(x=LOJunDay$LOON14, y=LCJunDay$LCON1)
plot(x=LOJunDay$LOON14, y=LCJunDay$LCON4)
plot(x=LOJunDay$LOON14, y=LCJunDay$LCON6)
plot(x=LOJunDay$LOON14, y=LCJunDay$LCON8)

plot(x=LOJunDay$LOOS14, y=LCJunDay$LCOS1)
plot(x=LOJunDay$LOOS14, y=LCJunDay$LCOS4)
plot(x=LOJunDay$LOOS14, y=LCJunDay$LCOS6)
plot(x=LOJunDay$LOOS14, y=LCJunDay$LCOS8)

LCJuneNite<-read.xlsx("LCJuneNite.xlsx")
plot(x=LOJunNight$LOON14, y=LCJuneNite$LCON1)
plot(x=LOJunNight$LOON14, y=LCJuneNite$LCON4)
plot(x=LOJunNight$LOON14, y=LCJuneNite$LCON6)
plot(x=LOJunNight$LOON14, y=LCJuneNite$LCON8)

plot(x=LOJunNight$LOOS14, y=LCJuneNite$LCOS1)
plot(x=LOJunNight$LOOS14, y=LCJuneNite$LCOS4)
plot(x=LOJunNight$LOOS14, y=LCJuneNite$LCOS6)
plot(x=LOJunNight$LOOS14, y=LCJuneNite$LCOS8)



LBJunDay<-read.xlsx("LBJuneDay.xlsx")
plot(x=LOJunDay$LOON14, y=LBJunDay$LBON11)
plot(x=LOJunDay$LOON14, y=LBJunDay$LBON12)
plot(x=LOJunDay$LOON14, y=LBJunDay$LBON14)
plot(x=LOJunDay$LOON14, y=LBJunDay$LBON16)

plot(x=LOJunDay$LOOS14, y=LBJunDay$LBOS11)
plot(x=LOJunDay$LOOS14, y=LBJunDay$LBOS12)
plot(x=LOJunDay$LOOS14, y=LBJunDay$LBOS14)
plot(x=LOJunDay$LOOS14, y=LBJunDay$LBOS16)

LBJuneNite<-read.xlsx("LBJuneNite.xlsx")
plot(x=LOJunNight$LOON14, y=LBJuneNite$LBON11)
plot(x=LOJunNight$LOON14, y=LBJuneNite$LBON12)
plot(x=LOJunNight$LOON14, y=LBJuneNite$LBON14)

plot(x=LOJunNight$LOOS14, y=LBJuneNite$LBOS11)
plot(x=LOJunNight$LOOS14, y=LBJuneNite$LBOS12)
plot(x=LOJunNight$LOOS14, y=LBJuneNite$LBOS14)

LAJunDay<-read.xlsx("LAJuneDay.xlsx")
plot(x=LOJunDay$LOON14, y=LAJunDay$LAON1)
plot(x=LOJunDay$LOON14, y=LAJunDay$LAON2)
plot(x=LOJunDay$LOON14, y=LAJunDay$LAON4)
plot(x=LOJunDay$LOON14, y=LAJunDay$LAON6)
plot(x=LOJunDay$LOON14, y=LAJunDay$LAON8)

plot(x=LOJunDay$LOOS14, y=LAJunDay$LAOS1)
plot(x=LOJunDay$LOOS14, y=LAJunDay$LAOS2)
plot(x=LOJunDay$LOOS14, y=LAJunDay$LAOS4)
plot(x=LOJunDay$LOOS14, y=LAJunDay$LAOS6)
plot(x=LOJunDay$LOOS14, y=LAJunDay$LAOS8)

LAJuneNite<-read.xlsx("LAJuneNite.xlsx")
plot(x=LAJuneNite$LAON8, y=LAJuneNite$LAON1)
plot(x=LAJuneNite$LAON8, y=LAJuneNite$LAON2)
plot(x=LAJuneNite$LAON8, y=LAJuneNite$LAON4)
plot(x=LAJuneNite$LAON8, y=LAJuneNite$LAON6)


plot(x=LAJuneNite$LAOS8, y=LAJuneNite$LAOS1)
plot(x=LAJuneNite$LAOS8, y=LAJuneNite$LAOS2)
plot(x=LAJuneNite$LAOS8, y=LAJuneNite$LAOS4)
plot(x=LAJuneNite$LAOS8, y=LAJuneNite$LAOS6)


LCJunDay<-read.xlsx("LCJuneDay.xlsx")
plot(x=LCJunDay$LCON1, y=LCJunDay$LCON6)
plot(x=LCJunDay$LCON4, y=LCJunDay$LCON6)

