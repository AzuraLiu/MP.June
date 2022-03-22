setwd("C:/Users/liux3/Desktop/work/Data/Raw/Coded/System1")
library(readxl)
a = read_excel('VT Sap1 k values 01-2017.xlsx')
str(a)
library(dplyr)
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)
# a$LOON11 = as.numeric(a$LOON11)
# a$Date = as.Date(a$Date)
# attach(a)
# plot(HCOS1, HCON1)
# sum(HCOS1 == 0)
# sum(HCON1 == 0)




library(data.table)


impute = function(x_nam, y_nam, D, gap_ind, plot_ = TRUE, bidirect = F){
  D_copy = D
  time = D[gap_ind, 3]
  Date_point = unique(D[gap_ind,1])
  time_span = c(min(Date_point) - 1,Date_point,  max(Date_point) + 1)
  D = D %>% filter(Date %in% time_span)
  x = D[x_nam]
  y = D[y_nam]
  ## Use three days to impute
  D0 = data.frame(x,  y, date = D$Date, time = D$Time)
  colnames(D0) = c('x','y' ,'Date','Time')
  fit = lm(x ~ y, data = D0)
  ind1 = is.na(x)
  y0 = D_copy %>% select(y_nam) 
  ## Impute the fitted y into original table:
  pred= predict(fit, newdata = data.frame(y = y0[gap_ind,1]))
  D_copy[gap_ind, x_nam] = pred
  
  
  ## plot:
  if(bidirect == F &  plot_ == T){
    plot(D0$x ~ D0$y, xlab = y_nam, ylab = x_nam, main = paste(x_nam, 'by', y_nam, Date_point))
    points(D_copy[gap_ind, y_nam], D_copy[gap_ind, x_nam], pch = 20, col = 'red')
    abline(fit, col = 'red')
    plotD = D0 %>% filter(Date %in% Date_point)
    plot(plotD$y, pch = 20, type = 'l', ylim = c(0,2),col = 'gray', xlab = 'Time', ylab = 'Observations', main = paste(x_nam, 'by', y_nam))
    points(plotD$x, type = 'l', lty = 'dashed', col = 'black')
    ## get the location of imputation in the dataframe
    plot_ind = gap_ind - nrow(D_copy %>% filter(Date < min(Date_point)))
    points(D_copy[gap_ind, x_nam]~ plot_ind, col = 'red', pch = 20, main = paste(x_nam, 'by', y_nam, Date_point))
    legend("topleft", legend = c('Ground Truth', 'Observation', 'Imputed Missing'), col = c('gray', 'black', 'red'), 
           pch = c(20, 20, 20))
  }
  return(list(x = D_copy[gap_ind, x_nam], y = y))
}


a[1193:1198,'LOON11'] = impute('LOON11', 'LOOS11', a , 1193:1198)$x
a[1806:1871,'LOON11'] = impute('LOON11', 'LOOS11', a , 1806:1871)$x
a[2668:2676,'LOON11'] = impute('LOON11', 'LOOS11', a , 2668:2676)$x
a[2861:2862,'LOON11'] = impute('LOON11', 'LOOS11', a , 2861:2862)$x

write.csv(a, file = 'JAN.csv')


############################
### Find best correlated:###
############################
a = read_excel('VT Sap1 k values 02-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)
## HCIS9 is the fourth variable:

whose_cor = function(x_nam, D){
  D = D[,-(1:5)]
  x = D[x_nam]
  ind = which(colnames(D) == x_nam)
  re = rep(NA, ncol(D))
  for(i in 1:ncol(D)){
    if(i == ind){
      re[i] = NA
    }
    else{
      D0 = cbind(D[,i], x[,1])
      re[i] =cor(D0[complete.cases(D0),])[1,2]
    }
    
  }
  nam = colnames(D)
  names(re) = nam
  ind = sort(re)
  return(ind)
}





whose_cor('LOON16', a)

a[439:440, 'LOON16'] = impute('LOON16', 'LOOS14', a, 439:440)$x   ###Interpolation

a[1104:1105, 'LOON11'] = impute('LOON11', 'LOOS11', a, 1104:1105)$x
a[1143:1166, 'LOON11'] = impute('LOON11', 'LOOS11', a, 1143:1166)$x
write.csv(a, file = 'FEB.csv')


## March
a = read_excel('VT Sap1 k values 03-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)
whose_cor('LOON16', a)
a[1576:1585, 'LOON16'] = impute('LOON16', 'LOOS17', a, 1576:1585)$x
write.csv(a, 'MAR.csv')

## April
a = read_excel('VT Sap1 k values 04-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)
whose_cor('LOON16', a)

a[2455:2457, 'LOOS10'] = impute('LOOS10', 'LOON10', a, 2455:2457)$x

write.csv(a, 'April.csv')


## May
a = read_excel('VT Sap1 k values 05-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

a[1309:1311, 'LOIS10'] = impute('LOIS10', 'LOIN10',a,1309:1311)$x
a[1379:1407, 'LOIS10'] = impute('LOIS10', 'LOIN10', a, 1379:1407)$x


a[1486:1491, 'LOOS15'] = impute('LOOS15', 'LOON15', a, 1486 : 1491)$x

a[1786:1787, 'HCON1'] = impute('HCON1', 'HCOS1', a, 1786:1787)$x
write.csv(a, 'MAY.csv')


## Jun
a = read_excel('VT Sap1 k values 06-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

a[1940:1941, 'LOIN10'] = impute('LOIN10', 'LOIS10', a, 1940:1941)$x
a[1420:1423,'LOOS14'] = impute('LOOS14', 'LOON14', a, 1420:1423)$x

write.csv(a, 'JUN.csv')

## JUL
a = read_excel('VT Sap1 k values 07-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

a[482:483, 'LOIN10'] = impute('LOIN10', 'LOIS10', a, 482:483)$x
a[507:508, 'LOIN10'] = impute('LOIN10', 'LOIS10', a,507:508)$x
a[991:992, 'LOIN10'] = impute('LOIN10', 'LOIS10', a, 991:992)$x
write.csv(a, 'JUL.csv')


## Aug
a = read_excel('VT Sap1 k values 08-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)
a[1390:1392, 'LOON11'] = impute('LOON11', 'LOOS11', a , 1390:1392)$x
a[1401:1403, 'LOON11'] = impute('LOON11', 'LOOS11', a, 1401:1403)$x
a[2280:2295, 'HCON2'] = impute('HCON2', 'HCOS2', a, 2280:2295)$x
write.csv(a, 'AUG.csv')

## Sep
a = read_excel('VT Sap1 k values 09-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

a[2066:2067, 'LOON10'] = impute('LOON10', 'LOOS10', a, 2066:2067)$x
whose_cor('LOON10', a)
a[2066:2067, 'LOON10'] = impute('LOON10', 'LOIN10', a, 2066:2067)$x
a[714:717, 'LOON10'] = impute('LOON10', 'LOIN10', a, 714:717)$x
a[2538:2548, 'LOON10'] = impute('LOON10', 'LOOS10', a, 2538:2548)$x

a[2555:2564, 'LOON10'] = impute('LOON10', 'LOOS10', a, 2555:2564)$x
a[2603:2640, 'LOON10'] = impute('LOON10', 'LOOS10', a, 2603:2640)$x
write.csv(a, 'SEP.csv')


## Nov
a = read_excel('VT Sap1 k values 11-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

a[2101:2102, 'HCON6'] = impute('HCON6', 'HCOS6', a, 2101:2102)$x
a[2135:2138, 'LOOS14'] = impute('LOOS14', 'LOON14', a, 2135:2138)$x
a[2144:2146, 'LOOS14'] = impute('LOOS14', 'LOON14', a, 2144:2146)$x
a[2238:2268, 'LOOS14'] = impute('LOOS14', 'LOON14', a, 2238:2268)$x

a[2142:2143, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2142:2143)$x
write.csv(a, 'NOV.csv')

## Dec
a = read_excel('VT Sap1 k values 12-2017.xlsx')
a[,-(1:5)] = apply(a[,-(1:5)], 2, as.numeric)
a = data.frame(a)

whose_cor('HCON9',a)
a[804:805, 'HCON9'] = impute('HCON9', 'LOON17', a, 804:805)$x
a[834:837, 'HCON9'] = impute('HCON9', 'LOON17', a, 834:837)$x
a[1753:1754, 'HCON9'] = impute('HCON9', 'LOON17', a, 1753:1754)$x
a[1803:1805, 'HCON9'] = impute('HCON9', 'HCON6', a, 1803:1805)$x
a[1901:1902, 'HCON9'] = impute('HCON9', 'HCON6', a, 1901:1902)$x

a[2047:20048, 'HCON9'] = impute('HCON9', 'LOON17', a, 2047:2048)$x
a[2112:2114, 'HCON9'] = impute('HCON9', 'HCON6', a, 2112:2114)$x
a[2138:2140, 'HCON9'] = impute('HCON9', 'LOON17', a, 2138:2140)$x
a[2196:2198, 'HCON9'] = impute('HCON9', 'LOON17', a, 2196:2198)$x
a[2411:2412, 'HCON9'] = impute('HCON9', 'LOON17', a, 2411:2412)$x
a[2426:2427, 'HCON9'] = impute('HCON9', 'LOON17', a, 2426:2427)$x
a[2786:2787, 'HCON9'] = impute('HCON9', 'LOON17', a, 2786:2787)$x
a[804:805, 'HCON9'] = impute('HCON9', 'LOON17', a, 804:805)$x


a[837:838, 'HCON6'] = impute('HCON6', 'HCOS6', a, 837:838)$x
a[2426:2427, 'HCON6'] = impute('HCON6', 'HCOS6', a, 2426:2427)$x
a[1672:1673, 'HCON6'] = impute('HCON6', 'HCOS6', a, 1672:1673)$x

whose_cor('LOON17', a)
a[1344:1345, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1344:1345)$x
a[1730:1734, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1730:1734)$x
a[1803:1818, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1803:1818)$x
a[1851:1853, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1851:1853)$x
a[1909:1910, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1909:1910)$x
a[1915:1923, 'LOON17'] = impute('LOON17', 'LOOS17', a, 1915:1923)$x
a[1942:1948, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1942:1948)$x
a[1989:1991, 'LOON17'] = impute('LOON17', 'LOOS12', a, 1989:1991)$x
a[2007:2009, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2007:2009)$x
a[2028:2029, 'LOON17'] = impute('LOON17', 'LOOS12', a, 2028:2029)$x
a[2079:2080, 'LOON17'] = impute('LOON17', 'LOOS12', a, 2079:2080)$x
a[2087:2089, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2087:2089)$x
a[2094:2095, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2094:2095)$x
a[2103:2116, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2103:2116)$x
a[2138:2149, 'LOON17'] = impute('LOON17', 'LOOS12', a, 2138:2149)$x
a[2178:2198, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2178:2198)$x
a[2199:2208, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2199:2208)$x
a[2224:2225, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2224:2225)$x
a[2412:2413, 'LOON17'] = impute('LOON17', 'LOOS12', a, 2412:2413)$x
a[2426:2427, 'LOON17'] = impute('LOON17', 'LOOS12', a, 2426:2427)$x
a[2466:2467, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2466:2467)$x
a[2558:2559, 'LOON17'] = impute('LOON17', 'LOOS17', a, 2558:2559)$x

whose_cor('LOOS11', a)
a[1565:1566, 'LOOS11'] = impute('LOOS11', 'LOOS12', a, 1565:1566)$x
a[2622:2642, 'LOOS11'] = impute('LOOS11', 'LOOS12', a, 2622:2642)$x

a[2700:2739, 'LOOS11'] = impute('LOOS11', 'LOOS12', a, 2700:2739)$x
a[2624:2633, 'LOOS11'] = impute('LOOS11', 'LOOS12', a, 2624:2633)$x

write.csv(a, 'DEC.csv')
