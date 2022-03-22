setwd("C:/Users/liux3/Desktop/work/data/Raw/Coded/System1")

library(readxl)
a = read_excel('VT Sap1 k values 01-2017.xlsx')
library(dplyr)
a$LOON11 = as.numeric(a$LOON11)
a$Date = as.Date(a$Date)
attach(a)
plot(HCOS1, HCON1)
sum(HCOS1 == 0)
sum(HCON1 == 0)

D = data.frame(HCOS1, HCON1)
zerona = function(x){
  ind = which(x == 0)
  x[ind] = NA
  return(x)
}

a = data.frame(a)
library(data.table)

#
time = a[1193:1198, 3]
time_point = unique(a[1193:1198,1])
time_span = c(time_point - 1, time_point, time_point + 1)


#HCOS1 HCON1
impute = function(x_nam, y_nam, D,  time_point, time, plot_ = TRUE, bidirect = F){
  time_span = c(min(time_point) - 1,time_point, max(time_point) + 1)
  D = D %>% filter(Date %in% time_span)
  x = D[x_nam]
  y = D[y_nam]
  D0 = data.frame(x,  y, date = D$Date, time = D$Time)
  colnames(D0) = c('x','y' ,'Date','Time')
  fit = lm(x ~ y, data = D0)
  ind1 = is.na(x)
  gap_ind = which(D$Date %in% time_point & D$Time %in% time)
  gap_y = D %>% filter(Date %in% time_point & Time %in% time) %>% select(y_nam)
  pred= predict(fit, newdata = data.frame(y = y[gap_ind, 1]))
  D0$x[gap_ind] = pred

  if(bidirect == F &  plot_ == T){
    plot(D0$x ~ D0$y, xlab = y_nam, ylab = x_nam, main = paste(x_nam, 'by', y_nam))
    points(D0[gap_ind, 2], D0[gap_ind, 1], pch = 20, col = 'red')
    abline(fit, col = 'red')
    plotD = D0 %>% filter(Date %in% time_point)
    plot(plotD$y, pch = 20, type = 'l', col = 'gray', xlab = 'Time', ylab = 'Observations', main = paste(x_nam, 'by', y_nam))
    points(plotD$x, type = 'l', lty = 'dashed', col = 'black')
    plot_ind = gap_ind - 96
    points(D0[gap_ind, 1]~ plot_ind, col = 'red', pch = 20)
    legend("topleft", legend = c('Ground Truth', 'Observation', 'Imputed Missing'), col = c('gray', 'black', 'red'), 
           pch = c(20, 20, 20))
  }
    return(list(x = x, y = y))
  # if(bidirect = T & plot_ = T){
  #   fit1 = lm(y ~ x, data = D0)
  #   ind2 = is.na(y)
  #   pred1 = predict(fit1, newdata = data.frame(x = x[ind2]))
  #   y[ind2] = pred1
  #   result  = data.frame(x, y)
  #   colnames(result) = c('x', 'y')
  #   if(plot_ == TRUE){
  #     plot(y[,1], x[,1], xlab = y_nam, ylab = x_nam)
  #     points(y[c(ind1, ind2), 1], x[c(ind1, ind2), 1], col = 'red', pch = 20)
  #     abline(lm(x ~ y, data = D0), col = 'red')
  #     # plot(x[,1] ~ Time)
  #     # points(x[ind1, 1] ~ Time[ind1], pch = 20, col = 'red')
  #     # plot(y[,1] ~ Time)
  #     # points(y[ind2, 1] ~ Time[ind2], pch = 20 ,col = 'red')
  #   }
  #   return(list(x = x, y= y, ind1 = ind1, ind2 = ind2))
  # }
}


re = impute('LOON11', 'LOOS11', a , time_span, time )
D$HCON1 = re$x
D$HCOS1 = re$y

## Day 19 20  input name and row number

time = a[1806:1871, 3]
Date_point = unique(a[1806:1871,1])
re = impute('LOON11', 'LOOS11', a , Date_point, time)















##### Do the imputation

re = impute('HCON6', 'HCOS6', D, Time)
D$HCON6 = re$x
D$HCOS6 = re$y

re = impute('HCON8', 'HCOS8', D, Time)
D$HCON8 = re$x
D$HCOS8 = re$y

## BY the pair rule, we can repeat the procedure above:
re = impute('LOON10', 'LOOS10', D, Time)
re = impute('LOIN10', 'LOIS10', D, Time)
re = impute('LOON11', 'LOOS11', D, Time)
re = impute('LOON17', 'LOOS17', D, Time)
re = impute('LOON15', 'LOOS15', D, Time)
re = impute('LOON14', 'LOOS14', D, Time)
re = impute('LOON12', 'LOOS12', D, Time)

## For those variables without pair, we select the most correlated variable as its pair:
############################
### Find best correlated:###
############################
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
  ind = which.max(re)
  return(colnames(D)[ind])
}

## The most correlated is LOON14
whose_cor('HCIS9', D)
impute('HCIS9', 'LOON14', D, Time)

## Then all the things can be done as above.
## If you want to impute as more as possible, you just need to repeat all the work above, from HCON1 and HCOS1 again.

