source("second-sample/functions/func-ar.R")
library(HDeconometrics)
load("second-sample/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##


bar1c=ar.rolling.window(Y,nprev,1,1,type="bic")
bar2c=ar.rolling.window(Y,nprev,1,2,type="bic")
bar3c=ar.rolling.window(Y,nprev,1,3,type="bic")
bar4c=ar.rolling.window(Y,nprev,1,4,type="bic")
bar5c=ar.rolling.window(Y,nprev,1,5,type="bic")
bar6c=ar.rolling.window(Y,nprev,1,6,type="bic")
bar7c=ar.rolling.window(Y,nprev,1,7,type="bic")
bar8c=ar.rolling.window(Y,nprev,1,8,type="bic")
bar9c=ar.rolling.window(Y,nprev,1,9,type="bic")
bar10c=ar.rolling.window(Y,nprev,1,10,type="bic")
bar11c=ar.rolling.window(Y,nprev,1,11,type="bic")
bar12c=ar.rolling.window(Y,nprev,1,12,type="bic")



### == juntando tudo ==  ###

forecast=cbind(bar1c$pred,bar2c$pred,bar3c$pred,bar4c$pred,
           bar5c$pred,bar6c$pred,bar7c$pred,bar8c$pred,
           bar9c$pred,bar10c$pred,bar11c$pred,bar12c$pred)


forecast=accumulate_model(forecast)


##

save(forecast, file = "forecasts-samples/ar/bicar-cpi2.rda")


