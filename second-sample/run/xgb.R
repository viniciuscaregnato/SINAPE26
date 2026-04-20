source("second-sample/functions/func-xgb.R")
#library(HDeconometrics)
library(xgboost)
load("second-sample/data/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##

xgb1c=xgb.rolling.window(Y,nprev,1,1)
xgb2c=xgb.rolling.window(Y,nprev,1,2)
xgb3c=xgb.rolling.window(Y,nprev,1,3)
xgb4c=xgb.rolling.window(Y,nprev,1,4)
xgb5c=xgb.rolling.window(Y,nprev,1,5)
xgb6c=xgb.rolling.window(Y,nprev,1,6)
xgb7c=xgb.rolling.window(Y,nprev,1,7)
xgb8c=xgb.rolling.window(Y,nprev,1,8)
xgb9c=xgb.rolling.window(Y,nprev,1,9)
xgb10c=xgb.rolling.window(Y,nprev,1,10)
xgb11c=xgb.rolling.window(Y,nprev,1,11)
xgb12c=xgb.rolling.window(Y,nprev,1,12)



### == juntando tudo ==  ###

forecast=cbind(xgb1c$pred,xgb2c$pred,xgb3c$pred,xgb4c$pred,
          xgb5c$pred,xgb6c$pred,xgb7c$pred,xgb8c$pred,
          xgb9c$pred,xgb10c$pred,xgb11c$pred,xgb12c$pred)

forecast = accumulate_model(forecast)

##
save(forecast,file="forecasts-samples/xgb/xgboost-cpi2.rda")