source("second-sample/functions/func-lasso.R")
library(HDeconometrics)
load("second-sample/data/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180
alpha=0.5

## == presente == ##

elasticnet1c=lasso.rolling.window(Y,nprev,1,1,alpha,type="lasso")
elasticnet2c=lasso.rolling.window(Y,nprev,1,2,alpha,type="lasso")
elasticnet3c=lasso.rolling.window(Y,nprev,1,3,alpha,type="lasso")
elasticnet4c=lasso.rolling.window(Y,nprev,1,4,alpha,type="lasso")
elasticnet5c=lasso.rolling.window(Y,nprev,1,5,alpha,type="lasso")
elasticnet6c=lasso.rolling.window(Y,nprev,1,6,alpha,type="lasso")
elasticnet7c=lasso.rolling.window(Y,nprev,1,7,alpha,type="lasso")
elasticnet8c=lasso.rolling.window(Y,nprev,1,8,alpha,type="lasso")
elasticnet9c=lasso.rolling.window(Y,nprev,1,9,alpha,type="lasso")
elasticnet10c=lasso.rolling.window(Y,nprev,1,10,alpha,type="lasso")
elasticnet11c=lasso.rolling.window(Y,nprev,1,11,alpha,type="lasso")
elasticnet12c=lasso.rolling.window(Y,nprev,1,12,alpha,type="lasso")




### == juntando tudo ==  ###

forecast=cbind(elasticnet1c$pred,elasticnet2c$pred,elasticnet3c$pred,elasticnet4c$pred,
          elasticnet5c$pred,elasticnet6c$pred,elasticnet7c$pred,elasticnet8c$pred,
          elasticnet9c$pred,elasticnet10c$pred,elasticnet11c$pred,elasticnet12c$pred)

forecast=accumulate_model(forecast)

##
save(forecast, file = "forecasts-samples/elasticnet/elasticnet-cpi2.rda")