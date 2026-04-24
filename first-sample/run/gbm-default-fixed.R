source("first-sample/functions/func-gbm-default-fixed.R")
library(HDeconometricsBeta)
library(gbm)
library(parallel)
load("first-sample/data/rawdata.rda")
Y=dados

nprev=132

## == passado == ##

gbm_default1c=gbm_default.rolling.window(Y,nprev,1,1)
gbm_default2c=gbm_default.rolling.window(Y,nprev,1,2)
gbm_default3c=gbm_default.rolling.window(Y,nprev,1,3)
gbm_default4c=gbm_default.rolling.window(Y,nprev,1,4)
gbm_default5c=gbm_default.rolling.window(Y,nprev,1,5)
gbm_default6c=gbm_default.rolling.window(Y,nprev,1,6)
gbm_default7c=gbm_default.rolling.window(Y,nprev,1,7)
gbm_default8c=gbm_default.rolling.window(Y,nprev,1,8)
gbm_default9c=gbm_default.rolling.window(Y,nprev,1,9)
gbm_default10c=gbm_default.rolling.window(Y,nprev,1,10)
gbm_default11c=gbm_default.rolling.window(Y,nprev,1,11)
gbm_default12c=gbm_default.rolling.window(Y,nprev,1,12)


### == juntando tudo ==  ###

forecast=cbind(gbm_default1c$pred,gbm_default2c$pred,gbm_default3c$pred,gbm_default4c$pred,
               gbm_default5c$pred,gbm_default6c$pred,gbm_default7c$pred,gbm_default8c$pred,
               gbm_default9c$pred,gbm_default10c$pred,gbm_default11c$pred,gbm_default12c$pred)

forecast = accumulate_model(forecast)


save(forecast,file="forecasts-samples/gbm_default/gbm_default-cpi1.rda")
