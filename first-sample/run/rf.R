source("first-sample/functions/func-rf.R")
library(HDeconometricsBeta)
library(randomForest)
load("first-sample/data/rawdata.rda")
Y=dados

nprev=132


## == presente == ##
set.seed(123)
rf1c=rf.rolling.window(Y,nprev,1,1)
rf2c=rf.rolling.window(Y,nprev,1,2)
rf3c=rf.rolling.window(Y,nprev,1,3)
rf4c=rf.rolling.window(Y,nprev,1,4)
rf5c=rf.rolling.window(Y,nprev,1,5)
rf6c=rf.rolling.window(Y,nprev,1,6)
rf7c=rf.rolling.window(Y,nprev,1,7)
rf8c=rf.rolling.window(Y,nprev,1,8)
rf9c=rf.rolling.window(Y,nprev,1,9)
rf10c=rf.rolling.window(Y,nprev,1,10)
rf11c=rf.rolling.window(Y,nprev,1,11)
rf12c=rf.rolling.window(Y,nprev,1,12)



### == juntando tudo ==  ###

forecast=cbind(rf1c$pred,rf2c$pred,rf3c$pred,rf4c$pred,
          rf5c$pred,rf6c$pred,rf7c$pred,rf8c$pred,
          rf9c$pred,rf10c$pred,rf11c$pred,rf12c$pred)

forecast = accumulate_model(forecast)

##
save(forecast,file="forecasts-samples/rf/rf-cpi1.rda")

