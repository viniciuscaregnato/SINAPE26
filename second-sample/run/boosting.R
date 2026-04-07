source("second-sample/functions/func-boosting.R")
library(HDeconometricsBeta)
load("second-sample/data/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##

boosting1c=boosting.rolling.window(Y,nprev,1,1)
boosting2c=boosting.rolling.window(Y,nprev,1,2)
boosting3c=boosting.rolling.window(Y,nprev,1,3)
boosting4c=boosting.rolling.window(Y,nprev,1,4)
boosting5c=boosting.rolling.window(Y,nprev,1,5)
boosting6c=boosting.rolling.window(Y,nprev,1,6)
boosting7c=boosting.rolling.window(Y,nprev,1,7)
boosting8c=boosting.rolling.window(Y,nprev,1,8)
boosting9c=boosting.rolling.window(Y,nprev,1,9)
boosting10c=boosting.rolling.window(Y,nprev,1,10)
boosting11c=boosting.rolling.window(Y,nprev,1,11)
boosting12c=boosting.rolling.window(Y,nprev,1,12)

### == juntando tudo ==  ###

forecast=cbind(boosting1c$pred,boosting2c$pred,boosting3c$pred,boosting4c$pred,
          boosting5c$pred,boosting6c$pred,boosting7c$pred,boosting8c$pred,
          boosting9c$pred,boosting10c$pred,boosting11c$pred,boosting12c$pred)


forecast=accumulate_model(forecast)


##
save(forecast, file = "forecasts-samples/boosting/boosting-cpi2.rda")
