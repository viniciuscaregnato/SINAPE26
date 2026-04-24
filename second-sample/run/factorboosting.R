source("second-sample/functions/func-factorboosting.R")
library(HDeconometrics)
load("first-sample/data/rawdata.rda")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##

factorboosting1c=factorboosting.rolling.window(Y,nprev,1,1)
factorboosting2c=factorboosting.rolling.window(Y,nprev,1,2)
factorboosting3c=factorboosting.rolling.window(Y,nprev,1,3)
factorboosting4c=factorboosting.rolling.window(Y,nprev,1,4)
factorboosting5c=factorboosting.rolling.window(Y,nprev,1,5)
factorboosting6c=factorboosting.rolling.window(Y,nprev,1,6)
factorboosting7c=factorboosting.rolling.window(Y,nprev,1,7)
factorboosting8c=factorboosting.rolling.window(Y,nprev,1,8)
factorboosting9c=factorboosting.rolling.window(Y,nprev,1,9)
factorboosting10c=factorboosting.rolling.window(Y,nprev,1,10)
factorboosting11c=factorboosting.rolling.window(Y,nprev,1,11)
factorboosting12c=factorboosting.rolling.window(Y,nprev,1,12)


### == juntando tudo ==  ###

forecasts=cbind(factorboosting1c$pred,factorboosting2c$pred,factorboosting3c$pred,factorboosting4c$pred,
          factorboosting5c$pred,factorboosting6c$pred,factorboosting7c$pred,factorboosting8c$pred,
          factorboosting9c$pred,factorboosting10c$pred,factorboosting11c$pred,factorboosting12c$pred)

forecast = accumulate_model(forecasts)


save(forecast,file="forecasts-samples/factorboosting/factor_boosting-cpi2.rda")
