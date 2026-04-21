source("second-sample/functions/func-cwbcv10.R")
library(mboost)
load("second-sample/data/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##

cwbcv101c=cwbcv10.rolling.window(Y,nprev,1,1)
cwbcv102c=cwbcv10.rolling.window(Y,nprev,1,2)
cwbcv103c=cwbcv10.rolling.window(Y,nprev,1,3)
cwbcv104c=cwbcv10.rolling.window(Y,nprev,1,4)
cwbcv105c=cwbcv10.rolling.window(Y,nprev,1,5)
cwbcv106c=cwbcv10.rolling.window(Y,nprev,1,6)
cwbcv107c=cwbcv10.rolling.window(Y,nprev,1,7)
cwbcv108c=cwbcv10.rolling.window(Y,nprev,1,8)
cwbcv109c=cwbcv10.rolling.window(Y,nprev,1,9)
cwbcv1010c=cwbcv10.rolling.window(Y,nprev,1,10)
cwbcv1011c=cwbcv10.rolling.window(Y,nprev,1,11)
cwbcv1012c=cwbcv10.rolling.window(Y,nprev,1,12)

### == juntando tudo ==  ###

forecast=cbind(cwbcv101c$pred,cwbcv102c$pred,cwbcv103c$pred,cwbcv104c$pred,
               cwbcv105c$pred,cwbcv106c$pred,cwbcv107c$pred,cwbcv108c$pred,
               cwbcv109c$pred,cwbcv1010c$pred,cwbcv1011c$pred,cwbcv1012c$pred)


forecast=accumulate_model(forecast)


##
save(forecast, file = "forecasts-samples/cwbcv10/cwbcv10-cpi2.rda")