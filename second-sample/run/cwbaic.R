source("second-sample/functions/func-cwbaic.R")
library(mboost)
load("second-sample/data/rawdata.RData")
Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=180


## == presente == ##

cwbaic1c=cwbaic.rolling.window(Y,nprev,1,1)
cwbaic2c=cwbaic.rolling.window(Y,nprev,1,2)
cwbaic3c=cwbaic.rolling.window(Y,nprev,1,3)
cwbaic4c=cwbaic.rolling.window(Y,nprev,1,4)
cwbaic5c=cwbaic.rolling.window(Y,nprev,1,5)
cwbaic6c=cwbaic.rolling.window(Y,nprev,1,6)
cwbaic7c=cwbaic.rolling.window(Y,nprev,1,7)
cwbaic8c=cwbaic.rolling.window(Y,nprev,1,8)
cwbaic9c=cwbaic.rolling.window(Y,nprev,1,9)
cwbaic10c=cwbaic.rolling.window(Y,nprev,1,10)
cwbaic11c=cwbaic.rolling.window(Y,nprev,1,11)
cwbaic12c=cwbaic.rolling.window(Y,nprev,1,12)

### == juntando tudo ==  ###

forecast=cbind(cwbaic1c$pred,cwbaic2c$pred,cwbaic3c$pred,cwbaic4c$pred,
               cwbaic5c$pred,cwbaic6c$pred,cwbaic7c$pred,cwbaic8c$pred,
               cwbaic9c$pred,cwbaic10c$pred,cwbaic11c$pred,cwbaic12c$pred)


forecast=accumulate_model(forecast)


##
save(forecast, file = "forecasts-samples/cwbaic/cwbaic-cpi2.rda")