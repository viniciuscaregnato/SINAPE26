source("first-sample/functions/func-cwbcv5.R")
library(mboost)
load("first-sample/data/rawdata.rda")
Y=dados

nprev=132

## == passado == ##

cwbcv51c=cwbcv5.rolling.window(Y,nprev,1,1)
cwbcv52c=cwbcv5.rolling.window(Y,nprev,1,2)
cwbcv53c=cwbcv5.rolling.window(Y,nprev,1,3)
cwbcv54c=cwbcv5.rolling.window(Y,nprev,1,4)
cwbcv55c=cwbcv5.rolling.window(Y,nprev,1,5)
cwbcv56c=cwbcv5.rolling.window(Y,nprev,1,6)
cwbcv57c=cwbcv5.rolling.window(Y,nprev,1,7)
cwbcv58c=cwbcv5.rolling.window(Y,nprev,1,8)
cwbcv59c=cwbcv5.rolling.window(Y,nprev,1,9)
cwbcv510c=cwbcv5.rolling.window(Y,nprev,1,10)
cwbcv511c=cwbcv5.rolling.window(Y,nprev,1,11)
cwbcv512c=cwbcv5.rolling.window(Y,nprev,1,12)


### == juntando tudo ==  ###

forecast=cbind(cwbcv51c$pred,cwbcv52c$pred,cwbcv53c$pred,cwbcv54c$pred,
               cwbcv55c$pred,cwbcv56c$pred,cwbcv57c$pred,cwbcv58c$pred,
               cwbcv59c$pred,cwbcv510c$pred,cwbcv511c$pred,cwbcv512c$pred)

forecast = accumulate_model(forecast)


save(forecast,file="forecasts-samples/cwbcv5/cwbcv5-cpi1.rda")