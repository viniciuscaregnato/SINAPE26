source("first-sample/functions/func-lasso.R")
library(HDeconometrics)
load("first-sample/data/rawdata.rda")
Y=dados

nprev=132
alpha=0.5

## == passado == ##

lasso1c=lasso.rolling.window(Y,nprev,1,1,alpha,type="lasso")
lasso2c=lasso.rolling.window(Y,nprev,1,2,alpha,type="lasso")
lasso3c=lasso.rolling.window(Y,nprev,1,3,alpha,type="lasso")
lasso4c=lasso.rolling.window(Y,nprev,1,4,alpha,type="lasso")
lasso5c=lasso.rolling.window(Y,nprev,1,5,alpha,type="lasso")
lasso6c=lasso.rolling.window(Y,nprev,1,6,alpha,type="lasso")
lasso7c=lasso.rolling.window(Y,nprev,1,7,alpha,type="lasso")
lasso8c=lasso.rolling.window(Y,nprev,1,8,alpha,type="lasso")
lasso9c=lasso.rolling.window(Y,nprev,1,9,alpha,type="lasso")
lasso10c=lasso.rolling.window(Y,nprev,1,10,alpha,type="lasso")
lasso11c=lasso.rolling.window(Y,nprev,1,11,alpha,type="lasso")
lasso12c=lasso.rolling.window(Y,nprev,1,12,alpha,type="lasso")




### == juntando tudo ==  ###

forecast=cbind(lasso1c$pred,lasso2c$pred,lasso3c$pred,lasso4c$pred,
               lasso5c$pred,lasso6c$pred,lasso7c$pred,lasso8c$pred,
               lasso9c$pred,lasso10c$pred,lasso11c$pred,lasso12c$pred)





forecast = accumulate_model(forecast)


save(forecast,file="forecasts-samples/elasticnet/elasticnet-cpi1.rda")