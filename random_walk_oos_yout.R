library(roll)
library(dplyr)

load("second-sample/rawdata.RData")

nprev = 312

y = dados[,"CPI"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)    
yout = tail(y,nprev)                                                   


rw = matrix(NA,nprev,12)
for(i in 1:12){                                                        
  aux=dados[(nrow(dados)-nprev-i+1):(nrow(dados)-i),"CPI"]          
  rw[,i]=aux;                                                           
}                                                                        



rw3 = embed(y[,2],4)                                                     
rw3 = tail(embed(y[,2],4)[,4],nprev)



rw6 = tail(embed(y[,3],7)[,7],nprev)                                  
rw12 = tail(embed(y[,4],13)[,13],nprev)
rw = cbind(rw,rw3,rw6,rw12)
colnames(rw) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")        

save(yout,file = "forecasts/yout.rda")
save(rw,file = "forecasts/rw.rda")


