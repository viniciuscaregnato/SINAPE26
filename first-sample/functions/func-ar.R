runAR=function(Y,indice,lag,type="fixed"){
  Y2=cbind(Y[,indice])
  aux=embed(Y2,4+lag)
  y=aux[,1]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  y = y[1:(length(y)-lag+1)]
  X = X[1:(nrow(X)-lag+1),]
  
  
  if(type=="fixed"){
    model=lm(y~X)
    coef=coef(model)
  }
  
  if(type=="bic"){
    bb=Inf
    for(i in seq(1,ncol(X),1)){
      m=lm(y~X[,1:i])
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        model=m
        ar.coef=coef(model)
      }
    }
    coef=rep(0,ncol(X)+1)
    coef[1:length(ar.coef)]=ar.coef
  }
  pred=c(1,X.out)%*%coef
  
  return(list("model"=model,"pred"=pred,"coef"=coef))
}



ar.rolling.window=function(Y,nprev,indice=1,lag=1,type="fixed"){
  
  save.coef=matrix(NA,nprev,5)
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    fact=runAR(Y.window,indice,lag)
    save.coef[(1+nprev-i),]=fact$coef
    save.pred[(1+nprev-i),]=fact$pred
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}

accumulate_model = function(forecasts){
  
  acc3 = c(rep(NA,2),sapply(1:(nrow(forecasts)-2), function(x){
    prod(1+diag(forecasts[x:(x+2),1:3]))-1
  })) 
  acc6 = c(rep(NA,5),sapply(1:(nrow(forecasts)-5), function(x){
    prod(1+diag(forecasts[x:(x+5),1:6]))-1
  }))
  acc12 = c(rep(NA,11),sapply(1:(nrow(forecasts)-11), function(x){
    prod(1+diag(forecasts[x:(x+11),1:12]))-1
  }))
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  return(forecasts)
  
}
