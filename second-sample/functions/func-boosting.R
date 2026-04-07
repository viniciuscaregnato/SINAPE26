#nao é o mesmo boosting da Naghi

runboost=function(Y,indice,lag){
  
  dum=Y[1:(nrow(Y)-lag+1),ncol(Y)]
  Y=Y[,-ncol(Y)]
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  y = y[1:(length(y)-lag+1)]
  X = X[1:(nrow(X)-lag+1),]
  dum=tail(dum,length(y))
  
  if(sum(dum)==1){
    X2=cbind(X,dum)
  }else{
    X2=X
  }
  
  model=boosting(y,X2,v=0.2,display=FALSE)
  
  
  if(sum(dum)==1){
    coef=model$coef
    pred=predict.HDeconometrics(model,c(X.out,0))
    
  }else{
    coef=c(model$coef,0)
    pred=predict.HDeconometrics(model,X.out)
    
  }
  
  return(list("model"=model,"pred"=pred))
}


boosting.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    bo=runboost(Y.window,indice,lag)
    aux=bo$model$coef
    if(length(aux)<37){
      aux=c(aux,0)
    }
    
    save.pred[(1+nprev-i),]=bo$pred
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors))
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
