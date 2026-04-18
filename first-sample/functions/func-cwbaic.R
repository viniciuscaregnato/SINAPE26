runcwbaic=function(Y,indice,lag, type = "aic"){
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
  
  

  best_fit = glmboost(
    y = y,
    x = X,
    offset = 0,
    center = TRUE,
    control = boost_control(
      mstop = 100,
      nu = 0.1
    )
)


  if (type == "aic") { 
    aic_obj <- AIC(best_fit)
    m_opt <- mstop(aic_obj)
    cat("M_opt selecionado:", m_opt, "de um máximo de 100\n") 
    
    model <- best_fit[m_opt] 
  }

  
  
  if (type == "bic") {
    m_opt <- mstop(AIC(best_fit, k = log(length(y))))
    model <- best_fit[m_opt] 
  }

  if (type == "cv5") {
    cv5f <- cv(model.weights(best_fit), type = "kfold", B = 5)
    cvm <- cvrisk(best_fit, folds = cv5f, papply = lapply)
    m_opt <- mstop(cvm)
    model <- best_fit[m_opt] 
  }

  if (type == "cv10") {
    cv5f <- cv(model.weights(best_fit), type = "kfold", B = 10)
    cvm <- cvrisk(best_fit, folds = cv5f, papply = lapply)
    m_opt <- mstop(cvm)
    model <- best_fit[m_opt] 
  }
  
  
  coef_temp <- coef(model)
  
  coef_opt <- rep(0, ncol(X))
  names(coef_opt) <- c(nomes = paste0("V", 1:504))
  coef_opt[names(coef_temp)] <- coef_temp
  
  
  Xin_mean = as.vector(apply(X, 2, mean))
  yin_mean = mean(y)
  
  pred = sum(((X.out - Xin_mean) * coef_opt)) + yin_mean
  
  #outputs=list(coef_opt)
  
  return(list("model"=model, "pred"=pred))


}


cwbaic.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    bo=runcwbaic(Y.window,indice,lag)
    
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