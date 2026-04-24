rungbm_default=function(Y,indice,lag){
  dum=Y[,ncol(Y)]
  Y=Y[,-ncol(Y)]
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    #added drop=FALSE because there is only one row in the tail
    #and subsetting matrices automatically removes dimensions of length 1 int he resulting object
    #This ensures that X.out is matrix with one row. Not a vector.
    X.out=tail(aux,1)[,1:ncol(X), drop= FALSE] 
  }else{
    #added drop=FALSE because there is only one row in the tail
    #and subsetting matrices automatically removes dimensions of length 1 int he resulting object
    #This ensures that X.out is matrix with one row. Not a vector.
    
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[,1:ncol(X), drop= FALSE]
  }
  
  X.out=tail(aux,1)[,1:ncol(X), drop= FALSE] # y = y[1:(length(y)-lag+1)]
  dum=tail(dum,length(y)); X = cbind(X,dum); X.out = cbind(X.out,0); colnames(X.out)= NULL; colnames(X) = NULL #  X = X[1:(nrow(X)-lag+1),]
  
  # print("line 24")
  
  data.temp <- data.frame(y = y, x= X)
  
  data.test <- data.frame(x = X.out)
  
  model <- gbm(y ~ ., 
               data = data.temp, 
               distribution = "gaussian",
               n.trees = 100)
  
  
  # print("line 42")
  
  pred <- predict(model, 
                  newdata = data.test,
                  n.trees = 100)
  
  
  
  #columns correspond to individuals
  #obtain the 2.5% and 97.5% quantiles for each column
  
  #taking the transpose so that first column is vector of 2.5% values for each individual
  #second column is a vector of 97.5 values for each individual
  # pred_intervals <- t(apply(test_y_samps, 2, quantile, probs = c(0.025, 0.975)))
  
  
  model_ql <- gbm(y ~ ., data = data.temp, 
                  distribution = list(name = "quantile",alpha = 0.025),
                  n.trees = 100)
  
  # print("line 42")
  
  pred_ql <- predict(model_ql, newdata = data.test,
                     n.trees = 100)
  
  model_qu <- gbm(y ~ ., data = data.temp, 
                  distribution = list(name = "quantile",alpha = 0.975),
                  n.trees = 100)
  
  
  # print("line 42")
  
  pred_qu <- predict(model_qu, newdata = data.test,
                     n.trees = 100)
  
  
  #only one prediction, so perhaps unnecessary to save as matrix instead of column
  pred_intervals <- t( c(pred_ql , pred_qu ))
  
  # print("line 73")
  
  #MCMC convergence diagnostics
  #convergence of draws of conditional mean for test observation
  # post.mcmc <- test_mcmcsamps[ , 1]
  # post.mcmc <- test_mcmcsamps
  # print("line 77")
  # geweke_bart_testpreds <- gewekediag(post.mcmc)$z
  
  # geweke_bart_testpreds <- gewekediag(as.matrix(test_mcmcsamps))$z
  
  #convergence of draws of sigma
  # print("line 73")
  
  # geweke_bart_sigma <- gewekediag(as.matrix(sigmadraws))$z
  
  
  return(list("model"= model,
              "pred"= pred,
              "pred_intervals" = pred_intervals#,
              # "geweke_bart_testpreds"= geweke_bart_testpreds,
              # "geweke_bart_sigma" = geweke_bart_sigma
  ))
}


gbm_default.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list()
  #posterior inclusion probabilities can also be used as variable importance measues
  # save.pip=list()
  save.pred=matrix(NA,nprev,1)
  
  
  save.pred_intervals = matrix(NA,nprev,2)
  
  save.geweke_bart_testpreds = matrix(NA,nprev,1)
  save.geweke_bart_sigma = matrix(NA,nprev,1)
  
  # i <- nprev
  
  #code for parallelization
  
  
  
  
  #for(i in nprev:1){
  myfunction <- function(i) {
    library(gbm)
    
    res_temp <- list()
    
    window_len <- nrow(Y)-nprev-12; Tplush <- nrow(Y)+1-i ; endwind <- Tplush-lag ;  beginwind <- endwind-window_len ; Y.window=Y[(1+nprev-i):endwind,]
    #original medeiros et al. code uses "lasso" here to refer to all models
    
    # print("line 149")
    lasso=rungbm_default(Y.window,indice,lag)
    
    # print("line 152")
    
    
    
    #SAVE PREDICTIONS
    #this should be included for all methods
    res_temp$save.pred=lasso$pred
    
    #SAVE PREDICTION INTERVALS
    #not all methods include prediciton intervals
    #ensure that all prediction intervals are saved in the same format for all methods
    res_temp$save.pred_intervals=lasso$pred_intervals
    
    
    #SAVE MCMC DIAGNOSTICS
    #onlyapplicable to methods that use MCMC
    
    # res_temp$save.geweke_bart_testpreds=lasso$geweke_bart_testpreds
    # res_temp$save.geweke_bart_sigma=lasso$geweke_bart_sigma
    
    
    #SAVE VARIABLE IMPORTANCE RESULTS
    #Note: This is specific to the methods
    #Different methods have different variable importance measures
    #some methods have no variable importance measures
    #some methods have more than one variable importance measure
    tempsum <- summary(lasso$model)
    
    temp_varcounts <- tempsum[lasso$model$var.names, ]
    #BART variable inclusion probabilities as defined by Chipman et al
    #For each sum-of-tree model, obtain the proportions of splits for each variable
    #then average this across mcmc draws of sum-of-tree models
    
    res_temp$save.importance = temp_varcounts
    #the posterior inclusion probabilities are averages across model draws of 
    #indicators for whether the variable was used for any splits in the sum-of-tree model
    # res_temp$save.pip = colMeans(1*(temp_varcounts>0))
    
    #cat("iteration",(1+nprev-i),"\n")
    
    
    return(res_temp)
    
    
  }
  
  
  
  # no_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  
  cl <- makeCluster(120)
  clusterSetRNGStream(cl = cl, iseed = 123)
  
  clusterExport(cl,c('myfunction',
                     'nprev',
                     'indice',
                     'lag',
                     'Y',
                     'rungbm_default'
  ),
  envir = environment()
  )
  
  #registerDoParallel(cl)
  
  
  # start.time <- Sys.time()
  
  res_list <- parallel::parLapply(cl = cl, 1:nprev, fun = myfunction)
  
  stopCluster(cl)
  
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # time.taken
  
  for (i in nprev:1){
    
    save.pred[(1+nprev-i),]=res_list[[i]]$save.pred
    save.pred_intervals[(1+nprev-i),]=res_list[[i]]$save.pred_intervals
    # save.geweke_bart_testpreds[(1+nprev-i),]=res_list[[i]]$save.geweke_bart_testpreds
    # save.geweke_bart_sigma[(1+nprev-i),]=res_list[[i]]$save.geweke_bart_sigma
    save.importance[[i]] =res_list[[i]]$save.importance
    # save.pip[[i]] = res_list[[i]]$save.pip
    
    
  }
  
  
  
  
  real=Y[,indice]
  
  #these lines seem pointless, commenting out
  # plot(real,type="l")
  # lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  
  #median absolute deviation from the median in paper, but not in code
  mad = median(abs(tail(real,nprev)-save.pred - median(tail(real,nprev)-save.pred)))
  
  #mean absolute deviation from  he mean
  mean_ad = mean(abs(tail(real,nprev)-save.pred - mean(tail(real,nprev)-save.pred)))
  
  
  #mean relative absolute error (relative to random walk)
  #last 132 lagged one month values are
  #real[(nrow(dados)-nprev):(nrow(dados)-1)]
  mrae = mean(abs( (tail(real,nprev)-save.pred)/
                     (tail(real,nprev)-real[(nrow(dados)-nprev):(nrow(dados)-1)]  )     ))
  
  #mean absolute scaled error
  #equivalent to mae of method divided by nae of naive forecast
  #first calculate the denomiator
  #mean of vector of length nprev-1
  tempdenom = mean( abs(tail(real,nprev-1) - real[(nrow(dados)-nprev+1):(nrow(dados)-1)]  ) )
  #then the overall measure is
  mase=mae/tempdenom
  
  #mean absolute percentage error
  mape = (100/nprev)*mean(abs((tail(real,nprev)-save.pred)/tail(real,nprev)))
  
  #normalized rmse
  nrmse = rmse/(max(tail(real,nprev))-min(tail(real,nprev)))
  
  #rmse relative to random walk
  
  #rmse of naive rw forecast
  rwrmse=sqrt(mean((tail(real,nprev)-real[(nrow(dados)-nprev):(nrow(dados)-1)])^2))
  
  rmse_rel_rw = rmse/rwrmse
  
  
  errors=c("rmse"=rmse,
           "mae"=mae,
           "mad"=mad,
           "mean_ad"=mean_ad,
           "mrae"=mrae,
           "mase"=mase,
           "mape"=mape,
           "nrmse"=nrmse,
           "rmse_rel_rw"=rmse_rel_rw)
  
  #mean prediction interval coverage
  predint_cov = mean( 1*((save.pred_intervals[,1] < tail(real,nprev))  & (tail(real,nprev) < save.pred_intervals[,2])   ))
  
  #mean prediction interval width
  predint_width = mean(save.pred_intervals[,2] - save.pred_intervals[,1])
  
  
  
  
  
  
  
  return(list("pred"= save.pred,
              "errors"= errors,
              "save.pred_intervals"= save.pred_intervals,
              #"save.geweke_bart_testpreds"= save.geweke_bart_testpreds,
              #"save.geweke_bart_sigma"= save.geweke_bart_sigma,
              "save.importance"= save.importance,
              # "save.pip"= save.pip,
              "predint_cov"= predint_cov,
              "predint_width"= predint_width))
  
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
