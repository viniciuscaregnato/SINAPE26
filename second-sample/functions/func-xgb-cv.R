runxgb_cv=function(Y,indice,lag){
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
  
  
  
  dtrain <- xgb.DMatrix(X, label = y)
  dtest <- xgb.DMatrix(X.out)
  
  best_param <- list()
  best_seednumber <- 1234
  best_rmse <- Inf
  best_rmse_index <- 0
  
  set.seed(123)
  for (iter in 1:100) {
    param <- list(objective = "reg:squarederror",
                  eval_metric = "rmse",
                  max_depth = sample(6:10, 1),
                  eta = runif(1, .01, .3), # Learning rate, default: 0.3
                  subsample = runif(1, .6, .9),
                  colsample_bytree = runif(1, .5, .8), 
                  min_child_weight = sample(1:40, 1),
                  max_delta_step = sample(1:10, 1),
                  nthread = 1
    )
    cv.nround <-  1000
    cv.nfold <-  5 # 5-fold cross-validation
    seed.number  <-  sample.int(10000, 1) # set seed for the cv
    set.seed(seed.number)
    mdcv <- xgb.cv(data = dtrain, params = param,  
                   nfold = cv.nfold, nrounds = cv.nround,
                   verbose = F, early_stopping_rounds = 8, maximize = FALSE)
    
    min_rmse_index  <-  mdcv$best_iteration
    min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
    
    if (min_rmse < best_rmse) {
      best_rmse <- min_rmse
      best_rmse_index <- min_rmse_index
      best_seednumber <- seed.number
      best_param <- param
    }
  }
  
  # The best index (min_rmse_index) is the best "nround" in the model
  nround = best_rmse_index
  set.seed(best_seednumber)
  xg_mod <- xgboost(data = dtrain, params = best_param, nround = nround, verbose = F)
  
  
  
  
  
  
  
  
  
  
  # model = xgboost(X,label = y,nrounds = 1000, verbose = FALSE,
  #                 params=list(eta=0.05,nthread=1,colsample_bylevel=2/3,subsample=1,max_depth=4,min_child_weigth=nrow(X)/200))
  # 
  pred=predict(xg_mod,X.out)
  
  
  #columns correspond to individuals
  #obtain the 2.5% and 97.5% quantiles for each column
  
  
  #############################################################################
  
  #Need to add quantile regression XGB for prediction intervals
  
  
  #############################################################################
  
  #taking the transpose so that first column is vector of 2.5% values for each individual
  #second column is a vector of 97.5 values for each individual
  # pred_intervals <- t(apply(test_y_samps, 2, quantile, probs = c(0.025, 0.975)))
  
  #only one prediction, so perhaps unnecessary to save as matrix instead of column
  # pred_intervals <- t(quantile(test_y_samps, probs = c(0.025, 0.975) ) )
  
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
  
  
  return(list(#"model"= model,
    "pred"= pred
    # "pred_intervals" = pred_intervals#,
    # "geweke_bart_testpreds"= geweke_bart_testpreds,
    # "geweke_bart_sigma" = geweke_bart_sigma
  ))
}


xgb_cv.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  # save.importance=list()
  #posterior inclusion probabilities can also be used as variable importance measues
  # save.pip=list()
  save.pred=matrix(NA,nprev,1)
  
  
  for(i in 1:nprev){
    cat("Iteração", i, "de", nprev, "\n")  # mostra progresso
    
    Y.window = Y[(1+nprev-i):(nrow(Y)-i), ]
    lasso = runxgb_cv(Y.window, indice, lag)
    save.pred[(1+nprev-i), ] = lasso$pred
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
  # predint_cov = mean( 1*((save.pred_intervals[,1] < tail(real,nprev))  & (tail(real,nprev) < save.pred_intervals[,2])   ))
  
  #mean prediction interval width
  # predint_width = mean(save.pred_intervals[,2] - save.pred_intervals[,1])
  
  
  
  
  
  
  
  return(list("pred"= save.pred,
              "errors"= errors#,
              # "save.pred_intervals"= save.pred_intervals,
              #"save.geweke_bart_testpreds"= save.geweke_bart_testpreds,
              #"save.geweke_bart_sigma"= save.geweke_bart_sigma,
              # "save.importance"= save.importance,
              # "save.pip"= save.pip#,
              # "predint_cov"= predint_cov,
              # "predint_width"= predint_width
  ))
  
}

