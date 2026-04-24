library(tidyverse)


load("forecasts/yout.rda")
load("forecasts/rw.rda")


model_files = setdiff(list.files("forecasts/"),c("rw.rda","yout.rda"))


models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts/",model_files[i],sep = ""))
  models_list[[i]] = forecast 
  
}
names(models_list) = model_files

rwe = colMeans(abs(rw[,1:12]-yout[,1]))

errors = Reduce(cbind,lapply(models_list, function(x){colMeans(abs(x[,1:12]-yout[,1]))}))

colnames(errors) = model_files


rweacc = colMeans(abs(rw[,13:15]-yout[,2:4]))

errorsacc = Reduce(cbind,lapply(models_list, function(x){
  colMeans(abs(x[,13:15]-yout[,2:4]),na.rm=TRUE)
}))
colnames(errorsacc) = model_files


vecs_errors = list()
for(i in 1:length(models_list)){
  
  vec = c(errors[,i], errorsacc[,i])
  vecs_errors[[i]] = vec
  
  vec_rwerrors = c(rwe,rweacc)
  
}
mat_errors=do.call(rbind, vecs_errors)

res=matrix(NA, nrow=nrow(mat_errors), ncol=ncol(mat_errors))
for(i in 1:nrow(mat_errors)){
  row=mat_errors[i,]/vec_rwerrors
  res[i,]=row
  
}

colnames(res)=colnames(mat_errors)
rownames(res)=names(models_list)

View(res)
