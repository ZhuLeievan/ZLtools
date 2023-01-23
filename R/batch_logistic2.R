zl.batch.logistic.binomial <- function(df,group){       #第一列为因变量的表达谱，因变量列名
varsU<-names(otu1[,2:ncol(df)])#自变量
Result<-c()
for (i in 1:length(varsU)){
  fit<-glm(substitute(group~x,list(x=as.name(varsU[i]))),data=df,family=binomial())
  fitSum<-summary(fit)
  result1<-c()
  result1<-rbind(result1,fitSum$coef)
  OR<-exp(fitSum$coef[,'Estimate'])
  result1<-data.frame(cbind(result1,cbind(OR,exp(confint(fit)))))
  result1$Characteristics<-varsU[i]   #添加变量名
  Result<-rbind(Result,result1[-1,])#[-1,],删除常数项
  }
  Uni_log<-data.frame(Result[,c(1,4:8)]) #提取"P","OR","CIlower","CIupper"和变量名
  colnames(Uni_log)[2:5]<-c("P","OR","CIlower","CIupper")#变量重命名
  return(Uni_log)
}
