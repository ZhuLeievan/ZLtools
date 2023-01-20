zl.batchWilcoxtest <- function(expr, num1, num2){
 if (num1 + num2 != ncol(expr)) {
   print("sample number is wrong!!!only two group needed in the matrix.")
 }
  else{
  res <- list()
  
  for( i in 1:nrow(expr)){
    
    fit <- wilcox.test(as.numeric(expr[i,1:num1]),as.numeric(expr[i,(num1 + 1):(num1 + num2)]))
    res[[i]] <- data.frame(pvalue = fit$p.value,stats = fit$statistic)
  }
  
  res_df <- do.call(rbind, res)
  res_df$gene <- rownames(expr)
  res_df$FDR <- p.adjust(res_df$pvalue, method = "fdr")
  return(res_df)}
}
