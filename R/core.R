zl.core <- function(otu, num){
  cutoff = num
  gene_filter <- data.frame(otu[which(apply(otu, 1, function(x){length(which(x!= 0))/length(x)}) >= cutoff),])
  }
