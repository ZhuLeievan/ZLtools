zl.get.relative.abundance <- function(otu){
  otu_precent <- as.data.frame(apply(otu, 2, function(x){x/sum(x)}))
}
