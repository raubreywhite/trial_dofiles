ConvertAllFactorsToChar <- function(d){
  for(i in 1:ncol(d)){
    n <- names(d)[i]
    if(is.factor(d[[n]])) d[,(n):=as.character(get(n))]
  }
}