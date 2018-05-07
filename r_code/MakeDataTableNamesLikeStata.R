MakeDataTableNamesLikeStata <- function(d){
  for (i in names(d)){
    if(sum(names(d)==i)>1){
      locs <- which(names(d)==i)[-1]
      d[[locs]] <- NULL
    }
  }
  for (i in names(d)) setnames(d, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])
  for (i in names(d)){
    if(sum(names(d)==i)>1){
      locs <- which(names(d)==i)[-1]
      d[[locs]] <- NULL
    }
  }
  return(d)
}