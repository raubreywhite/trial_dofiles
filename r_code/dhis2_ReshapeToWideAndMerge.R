ReshapeToWideAndMerge <- function(base,additional,valueVarsRegex,dcastFormula,mergeVars,identName=NULL){
  if(nrow(additional)==0) return(base)
  
  valueVars <- names(additional)
  valueVars <- valueVars[stringr::str_detect(valueVars,valueVarsRegex)]
  w <- dcast.data.table(data=additional[get(mergeVars[1]) %in% unique(base[[mergeVars[1]]])],
                        formula=as.formula(dcastFormula),
                        value.var = valueVars)
  if(!is.null(identName)) w[,(identName):=TRUE]
  
  nrow(base)
  mergedData <- merge(base,w,by=mergeVars,all.x=T)
  nrow(mergedData)
  
  return(mergedData)
}