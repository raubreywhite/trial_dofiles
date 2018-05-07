ReshapeToWideAndMerge <- function(base,additional,valueVarsRegex,dcastFormula,mergeVars){
  valueVars <- names(additional)
  valueVars <- valueVars[stringr::str_detect(valueVars,valueVarsRegex)]
  w <- dcast.data.table(data=additional[uniqueid %in% unique(base$uniqueid)],
                        formula=as.formula(dcastFormula),
                        value.var = valueVars)
  
  nrow(base)
  mergedData <- merge(base,w,by=mergeVars,all.x=T)
  nrow(mergedData)
  
  return(mergedData)
}