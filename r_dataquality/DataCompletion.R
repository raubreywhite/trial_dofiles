DataCompletion <- function(){
  long <- LoadDataLongFromNetworkPal()
  
  # if you want to restrict the dataset, obviously just do it right after:
  # long[XXXXX,
  # the result of this is in wide format, you have two rows, one that
  # is for gaza, one that is for WB
  resN <- long[, lapply(.SD, function(x) length(x)),keyby=.(ident_gaza)]
  # we now need to make this into long format, so we melt it and turn it into
  # long format. We keep "idvars=ident_gaza" so that we have an extra column
  # that retains the information if each row is gaza or WB
  resN <- melt.data.table(resN,id.vars="ident_gaza")
  setnames(resN,"value","resN")
  
  resNA <- long[, lapply(.SD, function(x) sum(is.na(x))),keyby=.(ident_gaza)]
  resNA <- melt.data.table(resNA,id.vars="ident_gaza")
  setnames(resNA,"value","resNA")
  
  resNotNA <- long[, lapply(.SD, function(x) sum(!is.na(x))),keyby=.(ident_gaza)]
  resNotNA <- melt.data.table(resNotNA,id.vars="ident_gaza")
  setnames(resNotNA,"value","resNotNA")
  
  res0 <- long[, lapply(.SD, function(x)if(is.numeric(x[1])){
    sum(x==0,na.rm=T)} else {
      return("0")
    }),keyby=.(ident_gaza)]
  res0 <- melt.data.table(res0,id.vars="ident_gaza")
  setnames(res0,"value","res0")
  
  res <- merge(resN, resNA, by=c("variable","ident_gaza"))
  res <- merge(res, resNotNA, by=c("variable","ident_gaza"))
  res <- merge(res, res0, by=c("variable","ident_gaza"))
  
  res[,key2:=stringr::str_sub(variable,1,2)]
  res[,key3:=stringr::str_sub(variable,1,3)]
  res[,key:=key2]
  res[key %in% "nb", key:=key3]
  
  denoms <- res[stringr::str_detect(variable,"event")]
  denoms <- unique(denoms[,c("key","ident_gaza","resNotNA")])
  setnames(denoms,"resNotNA","denom")
  
  res <- merge(res,denoms,by=c("key","ident_gaza"))
  
  res[,resN:=NULL]
  res[,resNA:=NULL]
  
  res <- res[!variable %in% c("anlong")]
  openxlsx::write.xlsx(res,
                       file.path(FOLDER_DATA_RESULTS_PAL,
                                 sprintf("data_completeness_names_%s.xlsx",lubridate::today())
                                 ))
  
  
}