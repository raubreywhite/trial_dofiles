NumberOf0 <- function(x){
  if(length(x)==0) return(0)
  if(is.numeric(x[1])){
    sum(x==0,na.rm=T)
  } else {
    return(0)
  }
}

NumberOfEmptyQuotes <- function(x){
  if(length(x)==0) return(0)
  if(is.numeric(x[1])){
    return(0)
  } else if(lubridate::is.Date(x[1])){
    return(0)
  } else {
    sum(x=="",na.rm=T)
  }
}

MeanNo0 <- function(x){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(mean(y))
}

MedianNo0 <- function(x){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(median(y))
}

QuantileNo0 <- function(x,probs){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(quantile(y,probs=probs))
}

DataCompletion <- function(){
  long <- LoadDataLongFromNetworkPal()
  
  # if you want to restrict the dataset, obviously just do it right after:
  # long[XXXXX,
  # the result of this is in wide format, you have two rows, one that
  # is for gaza, one that is for WB
  #resN <- long[, lapply(.SD, function(x) length(x)),keyby=.(ident_gaza)]
  # we now need to make this into long format, so we melt it and turn it into
  # long format. We keep "idvars=ident_gaza" so that we have an extra column
  # that retains the information if each row is gaza or WB
  #resN <- melt.data.table(resN,id.vars="ident_gaza")
  #setnames(resN,"value","resN")
  
  res <- long[, as.list(unlist(lapply(.SD, function(x)
    list(
      xIsNumeric=is.numeric(x),
      xN=length(x),
      xNA=sum(is.na(x)),
      xNotNA=sum(!is.na(x)),
      xNum0=NumberOf0(x),
      xNumEmptyQuotes=NumberOfEmptyQuotes(x),
      xUnique=length(unique(x)),
      xMean=mean(as.numeric(x),na.rm=T),
      xMedian=median(as.numeric(x),na.rm=T),
      xp0=as.numeric(quantile(as.numeric(x),probs=0,na.rm=T)),
      xp1=as.numeric(quantile(as.numeric(x),probs=0.01,na.rm=T)),
      xp5=as.numeric(quantile(as.numeric(x),probs=0.05,na.rm=T)),
      xp25=as.numeric(quantile(as.numeric(x),probs=0.25,na.rm=T)),
      xp75=as.numeric(quantile(as.numeric(x),probs=0.75,na.rm=T)),
      xp95=as.numeric(quantile(as.numeric(x),probs=0.95,na.rm=T)),
      xp99=as.numeric(quantile(as.numeric(x),probs=0.99,na.rm=T)),
      xp100=as.numeric(quantile(as.numeric(x),probs=1,na.rm=T)),
      xMeanNo0=MeanNo0(as.numeric(x)),
      xMedianNo0=MedianNo0(as.numeric(x)),
      xp0No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.0)),
      xp1No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.01)),
      xp5No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.05)),
      xp25No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.25)),
      xp75No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.75)),
      xp95No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.95)),
      xp99No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.99)),
      xp100No0=as.numeric(QuantileNo0(as.numeric(x),probs=1))
      
      # QuantileNo0: ....we do this for all variables just to solve the problem of variables 
      # that have zero and zero means missing not value ...so the best mean for those variables is 
      # mean no 0 but for other variables the best mean is (mean) and this is ugly table , down here 
      #we can make the pretty table for those means 
    )))  
  ),keyby=.(ident_gaza)]
  
  # turn it into long format
  res <- melt.data.table(res, id.vars="ident_gaza")
  # split the variable name into 2 variables ("variable name", and "function")
  res[,variable:=stringr::str_replace(variable,"\\.1\\.","_1.")]
  res[,var:=stringr::str_split_fixed(variable,"\\.",2)[,1]]
  res[,method:=stringr::str_split_fixed(variable,"\\.",2)[,2]]
  
  # put all of the different values into wide format
  res <- dcast.data.table(res,ident_gaza+var~method,value.var = "value")
  
  # make it a bit prettier
  setorder(res,var,-ident_gaza)
  setcolorder(res,c("var","ident_gaza"))
  
  # getting out the denominators (events!!!)
  res[,key2:=stringr::str_sub(var,1,2)]
  res[,key3:=stringr::str_sub(var,1,3)]
  res[,key:=key2]
  res[key %in% "nb", key:=key3]
  
  denoms <- res[stringr::str_detect(var,"event")]
  denoms <- unique(denoms[,c("key","ident_gaza","xNotNA")])
  setnames(denoms,"xNotNA","denom")
  
  # fix denom for booking (because booking is booking+demographics)
  denomsBook <- long[ident_dhis2_booking==T,
                     .(denom=sum(!is.na(bookevent))),
       keyby=.(ident_gaza)]
  denomsBook[,key:="bo"]
  
  # delete the "wrong booking" (which is really booking+demon)
  denoms <- denoms[key!="bo"]
  # add in the "right booking" (which is really just booking)
  denoms <- rbind(denoms,denomsBook)
  
  nrow(res)
  res <- merge(res,denoms,by=c("key","ident_gaza"))
  nrow(res)
  ## end denominators
  
  # doing manual fixing of what is considered to be missing
  res[,notMissing:=xNotNA]
  # for numeric, 0s are missing
  res[xIsNumeric==TRUE & xUnique>5,notMissing:=xNotNA-xNum0]
  # for strings, "" are missing
  res[xIsNumeric==FALSE,notMissing:=xNotNA-xNumEmptyQuotes]
  
  # fix mean, median, p25, and p75 for when 0s are actually missing
  res[xIsNumeric==TRUE & xUnique>5,xMean:=xMeanNo0]
  res[xIsNumeric==TRUE & xUnique>5,xMedian:=xMedianNo0]
  res[xIsNumeric==TRUE & xUnique>5,xp25:=xp25No0]
  res[xIsNumeric==TRUE & xUnique>5,xp75:=xp75No0]
  res[xIsNumeric==TRUE & xUnique>5,xp0:=xp0No0]
  res[xIsNumeric==TRUE & xUnique>5,xp1:=xp1No0]
  res[xIsNumeric==TRUE & xUnique>5,xp5:=xp5No0]
  res[xIsNumeric==TRUE & xUnique>5,xp95:=xp95No0]
  res[xIsNumeric==TRUE & xUnique>5,xp99:=xp99No0]
  res[xIsNumeric==TRUE & xUnique>5,xp100:=xp100No0]

  
  res[,xMeanNo0:=NULL]
  res[,xMedianNo0:=NULL]
  res[,xp25No0:=NULL]
  res[,xp75No0:=NULL]
  res[,xp0No0:=NULL]
  res[,xp1No0:=NULL]
  res[,xp5No0:=NULL]
  res[,xp95No0:=NULL]
  res[,xp99No0:=NULL]
  res[,xp100No0:=NULL]
  
  # delete useless variables
  res[,xN:=NULL]
  res[,xNA:=NULL]
  res[,xNotNA:=NULL]
  res[,xNum0:=NULL]
  
  res[,key2:=NULL]
  res[,key3:=NULL]
  
  res[,xNumEmptyQuotes:=NULL]
  
  # make things pretty
  res[,varType:="Numeric"]
  res[xIsNumeric==0,varType:="String"]
  res[,xIsNumeric:=NULL]
  
  res[,percComplete:=round(100*notMissing/denom,1)]
  res[,xMean:=round(xMean,1)]
  
  res <- res[denom>0]
  
  setorder(res,ident_gaza,key,varType,var)
  setcolorder(res,c(
    "ident_gaza",
    "key",
    "varType",
    "var",
    "percComplete",
    "denom",
    "notMissing"
    ))
  openxlsx::write.xlsx(res,
                       file.path(FOLDER_DROPBOX_RESULTS_PAL,
                                 "data_quality",
                                 "data_completeness.xlsx"
                                 ))
  
  
}