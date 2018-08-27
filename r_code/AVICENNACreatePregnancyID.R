AVICENNACreatePregnancyIDAndMakeItWide <- function(
  d,
  tag,
  nameofid="motheridno"){
  
  # drop if missing date
  d <- d[!is.na(date)]
  
  setorderv(d,c(nameofid,"date"))
  d[,lastday:=shift(date,type = "lag"),by=get(nameofid)]
  d[is.na(lastday),lastday:=date]
  d[,timeToLast:=as.numeric(difftime(date,lastday,units="days"))]
  d[,increment:=ifelse(timeToLast>100,1,0)]
  d[,eventnum:=cumsum(increment)+1,by=get(nameofid)]
  d[,timeToLast:=NULL]
  d[,increment:=NULL]
  d[,lastday:=NULL]
  
  d[,wideNumber:=1:.N,by=.(get(nameofid),eventnum)]
  wideVars <- names(d)[!names(d)%in%c(nameofid,"eventnum","wideNumber")]
  for(i in wideVars) setnames(d,i,sprintf("%s%s",tag,i))
  
  w <- dcast.data.table(d,as.formula(sprintf("%s+eventnum~wideNumber",nameofid)),value.var = sprintf("%s%s",tag,wideVars))
  setnames(w,"eventnum",sprintf("%seventnum",tag))
  
  w[,minDate:=get(sprintf("%sdate_1",tag))]
  dates <- names(w)[stringr::str_detect(names(w),sprintf("^%sdate",tag))]
  for(i in dates){
    w[!is.na(get(i)),maxDate:=get(i)]  
  }
  
  
  return(w)
}