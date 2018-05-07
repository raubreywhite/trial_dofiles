
MergeAvicennaFiles <- function(earlyData,lateData,earlyTag,lateTag){
  
  earlyDataKey <- na.omit(earlyData[,c("motheridno",sprintf("%seventnum",earlyTag),"minDate","maxDate"),with=F])
  lateDataKey <- na.omit(lateData[,c("motheridno",sprintf("%seventnum",lateTag),"minDate","maxDate"),with=F])
  
  lateDataKey[maxDate<minDate,maxDate:=minDate+1]
  earlyDataKey[maxDate<minDate,maxDate:=minDate+1]
  
  setkey(lateDataKey, motheridno, minDate, maxDate)
  setkey(earlyDataKey, motheridno, minDate, maxDate)
  
  m <- foverlaps(lateDataKey, earlyDataKey, by.x=c("motheridno", "minDate", "maxDate"),type="within")
  m <- na.omit(m[,c("motheridno",sprintf("%seventnum",earlyTag),sprintf("%seventnum",lateTag)),with=F])
  
  earlyData <- merge(earlyData,m,by=c("motheridno",sprintf("%seventnum",earlyTag)),all.x=T)
  d <- merge(earlyData,lateData,by=c("motheridno",sprintf("%seventnum",lateTag)),all.x=T)
  d[,minDate.y:=NULL]
  d[,maxDate.y:=NULL]
  setnames(d,c("minDate.x","maxDate.x"),c("minDate","maxDate"))
  
  return(d)
}