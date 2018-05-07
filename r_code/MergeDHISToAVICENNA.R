
MergeDHISToAVICENNA <- function(dhis,avicenna){
  
  earlyDataKey <- na.omit(dhis[,c("motheridno","motheridbooknum","motheridbook_earlyDate","motheridbook_lateDate"),with=F])
  setnames(earlyDataKey,c("motheridbook_earlyDate","motheridbook_lateDate"),c("minDate","maxDate"))
  lateDataKey <- na.omit(avicenna[,c("motheridno","avicennanum","minDate"),with=F])
  
  lateDataKey[,maxDate:=minDate+1]
  earlyDataKey[maxDate<minDate,maxDate:=minDate+1]
  
  setkey(lateDataKey, motheridno, minDate, maxDate)
  setkey(earlyDataKey, motheridno, minDate, maxDate)
  
  m <- foverlaps(lateDataKey, earlyDataKey, by.x=c("motheridno", "minDate", "maxDate"),type="within")
  m <- na.omit(m[,c("motheridno","motheridbooknum","avicennanum"),with=F])
  
  dhis <- merge(dhis,m,by=c("motheridno","motheridbooknum"),all.x=T)
  d <- merge(dhis,avicenna,by=c("motheridno","avicennanum"),all.x=T)
  # d[,minDate.y:=NULL]
  # d[,maxDate.y:=NULL]
  # setnames(d,c("minDate.x","maxDate.x"),c("minDate","maxDate"))
  return(d)
}