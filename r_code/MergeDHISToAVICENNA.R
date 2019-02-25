MergeDHISToAVICENNA_Internal <- function(idvar="motheridno",dhis,avicenna){
  # merging by motheridno
  earlyDataKey <- na.omit(dhis[,c(idvar,"motheridbooknum","motheridbook_earlyDate","motheridbook_lateDate"),with=F])
  setnames(earlyDataKey,c("motheridbook_earlyDate","motheridbook_lateDate"),c("minDate","maxDate"))
  lateDataKey <- na.omit(avicenna[,c(idvar,"avicennanum","minDate"),with=F])
  
  lateDataKey[,maxDate:=minDate+1]
  earlyDataKey[maxDate<minDate,maxDate:=minDate+1]
  
  setkeyv(lateDataKey, c(idvar, "minDate", "maxDate"))
  setkeyv(earlyDataKey, c(idvar, "minDate", "maxDate"))
  
  m <- foverlaps(lateDataKey, earlyDataKey, by.x=c(idvar, "minDate", "maxDate"),type="within")
  m <- na.omit(m[,c(idvar,"motheridbooknum","avicennanum"),with=F])
  
  # make sure we arent matching multiple times
  # How many times does a woman's booknum get matched to avicenna?
  m[,x:=1:.N,by=.(motheridno,motheridbooknum)]
  # make sure we only take 1 avicenna per woman's booknum
  m <- m[x==1]
  # How many times does a woman's avicenna num get matched to dhis2?
  m[,x:=1:.N,by=.(motheridno,avicennanum)]
  # make sure we only take 1 booknum per woman's avicenna
  m <- m[x==1]
  m[,x:=NULL]
  nrow(m)
  return(m)
}


MergeDHISToAVICENNA <- function(dhis,avicenna){
  print("****MergeDHISToAVICENNA 1")
  # merging by motheridno
  m1 <- MergeDHISToAVICENNA_Internal(idvar="motheridno",
                                     dhis=dhis,
                                     avicenna=avicenna)
  nrow(m1)
  
  print("****MergeDHISToAVICENNA 2")
  nrow(dhis)
  dhis <- merge(dhis,m1,by=c("motheridno","motheridbooknum"),all.x=T)
  nrow(dhis)

  print("****MergeDHISToAVICENNA 3")
  d <- merge(dhis,avicenna,by=c("motheridno","avicennanum"),all.x=T)
  nrow(d)  
  
  # d[,minDate.y:=NULL]
  # d[,maxDate.y:=NULL]
  # setnames(d,c("minDate.x","maxDate.x"),c("minDate","maxDate"))
  return(d)
}