MatchEarlyToLate <- function(earlyData,lateData,id,earlyDate,earlyNum,lateDate,lateNum,lengthAfterEarlyEvent){
  # id <- "uniqueid"
  # earlyDate <- "bookdate"
  # earlyNum <- "booknum"
  # 
  # lateDate <- "eventdate"
  # lateNum <- "eventnum"
  # 
  # lengthAfterEarlyEvent <- 365
  # 
  earlyData[,startDate:=get(earlyDate)]
  earlyData[,endDate:=shift(startDate,n=1L,type="lead"),by=get(id)]
  earlyData[is.na(endDate),endDate:=startDate+lengthAfterEarlyEvent]
  earlyData[endDate>startDate+lengthAfterEarlyEvent,endDate:=startDate+lengthAfterEarlyEvent]
  
  lateData[,startDate:=get(lateDate)]
  lateData[,endDate:=startDate+1]
  
  earlyData[,id:=as.character(get(id))]
  lateData[,id:=as.character(get(id))]
  
  setkey(lateData, id, startDate, endDate)
  setkey(earlyData, id, startDate, endDate)
  
  m <- foverlaps(lateData, earlyData, by.x=c("id", "startDate", "endDate"),type="within")
  m <- na.omit(m[,c(id,earlyNum,lateNum),with=F])
 
  earlyData[,id:=NULL] 
  earlyData[,startDate:=NULL] 
  earlyData[,endDate:=NULL] 
  
  lateData[,id:=NULL] 
  lateData[,startDate:=NULL] 
  lateData[,endDate:=NULL] 
  
  return(m)
}

GiveItABookEvent <- function(
  d,
  booklmp,
  earlyData,
  id,
  earlyDate,
  earlyNum,
  lateDate,
  lengthAfterEarlyEvent,
  keepbooklmp=FALSE
  ){
 
  d[,(id):=as.character(get(id))]
  d[,(lateDate):=as.Date(get(lateDate))] 
  setorderv(d,cols=c(id,lateDate))
  d[,eventnum:=1:.N,by=get(id)]
  
  lateData <- unique(d[,c(id,lateDate,"eventnum"),with=F])
  
  m <- MatchEarlyToLate(
    earlyData=earlyData,
    lateData=lateData,
    id=id,
    earlyDate=earlyDate,
    earlyNum=earlyNum,
    lateDate=lateDate,
    lateNum="eventnum",
    lengthAfterEarlyEvent=lengthAfterEarlyEvent)
  
  nrow(d)
  d <- merge(d,m,by=c(id,"eventnum"))
  nrow(d)
  d <- merge(d,booklmp,by=c(id,earlyNum))
  nrow(d)
  d[,eventnum:=NULL]
  if(!keepbooklmp) d[,booklmp:=NULL]
  
  setorderv(d,cols=c(id,"bookevent","booknum",lateDate))
  d[,eventnum:=1:.N,by=.(uniqueid,bookevent,booknum)]
  if(id!="uniqueid") stop("ERROR ID MUST BE UNIQUEID")
  
  return(d)
}