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
  setorderv(earlyData,c(id,earlyDate))
  earlyData[,startDate:=get(earlyDate)]
  earlyData[,endDate:=shift(startDate,n=1L,type="lead"),by=get(id)]
  # those two lines describe if the enddate will be not available or more than 42 weeks (that means the next pregnancy)
  earlyData[is.na(endDate),endDate:=startDate+lengthAfterEarlyEvent]
  earlyData[endDate>startDate+lengthAfterEarlyEvent,endDate:=startDate+lengthAfterEarlyEvent]
  
  # the "late stuff" e.g. ultrasound, bp, birth happens in 1 day
  lateData[,startDate:=get(lateDate)]
  lateData[,endDate:=startDate+1]
  
  earlyData[,id:=as.character(get(id))]
  lateData[,id:=as.character(get(id))]
  
  # tell R what the id is, the start date, and the end date
  setkey(lateData, id, startDate, endDate)
  setkey(earlyData, id, startDate, endDate)
  
  # do the merging
  # this is the same as "merge", except it lets us merge on date ranges
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
  keepbooklmp=FALSE,
  numberOfEventsIfAbnormal=NULL,
  fileNameForPotentialDuplicates=NULL
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
  
  #temp <- merge(d,m,by=c(id,"eventnum"),all.x=T)
  #temp[is.na(booknum)]$uniqueid[1]
  #d[uniqueid=="A04QuXDWMfh"]
  #earlyData[uniqueid=="A04QuXDWMfh"]
  
  nrow(m)
  nrow(d)
  d <- merge(d,m,by=c(id,"eventnum"))
  nrow(d)
  d <- merge(d,booklmp,by=c(id,earlyNum))
  nrow(d)
  d[,eventnum:=NULL]
  if(!keepbooklmp) d[,booklmp:=NULL]
  
  setorderv(d,cols=c(id,"bookevent","booknum",lateDate))
  d[,eventnum:=1:.N,by=.(get(id),bookevent,booknum)]
  #if(id!="uniqueid") stop("ERROR ID MUST BE UNIQUEID")
  
  if(!is.null(numberOfEventsIfAbnormal) & !is.null(fileNameForPotentialDuplicates)){
    # automatically pull out all the rows
    # for women with more than numberOfEventsIfNormal events!
    if(nrow(d[eventnum>=numberOfEventsIfAbnormal])>0){
      badWomen <- unique(d[eventnum>=numberOfEventsIfAbnormal][[id]])
      
      openxlsx::write.xlsx(
        d[get(id) %in% badWomen],
        file=file.path(
          FOLDER_DATA_RAW,
          "possible_duplicates",
          sprintf("%s_%s.xlsx",DATA_DATE,fileNameForPotentialDuplicates))
      )
    }
  }
  
  return(d)
}