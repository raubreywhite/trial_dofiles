Get_AVICENNA_Data <- function(folderName,
                              ignoreAttributes=F,
                              takeFirstObs=FALSE,
                              dateName=NULL
                              ){
  attributeList <- d <- vector("list",1000)
  
  i <- 1
  
  #load in the data, but we also store the kind of variables we load in
  attributeList <- NULL
  for(yearMonth in list.files(file.path(FOLDER_DATA_RAW,"avicenna"),"^[0-9][0-9][0-9][0-9]-[0-9][0-9]")){
    for(f in list.files(path=file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName))){
      if(!(stringr::str_detect(f,"xlsx$") | stringr::str_detect(f,"xls$"))) next
      d[[i]] <- readxl::read_excel(
        file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName,f)
      )
      a <- c()
      for(j in 1:ncol(d[[i]])) a <- c(a,class(d[[i]][[j]])[1])
      attributeList[[i]] <- data.table(t(a))
      
      if(!ignoreAttributes){
        ## delete d[[i]] because it takes up too much space
        d[[i]] <- NA
      } else if(takeFirstObs){
        # take out the first observation per woman
        setDT(d[[i]])
        # fixes the broken dates
        d[[i]] <- MakeDataTableNamesLikeStata(d[[i]])
        d[[i]] <- d[[i]][!is.na(motheridno)]
        d[[i]][,xxx_date:=Fix2DigitYear(Fix3LetterMonthToNumber(get(dateName)))]
        # sort the data
        setorder(d[[i]], motheridno, dateprocess)
        # within each person, label their data 1, 2, ..., N (observation num)
        d[[i]][,obsnum:=1:.N,by=motheridno]
        # take the first obs per woman
        d[[i]] <- d[[i]][obsnum==1]
        # delete the new variable names
        d[[i]][,xxx_date:=NULL]
        d[[i]][,obsnum:=NULL]
      }
      
      i <- i + 1
    }
  }

  # if we dont ignore the attributes
  if(!ignoreAttributes){
    attributeList <- rbindlist(attributeList)
    attributeList <- apply(attributeList,2,function(x){names(sort(table(x),decreasing=TRUE)[1])})
    attributeList[attributeList=="character"] <- "text"
    
    i <- 1
    for(yearMonth in list.files(file.path(FOLDER_DATA_RAW,"avicenna"),"^[0-9][0-9][0-9][0-9]-[0-9][0-9]")){
      for(f in list.files(file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName))){
        if(!(stringr::str_detect(f,"xlsx$") | stringr::str_detect(f,"xls$"))) next
        d[[i]] <- readxl::read_excel(
          file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName,f),
          col_types=attributeList
        )
        
        if(takeFirstObs){
          # take out the first observation per woman
          setDT(d[[i]])
          # fixes the broken dates
          d[[i]] <- MakeDataTableNamesLikeStata(d[[i]])
          d[[i]] <- d[[i]][!is.na(motheridno)]
          d[[i]][,xxx_date:=Fix2DigitYear(Fix3LetterMonthToNumber(get(dateName)))]
          # sort the data
          setorder(d[[i]], motheridno, dateprocess)
          # within each person, label their data 1, 2, ..., N (observation num)
          d[[i]][,obsnum:=1:.N,by=motheridno]
          # take the first obs per woman
          d[[i]] <- d[[i]][obsnum==1]
          # delete the new variable names
          d[[i]][,xxx_date:=NULL]
          d[[i]][,obsnum:=NULL]
        }
        
        i <- i + 1
      }
    }
  }
  
  for(i in 1:length(d)){
    if(!is.null(d[[i]])) d[[i]] <- MakeDataTableNamesLikeStata(d[[i]])
    
  }
  d <- rbindlist(d,fill=T)[!is.na(motheridno)]
  
  #d <- MakeDataTableNamesLikeStata(rbindlist(d,fill=T))[!is.na(motheridno)]
  
  ConvertAllFactorsToChar(d)
  d[,motheridno:=as.numeric(motheridno)]
  return(d)
}