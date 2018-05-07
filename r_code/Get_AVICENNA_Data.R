Get_AVICENNA_Data <- function(folderName, ignoreAttributes=F){
  attributeList <- d <- vector("list",1000)
  
  i <- 1
  
  attributeList <- NULL
  for(yearMonth in list.files(file.path(FOLDER_DATA_RAW,"avicenna"))){
    for(f in list.files(path=file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName))){
      if(!(stringr::str_detect(f,"xlsx$") | stringr::str_detect(f,"xls$"))) next
      d[[i]] <- readxl::read_excel(
        file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName,f)
      )
      a <- c()
      for(j in 1:ncol(d[[i]])) a <- c(a,class(d[[i]][[j]])[1])
      attributeList[[i]] <- data.table(t(a))
      i <- i + 1
    }
  }

  if(!ignoreAttributes){
    attributeList <- rbindlist(attributeList)
    attributeList <- apply(attributeList,2,function(x){names(sort(table(x),decreasing=TRUE)[1])})
    attributeList[attributeList=="character"] <- "text"
    
    i <- 1
    for(yearMonth in list.files(file.path(FOLDER_DATA_RAW,"avicenna"))){
      for(f in list.files(file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName))){
        if(!(stringr::str_detect(f,"xlsx$") | stringr::str_detect(f,"xls$"))) next
        d[[i]] <- readxl::read_excel(
          file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName,f),
          col_types=attributeList
        )
        i <- i + 1
      }
    }
  }
  d <- MakeDataTableNamesLikeStata(rbindlist(d,fill=T))[!is.na(motheridno)]
  ConvertAllFactorsToChar(d)
  d[,motheridno:=as.numeric(motheridno)]
  return(d)
}