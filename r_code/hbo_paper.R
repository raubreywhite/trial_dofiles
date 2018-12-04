paperhbo<- function(src="original",tagWithPaperHBO=FALSE){
  files <- list.files(file.path(FOLDER_DATA_RAW,"hbo_paper",src),".xlsx$")
  d <- vector("list",length=length(files))
  
  for(i in seq_along(files)){
    d[[i]] <- readxl::read_excel(
      file.path(FOLDER_DATA_RAW,"hbo_paper",src,files[i]),
      col_types="text"                    
      )
    if(i==1){
      n <- names(d[[i]])
    } else {
      names(d[[i]]) <- n
    }
    d[[i]]$file=files[[i]]
    
  }
  
  d <- rbindlist(d)
  setnames(d,arabicStemR::transliterate(names(d)))
  setnames(d,unlist(ExtractOnlyEnglishLetters(names(d))))
  
  names(d)
  
  unique(d$birthdate)
  d[,birthdate:=gsub("\\\\","-",birthdate)]
  d[,birthdate:=gsub("\\.","-",birthdate)]
  d[,birthdate:=gsub("/","-",birthdate)]
  unique(d$birthdate)
  d[,birthdate:=sub("-([0-9])-", "-0\\1-", birthdate)]
  unique(d$birthdate)
  d[,birthdate:=sub("^([0-9])-", "0\\1-", birthdate)]
  unique(d$birthdate)
  d[stringr::str_detect(birthdate,"^[0-9][0-9][0-9][0-9][0-9]"),
    birthdate:=strftime(as.Date(as.numeric(birthdate),origin="1900-01-01"),format="%d-%m-%Y")]
  unique(d$birthdate)
  
  if(src=="original"){
    d[stringr::str_detect(systolicbp,"/"),flag:=T]
    d[stringr::str_detect(systolicbp,"/"),
      diastolicbp := sub("([0-9]*)/([0-9]*)", "\\2", systolicbp)]
    d[stringr::str_detect(systolicbp,"/"),
      systolicbp := sub("([0-9]*)/([0-9]*)", "\\1", systolicbp)]
    
    d[,birthdate:=as.Date(birthdate,format="%d-%m-%Y")]
  } else {
    d[,birthdate:=as.Date(birthdate,format="%d-%m-%Y")]
  }
  
  if(tagWithPaperHBO){
    for(n in names(d)){
      if(n %in% c("bookevent")) next
      setnames(d,n,sprintf("paperhbo_%s",n))
    }
    
    d[, ident_paperhbo:=TRUE]
  }
  
  return(d)
}

paperhbo_search_for_bookevent <- function(d){
  p <- paperhbo()
  setnames(p,"bookevent","OLD_bookevent")
  setnames(p,"bookdate","OLD_bookdate")
  
  setnames(p,"idnumber","uniqueid")
  setnames(p,"birthdate","eventdate")
  earlyData <- unique(d[ident_dhis2_booking==T,c("motheridno","bookdate","booknum")])
  booklmp <- unique(d[ident_dhis2_booking==T,c("motheridno","bookevent","booknum","booklmp")])
  
  setnames(earlyData,"motheridno","uniqueid")
  setnames(booklmp,"motheridno","uniqueid")
  earlyData[,uniqueid:=as.character(uniqueid)]
  booklmp[,uniqueid:=as.character(uniqueid)]
  
  p2 <- GiveItABookEvent(
    d=p[!is.na(eventdate) & !is.na(uniqueid)],
    booklmp=booklmp,
    earlyData=earlyData,
    id="uniqueid",
    earlyDate="bookdate",
    earlyNum="booknum",
    lateDate="eventdate",
    lengthAfterEarlyEvent=42*7,
    keepbooklmp=FALSE,
    numberOfEventsIfAbnormal=3,
    fileNameForPotentialDuplicates="paperhbo"
  )
  
  names(p2)[!names(p2) %in% names(p)]
  p2[,eventnum:=NULL]
  p2[,booknum:=NULL]
  p[,bookevent:=as.character(NA)]
  
  p3 <- rbind(p[!uniqueid %in% p2$uniqueid],p2)
  nrow(p)
  nrow(p3)
  
  setnames(p3,"uniqueid","idnumber")
  setnames(p3,"eventdate","birthdate")
  
  p3[is.na(bookevent) & !is.na(OLD_bookevent),bookevent:=OLD_bookevent]
  
  # only take the first of duplicates
  # for each bookevent, label it 1, 2, ...., 3 (within bookevent)
  message("FIX THIS DUPLICATED PROBLEMS")
  p3[,bn:=1:.N,by=.(bookevent)]
  p3 <- p3[bn==1]
  p3[,bn:=NULL]
  
  openxlsx::write.xlsx(p3[is.na(bookevent)],
                       file=file.path(
                         FOLDER_DATA_RAW,
                         "hbo_paper",
                         "bookeventsnotfound",
                         "notfound.xlsx"))
  
  openxlsx::write.xlsx(p3[!is.na(bookevent)],
                       file=file.path(
                         FOLDER_DATA_RAW,
                         "hbo_paper",
                         "bookeventsfound",
                         "found.xlsx"))
}


