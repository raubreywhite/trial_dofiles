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
  if("idnumber" %in% names(d)) setnames(d,"idnumber","motheridno")
  d[,motheridno:=as.numeric(motheridno)]
  d[is.na(motheridno),motheridno:=1:.N]
  
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
    d[,file_original:=file]
  } else {
    d[,birthdate:=as.Date(birthdate,format="%d-%m-%Y")]
    d[,booknum:=as.numeric(booknum)]
  }
  
  ###after we ran the data and got the "aggregate function missing"
  ###function went to length instead and numbered the rows 1, 2,3, etc
  ###based on if theyre duplicates, etc
  ###because of the duplicates, did: nrow(d[paperhbo_birthdate_1==2])
  ###to get the number of duplicate cases
  # openxlsx::write.xlsx(d[paperhbo_birthdate_1>=2, 
  #                        c("bookevent",
  #                          "motheridno",
  #                          "bookdate",
  #                          "bookorgname",
  #                          "firstname",
  #                          "fathersname",
  #                          "familyname1")],
  #                      file=file.path(
  #                        FOLDER_DATA_RESULTS_WB,
  #                        sprintf("%s_Duplicate_paperhbos.xlsx",lubridate::today())))
  
  if(tagWithPaperHBO){
    d <- d[is.na(removebecausetwins)]
    for(n in names(d)){
      if(n %in% c("bookevent","eventnum","booknum","motheridno")) next
      setnames(d,n,sprintf("paperhbo_%s",n))
    }
  }
  
  return(unique(d))
}

paperhbo_search_for_bookevent <- function(d){
  p <- paperhbo()
  setnames(p,"bookevent","OLD_bookevent")
  setnames(p,"bookdate","OLD_bookdate")
  
 # setnames(p,"motheridno","uniqueid")
  setnames(p,"birthdate","eventdate")
  earlyData <- unique(d[ident_dhis2_booking==T,c("motheridno","bookdate","booknum")])
  booklmp <- unique(d[ident_dhis2_booking==T,c("motheridno","bookevent","booknum","booklmp")])
  
  #setnames(earlyData,"motheridno","uniqueid")
  #setnames(booklmp,"motheridno","uniqueid")
  earlyData[,motheridno:=as.character(motheridno)]
  booklmp[,motheridno:=as.character(motheridno)]
  
  p[,xxxtempuniqueid:=1:.N]
  
  p2 <- GiveItABookEvent(
    d=p[!is.na(eventdate) & !is.na(motheridno)],
    booklmp=booklmp,
    earlyData=earlyData,
    id="motheridno",
    earlyDate="bookdate",
    earlyNum="booknum",
    lateDate="eventdate",
    lengthAfterEarlyEvent=42*7,
    keepbooklmp=FALSE,
    numberOfEventsIfAbnormal=3,
    fileNameForPotentialDuplicates="paperhbo"
  )
  
  names(p2)[!names(p2) %in% names(p)]
  p[,eventnum:=1]
  p[,booknum:=1]
  p[,bookevent:=as.character(NA)]
  
  p3 <- rbind(p[!xxxtempuniqueid %in% p2$xxxtempuniqueid],p2)
  p3[,xxxtempuniqueid:=NULL]
  nrow(p)
  nrow(p3)
  
  #setnames(p3,"uniqueid","motheridno")
  setnames(p3,"eventdate","birthdate")
 
   
 p3[!is.na(OLD_bookevent),bookevent:=OLD_bookevent]
 # recreating eventnum so that it is compatible with both bookevent and OLD_bookevent
 p3[!is.na(bookevent),eventnum:=as.numeric(1:.N),by=.(motheridno,bookevent,booknum)]
 
 openxlsx::write.xlsx(p3[!is.na(bookevent)],
                      file=file.path(
                        FOLDER_DATA_RAW,
                        "hbo_paper",
                        "bookeventsfound",
                        "found.xlsx"))
 
  p3[is.na(bookevent),bookevent:=sprintf("x-%s",1:.N)]
  
  openxlsx::write.xlsx(p3[is.na(bookevent)],
                       file=file.path(
                         FOLDER_DATA_RAW,
                         "hbo_paper",
                         "bookeventsnotfound",
                         "notfound.xlsx"))
}


