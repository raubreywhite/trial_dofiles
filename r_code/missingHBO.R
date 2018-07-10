MissingHBO <- function(d=NULL){
  if(is.null(d)) d <- LoadDataFileFromNetwork()
  
  SaveWomenWithAbortionsIn2017(d)
  
  abortions <- readRDS(file.path(FOLDER_DATA_MBO,"abortions.RDS"))$bookevent
  
  ppcplaceofdelivery
  
  tokeep <- d[
    isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE &
      is.na(d$ident_avic_any) &
      is.na(d$ident_dhis2_dhis2hbo) &
      is.na(d$ident_hbo),
    c("bookevent",
      "motheridno",
      "bookorgdistrict",
      "bookorgname",
      "bookdate",
      "firstname",
      "fathersname",
      "middlename",
      "familyname1",
      "familyname2",
      "husbandsname",
      "dob",
      "mobile",
      "booklmp",
      "village",
      "bookintendbirth",
      "bookrecobirth",
      "calc_expected_due_delivery",
      "ppcplaceofdelivery_1",
      "ppcplaceofdelivery_2",
      "ppcplaceofdelivery_3",
      "ppcplaceofdelivery_4",
      "cpoplaceofbirth_1",
      "cpoplaceofbirth_2",
      "cpoplaceofbirth_3",
      "cpoplaceofbirth_4"
    )]
  
  #gen IS_ABORTION="ABORTION" if bookevent==0492304932
  tokeep[bookevent %in% abortions, IS_ABORTION:="ABORTION"]
  tokeep[,bookevent:=NULL] # delete bookevent column
  sum(tokeep$IS_ABORTION=="ABORTION",na.rm=T)/nrow(tokeep)
  
  #count if IS_ABORTION=="ABORTION" <- STATA
  #sum(tokeep$IS_ABORTION=="ABORTION",na.rm=T) <- R
  
  identifiedWomen <- tokeep[,c("motheridno","bookdate")]
  identifiedWomen[,identifiedMonth:=yearmonth]
  
  previouslyIdentifiedWomen <- readRDS(file.path(FOLDER_DATA_CLEAN,"identified_women.RDS"))
  
  temp <- merge(identifiedWomen,previouslyIdentifiedWomen,all.x=T,by=c("motheridno","bookdate"))
  newWomen <- temp[is.na(identifiedMonth.y) | identifiedMonth.x==identifiedMonth.y]
  newWomen[,identifiedMonth.x:=NULL] 
  newWomen[,identifiedMonth.y:=NULL] 
  newWomen[,identifiedMonth:=yearmonth]
  
  tokeep <- merge(tokeep,newWomen,by=c("motheridno","bookdate"),all.x=T)
  
  womenNew <- tokeep[!is.na(identifiedMonth)]
  womenOld <- tokeep[is.na(identifiedMonth)]
  
  womenNew[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),lubridate::month(bookdate))]
  womenOld[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),lubridate::month(bookdate))]
  
  for(i in unique(womenOld$bookyearmonth)){
    dir.create(file.path(FOLDER_DATA_MBO,i))
    dataTemp <- womenOld[bookyearmonth==i]
    openxlsx::write.xlsx(dataTemp,
                         file.path(FOLDER_DATA_MBO,i,"Palestine.xlsx"))
    for(j in unique(dataTemp$bookorgdistrict)){
      openxlsx::write.xlsx(dataTemp[bookorgdistrict==j],
                           file.path(FOLDER_DATA_MBO,i,sprintf("%s.xlsx",j)))
    }
  }
  
  if(nrow(womenNew)>0) openxlsx::write.xlsx(womenNew,
                                            file.path(FOLDER_DATA_MBO,"new",sprintf("%s.xlsx",yearmonth)))
  
  saveRDS(rbind(previouslyIdentifiedWomen,newWomen),file.path(FOLDER_DATA_CLEAN,"identified_women.RDS"))
  
}