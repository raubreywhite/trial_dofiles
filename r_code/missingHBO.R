MissingHBOInternal <- function(d){
  SaveWomenWithAbortionsIn2017(d)
  
  abortions <- readRDS(file.path(FOLDER_DATA_MBO,"abortions.RDS"))$bookevent

  tokeep <- d[
    ident_dhis2_booking==1 & 
    isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE &
      is.na(d$ident_avic_any) &
      is.na(d$ident_dhis2_dhis2hbo) &
      is.na(d$ident_hbo) &
      is.na(d$ident_paperhbo),
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
      "calc_expected_due_delivery"
    )]
  
  #gen IS_ABORTION="ABORTION" if bookevent==0492304932
  tokeep[bookevent %in% abortions, IS_ABORTION:="ABORTION"]
  sum(tokeep$IS_ABORTION=="ABORTION",na.rm=T)/nrow(tokeep)
  
  return(tokeep)
}

MissingHBO <- function(){
  print("REMOVING D FROM MEMORY AS IT TAKES UP TOO MUCH SPACE")
  # delete the dataset "d" to make space in the memory
  rm("d", envir=.GlobalEnv)
  
  # garbage cleaning
  gc()
  
  # identify the two datafiles we need
  data_files <- IdentifyArchivedData()
  
  ## BAD CODE (bad for memory!!)
  # d1 <- readRDS(data_files[1])
  # d2 <- readRDS(data_files[2])
  # tokeep_new <- MissingHBOInternal(d=d1)
  # tokeep_old <- MissingHBOInternal(d=d2)
  
  # GOOD CODE
  # get missing HBO from previous export
  tokeep_old <- MissingHBOInternal(d=readRDS(data_files[2]))
  abortions_old <- SaveWomenWithAbortionsIn2017(d=readRDS(data_files[2]))
  # get missing HBO from this export
  tokeep_new <- MissingHBOInternal(d=readRDS(data_files[1]))
  abortions_new <- SaveWomenWithAbortionsIn2017(d=readRDS(data_files[1]))
  
  # identify the new people
  tokeep_new[,is_new:=FALSE]
  tokeep_new[!bookevent %in% tokeep_old$bookevent,is_new:=TRUE]
  
  # identify the new people
  abortions_new[,is_new:=FALSE]
  abortions_new[!bookevent %in% abortions_old$bookevent,is_new:=TRUE]
  
  # here, we can see that the number of missing HBO is not consistent between the three
  # i.e. nrow(tokeep_new)-nrow(tokeep_old) is not equal to sum(tokeep_new$is_new)
  # this is because we "lose" people from tokeep_old because we fill in their HBOs
  # (i.e. the entire purpose of this exercise)
  #nrow(tokeep_old)
  #nrow(tokeep_new)
  #xtabs(~tokeep_new$is_new)
  
  #tokeep_old[!bookevent %in% tokeep_new$bookevent,is_lost:=TRUE]
  #xtabs(~tokeep_old$is_lost)
  #tokeep_old[is_lost==TRUE]$bookevent
  #tokeep[,bookevent:=NULL] # delete bookevent column
  
  womenNew <- tokeep_new[is_new==TRUE]
  womenOld <- tokeep_new[is_new==FALSE]
  womenNew[,bookyearmonth:=YearMonth(bookdate)]
  womenOld[,bookyearmonth:=YearMonth(bookdate)]
  
  
  abortionsNew <- abortions_new[is_new==TRUE]
  abortionsOld <- abortions_new[is_new==FALSE]
  abortionsNew[,bookyearmonth:=YearMonth(bookdate)]
  abortionsOld[,bookyearmonth:=YearMonth(bookdate)]
  
  
  unlink(FOLDER_DATA_MBO,force=T,recursive = T)
  Sys.sleep(5) # we need to make the computer "sleep" (i.e. pause)
  #because otherwise the server wont respond, and these folders wont get created
  dir.create(FOLDER_DATA_MBO)
  Sys.sleep(5)
  dir.create(file.path(FOLDER_DATA_MBO,"new"))
  Sys.sleep(5)
  for(i in sort(unique(womenOld$bookyearmonth))){
    Sys.sleep(5)
    dir.create(file.path(FOLDER_DATA_MBO,i))
    dataTemp <- womenOld[bookyearmonth==i]
    openxlsx::write.xlsx(dataTemp,
                         file.path(FOLDER_DATA_MBO,i,"Palestine.xlsx"))
    for(j in unique(dataTemp$bookorgdistrict)){
      openxlsx::write.xlsx(dataTemp[bookorgdistrict==j],
                           file.path(FOLDER_DATA_MBO,i,sprintf("%s.xlsx",j)))
    }
  }
  
  if(nrow(womenNew)>0){
    setorder(womenNew,bookyearmonth)
    openxlsx::write.xlsx(womenNew,
                       file.path(FOLDER_DATA_MBO,
                                 "new",
                                 sprintf("%s.xlsx",yearmonth)))
    
  }
 
  
  setorder(abortionsOld,ident_TRIAL_1_clinics,bookyearmonth)
  openxlsx::write.xlsx(abortionsOld,
                       file.path(FOLDER_DATA_MBO,"abortions_old.xlsx"))
  if(nrow(abortionsNew)>0){
    setorder(abortionsNew,ident_TRIAL_1_clinics,bookyearmonth)
    openxlsx::write.xlsx(abortionsNew,
                       file.path(FOLDER_DATA_MBO,"abortions_new.xlsx"))
  }
  
}





