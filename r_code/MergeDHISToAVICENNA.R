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
  nrow(m)
  return(m)
}


MergeDHISToAVICENNA <- function(dhis,avicenna){
  # merging by motheridno
  m1 <- MergeDHISToAVICENNA_Internal(idvar="motheridno",dhis=dhis,avicenna=avicenna)
  nrow(m1)
  
  # merging by names 1
  dhis[,combined_name1:=GetRidOfEnglish(paste0(
    firstname,
    familyname1,
    fathersname,
    middlename
  ))]
  
  if(FALSE){
    avicenna[,combined_name1:=abbmotherfullname_1]
    avicenna[is.na(combined_name1),combined_name1:=amdmotherfullname_1]
    avicenna[,combined_name1:=GetRidOfEnglish(combined_name1)]
    
    m2 <- MergeDHISToAVICENNA_Internal(idvar="combined_name1",dhis=dhis,avicenna=avicenna)
    nrow(m2)
    ## end merging by names 1
    
    # merging by names 2
    dhis[,combined_name2:=GetRidOfEnglish(paste0(
      firstname,
      familyname2,
      fathersname,
      middlename
    ))]
    
    avicenna[,combined_name2:=abbmotherfullname_1]
    avicenna[is.na(combined_name2),combined_name2:=amdmotherfullname_1]
    avicenna[,combined_name2:=GetRidOfEnglish(combined_name2)]
    
    m3 <- MergeDHISToAVICENNA_Internal(idvar="combined_name2",dhis=dhis,avicenna=avicenna)
    nrow(m3)
    ## end merging by names 2
    
    # merging by names 3
    dhis[,combined_name3:=GetRidOfEnglish(paste0(
      firstname,
      familyname1,
      fathersname,
      middlename
    ))]
    
    avicenna[,combined_name3:=amdmotherfullname_1]
    avicenna[is.na(combined_name3),combined_name3:=abbmotherfullname_1]
    avicenna[,combined_name3:=GetRidOfEnglish(combined_name3)]
    
    m4 <- MergeDHISToAVICENNA_Internal(idvar="combined_name3",dhis=dhis,avicenna=avicenna)
    nrow(m4)
    ## end merging by names 3
    
    # merging by names 4
    dhis[,combined_name4:=GetRidOfEnglish(paste0(
      firstname,
      familyname2,
      fathersname,
      middlename
    ))]
    
    avicenna[,combined_name4:=amdmotherfullname_1]
    avicenna[is.na(combined_name4),combined_name4:=abbmotherfullname_1]
    avicenna[,combined_name4:=GetRidOfEnglish(combined_name4)]
    
    m5 <- MergeDHISToAVICENNA_Internal(idvar="combined_name4",dhis=dhis,avicenna=avicenna)
    nrow(m5)
    ## end merging by names 2
  }

  if(FALSE){
    setnames(m1,"avicennanum","avicennanum_m1")
    setnames(m2,"avicennanum","avicennanum_m2")
    setnames(m3,"avicennanum","avicennanum_m3")
    setnames(m4,"avicennanum","avicennanum_m4")
    setnames(m5,"avicennanum","avicennanum_m5")
    
    nrow(dhis)
    dhis <- merge(dhis,m1,by=c("motheridno","motheridbooknum"),all.x=T)
    nrow(dhis)
    dhis <- merge(dhis,m2,by=c("combined_name1","motheridbooknum"),all.x=T)
    nrow(dhis)
    dhis <- merge(dhis,m3,by=c("combined_name2","motheridbooknum"),all.x=T)
    nrow(dhis)
    dhis <- merge(dhis,m4,by=c("combined_name3","motheridbooknum"),all.x=T)
    nrow(dhis)
    dhis <- merge(dhis,m5,by=c("combined_name4","motheridbooknum"),all.x=T)
    nrow(dhis)
    
    dhis[,ident_avicenna_merge_source:=as.character(NA)]
    dhis[!is.na(avicennanum_m1) & is.na(ident_avicenna_merge_source),
         ident_avicenna_merge_source:="motheridno"]
    dhis[!is.na(avicennanum_m2) & is.na(ident_avicenna_merge_source),
         ident_avicenna_merge_source:="name1"]
    dhis[!is.na(avicennanum_m3) & is.na(ident_avicenna_merge_source),
         ident_avicenna_merge_source:="name2"]
    dhis[!is.na(avicennanum_m4) & is.na(ident_avicenna_merge_source),
         ident_avicenna_merge_source:="name3"]
    dhis[!is.na(avicennanum_m5) & is.na(ident_avicenna_merge_source),
         ident_avicenna_merge_source:="name4"]
    
    xtabs(~dhis$ident_avicenna_merge_source,addNA=T)
  }
  
  d <- merge(dhis,avicenna,by=c("motheridno","avicennanum"),all.x=T)
  
  
  # d[,minDate.y:=NULL]
  # d[,maxDate.y:=NULL]
  # setnames(d,c("minDate.x","maxDate.x"),c("minDate","maxDate"))
  return(d)
}