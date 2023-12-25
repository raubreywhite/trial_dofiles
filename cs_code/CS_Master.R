CS_Master <- function(keepMotherID=NULL, includeObs=TRUE){
  
  
  files_to_be_read_in_master <- list.files(file.path(FOLDER_DATA_RAW,"cs"),"^[0-9][0-9][0-9][0-9]-[0-9][0-9]")
  # files_to_be_read_in <- files_to_be_read_in_master[files_to_be_read_in>="2018-12"]
  # files_to_be_read_in <- files_to_be_read_in_master[files_to_be_read_in<"2018-12"]
  
 
  print("CS MOTHERS DETAILS")
  a <- CS_Mothers_Details(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master>="2018-12"]
  )
  b <- CS_Mothers_Details(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master<"2018-12"]
  )
  ab <- rbind(b,
                                         a,
                                         fill=T)
  data_CS_Mothers_Details <- CSCreatePregnancyIDAndMakeItWide(ab,tag="amd")
  data_CS_Mothers_Details[,maxDate:=maxDate+10]
  data_CS_Mothers_Details[,ident_avic_amd:=TRUE]
  
  print("CS CAUSE OF CS")
  a <- CS_Cause_Of_CS(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master>="2018-12"]
  )
  b <- CS_Cause_Of_CS(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master<"2018-12"]
  )
  ab <- rbind(b,a, fill=T)
  data_CS_Cause_Of_CS <- CSCreatePregnancyIDAndMakeItWide(ab,tag="acs")
  data_CS_Cause_Of_CS[,ident_avic_acs:=TRUE]
  
  print("CS LAB DETAILS")

  a <- CS_Lab_Details(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master>="2018-12"]
  )
  b <- CS_Lab_Details(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master<"2018-12"]
  )
  a <- a[!is.na(motheridno)]
  b <- b[!is.na(motheridno)]
  
  ab <- rbind(b,a, fill=T)
  data_CS_Lab_Details <- CSCreatePregnancyIDAndMakeItWide(ab,tag="alab")
  data_CS_Lab_Details[,ident_avic_alab:=TRUE]
  
  if(includeObs){
    print("CS OBSERVATIONS")
    a <- CS_Observations(
      keepMotherID=keepMotherID,
      files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master>="2018-12"]
    )
    b <- CS_Observations(
      keepMotherID=keepMotherID,
      files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master<"2018-12"]
    )
    a <- a[!is.na(motheridno)]
    b <- b[!is.na(motheridno)]
    ab <- rbind(b,a, fill=T)
    data_CS_Observations <- CSCreatePregnancyIDAndMakeItWide(ab,tag="aobs")
    data_CS_Observations[,ident_avic_aobs:=TRUE]
  }
  
  
  
  print("CS BABY BIRTH")
  a <- CS_BabyBirth(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master>="2018-12"]
  )
  b <- CS_BabyBirth(
    keepMotherID=keepMotherID,
    files_to_be_read_in = files_to_be_read_in_master[files_to_be_read_in_master<"2018-12"]
  )
  a <- a[!is.na(motheridno)]
  b <- b[!is.na(motheridno)]
  ab <- rbind(b,a,fill=T)
  data_CS_BabyBirth <- CSCreatePregnancyIDAndMakeItWide(ab,tag="abb")
  data_CS_BabyBirth[,ident_avic_abb:=TRUE]
  
  
  
  
  print("Merging data") 
  d <- MergeCSFiles(
    earlyData=data_CS_Mothers_Details,
    lateData=data_CS_Cause_Of_CS,
    earlyTag="amd",
    lateTag="acs"
  )
  
  d <- MergeCSFiles(
    earlyData=d,
    lateData=data_CS_Lab_Details,
    earlyTag="amd",
    lateTag="alab"
  )
  
  if(includeObs){
    d <- MergeCSFiles(
      earlyData=d,
      lateData=data_CS_Observations,
      earlyTag="amd",
      lateTag="aobs"
    )
  }
  
  d <- MergeCSFiles(
    earlyData=d,
    lateData=data_CS_BabyBirth,
    earlyTag="amd",
    lateTag="abb"
  )
  
  setorderv(d,cols=c("motheridno","minDate"))
  d[,avicennanum:=1:.N,by=.(motheridno)]
  
  d[,ident_avic_any:=TRUE]
  
 
  sum(is.na(d$alabtestresult_1))
  nrow(d[alabdate_1<=abbdate_1])
  sum(!is.na(d$mahima_alab_hb_closest_to_delivery_1))
  
  return(d)
}