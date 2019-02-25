AVICENNA_Master <- function(keepMotherID, includeObs=TRUE){
  print("AVICENNA MOTHERS DETAILS")
  data_AVICENNA_Mothers_Details <- AVICENNA_Mothers_Details(keepMotherID=keepMotherID)
  data_AVICENNA_Mothers_Details[,ident_avic_amd:=TRUE]
  
  print("AVICENNA CAUSE OF CS")
  data_AVICENNA_Cause_Of_CS <- AVICENNA_Cause_Of_CS(keepMotherID=keepMotherID)
  data_AVICENNA_Cause_Of_CS[,ident_avic_acs:=TRUE]
  
  print("AVICENNA LAB DETAILS")
  data_AVICENNA_Lab_Details <- AVICENNA_Lab_Details(keepMotherID=keepMotherID)
  data_AVICENNA_Lab_Details[,ident_avic_alab:=TRUE]
  
  if(includeObs){
    print("AVICENNA OBSERVATIONS")
    data_AVICENNA_Observations <- AVICENNA_Observations(keepMotherID=keepMotherID)
    data_AVICENNA_Observations[,ident_avic_aobs:=TRUE]
  }
  
  print("AVICENNA BABY BIRTH")
  data_AVICENNA_BabyBirth <- AVICENNA_BabyBirth(keepMotherID=keepMotherID)
  data_AVICENNA_BabyBirth[,ident_avic_abb:=TRUE]
  
  d <- MergeAvicennaFiles(
    earlyData=data_AVICENNA_Mothers_Details,
    lateData=data_AVICENNA_Cause_Of_CS,
    earlyTag="amd",
    lateTag="acs"
  )
  
  d <- MergeAvicennaFiles(
    earlyData=d,
    lateData=data_AVICENNA_Lab_Details,
    earlyTag="amd",
    lateTag="alab"
  )
  
  if(includeObs){
    d <- MergeAvicennaFiles(
      earlyData=d,
      lateData=data_AVICENNA_Observations,
      earlyTag="amd",
      lateTag="aobs"
    )
  }
  
  d <- MergeAvicennaFiles(
    earlyData=d,
    lateData=data_AVICENNA_BabyBirth,
    earlyTag="amd",
    lateTag="abb"
  )
  
  setorderv(d,cols=c("motheridno","minDate"))
  d[,avicennanum:=1:.N,by=.(motheridno)]
  
  d[,ident_avic_any:=TRUE]
  
  nam <- rev(names(d)[stringr::str_detect(names(d),"^alabdate_[0-9]*$")])
  num <- stringr::str_remove(nam,"alabdate_")
  
  d[,mahima_alab_hb_closest_to_delivery_1:=as.character(NA)]
  for(i in num){
    print(i)
    d[is.na(mahima_alab_hb_closest_to_delivery_1) & 
        get(sprintf("alabdate_%s",i))<=abbdate_1 & 
        !is.na(abbdate_1) & 
        !is.na(get(sprintf("alabdate_%s",i))),
      mahima_alab_hb_closest_to_delivery_1:=get(sprintf("alabtestresult_%s",i))]
    
  }
  sum(is.na(d$alabtestresult_1))
  nrow(d[alabdate_1<=abbdate_1])
  sum(!is.na(d$mahima_alab_hb_closest_to_delivery_1))
  
  return(d)
}