AVICENNA_Master <- function(keepMotherID, includeObs=FALSE){
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
  
  return(d)
}