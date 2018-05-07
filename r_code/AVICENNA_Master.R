AVICENNA_Master <- function(){
  print("AVICENNA MOTHERS DETAILS")
  data_AVICENNA_Mothers_Details <- AVICENNA_Mothers_Details()
  data_AVICENNA_Mothers_Details[,ident_avic_amd:=FALSE]
  
  print("AVICENNA CAUSE OF CS")
  data_AVICENNA_Cause_Of_CS <- AVICENNA_Cause_Of_CS()
  data_AVICENNA_Cause_Of_CS[,ident_avic_acs:=FALSE]
  
  print("AVICENNA LAB DETAILS")
  data_AVICENNA_Lab_Details <- AVICENNA_Lab_Details()
  data_AVICENNA_Lab_Details[,ident_avic_alab:=FALSE]
  
  print("AVICENNA OBSERVATIONS")
  data_AVICENNA_Observations <- AVICENNA_Observations()
  data_AVICENNA_Observations[,ident_avic_aobs:=FALSE]
  
  print("AVICENNA BABY BIRTH")
  data_AVICENNA_BabyBirth <- AVICENNA_BabyBirth()
  data_AVICENNA_BabyBirth[,ident_avic_abb:=FALSE]
  
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
  
  d <- MergeAvicennaFiles(
    earlyData=d,
    lateData=data_AVICENNA_Observations,
    earlyTag="amd",
    lateTag="aobs"
  )
  
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