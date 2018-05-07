DHIS2_Folders <- function(isControl){
  if(isControl){
    DATA_DATE <- CLINIC_CONTROL_DATE
    FOLDER_DATA <- sprintf("%s/e.reg-control/%s",FOLDER_DATA_RAW,DATA_DATE)
    TAG <- "CON"
    CLINICAL_OR_CONTROL <- "Control"
  } else {
    DATA_DATE <- CLINIC_INTERVENTION_DATE
    FOLDER_DATA <- sprintf("%s/e.reg-intervention/%s",FOLDER_DATA_RAW,DATA_DATE)
    TAG <- "INT"
    CLINICAL_OR_CONTROL <- "Clinical"
  }
  return(list(
    "DATA_DATE"=DATA_DATE,
    "FOLDER_DATA"=FOLDER_DATA,
    "TAG"=TAG,
    "CLINICAL_OR_CONTROL"=CLINICAL_OR_CONTROL))
}

Get_DHIS2_Data <- function(controlName,
                           clinicName,
                           isControl,useFreadControl=F,useFreadClinic=F, isHBO=F, setuniqueid=T){
  FOLDERS <- DHIS2_Folders(isControl = isControl)
  
  if(useFreadControl){
    controlRead <- fread
  } else {
    controlRead <- read.csv
  }
  
  if(useFreadClinic){
    clinicRead <- fread
  } else {
    clinicRead <- read.csv
  }
  
  if(isHBO){
    FOLDERS$CLINICAL_OR_CONTROL <- "Hospital"
  }
  
  if(isControl){
    d <- controlRead(
      sprintf(
        "%s/%s %s",
        FOLDERS$FOLDER_DATA,
        FOLDERS$CLINICAL_OR_CONTROL,
        controlName
      ),
      encoding = "UTF-8"
    )
    setDT(d)
  } else {
    d <- clinicRead(
      sprintf(
        "%s/%s %s",
        FOLDERS$FOLDER_DATA,
        FOLDERS$CLINICAL_OR_CONTROL,
        clinicName
      ),
      encoding = "UTF-8"
    )
    setDT(d)
  }
  
  for (i in names(d)){
    if(sum(names(d)==i)>1){
      locs <- which(names(d)==i)[-1]
      d[[locs]] <- NULL
    }
  }
  for (i in names(d)) setnames(d, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])
  for (i in names(d)){
    if(sum(names(d)==i)>1){
      locs <- which(names(d)==i)[-1]
      d[[locs]] <- NULL
    }
  }
  
  ConvertAllFactorsToChar(d)
  
  if(!isHBO & setuniqueid){
    setnames(d, 2, "uniqueid")
    d[,uniqueid:=as.character(uniqueid)]
  }
  
  return(d)
}