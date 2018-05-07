
HBO_HBO <- function(isControl=T) {
  FOLDERS <- DHIS2_Folders(isControl = isControl)
  
  d <- Get_DHIS2_Data(
    controlName = "Birth Outcome Hospital Birth Outcome.csv",
    clinicName = "Birth Outcome Hospital Birth Outcome.csv",
    isControl=isControl,
    isHBO=T)
  d[,eventdate:=as.Date(eventdate)]
  
  setnames(d, 2, "uniqueid")
  d[,uniqueid:=as.character(uniqueid)]
  
  # for(i in names(d)){
  #   if(i %in% c("event","uniqueid","eventdate")) next
  #   setnames(d,i,sprintf("hbo%s",i))
  # }
  
  d <- d[stringr::str_length(dateofdeliveryhospital)==10]
  d[,dateofdeliveryhospital:=as.Date(dateofdeliveryhospital)]
  d <- d[!is.na(dateofdeliveryhospital)]
  
  
  setnames(d,"nncgestationalageatdelivery","gestagedeliv")
  setnames(d,"ancpreviouspregnancyoutcome","prevpregoutcome")
  setnames(d,"ancpreviouspregnancybirthweightgram","prevpregbweight")
  setnames(d,"ancmodeofpreviousdelivery","modeprevdeliv")
  setnames(d,"indicationforcsectionmentionedinanycolumn","indiccsectioninanycol")
  setnames(d,"conancsystolicbloodpressuremmhg","systbp")
  setnames(d,"conancdiastolicbloodpressuremmhg","diastbp")
  
  warning("drop if hbo_event=='NVmgZxLsHu6'")
  
  return(d)
}

HBO_Demographics <- function(isControl=T) {
  FOLDERS <- DHIS2_Folders(isControl = isControl)
  
  d <- Get_DHIS2_Data(
    controlName = "Demographics.csv",
    clinicName = "Demographics.csv",
    isControl=isControl,
    isHBO=T)
  
  setnames(d, 1, "uniqueid")
  d[,uniqueid:=as.character(uniqueid)]
  
  for(i in names(d)){
    if(i %in% c("event","uniqueid","eventdate")) next
    setnames(d,i,sprintf("hbo%s",i))
  }
  
  return(d)
}

HBO_Master <- function(){
  hbo <- HBO_HBO(isControl=T)
  demo <- HBO_Demographics(isControl=T)
  
  hbo[,date:=dateofdeliveryhospital]
  hbo <- AVICENNACreatePregnancyIDAndMakeItWide(hbo,tag="hbo",nameofid="uniqueid")
  
  d <- merge(demo,hbo,by="uniqueid") 
  setnames(d,"hboidentificationdocumentnumbercontroldata","motheridno")
  d <- d[!is.na(motheridno)]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  setorderv(d,cols=c("motheridno","minDate"))
  d[,avicennanum:=1:.N,by=.(motheridno)]
  d[,maxDate:=NULL]
  d[,uniqueid:=NULL]
  
  d[,ident_hbo:=TRUE]
   
  return(d)
}
