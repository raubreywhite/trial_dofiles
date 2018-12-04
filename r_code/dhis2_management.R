DHIS2_Management <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  if(isControl) stop("control code not written for DHIS2_Management")
  
  d <- Get_DHIS2_Data(
    controlName = "Ultrasound.csv",
    clinicName = "ANCManagements.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
  }
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  nrow(d)
  d<- Removeduplicate(d=d,tag="man",isControl=isControl,oneObsPerWomanDate=F)
  nrow(d)
  
  # give it a bookevent
  d <- GiveItABookEvent(
    d=d,
    booklmp=booklmp,
    earlyData=earlyData,
    id="uniqueid",
    earlyDate="bookdate",
    earlyNum="booknum",
    lateDate="eventdate",
    lengthAfterEarlyEvent=42*7,
    keepbooklmp=FALSE
  )
  
  setnames(d,"event","manevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","manprogstage")
  setnames(d,"eventdate","mandate")
  setnames(d,"longitude","manlong")
  setnames(d,"latitude","manlat")
  setnames(d,"organisationunitname","manorgname")
  setnames(d,"organisationunitcode","manorgcode")
  setnames(d,"organisationunit","manorgunit")
  setnames(d,"identificationdocumentnumber","manidnumber")
  varNum <- which(
    stringr::str_detect(names(d),"^ancgestationalageatvisitweeks") | 
      stringr::str_detect(names(d),"^ancgestationaageatvisitweeks"))
  setnames(d,varNum,"mangestage")
  setnames(d,"managementdetails","mandetail")
  setnames(d,"managementtype","mantypex")
  setnames(d,"managementperformed","manperf")
  setnames(d,"createdeventidentifier","mantypey")
  
  
  return(d)
}