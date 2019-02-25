DHIS2_NBManagement <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  if(isControl) stop("control code not written for DHIS2_Management")
  
  d <- Get_DHIS2_Data(
    controlName = "X",
    clinicName = "NBC Managements.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=as.Date(eventdate, "%d/%m/%Y")]
  } else {
    d[,eventdate:=as.Date(eventdate)]
  }
  setnames(d, 2, "uniqueid")
  
  nrow(d)
  d<- Removeduplicate(d=d,tag="nbman",isControl=isControl,maxObsPerWomanDate=NULL)
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
  
  setnames(d,"event","nbmanmanevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","nbmanprogstage")
  setnames(d,"eventdate","nbmandate")
  setnames(d,"longitude","nbmanlong")
  setnames(d,"latitude","nbmanlat")
  setnames(d,"organisationunitname","nbmanorgname")
  setnames(d,"organisationunitcode","nbmanorgcode")
  setnames(d,"organisationunit","nbmanorgunit")
  setnames(d,"identificationdocumentnumber","nbmanidnumber")
  if(IS_GAZA){
    if(!"managementdetails" %in% names(d)) d[,managementdetails:=""]
  }
  setnames(d,"managementdetails","nbmandetail")
  setnames(d,"managementtype","nbmantypex")
  setnames(d,"managementperformed","nbmanperf")
  setnames(d,"createdeventidentifier","nbmantypey")
  
  
  return(d)
}