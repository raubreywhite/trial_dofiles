DHIS2_RiskFactors <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Ultrasound.csv",
    clinicName = "ANCRisks.csv",
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
  d<- Removeduplicate(d=d,tag="risk",isControl=isControl,maxObsPerWomanDate=NULL)
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
  
  setnames(d,"event","riskevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","riskprogstage")
  setnames(d,"eventdate","riskdate")
  setnames(d,"longitude","risklong")
  setnames(d,"latitude","risklat")
  setnames(d,"organisationunitname","riskorgname")
  setnames(d,"organisationunitcode","riskorgcode")
  setnames(d,"organisationunit","riskorgunit")
  setnames(d,"identificationdocumentnumber","riskidnumber")
  setnames(d,"ancrisktype","risktype")
  setnames(d,"riskdescription","riskdesx")
  varNum <- which(
    stringr::str_detect(names(d),"^ancgestationalageatvisitweeks") | 
      stringr::str_detect(names(d),"^ancgestationaageatvisitweeks"))
  setnames(d,varNum,"riskgestage")
  setnames(d,"createdeventidentifier","riskdesy")
  
  return(d)
}