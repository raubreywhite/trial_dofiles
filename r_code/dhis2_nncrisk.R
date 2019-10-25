DHIS2_NNCRiskFactors <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "x",
    clinicName = "NNC Risks.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=as.Date(eventdate, "%m/%d/%Y")]
  } else {
    d[,eventdate:=as.Date(eventdate)]
  }
  setnames(d, 2, "uniqueid")
  
  nrow(d)
  d<- Removeduplicate(d=d,tag="nncrisk",isControl=isControl,maxObsPerWomanDate=NULL)
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
  
  setnames(d,"event","nncriskevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","nncriskprogstage")
  setnames(d,"eventdate","nncriskdate")
  setnames(d,"longitude","nncrisklong")
  setnames(d,"latitude","nncrisklat")
  setnames(d,"organisationunitname","nncriskorgname")
  setnames(d,"organisationunitcode","nncriskorgcode")
  setnames(d,"organisationunit","nncriskorgunit")
  setnames(d,"identificationdocumentnumber","nncriskidnumber")
  setnames(d,"riskdescription","nncriskdesx")
  setnames(d,"createdeventidentifier","nncriskdesy")
  
  return(d)
}