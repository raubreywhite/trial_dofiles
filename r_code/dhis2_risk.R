DHIS2_RiskFactors <- function(isControl, earlyData, booklmp) {
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Ultrasound.csv",
    clinicName = "ANCRisks.csv",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  d <- DHIS2_Remove_If_All_Cols_Empty(d=d,isControl=isControl)
  
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
  setnames(d,"ancgestationalageatvisitweeks","riskgestage")
  setnames(d,"createdeventidentifier","riskdesy")
  
  return(d)
}