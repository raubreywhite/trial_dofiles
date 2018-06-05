DHIS2_Management <- function(isControl, earlyData, booklmp) {
  if(isControl) stop("control code not written for DHIS2_Management")
  
  d <- Get_DHIS2_Data(
    controlName = "Ultrasound.csv",
    clinicName = "ANCManagements.csv",
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
    lengthAfterEarlyEvent=40*7,
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
  setnames(d,"ancgestationalageatvisitweeks","mangestage")
  setnames(d,"managementdetails","mandetail")
  setnames(d,"managementtype","mantypex")
  setnames(d,"managementperformed","manperf")
  setnames(d,"createdeventidentifier","mantypey")
  
  
  return(d)
}