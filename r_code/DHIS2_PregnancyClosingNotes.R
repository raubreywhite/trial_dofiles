DHIS2_PregnancyClosingNotes <- function(isControl, earlyData, booklmp) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Pregnancy file closing notes.csv",
    clinicName = "Pregnancy file closing notes.csv",
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
  
  setnames(d,"event","pcnevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","pcnprogstage")
  setnames(d,"eventdate","pcndate")
  
  setnames(d,"longitude","pcnlong")
  setnames(d,"latitude","pcnlat")
  setnames(d,"organisationunitname","pcnorgname")
  setnames(d,"organisationunitcode","pcnorgcode")
  setnames(d,"organisationunit","pcnorgunit")
  setnames(d,"identificationdocumentnumber","pcnidnumber")
  
  setnames(d,"pegnancyfileclosurereason","pcnfileclosurereason")
  
  return(d)
}