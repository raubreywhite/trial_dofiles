DHIS2_PregnancyClosingNotes <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Pregnancy file closing notes.csv",
    clinicName = "Pregnancy file closing notes.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=as.Date(eventdate, "%m/%d/%Y")]
  } else {
    d[,eventdate:=as.Date(eventdate)]
  }
  setnames(d, 2, "uniqueid")
  
  nrow(d)
  d <- Removeduplicate(d=d,
                       tag="pcn",
                       isControl=isControl,
                       maxObsPerWomanDate=NULL)
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
  
  setnames(d,"event","pcnevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","pcnprogstage")
  d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
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