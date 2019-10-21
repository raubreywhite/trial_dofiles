

DHIS2_DHIS2_PostPartumCare <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Postpartum care.csv",
    clinicName = "Postpartum care.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=as.Date(eventdate, "%d/%m/%Y")]
  } else {
    d[,eventdate:=as.Date(eventdate)]
  }
  setnames(d, 2, "uniqueid")
  d<- Removeduplicate(d=d,tag="ppc",isControl=isControl)
  
  print("Number of ALL PPC bookings")
  
  nrow(d)
  d <- RemoveEventByFile(d=d, filename="remove_from_dhis2_ppc.xlsx")
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
    keepbooklmp=FALSE,
    numberOfEventsIfAbnormal=3,
    fileNameForPotentialDuplicates=sprintf("dhis2_ppc_%s",isControl)
  )
  xtabs(~d$eventnum)
  
  print("Number of women who have a booking AND PPC")
  nrow(d)
  
  setnames(d,"event","ppcevent")
  setnames(d,"ancppcvisitundertakenbywhom","ppcvisitundertakenbywhom")
  setnames(d,"fundalmeasurementcm","ppcfundalmeasurementcm")
  
  
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","ppcprogstage")
  setnames(d,"eventdate","ppcdate")
  
  setnames(d,"longitude","ppclong")
  setnames(d,"latitude","ppclat")
  setnames(d,"organisationunitname","ppcorgname")
  setnames(d,"organisationunitcode","ppcorgcode")
  setnames(d,"organisationunit","ppcorgunit")
  setnames(d,"identificationdocumentnumber","ppcidnumber")
  setnames(d,"cpobirthoutcome","ppcbirthoutcome")
  
  
  d[,ppcorgname:=unlist(ExtractOnlyEnglishLetters(ppcorgname))]
  
  
  return(d)
}