DHIS2_NewbornCare <- function(isControl, earlyData, booklmp, IS_GAZA=IS_GAZA) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for controls")
  
  d <- Get_DHIS2_Data(
    controlName = "Newborn care.csv",
    clinicName = "Newborn care.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 0:00$")]
    d[,eventdate:=as.Date(eventdate, "%m/%d/%Y")]
  } else {
    print("BEFORE DATE FIXING")
    print(sum(!is.na(d$eventdate)))
    
    d[,eventdate:=stringr::str_remove_all(eventdate," 00:00:00.0$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00:00 AM$")]
    d[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
    
    print("AFTER DATE FIXING")
    print(sum(!is.na(d$eventdate)))
  }
  setnames(d, 2, "uniqueid")
  d<- Removeduplicate(d=d,tag="nbc",isControl=isControl)
  
  ####
  nrow(d)
  d <- Removeduplicate(d=d,
                       tag="nbc",
                       isControl=isControl,
                       maxObsPerWomanDate=NULL)
  nrow(d)
  
  nrow(d)
  d <- RemoveEventByFile(d=d, filename="remove_from_dhis2_nbc.xlsx")
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
    lengthAfterEarlyEvent=40*7,
    keepbooklmp=FALSE,
    numberOfEventsIfAbnormal=3,
    fileNameForPotentialDuplicates=sprintf("dhis2_nbc_%s",isControl)
  )
  xtabs(~d$eventnum)
  
  
  d$uniqueid[d$eventnum==5]
  d$uniqueid[d$eventnum==4]
  
  setnames(d,"event","nbcevent")
  setnames(d,"programstage","nbcprogstage")
  d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
  d[,eventdate:=stringr::str_remove_all(eventdate," 0:00$")]
  setnames(d,"eventdate","nbcdate")
  setnames(d,"longitude","nbclong")
  setnames(d,"latitude","nbclat")
  setnames(d,"organisationunitname","nbcorgname")
  setnames(d,"organisationunitcode","nbcorgcode")
  setnames(d,"organisationunit","nbcorgunit")
  setnames(d,"identificationdocumentnumber","nbcidnumber")
  
  nncNames <- names(d)[stringr::str_detect(names(d),"^nnc")]
  nbcNames <- stringr::str_replace_all(nncNames,"^nnc","nbc")
  
  setnames(d,nncNames,nbcNames)
  
  d[,nbcorgname:=unlist(ExtractOnlyEnglishLetters(nbcorgname))]
  
  return(d)
}