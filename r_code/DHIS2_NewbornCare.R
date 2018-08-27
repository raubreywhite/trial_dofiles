DHIS2_NewbornCare <- function(isControl, earlyData, booklmp) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for controls")
  
  d <- Get_DHIS2_Data(
    controlName = "Newborn care.csv",
    clinicName = "Newborn care.csv",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  ####
  nrow(d)
  d <- DHIS2_Remove_If_All_Cols_Empty(d=d,isControl=isControl)
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
  
  return(d)
}