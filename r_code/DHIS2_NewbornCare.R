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
    keepbooklmp=FALSE
  )
  xtabs(~d$eventnum)
  d$uniqueid[d$eventnum==4]
  
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
  
  return(d)
}