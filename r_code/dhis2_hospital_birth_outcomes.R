DHIS2_HospitalBirthOutcomes <- function(isControl, earlyData, booklmp) {
  if(!isControl) stop("clinic code not written for hospital birth outcomes")
  
  d <- Get_DHIS2_Data(
    controlName = "Hospital Birth Outcome.csv",
    clinicName = "",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  for(i in names(d)){
    if(i %in% c("event","uniqueid","eventdate")) next
    setnames(d,i,sprintf("hbo%s",i))
  }
  
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
  
  setnames(d,"eventdate","hbodate")
  
  return(d)
}