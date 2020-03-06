DHIS2_HospitalBirthOutcomes <- function(isControl, earlyData, booklmp) {
  if(!isControl) stop("clinic code not written for hospital birth outcomes")
  
  d <- Get_DHIS2_Data(
    controlName = "Hospital Birth Outcome.csv",
    clinicName = "",
    isControl=isControl)
  print("BEFORE DATE FIXING")
  print(sum(!is.na(d$eventdate)))
  
  d[,eventdate:=stringr::str_remove_all(eventdate," 00:00:00.0$")]
  d[,eventdate:=stringr::str_remove_all(eventdate," 12:00:00 AM$")]
  d[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
  
  print("AFTER DATE FIXING")
  print(sum(!is.na(d$eventdate)))
  
  setnames(d, 2, "uniqueid")
  
  nrow(d)
  d <- Removeduplicate(d=d,
                      tag="dhis2hbo",
                      isControl=isControl,
                      maxObsPerWomanDate=NULL)
  nrow(d)
 
  setnames(d,"nncgestationalageatdelivery","gestagedeliv")
  setnames(d,"ancpreviouspregnancyoutcome","pregoutcome")
  setnames(d,"ancpreviouspregnancybirthweightgram","pregbweight")
  setnames(d,"ancsystolicbloodpressuremmhg","systbp")
  setnames(d,"ancdiastolicbloodpressuremmhg","diastbp")
  setnames(d,"ancmodeofpreviousdelivery","modedeliv")
  setnames(d,"indicationforcsectionmentionedinanycolumn","indicforcsec")
  setnames(d,"labcbcredbloodcellcount","redbloodcellcount")
  setnames(d,"usrecommendationscomments","usrecommendcomment")
  
   
  for(i in names(d)){
    if(i %in% c("event","uniqueid","eventdate")) next
    setnames(d,i,sprintf("dhis2hbo%s",i))
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
    lengthAfterEarlyEvent=42*7,
    keepbooklmp=FALSE
  )
  
  setnames(d,"eventdate","dhis2hbodate")
  
  return(d)
}