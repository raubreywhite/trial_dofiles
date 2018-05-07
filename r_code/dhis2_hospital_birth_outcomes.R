DHIS2_HospitalBirthOutcomes <- function(isControl, earlyData, booklmp) {
  if(!isControl) stop("clinic code not written for hospital birth outcomes")
  
  d <- Get_DHIS2_Data(
    controlName = "Hospital Birth Outcome.csv",
    clinicName = "",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
 
  setnames(d,"nncgestationalageatdelivery","gestagedeliv")
  setnames(d,"ancpreviouspregnancyoutcome","prevpregoutcome")
  setnames(d,"ancpreviouspregnancybirthweightgram","prevpregbweight")
  setnames(d,"ancsystolicbloodpressuremmhg","systbp")
  setnames(d,"ancdiastolicbloodpressuremmhg","diastbp")
  setnames(d,"ancmodeofpreviousdelivery","modeprevdeliv")
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
    lengthAfterEarlyEvent=40*7,
    keepbooklmp=FALSE
  )
  
  setnames(d,"eventdate","dhis2hbodate")
  
  return(d)
}