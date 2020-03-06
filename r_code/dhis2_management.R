DHIS2_Management <- function(
  isControl, 
  earlyData, 
  booklmp, 
  IS_GAZA=FALSE) {
  
  d <- Get_DHIS2_Data(
    controlName = "Referrals in ANC period.csv",
    clinicName = "ANCManagements.csv",
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
  
  nrow(d)
  d<- Removeduplicate(d=d,tag="man",isControl=isControl,maxObsPerWomanDate=NULL)
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
  
  if(isControl){
    # TAMARA/MERVETT FIX CONTROL VARIABLES HERE
    setnames(d,"event","manevent")
    setnames(d,"programstage","manprogstage")
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 0:00$")]
    setnames(d,"eventdate","mandate")
    setnames(d,"longitude","manlong")
    setnames(d,"latitude","manlat")
    setnames(d,"organisationunitname","manorgname")
    setnames(d,"organisationunitcode","manorgcode")
    setnames(d,"organisationunit","manorgunit")
    d[,manidnumber:=as.numeric(NA)]
    setnames(d,"ancgestationalageatvisitweeks","mangestage")
    setnames(d,"managementtypereferralselector","mantypex")
    setnames(d,"usrecommendationscomments","mandetail")
    setnames(d,"managementdetailsselector","mantypey")
    d[,manperf:=as.integer(NA)]
    
    
   
    
    
    
  } else {
    # intervention stuff is done here
    setnames(d,"event","manevent")
    #setnames(d,"programstageinstance","uniqueid")
    setnames(d,"programstage","manprogstage")
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 0:00$")]
    setnames(d,"eventdate","mandate")
    setnames(d,"longitude","manlong")
    setnames(d,"latitude","manlat")
    setnames(d,"organisationunitname","manorgname")
    setnames(d,"organisationunitcode","manorgcode")
    setnames(d,"organisationunit","manorgunit")
    setnames(d,"identificationdocumentnumber","manidnumber")
    varNum <- which(
      stringr::str_detect(names(d),"^ancgestationalageatvisitweeks") | 
        stringr::str_detect(names(d),"^ancgestationaageatvisitweeks"))
    setnames(d,varNum,"mangestage")
    setnames(d,"managementdetails","mandetail")
    setnames(d,"managementtype","mantypex")
    setnames(d,"managementperformed","manperf")
    setnames(d,"createdeventidentifier","mantypey")
  }
  

  
  return(d)
}