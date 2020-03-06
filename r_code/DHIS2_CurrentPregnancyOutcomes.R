

DHIS2_CurrentPregnancyOutcomes <- function(isControl, earlyData, booklmp, data_ident_dhis2_booking, IS_GAZA=FALSE) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Current pregnancy outcome.csv",
    clinicName = "Current pregnancy outcome.csv",
    isControl=isControl)
  
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 0:00 AM$")]
    d[,eventdate:=as.Date(eventdate, "%m/%d/%Y")]
  } else {
    #d[,eventdate:=as.Date(eventdate)]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00:00 AM$")]
    d[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
    
  }
  setnames(d, 2, "uniqueid")
  
  ####
  nrow(d)
  d <- Removeduplicate(d=d,
                       tag="cpo",
                       isControl=isControl,
                       maxObsPerWomanDate=NULL)
  nrow(d)
  
  nrow(d)
  d <- RemoveEventByFile(d=d, filename="remove_from_dhis2_cpo.xlsx")
  nrow(d)
  
  nrow(d)
  d <- merge(d,data_ident_dhis2_booking,by="uniqueid", all.x=TRUE)
  nrow(d)
  
  nrow(d)
  # for women with no booking, their "book date" is
  # date of demographic creation.
  # this AWLAYS happens after the baby has been born
  # we therefore need to make the "baby birth event"
  # 60 days later, for purposes of matching
  # we then correct this afterwards
  d[ident_dhis2_booking==0,eventdate:=eventdate+60]
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
    fileNameForPotentialDuplicates=sprintf("dhis2_cpo_%s",isControl)
  )
  nrow(d)
  xtabs(~d$eventnum)
  
  d[ident_dhis2_booking==0,eventdate:=eventdate-60]
  d[,ident_dhis2_booking:=NULL]
  
  setnames(d,"event","cpoevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","cpoprogstage")
  setnames(d,"eventdate","cpodate")
  
  setnames(d,"longitude","cpolong")
  setnames(d,"latitude","cpolat")
  setnames(d,"organisationunitname","cpoorgname")
  setnames(d,"organisationunitcode","cpoorgcode")
  setnames(d,"organisationunit","cpoorgunit")
  setnames(d,"identificationdocumentnumber","cpoidnumber")
  setnames(d,"ancpreviouspregnancygestationalageatbirth","cpogestage")
  setnames(d,"ancpreviouspregnancyoutcome","cpopregoutcome")
  setnames(d,"previousplaceofbirth","cpoplaceofbirth")
  setnames(d,"ancmodeofpreviousdelivery","cpomodedelivery")
  setnames(d,"ancgenderofchildfrompreviouspregnancy","cpogender")
  setnames(d,"ancpreviouspregnancybirthweightgram","cpopregbw")
  setnames(d,"anchistoryofgestationaldiabetesmellitusinpreviouspregnancy","cpogestdiab")
  setnames(d,"anchistoryofgestationalhypertensionmellitusinpreviouspregnancy","cpogesthyper")
  setnames(d,"anchistoryofpreeclampsiainpreviouspregnancy","cpopreeclampsia")
  setnames(d,"anchistoryofeclampsiainpreviouspregnancy","cpoeclampsia")
  setnames(d,"anchistoryofpuerperalsepsisinpreviouspregnancy","cpopuerpalsepsis")
  setnames(d,"anchistoryofantepartumhemorrhageinpreviouspregnancy","cpoantepartumhemorrhage")
  setnames(d,"ancpostpartumhemorrhageinpreviouspregnancy","cpopostpartumhemorrhage")
  setnames(d,"anchistoryofdvtinpreviouspregnancy","cpodvt")
  setnames(d,"previouscomplicationsnone","cpocomplicationsnone")
  
  #if d doesnt have any data, tells R to not run it
  warning("WE NEED TO FIX THIS")
  
  if(nrow(d)>0){
  d <- CleanOrgName(data=d,nameToReplace="cpoorgname")
  }
  
  return(d)
}