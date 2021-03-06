

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
    print("BEFORE DATE FIXING")
    print(sum(!is.na(d$eventdate)))
    
    d[,eventdate:=stringr::str_remove_all(eventdate," 00:00:00.0$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00:00 AM$")]
    d[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
    
    print("AFTER DATE FIXING")
    print(sum(!is.na(d$eventdate)))
    
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
  
  ## Searching for duplicates
  #if do below code and then choose to only keep num==1, will get rid of ALL dups
  #data_DHIS2_CurrentPregnancyOutcomes[,num:=.N,by=.(bookevent, eventnum)]
  #xtabs(~data_DHIS2_CurrentPregnancyOutcomes$num)
  #cpodups <- data_DHIS2_CurrentPregnancyOutcomes[num>1]
  
  d[,num:=1:.N,by=.(uniqueid, bookevent, booknum, eventnum)]
  
  print("1")
  
  print(xtabs(~d$num))
  #cpodupsInv <- cpodups[num>1,]
  d <- d[num==1]
  d[,num:=NULL]
  
  #if d doesnt have any data, tells R to not run it
  warning("WE NEED TO FIX THIS")
  
  if(nrow(d)>0){
  d <- CleanOrgName(data=d,nameToReplace="cpoorgname", IS_GAZA)
  }
  
  #d[,num:=1:.N,by=.(uniqueid, bookevent, booknum, eventnum)]
  #print("2")
  #print(xtabs(~d$num))
  #cpodupsInv <- cpodups[num>1,]
  #d <- d[num==1]
  #print("3")
  #print(xtabs(~d$num))
  #d[,num:=NULL]
 
  
  return(d)
}