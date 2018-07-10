

DHIS2_CurrentPregnancyOutcomes <- function(isControl, earlyData, booklmp) {
  # if it is a conrol, then throw an error
  if(isControl) stop("control code not written for risk factors")
  
  d <- Get_DHIS2_Data(
    controlName = "Current pregnancy outcome.csv",
    clinicName = "Current pregnancy outcome.csv",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  ####
  d <- DHIS2_Remove_If_All_Cols_Empty(d=d,isControl=isControl)
  
  nrow(d)
  d <- RemoveEventByFile(d=d, filename="remove_from_dhis2_cpo.xlsx")
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
  
  
  return(d)
}