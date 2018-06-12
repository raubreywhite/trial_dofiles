DHIS2_PreviousPregnancies <- function(isControl, earlyData, booklmp) {
  
  d <- Get_DHIS2_Data(
    controlName = "Previous pregnancy table.csv",
    clinicName = "Previous pregnancies.csv",
    isControl=isControl,
    useFreadControl = F,
    useFreadClinic = T)
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")

  
  if(isControl){
    d[,identificationdocumentnumber:=as.character(NA)]
  } else {
    d[,programstage:=as.character(NA)]
  }
  setnames(d,"event","prevevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","prevprogstage")
  setnames(d,"eventdate","prevdate")
  setnames(d,"longitude","prevlong")
  setnames(d,"latitude","prevlat")
  setnames(d,"organisationunitname","prevorgname")
  setnames(d,"organisationunitcode","prevorgcode")
  setnames(d,"organisationunit","prevorgunit")
  setnames(d,"identificationdocumentnumber","previdnumber")
  setnames(d,"ancpreviouspregnancygestationalageatbirth","prevgestagebirth")
  setnames(d,"ancpreviouspregnancyoutcome","prevoutcome")
  setnames(d,"previousplaceofbirth","prevbirthplace")
  setnames(d,"ancmodeofpreviousdelivery","prevmodedelivery")
  setnames(d,"ancgenderofchildfrompreviouspregnancy","prevgender")
  setnames(d,"ancpreviouspregnancybirthweightgram","prevbirthweight")
  setnames(d,"anchistoryofgestationaldiabetesmellitusinpreviouspregnancy","prevgdm")
  setnames(d,"anchistoryofgestationalhypertensionmellitusinpreviouspregnancy","prevhtn")
  setnames(d,"anchistoryofpreeclampsiainpreviouspregnancy","prevpreeclampsia")
  setnames(d,"anchistoryofeclampsiainpreviouspregnancy","preveclampsia")
  setnames(d,"anchistoryofpuerperalsepsisinpreviouspregnancy","prevpuersep")
  setnames(d,"anchistoryofantepartumhemorrhageinpreviouspregnancy","prevaph")
  setnames(d,"ancpostpartumhemorrhageinpreviouspregnancy","prevpph")
  setnames(d,"anchistoryofdvtinpreviouspregnancy","prevdvt")
  setnames(d,"previouscomplicationsnone","prevnocompl")

  setorderv(d,cols=c("uniqueid","prevdate"))
  d[,eventnum:=1:.N,by=.(uniqueid)] 
  
  return(d)
}