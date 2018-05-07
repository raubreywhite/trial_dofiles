DHIS2_Antenatal <- function(isControl, earlyData, booklmp) {
  FOLDERS <- DHIS2_Folders(isControl = isControl)
  
  d <- Get_DHIS2_Data(
    controlName = "ANC Follow up sheet.csv",
    clinicName = "Antenatal care visit.csv",
    isControl=isControl)
  d[,eventdate:=as.Date(eventdate)]
  #setnames(d, 2, "uniqueid")
  
  
  if (isControl) {
    d[,conabortion:=NULL] 
    d[,ancvaccinatedforttaccordingtoguidelines:=NULL] 
    d[,ancbodyheightm:=NULL] 
    d[,conancgestationaageatvisitweeks:=NULL] 
    d[,conancgestationaageatvisitsize:=NULL] 
    d[,usrecommendationscomments:=NULL] 
    d[,consupplementsyesno:=NULL] 
    d[,conancsupplementprescription:=NULL]
  } else {
    d[,ancsuspectedprematureruptureofmembranesprom:=NULL]
    d[,ancsuspectedpretermprematureruptureofmembranesprom:=NULL]
    d[,anchypertensioncerebralorvisualsymptoms:=NULL]
    d[,anchistoryofdiabetesmellitusioriiinsulindependent:=NULL]
    d[,anchistoryofdeepveinthrombosisdvtoutsidepreviouspregnancies:=NULL]
    d[,anchistoryofotherchronicconditions:=NULL]
    d[,anchistoryofotherchronicconditionspecified:=NULL]
    d[,ancreferralneededforotherchronicconditions:=NULL]
  }
  
  if (isControl) {
    # read in ANC followup, keep the first obs per pregnancy
    #setnames(d, 2, "programstageinstance")
    setorder(d, uniqueid, eventdate)
    d[, visitNum := 1:.N, by = .(uniqueid)]
    d <- d[visitNum != 1]
    d[, visitNum := NULL]
    
    d[,identificationdocumentnumber:=as.character(NA)]
    d[,ancallergiesdrugsandorseverefood:=as.numeric(NA)]
    d[,ancpenicillinallergy:=as.numeric(NA)]
    d[,anchistoryofothermedicineallergy:=as.numeric(NA)]
    d[,ancothermedisineallergyspecified:=as.numeric(NA)]
    d[,anchistroyofothersevereallergy:=as.numeric(NA)]
    d[,ancothersevereallergyspecified:=as.numeric(NA)]
    d[,anchistoryofchronichypertension:=as.numeric(NA)]
    d[,anchistoryofblooddisorder:=as.numeric(NA)]
    d[,ancfetalmovement:=as.numeric(NA)]
    d[,anchypothyreoidism:=as.numeric(NA)]
    d[,anchistoryofblooddisorderspecif:=as.numeric(NA)]
    d[,ancfundalheightmeasurement:=as.numeric(NA)]
    d[,ancedema:=as.numeric(NA)]
    d[,ancreferralneededforotherchronic:=as.numeric(NA)]
    d[,whichancvisitisthis:=as.numeric(NA)]
    d[,ancmedicineprescription:=as.numeric(NA)]
    d[,anchighriskdesignatedwoman:=as.numeric(NA)]
    d[,anccounselingaboutironandfolicac:=as.numeric(NA)]
    d[,ancpreviousepisodesofthrombosis:=as.numeric(NA)]
    d[,anchistoryofklexaneprovidedtothe:=as.numeric(NA)]
    d[,anccounselingaboutdangersignsdur:=as.numeric(NA)]
    d[,anccounselingonnutrioninpregnanc:=as.numeric(NA)]
    d[,anccounselingaboutbreastfeeding:=as.numeric(NA)]
    d[,anccounselingaboutlaborsigns:=as.numeric(NA)]
    d[,anccounselingaboutpkuscreening:=as.numeric(NA)]
    d[,ancppcvisitundertakenbywhom:=as.numeric(NA)]
    d[,ppcwasthisinformationfirstcollec:=as.numeric(NA)]
    d[,anceclampticconvulsions:=as.numeric(NA)]
    d[,ancvaginalbleeding:=as.numeric(NA)]
    d[,ancgestationalageatvisitweeks:=as.numeric(NA)]
    d[,ancotheridentifiedconditions:=as.numeric(NA)]
    d[,anchistoryofrenaldisease:=as.numeric(NA)]
    d[,anchistoryofbronchialasthma:=as.numeric(NA)]
    d[,anchistoryofepilepsy:=as.numeric(NA)]
    d[,anchistoryofcardiacdisease:=as.numeric(NA)]
    d[,anchistoryofmentaldisturbance:=as.numeric(NA)]
    d[,ancreproductivetractinfectionrti:=as.numeric(NA)]
    d[,v42:=as.numeric(NA)]
    d[,anchomevisitoratthehealthclinic:=as.numeric(NA)]
  } else {
   # nothing
  }
  
  setnames(d,"event","anevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","anprogstage")
  setnames(d,"eventdate","andate")
  setnames(d,"longitude","anlong")
  setnames(d,"latitude","anlat")
  setnames(d,"organisationunitname","anorgname")
  setnames(d,"organisationunitcode","anorgcode")
  setnames(d,"organisationunit","anorgunit")
  setnames(d,"identificationdocumentnumber","anidnumber")
  
  #setnames(d,which(stringr::str_detect(names(d),"^ancsuspectedprematureruptureofme")),"anprom")
  #setnames(d,which(stringr::str_detect(names(d),"^ancsuspectedpretermprematurerupt")),"anpprom")
  
  setnames(d,"anceclampticconvulsions","aneclamp")
  setnames(d,"ancvaginalbleeding","anvagbleed")
  setnames(d,"ancfetalmovement","anfetalmove")
  setnames(d,"ancbodyweightkg","anweight")
  setnames(d,"ancdiastolicbloodpressuremmhg","anbpdiast")
  setnames(d,"ancsystolicbloodpressuremmhg","anbpsyst")
  setnames(d,"ancedema","anexamedema")
  setnames(d,"ancfetalheartsoundfhs","anexamfh")
  setnames(d,"ancfundalheightmeasurement","anexamsfh")
  
  #setnames(d,which(stringr::str_detect(names(d),"^anchypertensioncerebralorvisuals")),"anhisthtnsymp")
  
  setnames(d,which(stringr::str_detect(names(d),"^ancfetalpresentationcheckedbypalpation")),"anexampalp")
  setnames(d,which(stringr::str_detect(names(d),"^whichancvisitisthis")),"anvisitweeks")
  setnames(d,which(stringr::str_detect(names(d),"^ancmedicineprescription")),"anmedpres")
  setnames(d,which(stringr::str_detect(names(d),"^ancgestationalageatvisitweeks")),"angestage")
  setnames(d,which(stringr::str_detect(names(d),"^anchighriskdesignatedwoman")),"anhighrisk")
  setnames(d,which(stringr::str_detect(names(d),"^ancotheridentifiedconditions")),"anothercond")
  setnames(d,which(stringr::str_detect(names(d),"^anchistoryofchronichypertension")),"anhisthtn")
  #setnames(d,which(stringr::str_detect(names(d),"^anchistoryofdiabetesmellitusiori")),"anhistdm")
  
  setnames(d,"anchistoryofrenaldisease","anhistrd")
  setnames(d,"anchypothyreoidism","anhisthypothyr")
  setnames(d,"anchistoryofbronchialasthma","anhistasthma")
  setnames(d,"anchistoryofblooddisorder","anhistblood")
  
  setnames(d,which(stringr::str_detect(names(d),"^anchistoryofblooddisorderspecif")),"anhistbloodspec")

  setnames(d,"anchistoryofepilepsy","anhistepi")
  setnames(d,"anchistoryofcardiacdisease","anhistcardiac")
  setnames(d,"anchistoryofmentaldisturbance","anhistpsy")
  setnames(d,"ancreproductivetractinfectionrti","anhistrti")
  
  if(isControl){
    setnames(d,"v42","anhistchronicspec")
    setnames(d,"ancreferralneededforotherchronic","anrefchronic")
  } else {
    d[,anhistchronicspec:=as.character(NA)]
    d[,anrefchronic:=as.character(NA)]
  }
  
  tryCatch({
    setnames(d,"ancallergiesdrugsandorseverefood","anallerfood")
  }, error = function(err) {
    setnames(d,"ancallergiesdrugsandorseverefoodallergies","anallerfood")
  })
  
  setnames(d,"anchistoryofothermedicineallergy","anallerdrug")
  setnames(d,"ancothermedisineallergyspecified","anallerdrugspec")
  setnames(d,"anchistroyofothersevereallergy","anallersev")
  setnames(d,"ancothersevereallergyspecified","anallersevspec")
  setnames(d,"ancpenicillinallergy","anallerpen")
  setnames(d,"ancothermedicationsthewomaniscurrentlytaking","anothermed")
  
  tryCatch({
    setnames(d,"anccounselingaboutironandfolicac","ancounsifa")
  }, error = function(err) {
    setnames(d,"anccounselingaboutironandfolicacidsupplementation","ancounsifa")
  })
  
  setnames(d,"ancpreviousepisodesofthrombosis","anhistthr")
  
  tryCatch({
    setnames(d,"anchistoryofklexaneprovidedtothe","anchistclex")
  }, error = function(err) {
    setnames(d,"anchistoryofklexaneprovidedtothepatient","anchistclex")
  })
  
  setnames(d,"anccounselingaboutlaborsigns","ancounslabor")
  setnames(d,"anccounselingaboutpkuscreening","ancounspku")
  
  tryCatch({
    setnames(d,"anccounselingaboutdangersignsdur","ancounsdanger")
  }, error = function(err) {
    setnames(d,"anccounselingaboutdangersignsduringpregnancy","ancounsdanger")
  })
  
  tryCatch({
    setnames(d,"anccounselingonnutrioninpregnanc","ancounsnut")
  }, error = function(err) {
    setnames(d,"anccounselingonnutrioninpregnancy","ancounsnut")
  })
  
  setnames(d,"anchomevisitoratthehealthclinic","anhomeorclinic")
  
  tryCatch({
    setnames(d,"ppcwasthisinformationfirstcollec","anbackupfile")
  }, error = function(err) {
    setnames(d,"ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem","anbackupfile")
  })
  
  setnames(d,"ancppcvisitundertakenbywhom","anseenby")

  
  setnames(d,"anccounselingaboutbreastfeeding","anccounselingaboutbf")
  #setnames(d,"anchistoryofantepartumhemorrhageinpreviouspregnancy","anchistantparthemprevpreg")
  
  # give it a bookevent
  d <- GiveItABookEvent(
    d=d,
    booklmp=booklmp,
    earlyData=earlyData,
    id="uniqueid",
    earlyDate="bookdate",
    earlyNum="booknum",
    lateDate="andate",
    lengthAfterEarlyEvent=40*7,
    keepbooklmp=FALSE
  )
  
  return(d)
}