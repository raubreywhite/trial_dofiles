DHIS2_Lab <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  FOLDERS <- DHIS2_Folders(isControl = isControl)
  
  d <- Get_DHIS2_Data(
    controlName = "Lab results.csv",
    clinicName = "Lab results.csv",
    isControl=isControl)
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
  }
  d[,eventdate:=as.Date(eventdate)]
  setnames(d, 2, "uniqueid")
  
  d<- Removeduplicate(d=d,tag="lab",isControl=isControl)
  
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
    keepbooklmp=TRUE
  )
  
  if (isControl) {
    d[,ancgestationalageatvisitweeks:=floor(as.numeric(difftime(eventdate,booklmp,units="days"))/7)]
    d[ancgestationalageatvisitweeks<2 | ancgestationalageatvisitweeks>42,ancgestationalageatvisitweeks:=NA]
    
    d[,identificationdocumentnumber:=as.character(NA)]
    d[,labwherewerethetestsperformed:=as.numeric(NA)]
    d[,labtestsperformedatotherspecifiedclinic:=as.numeric(NA)]
    d[,ancorppcvisit:=as.numeric(NA)]
    d[,labcbcwhitebloodcells:=as.numeric(NA)]
    d[,labcbcredbloodcellcount:=as.numeric(NA)]
    d[,labanemiatreatmentresponse:=as.numeric(NA)]
    d[,labcbcmeancorpuscularvolume:=as.numeric(NA)]
    d[,labcbcplatelets:=as.numeric(NA)]
    d[,labsecondopiniononinitiatedtreatmentforuti:=as.numeric(NA)]
    d[,laboralglucosechallengetestogctmgdl:=as.numeric(NA)]
  } else {
    d[,usrecommendationscomments:=as.character(NA)]
  }
  d[,booklmp:=NULL]
  
  setnames(d,"event","labevent")
  #setnames(d,"programstageinstance","uniqueid")
  setnames(d,"programstage","labprogstage")
  setnames(d,"eventdate","labdate")
  setnames(d,"longitude","lablong")
  setnames(d,"latitude","lablat")
  setnames(d,"organisationunitname","laborgname")
  setnames(d,"organisationunitcode","laborgcode")
  setnames(d,"organisationunit","laborgunit")
  setnames(d,"identificationdocumentnumber","labidnumber")
  varNum <- which(
    stringr::str_detect(names(d),"^ancgestationalageatvisitweeks") | 
    stringr::str_detect(names(d),"^ancgestationaageatvisitweeks"))
  setnames(d,varNum,"labgestage")
  setnames(d,"labwherewerethetestsperformed","labplace")
  setnames(d,"labtestsperformedatotherspecifiedclinic","labplacespec")
  setnames(d,"ancorppcvisit","labanpp")
  setnames(d,"labcbcwhitebloodcells","labwbc")
  setnames(d,"labcbcredbloodcellcount","labrbc")
  setnames(d,"labcbchemoglobin","labhb")
  setnames(d,"labanemiatreatmentresponse","labanemresp")
  setnames(d,"labcbchematocrit","labhct")
  setnames(d,"labcbcmeancorpuscularvolume","labmcv")
  setnames(d,"labcbcplatelets","labplatelets")
  setnames(d,"labbloodgrouping","labbloodgp")
  setnames(d,"labrhdtyping","labrh")
  setnames(d,"labindirectcoombs","labict")
  setnames(d,"laburinestickprotein","laburpro")
  setnames(d,"laburinesticksugar","laburglu")
  setnames(d,"ancurineanalysisforurinarytrackinfectionuti","laburuti")
  setnames(d,"labsecondopiniononinitiatedtreatmentforuti","laburutirep")
  setnames(d,"labrandombloodsugarmgdl","labbloodglu")
  setnames(d,"labfastingbloodsugarmgdl","labfastbloodglu")
  setnames(d,"laboralglucosechallengetestogctmgdl","labogct")
  setnames(d,"ancotherlabtest1","labother1")
  setnames(d,"anclabresultofotherlabtest1","labotherres1")
  setnames(d,"ancotherlabtest2","labother2")
  setnames(d,"anclabresultofotherlabtest2","labotherres2")
  setnames(d,"ancotherlabtest3","labother3")
  setnames(d,"anclabresultofotherlabtest3","labotherres3")
  setnames(d,"usrecommendationscomments","labrecommendationscomments")
  
  return(d)
}