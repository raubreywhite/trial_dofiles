DHIS2_Ultrasound <- function(isControl, earlyData, booklmp, IS_GAZA=FALSE) {
  d <- Get_DHIS2_Data(
    controlName = "Ultrasound.csv",
    clinicName = "Ultrasound results.csv",
    isControl=isControl)
  #unique(d$eventdate)[1:10]
  if(IS_GAZA){
    message("no identification document number -- we create one")
    d[,identificationdocumentnumber:=1:.N]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00 AM$")]
    d[,eventdate:=as.Date(eventdate, "%m/%d/%Y")]
    #unique(d$eventdate)[1:10]
  } else {
    #d[,eventdate:=as.Date(eventdate)]
    d[,eventdate:=stringr::str_remove_all(eventdate," 00:00:00.0 AM$")]
    d[,eventdate:=stringr::str_remove_all(eventdate," 12:00:00 AM$")]
    d[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
  }
  setnames(d, 2, "uniqueid")
  
  d<- Removeduplicate(d=d,tag="ult",isControl=isControl)

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
  
  #sum(is.na(d$booklmp))
  #sum(is.na(d$bookdate))
  #sum(is.na(d$eventdate))
  
  if (isControl) {
    d[,ancgestationalageatvisitweeks:=floor(as.numeric(difftime(eventdate,booklmp,units="days"))/7)]
    d[ancgestationalageatvisitweeks<2 | ancgestationalageatvisitweeks>42,ancgestationalageatvisitweeks:=NA]
    
    d[,identificationdocumentnumber:=as.character(NA)]
    d[,usreason:=as.numeric(NA)]
    d[,usperformanceofultrasound:=as.numeric(NA)]
    d[,usperformedelsewhere:=as.numeric(NA)]
    d[,usectopicpregnancy:=as.numeric(NA)]
    d[,usmolarpregnancy:=as.numeric(NA)]
    d[,usfetusdesignationformultiplepregnancy:=as.numeric(NA)]
    d[,usplacentalocation:=as.numeric(NA)]
    d[,usplacentapreviagrade:=as.numeric(NA)]
    d[,usplacentaorientation:=as.numeric(NA)]
    d[,usfetalsex:=as.numeric(NA)]
    d[,usanomalies:=as.numeric(NA)]
    d[,usanomaliesspecified:=as.numeric(NA)]
    d[,uspelvicmass:=as.numeric(NA)]
    d[,usovariancysts:=as.numeric(NA)]
    d[,usfibroids:=as.numeric(NA)]
  } else {
    
  }
  d[,booklmp:=NULL]
  
  
  setnames(d,"event","usevent")
  NamesToChange(d,badname=c("programstageinstance",
                    "trackedentity"),
              goodname="uniqueid")
  setnames(d,"programstage","usprogstage")
  setnames(d,"eventdate","usdate")
  
  setnames(d,"longitude","uslong")
  setnames(d,"latitude","uslat")
  setnames(d,"organisationunitname","usorgname")
  setnames(d,"organisationunitcode","usorgcode")
  setnames(d,"organisationunit","usorgunit")
  setnames(d,"identificationdocumentnumber","usidnumber")
  varNum <- which(
    stringr::str_detect(names(d),"^ancgestationalageatvisitweeks") | 
      stringr::str_detect(names(d),"^ancgestationaageatvisitweeks"))
  setnames(d,varNum,"usgestage")
  setnames(d,"usreason","usreason")
  setnames(d,"usperformanceofultrasound","usplace")
  setnames(d,"usperformedelsewhere","usplaceother")
  setnames(d,"usectopicpregnancy","usectopic")
  setnames(d,"usmolarpregnancy","usmolar")
  setnames(d,"ancusnumberoffetuses","usnumberfetus")
  setnames(d,"usfetusdesignationformultiplepregnancy","usmultifetdesignation")
  setnames(d,"usfetusheartactivity","usfh")
  setnames(d,"usplacentalocation","usplacenplace")
  setnames(d,"usplacentapreviagrade","usplacenprev")
  setnames(d,"usplacentaorientation","usplacenor")
  setnames(d,"usfetalpresentation","uspres")
  setnames(d,"usfetalsex","ussex")
  setnames(d,"usamnioticfluiddeepestpocket","usamnideeppoc")
  setnames(d,"usamnioticfluidindexcm","usamniindex")
  setnames(d,"usamnioticfluidquantity","usamniquant")
  setnames(d,"usgestationalsac","usgestsac")
  setnames(d,"usgestationalsacinmm","usgestsacmm")
  setnames(d,"usgestationalsacinweeks","usgestsacweek")
  setnames(d,"uscrownrumplengthmm","uscrlmm")
  setnames(d,"uscrownrumplengthweeks","uscrlweeks")
  setnames(d,"usbiparietaldiametermm","usbpdmm")
  setnames(d,"usbiparietaldiameterweeks","usbpdweeks")
  setnames(d,"usfemurlengthmm","usfemurmm")
  setnames(d,"usfemurlengthweeks","usfemurweeks")
  setnames(d,"usabdominalcircumferencemm","usacmm")
  setnames(d,"usabdominalcircumferenceweeks","usacweeks")
  setnames(d,"usestimatedgestationalageegaweeks","usegaweeks")
  setnames(d,"usestimatedgestationalageegadays","usegadays")
  setnames(d,"usestimatedfetalweightgm","usefw")
  setnames(d,"usultrasoundestimateddateofdelivery","usedd")
  setnames(d,"usanomalies","usanom")
  setnames(d,"usanomaliesspecified","usanomspec")
  setnames(d,"uspelvicmass","usmass")
  setnames(d,"usovariancysts","usovcyst")
  setnames(d,"usfibroids","usfibroid")
  setnames(d,"ussuspectedintrauterinegrowthrestrictionsuspectedsmallforgestationalagefetus","usiugr")
  setnames(d,"ussuspectedlargeforgestationalagefetus","uslga")
  setnames(d,"usrecommendationscomments","uscomments")
  
  if(IS_GAZA){
    d[,usedd:=as.Date(usedd, "%m/%d/%Y")]
  } else {
    newdates<-as.Date(d$usedd, "%Y-%m-%d")
    unique(d[is.na(newdates) & !is.na(usedd)]$usedd)
    d[usedd=="",usedd:=NA]
    d[,usedd:=as.Date(usedd)]
  }
  
  return(d)
}