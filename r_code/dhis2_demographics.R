DHIS2_Demographics <- function(isControl, IS_GAZA=F){
  
  d <- Get_DHIS2_Data(
    controlName = "Demographics.csv",
    clinicName = "Demographics.csv",
    isControl=isControl,
    setuniqueid=F)
  
 
  
  if(isControl){
    setnames(d,"instance","uniqueid")
    setnames(d,"created","datecreated")
    setnames(d,"lastupdated","dateupdated")
    setnames(d,"organisationunit","demoorgunit")
    setnames(d,"organisationunitname","demoorgname")
    if("trackedentitytype" %in% names(d)) setnames(d, "trackedentitytype", "trackedentity")
    if(!"trackedentity" %in% names(d)) stop("cant find trackedentity")
    setnames(d,"inactive","dummy")
    setnames(d,"identificationdocumenttype","idtype")
    setnames(d,"dataextractorusername","dataextractor")
    setnames(d,"identificationdocumentnumbercontroldata","demoidnumber")
    
    setnames(d,which(stringr::str_detect(names(d),"^firstname")),"firstname")
    setnames(d,which(stringr::str_detect(names(d),"^fathersname")),"fathersname")
    setnames(d,which(stringr::str_detect(names(d),"^husbandsfamilyname")),"familyname2")
    setnames(d,which(stringr::str_detect(names(d),"^husbandsname")),"husbandsname")
    setnames(d,which(stringr::str_detect(names(d),"^middlename")),"middlename")
    setnames(d,which(stringr::str_detect(names(d),"^womanfamilyname")),"familyname1")
    
    setnames(d,"village","village")
    setnames(d,"city","city")
    setnames(d,"dateofbirth","dob")
    #setnames(d,"mobilenumber", "mobile")
    #print(names(d))
    #if("mobilenumber" %in% names(d)) setnames(d, "mobilenumber(withareacode)", "education")
    NamesToChange(d,badname =c("mobilenumber","mobilenumberwithareacode"), 
                  goodname = "mobile")
   
    #if("ageatfirstpregnancy" %in% names(d)) setnames(d, "ageatfirstpregnancy", "agepregnancy")
    #if(!"agepregnancy" %in% names(d)) stop("cant find agepregnancy")

    NamesToChange(d,"educationinyears","education")
    NamesToChange(d,"ageatmarriage","agemarriage")
    NamesToChange(d,"ageatfirstpregnancy","agepregnancy")
    NamesToChange(d,"monthlyhouseholdincomeils","income")
    NamesToChange(d,"numberofmembersinhousehold","members")
    if(!"street" %in% names(d)) d[,street:=""]
    d[,camp:=""]
    d[,phone:=""]
    d[,cosang:=""]
    d[,email:=""]
    d[,doyouwanttoreceivesms:=""]
    d[,alternateidentificationnumber:=""]
    
  } else {
    setnames(d,"instance","uniqueid")
    setnames(d,"created","datecreated")
    setnames(d,"lastupdated","dateupdated")
    setnames(d,"organisationunit","demoorgunit")
    setnames(d,"organisationunitname","demoorgname")
    # if(badname %in% names(d)) setnames(d, badname, goodname)
    if("trackedentitytype" %in% names(d)) setnames(d, "trackedentitytype", "trackedentity")
    # if(!goodname %in% names(d)) ERROR!!
    if(!"trackedentity" %in% names(d)) stop("cant find trackedentity")
      
    setnames(d,"inactive","dummy")
    setnames(d,"identificationdocumenttype","idtype")
    if(!"identificationdocumentnumber" %in% names(d)){
      warning("no identification document number -- we create one")
      d[,identificationdocumentnumber:=1:.N]
    }
    setnames(d,"identificationdocumentnumber","demoidnumber")
    #setnames(d,"areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits", "doyouwanttoreceivesms")
   
    
    if(IS_GAZA){
      d[,firstname:=""]
      d[,fathersname:=""]
      d[,familyname2:=""]
      d[,husbandsname:=""]
      d[,middlename:=""]
      d[,familyname1:=""]
      
      d[,village:=""]
      d[,city:=""]
      d[,mobile:=""]
      
      d[,street:=""]
      d[,camp:=""]
      d[,phone:=""]
    } else {
      setnames(d,which(stringr::str_detect(names(d),"^firstname")),"firstname")
      setnames(d,which(stringr::str_detect(names(d),"^fathersname")),"fathersname")
      setnames(d,which(stringr::str_detect(names(d),"^husbandsfamilyname")),"familyname2")
      setnames(d,which(stringr::str_detect(names(d),"^husbandsname")),"husbandsname")
      setnames(d,which(stringr::str_detect(names(d),"^middlename")),"middlename")
      setnames(d,which(stringr::str_detect(names(d),"^womanfamilyname")),"familyname1")
      
      setnames(d,"village","village")
      setnames(d,"city","city")
      NamesToChange(d,badname =c("mobilenumber","mobilenumberwithareacode"), 
                      goodname = "mobile")
      
      setnames(d,"streetname","street")
      setnames(d,"camp","camp")
      setnames(d,"telephonenumber","phone")
  
    }

    setnames(d,"dateofbirth","dob")
    
    setnames(d,"educationinyears","education")
    setnames(d,"ageatmarriage","agemarriage")
    setnames(d,"ageatfirstpregnancy","agepregnancy")
    setnames(d,"monthlyhouseholdincomeils","income")
    setnames(d,"numberofmembersinhousehold","members")
    setnames(d,which(stringr::str_detect(names(d),"^consanguinity")),"cosang")
    setnames(d,"emailaddress","email")
    
    d[,dataextractor:=""]
    #setnames(d,"areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits", "doyouwanttoreceivesms")
    
  }
  
  d[is.na(demoidnumber), demoidnumber:=c(1:.N)]
  d[,demoorgname:=ExtractOnlyEnglishLetters(stringr::str_to_lower(demoorgname))]
  
  
  d[,ident_dhis2_demo:=1]
  
  
  ConvertAllFactorsToChar(d)
  
 d[,datecreated:=stringr::str_sub(datecreated,1,10)]
 d[, c( "uniqueid","datecreated")]
 unique( d[, c( "demoidnumber","datecreated")])

 #d[,rownum:=1:.N,by=.(demoidnumber,datecreated)]
 #xtabs(~d$rownum)
 
 #d[rownum==1,]
 #d[rownum>1,]
 
  return(d)
}