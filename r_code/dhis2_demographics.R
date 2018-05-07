DHIS2_Demographics <- function(isControl){
  
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
    setnames(d,"trackedentity","trackedentity")
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
    setnames(d,"mobilenumber","mobile")
    setnames(d,"educationinyears","education")
    setnames(d,"ageatmarriage","agemarriage")
    setnames(d,"ageatfirstpregnancy","agepregnancy")
    setnames(d,"monthlyhouseholdincomeils","income")
    setnames(d,"numberofmembersinhousehold","members")
    d[,street:=""]
    d[,camp:=""]
    d[,phone:=""]
    d[,cosang:=""]
    d[,email:=""]
  } else {
    setnames(d,"instance","uniqueid")
    setnames(d,"created","datecreated")
    setnames(d,"lastupdated","dateupdated")
    setnames(d,"organisationunit","demoorgunit")
    setnames(d,"organisationunitname","demoorgname")
    setnames(d,"trackedentity","trackedentity")
    setnames(d,"inactive","dummy")
    setnames(d,"identificationdocumenttype","idtype")
    setnames(d,"identificationdocumentnumber","demoidnumber")
    
    setnames(d,which(stringr::str_detect(names(d),"^firstname")),"firstname")
    setnames(d,which(stringr::str_detect(names(d),"^fathersname")),"fathersname")
    setnames(d,which(stringr::str_detect(names(d),"^husbandsfamilyname")),"familyname2")
    setnames(d,which(stringr::str_detect(names(d),"^husbandsname")),"husbandsname")
    setnames(d,which(stringr::str_detect(names(d),"^middlename")),"middlename")
    setnames(d,which(stringr::str_detect(names(d),"^womanfamilyname")),"familyname1")

    setnames(d,"village","village")
    setnames(d,"city","city")
    setnames(d,"dateofbirth","dob")
    setnames(d,"mobilenumber","mobile")
    setnames(d,"educationinyears","education")
    setnames(d,"ageatmarriage","agemarriage")
    setnames(d,"ageatfirstpregnancy","agepregnancy")
    setnames(d,"monthlyhouseholdincomeils","income")
    setnames(d,"numberofmembersinhousehold","members")
    
    setnames(d,"streetname","street")
    setnames(d,"camp","camp")
    setnames(d,"telephonenumber","phone")
    setnames(d,which(stringr::str_detect(names(d),"^consanguinity")),"cosang")
    setnames(d,"emailaddress","email")
    
    d[,dataextractor:=""]
  }
  
  d[is.na(demoidnumber), demoidnumber:=c(1:.N)]
  d[,demoorgname:=ExtractOnlyEnglishLetters(stringr::str_to_lower(demoorgname))]
  
  #fwrite(d,"C:/Users/Mervett_Isbeih/Desktop/test.txt")
  if(FALSE){
    # adding in a variable for mahima's clniical trial 1
    mahima <- data.table(readxl::read_excel(file.path(
      "data_raw","structural_data","isMahimaClinicsTrial1.xlsx"
    )))
    
    d[,isMahimaClinicsTrial1:=FALSE]
    d[demoorgunit %in% mahima$demoorgunit,isMahimaClinicsTrial1:=TRUE]
    
    # fixing bookorgnames
    bookorgnames <- readxl::read_excel(file.path(
      "data_raw","structural_data","bad_to_good_bookorgname.xlsx"
    ))
    setnames(bookorgnames,"badbookorgname","demoorgname")
    bookorgnames[,X__1:=NULL]
    
    # joinby demorgname using "bookorgnames", unm(b)
    dim(d)
    d <- merge(d,bookorgnames,by="demoorgname",all.x=T)
    dim(d)
    
    # overwriting demorgname with goodbookorgname if it exists
    d[!is.na(goodbookorgname),demoorgname:=goodbookorgname]
    # delete goodbookorgname
    d[,goodbookorgname:=NULL]
    
    # fixing id numbers to be aviccena ("the truth")
    aviccena_id_numbers <- data.table(readxl::read_excel(file.path(
      "data_raw","structural_data","pretrial_export_to_aviccena_id_numbers.xlsx"
    )))
    aviccena_id_numbers <- aviccena_id_numbers[!is.na(aviccena) & !is.na(export),
                                               c("aviccena","export")]
    aviccena_id_numbers[,aviccena:=bit64::as.integer64(aviccena)]
    aviccena_id_numbers[,export:=bit64::as.integer64(export)]
    
    # only 43 of these excel numbers match in the data
    sum(aviccena_id_numbers$export %in% d$demoidnumber,na.rm=T)
    # these are the ones that dont match
    aviccena_id_numbers$export[!aviccena_id_numbers$export %in% d$demoidnumber]
    
    # these are the ones that dont match
    fwrite(aviccena_id_numbers[!export %in% d$demoidnumber],"data_debugging/id_numbers_not_matching.csv")
    
    setnames(aviccena_id_numbers,"export","demoidnumber")
    
    dim(d)
    d <- merge(d,aviccena_id_numbers,by="demoidnumber",all.x=T)
    dim(d)
    
    # overwriting demoidnumber with aviccena if it exists
    d[!is.na(aviccena),demoidnumber:=aviccena]
    # delete aviccena variable/column (because the information has already
    # been copied over to demoidnumber
    d[,aviccena:=NULL]
  }
  
  d[,agemarriagecat:=as.numeric(cut(agemarriage,
                                    breaks=c(0,20,25,30,35,40,100),
                                    include.lowest=T))]
  d[,agepregnancycat:=as.numeric(cut(agepregnancy,
                                     breaks=c(0,20,25,30,35,40,100),
                                     include.lowest=T))]
  
  d[,educationcat:=as.numeric(cut(education,
                                  breaks=c(0,9,13,100),
                                  include.lowest=T))]
  xtabs(~d$agemarriagecat)
  xtabs(~d$agepregnancycat)
  xtabs(~d$educationcat)
  
  d[,avgincome := income/members]
  
  d[,avgincomecat:=as.numeric(cut(avgincome,
                                  breaks=c(0,200,900,1824,3054,100000),
                                  include.lowest=T))]
  d[,incomecat:=as.numeric(cut(income,
                               breaks=c(0,200,900,1824,3054,100000),
                               include.lowest=T))]
  
  ConvertAllFactorsToChar(d)
  return(d)
}