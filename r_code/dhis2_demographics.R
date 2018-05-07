DHIS2_Demographics <- function(isControl){
  
  if(isControl){
    DATA_DATE <- CLINIC_CONTROL_DATE
    FOLDER_DATA <- sprintf("%s/e.reg-control/%s",FOLDER_DATA_RAW,DATA_DATE)
    TAG <- "CON"
    CLINICAL_OR_CONTROL <- "Control"
  } else {
    DATA_DATE <- CLINIC_INTERVENTION_DATE
    FOLDER_DATA <- sprintf("%s/e.reg-intervention/%s",FOLDER_DATA_RAW,DATA_DATE)
    TAG <- "INT"
    CLINICAL_OR_CONTROL <- "Clinical"
  }
  
  d <- fread(sprintf(
    "%s/%s Demographics.csv",
    FOLDER_DATA,
    CLINICAL_OR_CONTROL),
    encoding="UTF-8")
  
  if(isControl){
    setnames(d,"Instance","uniqueid")
    setnames(d,"Created","datecreated")
    setnames(d,"Last updated","dateupdated")
    setnames(d,"Organisation unit","demoorgunit")
    setnames(d,"Organisation unit name","demoorgname")
    setnames(d,"Tracked entity","trackedentity")
    setnames(d,"Inactive","dummy")
    setnames(d,"Identification document type","idtype")
    setnames(d,"Data extractor username","dataextractor")
    setnames(d,"Identification document number - control data","demoidnumber")
    setnames(d,"First name (الإسم الأول)","firstname")
    setnames(d,"Father's Name (اسم الأب)","fathername")
    setnames(d,"Husband's Family name (اسم عائلة الزوج)","familyname2")
    setnames(d,"Husband's name (اسم الزوج )","husbandname")
    setnames(d,"Middle Name (اسم الجد)","middlename")
    setnames(d,"Woman family name (اسم عائلة المرأة)","familyname1")
    setnames(d,"Village","village")
    setnames(d,"City","city")
    setnames(d,"Date of Birth","dob")
    setnames(d,"Mobile number","mobile")
    setnames(d,"Education in years","education")
    setnames(d,"Age at marriage","agemarriage")
    setnames(d,"Age at first pregnancy","agepregnancy")
    setnames(d,"Monthly household income (ILS)","income")
    setnames(d,"Number of members in household","members")
    d[,street:=""]
    d[,camp:=""]
    d[,phone:=""]
    d[,cosang:=""]
    d[,email:=""]
  } else {
    setnames(d,"Instance","uniqueid")
    setnames(d,"Created","datecreated")
    setnames(d,"Last updated","dateupdated")
    setnames(d,"Organisation unit","demoorgunit")
    setnames(d,"Organisation unit name","demoorgname")
    setnames(d,"Tracked entity","trackedentity")
    setnames(d,"Inactive","dummy")
    setnames(d,"Identification document type","idtype")
    setnames(d,"Identification document number","demoidnumber")
    setnames(d,"First name (الإسم الأول)","firstname")
    setnames(d,"Father's Name (اسم الأب)","fathername")
    setnames(d,"Husband's Family name (اسم عائلة الزوج)","familyname2")
    setnames(d,"Husband's name (اسم الزوج )","husbandname")
    setnames(d,"Middle Name (اسم الجد)","middlename")
    setnames(d,"Woman family name (اسم عائلة المرأة)","familyname1")
    setnames(d,"Village","village")
    setnames(d,"City","city")
    setnames(d,"Date of Birth","dob")
    setnames(d,"Mobile number","mobile")
    setnames(d,"Education in years","education")
    setnames(d,"Age at marriage","agemarriage")
    setnames(d,"Age at first pregnancy","agepregnancy")
    setnames(d,"Monthly household income (ILS)","income")
    setnames(d,"Number of members in household","members")
    
    setnames(d,"Street name","street")
    setnames(d,"Camp","camp")
    setnames(d,"Telephone number","phone")
    setnames(d,"Consanguinity(درجة القرابه بين الزوجين)","cosang")
    setnames(d,"Email address","email")
    
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
  
  
  return(d)
}