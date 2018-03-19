
CLINIC_INTERVENTION_DATE <- "2018-03-04"
CLINIC_CONTROL_DATE <- "2018-03-18"

tryCatch({
  setwd("X:/data processing/")
}, error=function(err){
  setwd("Z:/data processing/")
})

desiredPackages <- c("stringr","lubridate","data.table","bit64","readxl")
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)

MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()

isControl <- TRUE

if(isControl){
  DATA_DATE <- CLINIC_CONTROL_DATE
  DATA_RAW <- sprintf("data_raw/e.reg-control/%s",DATA_DATE)
  TAG <- "CON"
  CLINICAL_OR_CONTROL <- "Control"
}

d <- fread(sprintf(
  "%s/%s Demographics.csv",
  DATA_RAW,
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
  setnames(d,"Data extractor username","")
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
}

d[is.na(demoidnumber), demoidnumber:=c(1:.N)]

mahima <- data.table(readxl::read_excel(file.path(
  "data_raw","structural_data","isMahimaClinicsTrial1.xlsx"
)))

d[,isMahimaClinicsTrial1:=FALSE]
d[demoorgunit %in% mahima$demoorgunit,isMahimaClinicsTrial1:=TRUE]

bookorgnames <- readxl::read_excel(file.path(
  "data_raw","structural_data","bad_to_good_bookorgname.xlsx"
))
setnames(bookorgnames,"badbookorgname","demoorgname")

dim(d)
d <- merge(d,bookorgnames,by="demoorgname",all.x=T)
dim(d)

d[!is.na(goodbookorgname),demoorgname:=goodbookorgname]
d[,goodbookorgname:=NULL]
################ DONE UP TO HERE

//
  *Replacing MotherID numbers from clinics to the correct ones from Avicenna so that*
  *we can possible match more to Avicenna for pretrial//
  // this variable is numeric//
  
  replace demoidnumber=900466616 if demoidnumber==900466626
replace demoidnumber=901083527 if demoidnumber==901083521
replace demoidnumber=910567635 if demoidnumber==918567635
replace demoidnumber=946457421 if demoidnumber==946457471
replace demoidnumber=853637882 if demoidnumber==85367887
replace demoidnumber=411113970 if demoidnumber==411113470
replace demoidnumber=850465485 if demoidnumber==850465480
replace demoidnumber=850872078 if demoidnumber==850672078
replace demoidnumber=853203560 if demoidnumber==853203562
replace demoidnumber=414406918 if demoidnumber==414406916
replace demoidnumber=950337378 if demoidnumber==950337778
replace demoidnumber=850437997 if demoidnumber==850347997
replace demoidnumber=850535360 if demoidnumber==860626250
replace demoidnumber=700177637 if demoidnumber==854992138
replace demoidnumber=852327022 if demoidnumber==852327122
replace demoidnumber=850757634 if demoidnumber==85075736
replace demoidnumber=402822209 if demoidnumber==402022209
replace demoidnumber=853009645 if demoidnumber==833009645
replace demoidnumber=850470071 if demoidnumber==8501170071
replace demoidnumber=410852636 if demoidnumber==41085636
replace demoidnumber=946595428 if demoidnumber==94659532
replace demoidnumber=854049343 if demoidnumber==85049343
replace demoidnumber=852760750 if demoidnumber==832760750
replace demoidnumber=852467737 if demoidnumber==851478925
replace demoidnumber=852637727 if demoidnumber==852637722
replace demoidnumber=853672632 if demoidnumber==853672633
replace demoidnumber=854692548 if demoidnumber==854682548
replace demoidnumber=854503836 if demoidnumber==859503836
replace demoidnumber=900386871 if demoidnumber==900386821
replace demoidnumber=907612394 if demoidnumber==907612384
replace demoidnumber=911666873 if demoidnumber==936248172
replace demoidnumber=911783678 if demoidnumber==911703678
replace demoidnumber=936118678 if demoidnumber==936118628
replace demoidnumber=936748177 if demoidnumber==936248172
replace demoidnumber=937717684 if demoidnumber==937217684
replace demoidnumber=937718724 if demoidnumber==937218224
replace demoidnumber=937719615 if demoidnumber==937219615
replace demoidnumber=937720928 if demoidnumber==937220928
replace demoidnumber=941287377 if demoidnumber==941282322
replace demoidnumber=949987713 if demoidnumber==949487713
replace demoidnumber=949996987 if demoidnumber==949996982
replace demoidnumber=907623532 if demoidnumber==957623532
replace demoidnumber=907149876 if demoidnumber==90719876
replace demoidnumber=401949862 if demoidnumber==401941862
replace demoidnumber=850695412 if demoidnumber==90636311
replace demoidnumber=905171294 if demoidnumber==90517294
replace demoidnumber=907149876 if demoidnumber==90719876
replace demoidnumber=401949862 if demoidnumber==401941862
replace demoidnumber=850695412 if demoidnumber==90636311
replace demoidnumber=905171294 if demoidnumber==90517294
replace demoidnumber=850473687 if demoidnumber==8540473687
replace demoidnumber=415045400 if demoidnumber==118315274
replace demoidnumber=946641362 if demoidnumber==9466413630
replace demoidnumber=956792766 if demoidnumber==95679766
replace demoidnumber=900027046 if demoidnumber==90027046
replace demoidnumber=853640845 if demoidnumber==85364084
replace demoidnumber=853193944 if demoidnumber==85343944
replace demoidnumber=950232330 if demoidnumber==950232530
replace demoidnumber=946641362 if demoidnumber==946641262
replace demoidnumber=900027046 if demoidnumber==900270046
replace demoidnumber=905611695 if demoidnumber==856781048
replace demoidnumber=854909959 if demoidnumber==854909954
replace demoidnumber=853686848 if demoidnumber==853686845
replace demoidnumber=401466347 if demoidnumber==401466397
replace demoidnumber=859023665 if demoidnumber==859023660
replace demoidnumber=949296768 if demoidnumber==949296568
replace demoidnumber=920674827 if demoidnumber==920574827
replace demoidnumber=851937995 if demoidnumber==851937998
replace demoidnumber=852343722 if demoidnumber==805234372
replace demoidnumber=949559728 if demoidnumber==949559782
replace demoidnumber=941121956 if demoidnumber==941121556
replace demoidnumber=859894156 if demoidnumber==859894150
replace demoidnumber=853629798 if demoidnumber==853929798
replace demoidnumber=900435801 if demoidnumber==900433801
replace demoidnumber=853690501 if demoidnumber==816198217
replace demoidnumber=90636311 if demoidnumber==850695412


//replace demoidnumber=900466626 if demoidnumber==900466616
//replace demoidnumber=901083521 if demoidnumber==901083527
//replace demoidnumber=918567635 if demoidnumber==910567635
//replace demoidnumber=946457471 if demoidnumber==946457421
//replace demoidnumber=85367887	 if demoidnumber==853637882
//replace demoidnumber=411113470 if demoidnumber==411113970
//replace demoidnumber=850465480 if demoidnumber==850465485
//replace demoidnumber=850672078 if demoidnumber==850872078
//replace demoidnumber=853203562 if demoidnumber==853203560
//replace demoidnumber=414406916 if demoidnumber==414406918
//replace demoidnumber=950337778 if demoidnumber==950337378
//replace demoidnumber=850347997 if demoidnumber==850437997
//replace demoidnumber=860626250 if demoidnumber==850535360
//replace demoidnumber=854992138 if demoidnumber==700177637
//replace demoidnumber=852327122 if demoidnumber==852327022
//replace demoidnumber=85075736 if demoidnumber==850757634
//replace demoidnumber=402022209 if demoidnumber==402822209
//replace demoidnumber=833009645 if demoidnumber==853009645
//replace demoidnumber=8501170071 if demoidnumber==850470071
//replace demoidnumber=41085636 if demoidnumber==410852636
//replace demoidnumber=94659532 if demoidnumber==946595428
//replace demoidnumber=85049343 if demoidnumber==854049343
//replace demoidnumber=832760750 if demoidnumber==852760750
//replace demoidnumber=851478925 if demoidnumber==852467737
//replace demoidnumber=852637722 if demoidnumber==852637727
//replace demoidnumber=853672633 if demoidnumber==853672632
//replace demoidnumber=854682548 if demoidnumber==854692548
//replace demoidnumber=859503836 if demoidnumber==854503836
//replace demoidnumber=900386821 if demoidnumber==900386871
//replace demoidnumber=907612384 if demoidnumber==907612394
//replace demoidnumber=936248172 if demoidnumber==911666873
//replace demoidnumber=911703678 if demoidnumber==911783678
//replace demoidnumber=936118628 if demoidnumber==936118678
//replace demoidnumber=936248172 if demoidnumber==936748177
//replace demoidnumber=937217684 if demoidnumber==937717684
//replace demoidnumber=937218224 if demoidnumber==937718724
//replace demoidnumber=937219615 if demoidnumber==937719615
//replace demoidnumber=937220928 if demoidnumber==937720928
//replace demoidnumber=941282322 if demoidnumber==941287377
//replace demoidnumber=949487713 if demoidnumber==949987713
//replace demoidnumber=949996982 if demoidnumber==949996987
//replace demoidnumber=957623532 if demoidnumber==907623532
//replace demoidnumber=90719876 if demoidnumber==907149876
//replace demoidnumber=401941862 if demoidnumber==401949862
//replace demoidnumber=90636311 if demoidnumber==850695412
//replace demoidnumber=90517294 if demoidnumber==905171294
//replace demoidnumber=90719876 if demoidnumber==907149876
//replace demoidnumber=401941862 if demoidnumber==401949862
//replace demoidnumber=90636311 if demoidnumber==850695412
//replace demoidnumber=90517294 if demoidnumber==905171294
//replace demoidnumber=8540473687 if demoidnumber==850473687
//replace demoidnumber=118315274 if demoidnumber==415045400
//replace demoidnumber=9466413630 if demoidnumber==946641362
//replace demoidnumber=95679766 if demoidnumber==956792766
//replace demoidnumber=90027046 if demoidnumber==900027046
//replace demoidnumber=85364084 if demoidnumber==853640845
//replace demoidnumber=85343944 if demoidnumber==853193944
//replace demoidnumber=950232530 if demoidnumber==950232330
//replace demoidnumber=946641262 if demoidnumber==946641362
//replace demoidnumber=900270046 if demoidnumber==900027046
//replace demoidnumber=856781048 if demoidnumber==905611695
//replace demoidnumber=854909954 if demoidnumber==854909959
//replace demoidnumber=853686845 if demoidnumber==853686848
//replace demoidnumber=401466397 if demoidnumber==401466347
//replace demoidnumber=859023660 if demoidnumber==859023665
//replace demoidnumber=949296568 if demoidnumber==949296768
//replace demoidnumber=920574827 if demoidnumber==920674827
//replace demoidnumber=851937998 if demoidnumber==851937995
//replace demoidnumber=805234372 if demoidnumber==852343722
//replace demoidnumber=949559782 if demoidnumber==949559728
//replace demoidnumber=941121556 if demoidnumber==941121956
//replace demoidnumber=859894150 if demoidnumber==859894156
//replace demoidnumber=853929798 if demoidnumber==853629798
//replace demoidnumber=900433801 if demoidnumber==900435801
//replace demoidnumber=816198217 if demoidnumber==853690501
//replace demoidnumber=850695412 if demoidnumber==90636311












/*
  *Save dataset as "int_anc demographics_labelled"
save "data_temp/int_ demographics_labelled.dta", replace

*Open the last saved dataset
use "data_temp/int_ demographics_labelled.dta", replace

*???Create a dataset with only required dates??? ***Correlate with the booking visit dates***
  
  
  
  
  *Create a dataset with missing ID numbers
**Create a subset of data only with missing id numbers 
summarize demoidnumber 

*Recode missing values with numeric
replace demoidnumber=9 if missing(demoidnumber)

*Create a subset of data by keeping only women with missing id numbers
keep if demoidnumber==9

*Save this dataset as 'int_anc demographics_missing id'
save "data_temp/int_anc demographics_missing id.dta", replace




*Open dataset ""ANC demographics_labelled" to continue with processing
use "data_temp/int_ demographics_labelled.dta", clear
*/

**ageatmarriage
generate agemarriagecat = .
summarize agemarriagecat

replace agemarriagecat = 1 if (agemarriage <= 20)
replace agemarriagecat = 2 if (agemarriage >=21) & (agemarriage <=25)
replace agemarriagecat = 3 if (agemarriage >=26) & (agemarriage <=30)
replace agemarriagecat = 4 if (agemarriage >=31) & (agemarriage <=35)
replace agemarriagecat = 5 if (agemarriage >=36) & (agemarriage <=40)
replace agemarriagecat = 6 if (agemarriage >40)

summarize agemarriagecat

**ageatfirstpregnancy
generate agepregnancycat = .
summarize agepregnancycat

replace agepregnancycat = 1 if (agepregnancy <= 20)
replace agepregnancycat = 2 if (agepregnancy >=21) & (agepregnancy <=25)
replace agepregnancycat = 3 if (agepregnancy >=26) & (agepregnancy <=30)
replace agepregnancycat = 4 if (agepregnancy >=31) & (agepregnancy <=35)
replace agepregnancycat = 5 if (agepregnancy >=36) & (agepregnancy <=40)
replace agepregnancycat = 6 if (agepregnancy >40)

summarize agepregnancycat

**mothers education
generate educationcat = .
summarize educationcat

replace educationcat = 1 if (education <10)
replace educationcat = 2 if (agepregnancy >=10) & (agepregnancy <=13)
replace educationcat = 3 if (agepregnancy >13)

summarize educationcat

**income 

**Generate average monthly household income
gen avgincome = income/members

generate incomecat = .
summarize incomecat

replace incomecat = 1 if (avgincome <=200)
replace incomecat = 2 if (avgincome >=201) & (income <=900)
replace incomecat = 3 if (avgincome >=901) & (income <=1824)
replace incomecat = 4 if (avgincome >=1825) & (income <=3054)
replace incomecat = 5 if (avgincome >3055)

summarize incomecat

save "~/My Documents/trial_temp_data/$TAG Clinical Demographics.dta", replace




