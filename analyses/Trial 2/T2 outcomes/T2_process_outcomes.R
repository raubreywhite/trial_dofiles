
####################  Trial 2 Outcomes #################### 


###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)
###### SETUP ENDS ######


# LOAD in data
if(IS_GAZA==F){
  
  
  d <- LoadDataFileFromNetworkWB()
  fileTag <- "WB"
  
}else {
  
  d <- LoadDataFileFromNetworkGaza()
  fileTag <- "GAZA"
  
  
  
  
}



# trial arms
d[ident_TRIAL_2_3_Control==T,TrialArm:="Control"]

d[ident_TRIAL_2==T & 
     ident_TRIAL_3==F,TrialArm:="SMS only"]

d[ident_TRIAL_2==F & 
     ident_TRIAL_3==T,TrialArm:="QID only"]

d[ident_TRIAL_2==T & 
     ident_TRIAL_3==T,TrialArm:="SMS and QID"]



d[,precovid:=as.logical(NA)]
d[bookdate>="2019-12-01" &
     bookdate<="2020-03-30" & 
    !is.na(TrialArm),precovid:=TRUE]
d[bookdate>="2020-06-22" &
    !is.na(TrialArm), precovid:=FALSE]
xtabs(~d$TrialArm, addNA=T)
nrow(d)


if(IS_GAZA==T){
  
  nam <- names(d)[stringr::str_detect(names(d),"^usedd_[0-9]*$")]
  num <- stringr::str_replace(nam,"usedd_","")
  d[,first_1_21_usedd:=as.Date(NA)]
  for(i in num ){
    print(i)
    
    var_usedd <- sprintf("usedd_%s",i)
    var_usgestage <- sprintf("usgestage_%s",1)
    
    d[!is.na(get(var_usedd)) &
        
        !is.na(get(var_usgestage)) &
        get(var_usgestage) > 0 &
        get(var_usgestage) < 23 &
        is.na(first_1_21_usedd),
      first_1_21_usedd:=as.Date(get(var_usedd),format="%Y-%m-%d")]
  }
  #unique(d$usgestage_1)
  #unique(d$first_1_21_usedd)
  #sum(!is.na(d$first_1_21_usedd))
  
  #Making gA for first_1_21_usedd
  
 

  
  d[,usedddays:=first_1_21_usedd - as.difftime(280, unit="days")]
  
  d[,USorLMPdate:=booklmp]
  d[!is.na(usedddays),USorLMPdate:=usedddays]
  #d[is.na(USorLMPdate), USorLMPdate:=]
  
  
  
  d[,bookgestagedays:=round(as.numeric(difftime(
    bookdate,USorLMPdate, units="days")))]
  
  d[,bookgestagedays_cats:=cut(bookgestagedays,
                               breaks=c(-500,0,104,
                                        125,160,167,202,
                                        216,237,244,265,293),
                               include.lowest=T)]
  
  
  #gestagedaysCats
  ### making gestational ages based on usedd=<20, then lmp, then us>20 ###
  # + means repeat, $ means the end of the entire string
  d[,lmpT1:=as.Date(NA)]
  us_gestages <- stringr::str_subset(names(d), "^usgestage_[0-9]+")
  us_date <- stringr::str_subset(names(d), "^usdate_[0-9]+")
  
  
  #seq_along identical to 1:lenth()
  for(i in seq_along(us_gestages)){
    var_us_gestage <- us_gestages[i]
    var_us_date<- us_date[i]
    
    d[is.na(lmpT1) & 
        get(var_us_gestage)<= 20, 
      
      lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
    
  }
  
  d[is.na(lmpT1) & !is.na(booklmp), lmpT1:=booklmp]
  
  for(i in seq_along(us_gestages)){
    var_us_gestage <- us_gestages[i]
    var_us_date<- us_date[i]
    
    d[is.na(lmpT1) & get(var_us_gestage)>=20 & get(var_us_gestage)<40,
      
      lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
    
    
  }
  
  #qc new variables
  d[is.na(lmpT1), c("booklmp",us_gestages, us_date), with=F]
  
  
  
  ##looping through to make gestages for each of the program stages
  #Us gAs
  us_date <- stringr::str_subset(names(d), "^usdate_[0-9]+")
  usT1_gA <- stringr::str_replace(us_date, "usdate","usT1gestagedays")
  
  for(i in seq_along(us_date)){
    var_us_gestage <- usT1_gA[i]
    var_us_date<- us_date[i] 
    
    d[,(var_us_gestage):=as.numeric(floor(difftime(get(var_us_date),lmpT1, units="days")))]
    
  }
  
  
  #ANC gAs
  an_date <- stringr::str_subset(names(d), "^andate_[0-9]+")
  anT1_gA <- stringr::str_replace(an_date, "andate","anT2gestagedays")
  
  for(i in seq_along(an_date)){
    var_an_gestage <- anT1_gA[i]
    var_an_date<- an_date[i] 
    
    d[,(var_an_gestage):=as.numeric(floor(difftime(get(var_an_date),lmpT1, units="days")))]
    
  }
  
  #Lab gAs
  ##NEED TO ROUND OR USE FLOOR
  lab_date <- stringr::str_subset(names(d), "^labdate_[0-9]+")
  labT1_gA <- stringr::str_replace(lab_date, "labdate","labT1gestagedays")
  
  for(i in seq_along(lab_date)){
    var_lab_gestage <- labT1_gA[i]
    var_lab_date<- lab_date[i] 
    
    d[,(var_lab_gestage):=as.numeric(floor(difftime(get(var_lab_date),lmpT1, units="days")))]
    
  }
  
  #Man gAs
  man_date <- stringr::str_subset(names(d), "^mandate_[0-9]+")
  manT1_gA <- stringr::str_replace(man_date, "mandate","manT2gestagedays")
  
  for(i in seq_along(man_date)){
    var_man_gestage <- manT1_gA[i]
    var_man_date<- man_date[i] 
    
    d[,(var_man_gestage):=as.numeric(floor(difftime(get(var_man_date),lmpT1, units="days")))]
    
  }
  
  ##making categories of days for booking
  
  d[,bookgestagedays_cats:=cut(bookgestagedays,
                               breaks=c(-500,0,104,
                                        125,160,167,202,
                                        216,237,244,265,293),
                               include.lowest=T)]
  
  
  
  # MAKE BOOK VISIT FOR ANEMIA
  d[,booklabhb:=as.numeric(NA)]
  d[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]
  
  
  
  
  
} else{
  
  # do notghing
}



# defining dataset
smallD  <- d[(ident_dhis2_booking==T | ident_dhis2_an==T) &
               !is.na(TrialArm),]



# MAKE BOOK VISIT FOR ANEMIA
smallD[,booklabhb:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]

# MAKE BOOK VISIT FOR Laburglu
smallD[,booklaburglu:=as.character(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]

smallD[,booklaburglu:=NULL]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,
       booklaburglu:=laburglu_1]
xtabs(~smallD$booklaburglu)
str(smallD$booklaburglu)
unique(smallD$booklaburglu)

smallD[,booklaburglu:=NULL]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]
xtabs(~smallD$booklaburglu)


# MAKE BOOK VISIT FOR LABBLOODGLU
smallD[,booklabbloodglu:=as.integer(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabbloodglu:=labbloodglu_1]
xtabs(~smallD$booklabbloodglu, addNA=T)

# MAKE BOOK VISIT FOR LABBLOODGLU_HIGH
smallD[,booklabbloodglu_high:=as.logical(NA)]
smallD[!is.na(booklabbloodglu),booklabbloodglu_high:=FALSE]
smallD[booklabbloodglu>=140 & booklabbloodglu<500,booklabbloodglu_high:=TRUE]
xtabs(~smallD$booklabbloodglu_high, addNA=T)

# MAKE BOOK VISIT FOR LABFASTBLOODGLU
smallD[,booklabfastbloodglu:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabfastbloodglu:=labfastbloodglu_1]
xtabs(~smallD$booklabfastbloodglu)

# MAKE BOOK VISIT FOR LABfastBLOODGLU_HIGH
smallD[,booklabfastbloodglu_high:=as.logical(NA)]
smallD[!is.na(booklabfastbloodglu),booklabfastbloodglu_high:=FALSE]
smallD[booklabfastbloodglu>126 ,booklabfastbloodglu_high:=TRUE]
xtabs(~smallD$booklabfastbloodglu_high, addNA=T)

VisitVariables <- function(smallD,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="anT2gestagedays" ){
  
  if(!is.null(TruevaluesMin) & !is.null(TruevaluesMax) & !is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NOT NULL")
  }
  
  if(is.null(TruevaluesMin) & is.null(TruevaluesMax) & is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NULL")
  }
  
  
  # pull out a list of all of the gestage variables
  #browser()
  gestagedaysVariablewithcarrot <- sprintf("^%s",gestagedaysVariable)
  listOfGestAgeVars <- names(smallD)[stringr::str_detect(names(smallD),gestagedaysVariablewithcarrot)]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, gestagedaysVariable,variableOfInterestPattern)
  
  
  for(i in 1:length(days)){
    # name of new variable
    var <- sprintf("T2_%s_%s",variableOfInterestName,names(days)[i])
    # initialize all as FALSE if has booking variable
    smallD[!is.na(ident_dhis2_booking),(var):=FALSE]
    #xtabs(~smallD[[var]])
    
    # loop through the "gestage"/"bp" variables
    for(j in 1:length(listOfGestAgeVars)){
      gestageVar <- listOfGestAgeVars[j]
      interestVar <- listOfInterestVars[j]
      
      #asking discrete question
      if(!is.null(TruevaluesDiscrete)){
        
        smallD[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar) %in% TruevaluesDiscrete ,(var):=TRUE]
        
      }else{ #asking non discrete questions
        
        
        smallD[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar)>=TruevaluesMin & get(interestVar)<=TruevaluesMax, (var):=TRUE]
        
      }
      
      
      
    }
  }
  
  return(smallD)
  
  
}



###### identifying outcomes #######

# categories we want
days <- list(
  "00_14"=c(-500:104),
  "15_17"=c(105:125),
  "18_22"=c(126:160),
  "23_23"=c(161:167),
  "24_28"=c(168:202),
  "29_30"=c(203:216),
  "31_33"=c(217:237),
  "34_34"=c(238:244),
  "35_37"=c(245:265),
  "38_41"=c(266:293),
  
  #using below vectors for managementsinstead of using two seperate vectors
  
  "00_00"=0*7+c(0:6),
  "01_01"=1*7+c(0:6),
  "02_02"=2*7+c(0:6),
  "03_03"=3*7+c(0:6),
  "04_04"=4*7+c(0:6),
  "05_05"=5*7+c(0:6),
  "06_06"=6*7+c(0:6),
  "07_07"=7*7+c(0:6),
  "08_08"=8*7+c(0:6),
  "09_09"=9*7+c(0:6),
  "10_10"=10*7+c(0:6),
  "11_11"=11*7+c(0:6),
  "12_12"=12*7+c(0:6),
  "13_13"=13*7+c(0:6),
  "14_14"=14*7+c(0:6),
  "15_15"=15*7+c(0:6),
  "16_16"=16*7+c(0:6),
  "17_17"=17*7+c(0:6),
  "18_18"=18*7+c(0:6),
  "19_19"=9*7+c(0:6),
  "20_20"=20*7+c(0:6),
  "21_21"=21*7+c(0:6),
  "22_22"=22*7+c(0:6),
  #"23_23"=23*7+c(0:6),
  "24_24"=24*7+c(0:6),
  "25_25"=25*7+c(0:6),
  "26_26"=26*7+c(0:6),
  "27_27"=27*7+c(0:6),
  "28_28"=28*7+c(0:6),
  "29_29"=29*7+c(0:6),
  "30_30"=30*7+c(0:6),
  "31_31"=31*7+c(0:6),
  "32_32"=32*7+c(0:6),
  "33_33"=33*7+c(0:6),
  "34_34"=34*7+c(0:6),
  "35_35"=35*7+c(0:6),
  "36_36"=36*7+c(0:6),
  "37_37"=37*7+c(0:6),
  "38_38"=38*7+c(0:6),
  "39_39"=39*7+c(0:6),
  "40_40"=40*7+c(0:6),
  "41_41"=41*7+c(0:6),
  "42_42"=42*7+c(0:6)
  
)


######################### ATTENDANCE ######################### 

smallD[,andate_0:=bookdate]
smallD[,angestage_0:=bookgestage]

# id maximum amount of things
smallD[,firstvisitinT2:=as.numeric(NA)]

# need to change date back later to dec 01, 2019


temp <- stringr::str_subset(names(smallD),"^andate_")

# -1 because have 22 dates and have a 0 in there
for(i in 0:(length(temp)-1)){
  
  datevar <- paste0("andate_",i)
  
  gestagevar <- paste0("angestage_",i)
  
  smallD[is.na(firstvisitinT2) & 
           get(datevar)>="2019-12-01" &
           get(datevar)<="2020-03-22",firstvisitinT2:=i]
  
  # add other limits like in the precovid stuff
  
  
}

for(i in 0:(length(temp)-1)){
  
  datevar <- paste0("andate_",i)
  
  gestagevar <- paste0("angestage_",i)
  
  outcomevar <- paste0("anT2gestagedays_",i)
  
  smallD[i>=firstvisitinT2,(outcomevar):=as.numeric(floor(difftime(get(datevar),lmpT1, units="days")))]
  
}

xtabs(~TrialArm + firstvisitinT2, data=smallD, addNA=T)



#ANC gAs
an_date <- stringr::str_subset(names(smallD), "^andate_[0-9]+")
anT2_gA <- stringr::str_replace(an_date, "andate","anT2gestagedays")

for(i in seq_along(an_date)){
  var_an_gestage <- anT2_gA[i]
  var_an_date<- an_date[i] 
  
  smallD[,(var_an_gestage):=as.numeric(floor(difftime(get(var_an_date),lmpT1, units="days")))]
  
}


###ANC Visits####

smallD[,anT2gestagedays_0:=bookgestagedays]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anT2visit",
  variableOfInterestPattern="anT2gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
xtabs(~smallD$T2_anT2visit_00_14)


smallD[,anT2gestagedays_0:=bookgestagedays]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="anT2gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
xtabs(~smallD$T2_anvisitnew_00_00)

###ANC BP SYT ####
# BP SYST Present
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD<-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_present",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=60,
  TruevaluesMax=170,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$T2_anbpsyst_present_15_17)

# BP Diast Present
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]

smallD<- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_present",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=40,
  TruevaluesMax=170,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$T2_anbpdiast_present_00_14)

# BP Syst High
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_high",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$T2_anbpsyst_high_00_14)

# BP Syst MildHTN
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_mildHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=149,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$T2_anbpsyst_mildHTN_00_14)

# BP Syst ModSevHTN
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_modSevHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=150,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$T2_anbpsyst_modSevHTN_00_14)

# BP Diast High
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]

smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_high",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$T2_anbpdiast_high_00_14)


# BP Diast MildHTN
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_mildHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=99,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")
smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$T2_anbpdiast_mildHTN_00_14)


# BP Diast Mod/SevHTN
smallD[,anT2gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_modSevHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=100,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT2gestagedays")
smallD[,anT2gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$T2_anbpdiast_modSevHTN_00_14)


### ANC Anemia ####
# lab hb exists
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labhb_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_exists",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=20,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

nrow(smallD[labhb_1>=4 & labhb_1<=20])
smallD[,labT1gestagedays_0:=NULL]
smallD[,labhb_0:=NULL]
xtabs(~smallD$T2_labhb_exists_15_17)

#normal hb
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labhb_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_normal",
  variableOfInterestPattern="labhb",
  TruevaluesMin=11,
  TruevaluesMax=20,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

smallD[,labT1gestagedays_0:=NULL]
smallD[,labhb_0:=NULL]
xtabs(~smallD$T2_labhb_normal_15_17)

# sev anemia
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labhb_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_sev",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=6.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

nrow(smallD[labhb_1>=1 & labhb_1<7])
smallD[,labT1gestagedays_0:=NULL]
smallD[,labhb_0:=NULL]
xtabs(~smallD$T2_labhb_anemia_sev_15_17)


# mild and moderate anemia
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labhb_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_mild_mod",
  variableOfInterestPattern="labhb",
  TruevaluesMin=7,
  TruevaluesMax=10.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
nrow(smallD[labhb_1>=7 & labhb_1<11])
smallD[,labT1gestagedays_0:=NULL]
smallD[,labhb_0:=NULL]
nrow(smallD[labgestage_1<=15 & labgestage_1<=17 & labhb_1>7 & labhb_1<11])
xtabs(~smallD$T2_labhb_anemia_mild_mod_15_17, addNA=T)



### Lab RBS Normal ####
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,laburglu_0:=booklaburglu]
# normal urine glucose
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_exists",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("POS", "NEG"),
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,laburglu_0:=NULL]
xtabs(~smallD$T2_laburglu_exists_15_17)

# lab urglu pos
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,laburglu_0:=booklaburglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_pos",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete =c("POS"),
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,laburglu_0:=NULL]
nrow(smallD[laburglu_1=="POS" & labgestage_1>0 & labgestage_1<=14])
xtabs(~smallD$T2_laburglu_pos_00_14)


# labbloodglu exist
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_exists",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$T2_labbloodglu_exists_15_17)

# high blood glucose
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_high",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=140,
  TruevaluesMax=500,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$T2_labbloodglu_high_00_14)
xtabs(~smallD$T2_labbloodglu_high_18_22)


# Lab FBS exists
#http://perinatology.com/Reference/Reference%20Ranges/Glucose,%20fasting.htm
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$T2_labfastbloodglu_exists_15_17)

# Lab FBS Normal
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_normal",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=71,
  TruevaluesMax=91,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$T2_labfastbloodglu_normal_15_17)

# Lab FBS likely GDM
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_likelyGDM",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=92,
  TruevaluesMax=125,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$T2_labfastbloodglu_likelyGDM_24_28)


# Lab FBS High 
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_high",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=126,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$T2_labfastbloodglu_high_24_28)




# Referral variables

####Referrals####
# Ref to HR
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHR",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHighRisk",
  gestagedaysVariable = "manT2gestagedays")
nrow(smallD[mantypex_1=="RefHighRisk" & manT2gestagedays_1>=15 & manT2gestagedays_1<=17])
nrow(smallD[mantypex_1=="RefHighRisk" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$T2_refHR_00_14)
xtabs(~smallD[ident_dhis2_control==F]$T2_refHR_00_14)
xtabs(~smallD$T2_refHR_35_37)

# Ref to Hosp
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "manT2gestagedays")
nrow(smallD[mantypex_1=="RefHosp" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$T2_refHosp_00_14)
xtabs(~smallD[ident_dhis2_control==F]$mantypex_1, addNA=T)

# RefDiabetes
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refDiab",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefDiabetes",
  gestagedaysVariable = "manT2gestagedays")
nrow(smallD[mantypex_1=="RefDiabetes" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD$T2_refDiab_00_14)


# Management Performed
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="manperf",
  variableOfInterestPattern="manperf",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "manT2gestagedays")
xtabs(~smallD$T2_manperf_18_22)





###################################################################################################################
#########################    Managements    #########################
###################################################################################################################


#take into account the 4 weeks after 37

#sev anemia
for(i in 0:37){
  #i=23
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manhb <- sprintf("T2_manhb_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manhb <- "temp_manhb"
  
  #id source
  var_badhb <- sprintf("T2_labhb_anemia_sev_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manhb):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badhb)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badhb)==TRUE, (var_temp_manhb):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("T2_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manhb)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manhb):=TRUE]
  }
  #making var for sev anemia 
  smallD[,(var_manhb):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manhb):=get(var_temp_manhb)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manhb):=get(var_temp_manhb) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manhb):=NULL]
}
xtabs(~smallD$T2_manhb_24_24)

# create the man vars we want/join the weeks together
#pmax does horizontal maximum for wide format

# smallD[,TrialOne_manhb_07_12:=pmax(
#   TrialOne_manhb_07_07,
#   TrialOne_manhb_08_08,
#   TrialOne_manhb_09_09,
#   TrialOne_manhb_10_10,
#   TrialOne_manhb_11_11,
#   TrialOne_manhb_12_12,
#   na.rm=T)
#   ]
####### Check time ranges for each of the vars #########

#mild_mod anemia retest after one month 
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(3:5), width=2, flag="0")
  
  #output variable
  var_manhb <- sprintf("TrialOne_manhb_mildmodhbret_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manhb <- "temp_manhb"
  
  #id source
  var_badhb <- sprintf("TrialOne_labhb_anemia_mild_mod_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manhb):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badhb)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badhb)==TRUE, (var_temp_manhb):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_labhb_exists_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second anemia check
    #var_secondcheck <- sprintf("TrialOne_labhb_exists_%s_%s", 
    #    week_later, 
    #   week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manhb)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manhb):=TRUE]
  }
  #making var for sev anemia 
  smallD[,(var_manhb):=as.logical(NA)]
  
  #usualy only do this for control, but because we only want a retest not a manperf
  smallD[,(var_manhb):=get(var_temp_manhb)]
  
  #intervention
  #smallD[ident_dhis2_control==F,(var_manhb):=get(var_temp_manhb) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manhb):=NULL]
}
xtabs(~smallD$TrialOne_manhb_mildmodhbret_32_32)

#mild htn
#Urine stick AND LFT AND KFT AND ultrasound within a week 
#refer to hospital if proteinuria




























#### fix risk factors


# need to make a risktype_0 variable
smallD[,riskytype_0:=as.character(NA)]

smallD[bookdate==riskdate_1,risktype_0:=risktype_1]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="risktype_",
  variableOfInterestPattern="risktype",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("GDM","LikelyGDM","MildAnemia",
                         "SevereAnemia","MildAnemia","ModerateAnemia",
                         "ChronicHypertension","UrineStickProtein2"),
  gestagedaysVariable="anT2gestagedays")

smallD[,anT2gestagedays_0:=NULL]
xtabs(~smallD$T2_riskT_00_33, addNA=T)




# get atleast one risk factor true and false

# add selection criterial here





######################### BP ######################### 











# save smallD to be used for other outcomes
# create directory  per date ran for folder and save in that folder with the dates to load in outcomes

if(IS_GAZA==F){
 saveRDS(smallD,file.path(FOLDER_DATA_CLEAN,
                          "T2_clean",
                          "WB",
                          sprintf("T2_dataset_%s_%s.rds",
                                  CLINIC_INTERVENTION_DATE,
                                  fileTag)))
 

fwrite(smallD,file.path(FOLDER_DATA_CLEAN,
                        "T2_clean",
                        "WB",
                        sprintf("T2_dataset_%s_%s.csv",
                                CLINIC_INTERVENTION_DATE,
                                fileTag)))


} else{
  saveRDS(smallD,file.path(FOLDER_DATA_CLEAN_GAZA,
                           "T2_clean",
                           sprintf("T2_dataset_%s_%s.rds",
                                   CLINIC_INTERVENTION_DATE,
                                   fileTag)))
  
  
  fwrite(smallD,file.path(FOLDER_DATA_CLEAN_GAZA,
                          "T2_clean",
                          sprintf("T2_dataset_%s_%s.csv",
                                  CLINIC_INTERVENTION_DATE,
                                  fileTag)))
  
  
  
  
  
  
}
