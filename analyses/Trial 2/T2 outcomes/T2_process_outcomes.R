
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
  anT1_gA <- stringr::str_replace(an_date, "andate","anT1gestagedays")
  
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
  manT1_gA <- stringr::str_replace(man_date, "mandate","manT1gestagedays")
  
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
smallD  <- d[bookdate>="2019-03-01" & 
               (ident_dhis2_booking==T | ident_dhis2_an==T) &
               !is.na(precovid) &
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
           get(datevar)>="2019-12-01",firstvisitinT2:=i]
  
  # add other limits like in the precovid stuff
  
  
}

for(i in 0:(length(temp)-1)){
  
  datevar <- paste0("andate_",i)
  
  gestagevar <- paste0("angestage_",i)
  
  outcomevar <- paste0("anT2gestagedays_",i)
  
  smallD[i>=firstvisitinT2,(outcomevar):=as.numeric(floor(difftime(get(datevar),lmpT1, units="days")))]
  
}




#ANC gAs
an_date <- stringr::str_subset(names(smallD), "^andate_[0-9]+")
anT2_gA <- stringr::str_replace(an_date, "andate","anT2gestagedays")

for(i in seq_along(an_date)){
  var_an_gestage <- anT2_gA[i]
  var_an_date<- an_date[i] 
  
  smallD[,(var_an_gestage):=as.numeric(floor(difftime(get(var_an_date),lmpT1, units="days")))]
  
}

smallD[,anT2gestagedays_0:=bookgestagedays]


smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anT2visit",
  variableOfInterestPattern="anT2gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT2gestagedays")

smallD[,anT1gestagedays_0:=NULL]
xtabs(~smallD$T2_anT2visit_00_14)



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
