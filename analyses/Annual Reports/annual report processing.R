
####################  2020 Report #################### 

# create vars for analysis

###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)
#Setup(IS_GAZA=TRUE)

###### SETUP ENDS ######
if(IS_GAZA==F){
  
#Load data in data from Palestine
  
  d <- LoadDataFileFromNetwork()

} else {
  
  d <- LoadDataFileFromNetwork()
  
  
}


# defining dataset
smallD  <- d[bookdate >= "2019-01-01"]
if(IS_GAZA==F){
### use gestational ages from creatin further variables
smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                                  breaks=c(-500,0,104,
                                           125,160,167,202,
                                           216,237,244,265,293),
                                  include.lowest=T)]
  
} else {
  
  
  
  nam <- names(smallD)[stringr::str_detect(names(smallD),"^usedd_[0-9]*$")]
  num <- stringr::str_replace(nam,"usedd_","")
  smallD[,first_1_21_usedd:=as.Date(NA)]
  for(i in num ){
    print(i)
    
    var_usedd <- sprintf("usedd_%s",i)
    var_usgestage <- sprintf("usgestage_%s",1)
    
    smallD[!is.na(get(var_usedd)) &
        
        !is.na(get(var_usgestage)) &
        get(var_usgestage) > 0 &
        get(var_usgestage) < 23 &
        is.na(first_1_21_usedd),
      first_1_21_usedd:=as.Date(get(var_usedd),format="%Y-%m-%d")]
  }
  
  smallD[,usedddays:=first_1_21_usedd - as.difftime(280, unit="days")]
  smallD[,USorLMPdate:=booklmp]
  smallD[!is.na(usedddays),USorLMPdate:=usedddays]
  #d[is.na(USorLMPdate), USorLMPdate:=]
  
  smallD[,bookgestagedays:=round(as.numeric(difftime(
    bookdate,USorLMPdate, units="days")))] 
  
  #gestagedaysCats
  ### making gestational ages based on usedd=<20, then lmp, then us>20 ###
  # + means repeat, $ means the end of the entire string
  smallD[,lmpT1:=as.Date(NA)]
  us_gestages <- stringr::str_subset(names(smallD), "^usgestage_[0-9]+")
  us_date <- stringr::str_subset(names(smallD), "^usdate_[0-9]+")
  
  
  #seq_along identical to 1:lenth()
  for(i in seq_along(us_gestages)){
    var_us_gestage <- us_gestages[i]
    var_us_date<- us_date[i]
    
    smallD[is.na(lmpT1) & 
        get(var_us_gestage)<= 20, 
      
      lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
    
  }
  
  smallD[is.na(lmpT1) & !is.na(booklmp), lmpT1:=booklmp]
  
  for(i in seq_along(us_gestages)){
    var_us_gestage <- us_gestages[i]
    var_us_date<- us_date[i]
    
    smallD[is.na(lmpT1) & get(var_us_gestage)>=20 & get(var_us_gestage)<40,
      
      lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
    
    
  }
  
  #qc new variables
  smallD[is.na(lmpT1), c("booklmp",us_gestages, us_date), with=F]
  
  
  
  ##looping through to make gestages for each of the program stages
  #Us gAs
  us_date <- stringr::str_subset(names(smallD), "^usdate_[0-9]+")
  usT1_gA <- stringr::str_replace(us_date, "usdate","usT1gestagedays")
  
  for(i in seq_along(us_date)){
    var_us_gestage <- usT1_gA[i]
    var_us_date<- us_date[i] 
    
    smallD[,(var_us_gestage):=as.numeric(floor(difftime(get(var_us_date),lmpT1, units="days")))]
    
  }
  
  
  #ANC gAs
  an_date <- stringr::str_subset(names(smallD), "^andate_[0-9]+")
  anT1_gA <- stringr::str_replace(an_date, "andate","anT1gestagedays")
  
  for(i in seq_along(an_date)){
    var_an_gestage <- anT1_gA[i]
    var_an_date<- an_date[i] 
    
    smallD[,(var_an_gestage):=as.numeric(floor(difftime(get(var_an_date),lmpT1, units="days")))]
    
  }
  
  #Lab gAs
  ##NEED TO ROUND OR USE FLOOR
  lab_date <- stringr::str_subset(names(smallD), "^labdate_[0-9]+")
  labT1_gA <- stringr::str_replace(lab_date, "labdate","labT1gestagedays")
  
  for(i in seq_along(lab_date)){
    var_lab_gestage <- labT1_gA[i]
    var_lab_date<- lab_date[i] 
    
    smallD[,(var_lab_gestage):=as.numeric(floor(difftime(get(var_lab_date),lmpT1, units="days")))]
    
  }
  
  #Man gAs
  man_date <- stringr::str_subset(names(smallD), "^mandate_[0-9]+")
  manT1_gA <- stringr::str_replace(man_date, "mandate","manT1gestagedays")
  
  for(i in seq_along(man_date)){
    var_man_gestage <- manT1_gA[i]
    var_man_date<- man_date[i] 
    
    smallD[,(var_man_gestage):=as.numeric(floor(difftime(get(var_man_date),lmpT1, units="days")))]
    
  }
  
  ##making categories of days for booking
  
  smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                               breaks=c(-500,0,104,
                                        125,160,167,202,
                                        216,237,244,265,293),
                               include.lowest=T)]
  
  
}



xtabs(~smallD$bookgestagedays_cats, addNA=T)




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


# high sugar at booking
smallD[,bookhrhighsug:=as.logical(NA)]

smallD[(mandate_1-bookdate)<=7,bookhrhighsug:=FALSE] 
smallD[(mandate_1-bookdate)<=7 &
    manperf_1==1 &
    mantypex_1 %in% c("RefHighRisk","RefDiabetes","RefSpec"),bookhrhighsug:=TRUE] 

xtabs(~smallD$bookhrhighsug, addNA=T)



# Discrepancy Variable anexamsfh variable
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,angestage_0:=bookgestage]
vars <- stringr::str_subset(names(smallD), "^anexamsfh_")

vars <- stringr::str_remove(vars, "anexamsfh_")

#anexamsfh stuff
## this is a variable that is made using a combination of anexamsfh, angestage, and ## ancongestageatvisitweeks_1. control uses that last one, if its present. If not,
## if uses the anexamsfh

if(IS_GAZA==F){
for(i in vars){
  print(i)
  anexamsfh <-sprintf("anexamsfh_%s",i)
  conangestage <-sprintf("anconancgestationaageatvisitweeks_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrep <- sprintf("sfhDiscrep_%s",i)
  
  smallD[,(sfhDiscrep):=as.numeric(NA)]
  
  smallD[!is.na(get(angestage)) &
           !is.na(get(anexamsfh)), (sfhDiscrep):=abs(get(anexamsfh)-get(angestage))]
  
  smallD[!is.na(get(conangestage)) &
           !is.na(get(anexamsfh)), (sfhDiscrep):=abs(get(anexamsfh)-get(conangestage))]
  
 }

# SFH discrepancy with ancongestagesizevisitweek
vars <- stringr::str_subset(names(smallD), "^anconancgestationaageatvisitweeks_")
vars <- stringr::str_remove(vars, "anconancgestationaageatvisitweeks_")

#anconancgestationaageatvisitweeks var
for(i in vars){
  print(i)
  anconangestageweeks <-sprintf("anconancgestationaageatvisitweeks_%s",i)
  anexmsfh <- sprintf("anexamsfh_%s",i)
  sfhDiscrepCon <-  sprintf("sfhDiscrepCon_%s",i)
  
  
  smallD[,(sfhDiscrepCon):=as.numeric(NA)]
  
  smallD[!is.na(get(anexmsfh)) &
           !is.na(get(anconangestageweeks)), 
         (sfhDiscrepCon):=abs(get(anconangestageweeks)-get(anexamsfh))]
  
 }

}


# anT1 in weeks to calculate sfhDiscrep via anexamsfh and anT1gestagedays to weeks
smallD[,anT1gestagedays_0:=bookgestagedays]
vars <- stringr::str_subset(names(smallD), "^anT1gestagedays_")
vars <- stringr::str_remove(vars, "^anT1gestagedays_")
for (i in vars){
  anT1gestagedays <- sprintf("anT1gestagedays_%s",i)
  anT1gAweeks <- sprintf("anT1gAweeks_%s",i)
  
  smallD[, (anT1gAweeks):=floor(get(anT1gestagedays)/7)]
}

# Discrepancy Variable anexamsfh variable
smallD[,anexamsfh_0:=bookexamsfh]

vars <- stringr::str_subset(names(smallD), "^anexamsfh_")
vars <- stringr::str_remove(vars, "anexamsfh_")

# i in length will just print out the length
# i in 1:length same as seq_along. but seq_along preferred bc works when length of vector
#is 0. Use this if have predefined 
# i in vars (object) take every single value of vars and loop through them

#anexamsfh stuff with anT1gAweeks
for(i in vars){
  print(i)
  anexamsfh <-sprintf("anexamsfh_%s",i)
  anT1gAweeks <- sprintf("anT1gAweeks_%s",i)
  sfhDiscrepAnt1gas <-  sprintf("sfhDiscrepAnt1gas_%s",i)
  sfhDiscrepAnt1gasCat <-  sprintf("sfhDiscrepAnt1gasCat_%s",i)
  
  smallD[!is.na(anT1gAweeks) &
           !is.na(get(anexamsfh)), (sfhDiscrepAnt1gas):=abs(get(anexamsfh)-get(anT1gAweeks))]
  
  
}

# an exam malpresentation into one variable
vars_source <- names(d)[stringr::str_detect(names(smallD),"^anexampalp_")]
vars_outcome <- stringr::str_replace(vars_source, "anexampalp", "malpresanexam_")

for(i in 1:length(vars_source)){
  var_source <- vars_source[i]
  var_outcome <- vars_outcome[i]
  smallD[get(var_source) %in% c("Trasverse", "Breech"), (var_outcome):="Yes"]
}

# uspres malpresentation variable
vars_source <- names(d)[stringr::str_detect(names(smallD),"^uspres_")]
vars_outcome <- stringr::str_replace(vars_source, "uspres_", "us_malpres_")

for(i in 1:length(vars_source)){
  var_source <- vars_source[i]
  var_outcome <- vars_outcome[i]
  smallD[get(var_source) %in% c("Trasverse", "Breech"), (var_outcome):="Yes"]
  
}




# bookbmi and bookbmicat
# bookgestagecat, bookgestagedays_cat, 

# bookbp make into cats

smallD[,bookbpdiastcat:=cut(bookbpdiast,
                                breaks=c(0,50,89,99,110,500),
                                include.lowest=T)]
xtabs(~smallD$bookbpdiastcat)


smallD[,bookbpsystcat:=cut(bookbpsyst,
                            breaks=c(0,60,139,149,159,500),
                            include.lowest=T)]

xtabs(~smallD$bookbpsystcat)


# history
# on outcomes sheet
#bookrefchronic

# first ultrasound at or after booking
#bookus

smallD[,usdaysafterbooking:=difftime(usedd_1,
                                     bookdate,
                                     units="days")]

xtabs(~smallD[ident_dhis2_booking==1]$usdaysafterbooking, addNA=T)







VisitVariables <- function(smallD,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="anT1gestagedays" ){
  
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
    var <- sprintf("TrialOne_%s_%s",variableOfInterestName,names(days)[i])
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


###ANC Visits####
smallD[,anT1gestagedays_0:=bookgestagedays]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="anT1gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anvisitnew_00_00)

###ANC BP SYT ####
# BP SYST Present
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD<-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_present",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=60,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$TrialOne_anbpsyst_present_00_00)

# BP Diast Present
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]

smallD<- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_present",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=40,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$TrialOne_anbpdiast_present_00_14)

# BP Syst High
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_high",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$TrialOne_anbpsyst_high_00_14)

# BP Syst MildHTN
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_mildHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=149,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$TrialOne_anbpsyst_mildHTN_00_14)

# BP Syst ModSevHTN
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpsyst_0:=bookbpsyst]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_modSevHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=150,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpsyst_0:=NULL]
xtabs(~smallD$TrialOne_anbpsyst_modSevHTN_00_14)

# BP Diast High
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]

smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_high",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$TrialOne_anbpdiast_high_00_14)


# BP Diast MildHTN
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_mildHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=99,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$TrialOne_anbpdiast_mildHTN_00_14)


# BP Diast Mod/SevHTN
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD[,anbpdiast_0:=bookbpdiast]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_modSevHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=100,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
smallD[,anT1gestagedays_0:=NULL]
smallD[,anbpdiast_0:=NULL]
xtabs(~smallD$TrialOne_anbpdiast_modSevHTN_00_14)


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
xtabs(~smallD$TrialOne_labhb_exists_15_17)

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
xtabs(~smallD$TrialOne_labhb_normal_15_17)

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
xtabs(~smallD$TrialOne_labhb_anemia_sev_15_17)


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
xtabs(~smallD$TrialOne_labhb_anemia_mild_mod_15_17, addNA=T)


smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refSpec",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefSpec",
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefSpec" & manT1gestagedays_1>=15 & manT1gestagedays_1<=17])
nrow(smallD[mantypex_1=="RefSpec" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_refHR_00_14)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_refHR_00_14)
xtabs(~smallD$TrialOne_refSpec_35_37)

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
xtabs(~smallD$TrialOne_laburglu_exists_15_17)

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
xtabs(~smallD$TrialOne_laburglu_pos_00_14)


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
xtabs(~smallD$TrialOne_labbloodglu_exists_15_17)

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
xtabs(~smallD$TrialOne_labbloodglu_high_00_14)
xtabs(~smallD$TrialOne_labbloodglu_high_18_22)


# Lab FBS likely GDM
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_likelyGDM",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=92,
  TruevaluesMax=125,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labbloodglu_likelyGDM_24_28)


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
xtabs(~smallD$TrialOne_labfastbloodglu_exists_15_17)

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
xtabs(~smallD$TrialOne_labfastbloodglu_normal_15_17)

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
xtabs(~smallD$TrialOne_labfastbloodglu_likelyGDM_24_28)


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
xtabs(~smallD$TrialOne_labfastbloodglu_high_24_28)

#### US visits ####
# Has US visit
smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_exists",
  variableOfInterestPattern="usT1gestagedays",
  TruevaluesMin=10,
  TruevaluesMax=300,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~smallD$TrialOne_us_exists_00_14)

# US suspected IUGR
smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_iugrSuspected",
  variableOfInterestPattern="usiugr",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = 1,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~smallD$TrialOne_us_iugrSuspected_00_14)

# US expected LGA
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_lgaSuspected",
  variableOfInterestPattern="uslga",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_lgaSuspected_00_14)

# US pres-malpresentation
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_malpres",
  variableOfInterestPattern="us_malpres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete="Yes",
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_malpres_00_14)

# US pres-malpresentation
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_malpresvar",
  variableOfInterestPattern="uspres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete= c("Trasverse","Breech"),
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_malpresvar_00_14)

#uspres_checked
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_pres_checked",
  variableOfInterestPattern="uspres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete= c("Trasverse","Breech","Cephalic","Unknown"),
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_pres_checked_00_14, addNA=T)






####SFH Discrepancies####
#AN SFH measurements
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,anTgestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexamsfh_exists",
  variableOfInterestPattern="anexamsfh",
  TruevaluesMin=1,
  TruevaluesMax=44,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexamsfh_0:=NULL]
smallD[,anTgestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anexamsfh_exists_00_14)



# sfhDiscrepAnt1gas
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,anTgestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepExists",
  variableOfInterestPattern="sfhDiscrepAnt1gas",
  TruevaluesMin=3,
  TruevaluesMax=200,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexamsfh_0:=NULL]
smallD[,anTgestagedays_0:=NULL]
xtabs(~smallD$TrialOne_sfhDiscrepExists_15_17)


######## Re Check These SFH Discrep variables!!!!!################
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepExists",
  variableOfInterestPattern="sfhDiscrep",
  TruevaluesMin=2.1,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
xtabs(~smallD$TrialOne_sfhDiscrepExists_00_14)


#anexampalp malpres
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="malpresanexam",
  variableOfInterestPattern="malpresanexam_",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = "Yes",
  gestagedaysVariable = "anT1gestagedays")
xtabs(~smallD$TrialOne_sfhDiscrepExists_00_14)

#anexampalp source
smallD[,anexampalp_0:=bookexampalp]
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexampalpmal",
  variableOfInterestPattern="anexampalp",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("Trasverse","Breech"),
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexampalp_0:=NULL]
smallD[,anT1gestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anexampalpmal_00_14)

#anexampalp yes
smallD[,anexampalp_0:=bookexampalp]
smallD[,anT1gestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexampalpyes",
  variableOfInterestPattern="anexampalp",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("Trasverse","Breech","Cephalic","Unknown"),
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexampalp_0:=NULL]
smallD[,anT1gestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anexampalpyes_00_14)


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
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefHighRisk" & manT1gestagedays_1>=15 & manT1gestagedays_1<=17])
nrow(smallD[mantypex_1=="RefHighRisk" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_refHR_00_14)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_refHR_00_14)
xtabs(~smallD$TrialOne_refHR_35_37)

# Ref to Hosp
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefHosp" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_refHosp_00_14)
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
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefDiabetes" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD$TrialOne_refDiab_00_14)


# Management Performed
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="manperf",
  variableOfInterestPattern="manperf",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "manT1gestagedays")
xtabs(~smallD$TrialOne_manperf_18_22)



######### Managements ############

# take into account the 4 weeks after 37

#sev anemia
for(i in 0:37){
  #i=23
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manhb <- sprintf("TrialOne_manhb_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manhb <- "temp_manhb"
  
  #id source
  var_badhb <- sprintf("TrialOne_labhb_anemia_sev_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manhb):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badhb)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badhb)==TRUE, (var_temp_manhb):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
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
xtabs(~smallD$TrialOne_manhb_24_24)

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

#ModsevGHTbpsyst
for(i in 0:37){
  #i=23
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(3:4), width=2, flag="0")
  
  #output variable
  var_manght <- sprintf("TrialOne_manhtn_ModSev_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manght <- "temp_manght"
  
  #id source
  var_badght <- sprintf("TrialOne_anbpsyst_modSevHTN_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manght):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badght)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badght)==TRUE, (var_temp_manght):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second anemia check
    var_secondcheck <- sprintf("TrialOne_anbpsyst_present_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manght)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manght):=TRUE]
  }
  #making var for sev anemia 
  smallD[,(var_manght):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manght):=get(var_temp_manght)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manght):=get(var_temp_manght) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manght):=NULL]
}
xtabs(~smallD$TrialOne_manhtn_ModSev_18_18)

# High RBG, RefHosp
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_Hosp_24_24)


# High RBG, RefHR
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHR_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_HR_24_24)

# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_Diab_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_HR_24_24)


# malpresentation: us_malpres
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manpres <- sprintf("TrialOne_manmalpres_us_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manpres <- "temp_manpres"
  
  #id source
  var_badpres <- sprintf("TrialOne_us_malpresvar_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manpres):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badpres)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badpres)==TRUE, (var_temp_manpres):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manpres)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manpres):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_manpres):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manpres):=get(var_temp_manpres)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manpres):=get(var_temp_manpres) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manpres):=NULL]
  
}
xtabs(~smallD$TrialOne_manmalpres_us_36_36)


# malpresentation: anexampalpmal
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manpres <- sprintf("TrialOne_manmalpres_anexam_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manpres <- "temp_manpres"
  
  #id source
  var_badpres <- sprintf("TrialOne_anexampalpmal_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manpres):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badpres)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badpres)==TRUE, (var_temp_manpres):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manpres)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manpres):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_manpres):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manpres):=get(var_temp_manpres)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manpres):=get(var_temp_manpres) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manpres):=NULL]
  
}
xtabs(~smallD$TrialOne_manmalpres_anexam_35_35)


#iugr Ref hosp
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_maniugr <- sprintf("TrialOne_maniugr_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_maniugr <- "temp_maniugr"
  
  #id source
  var_badiugr <- sprintf("TrialOne_us_iugrSuspected_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_maniugr):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badiugr)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badiugr)==TRUE, (var_temp_maniugr):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_maniugr)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_maniugr):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_maniugr):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_maniugr):=get(var_temp_maniugr)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_maniugr):=get(var_temp_maniugr) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_maniugr):=NULL]
  
}
xtabs(~smallD$TrialOne_maniugr_Hosp_32_32)

#iugr Ref HR
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_maniugr <- sprintf("TrialOne_maniugr_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_maniugr <- "temp_maniugr"
  
  #id source
  var_badiugr <- sprintf("TrialOne_us_iugrSuspected_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_maniugr):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badiugr)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badiugr)==TRUE, (var_temp_maniugr):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHR_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_maniugr)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_maniugr):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_maniugr):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_maniugr):=get(var_temp_maniugr)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_maniugr):=get(var_temp_maniugr) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_maniugr):=NULL]
  
}
xtabs(~smallD$TrialOne_maniugr_HR_32_32)

#lga Ref HR
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manlga <- sprintf("TrialOne_manlga_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manlga <- "temp_manlga"
  
  #id source
  var_badlga <- sprintf("TrialOne_us_lgaSuspected_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manlga):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badlga)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badlga)==TRUE, (var_temp_manlga):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHR_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manlga)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manlga):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_manlga):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manlga):=get(var_temp_manlga)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manlga):=get(var_temp_manlga) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manlga):=NULL]
  
}
xtabs(~smallD$TrialOne_manlga_HR_32_32)

#lga Ref hosp
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_manlga <- sprintf("TrialOne_manlga_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_manlga <- "temp_manlga"
  
  #id source
  var_badlga <- sprintf("TrialOne_us_lgaSuspected_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_manlga):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badlga)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badlga)==TRUE, (var_temp_manlga):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manlga)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manlga):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_manlga):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_manlga):=get(var_temp_manlga)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manlga):=get(var_temp_manlga) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manlga):=NULL]
  
}
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manlga_Hosp_32_32)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manlga_Hosp_32_32)

########################## Referred for any management ##########################

############ Ref Hosp ####################
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:0), width=2, flag="0")
  
  #output variable
  var_refHosp <- sprintf("TrialOne_manRef_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_refHosp <- "temp_refHosp"
  
  #id source
  var_refHospsource <- sprintf("TrialOne_refHosp_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_refHosp):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  smallD[get(var_refHospsource)==TRUE, (var_temp_manperf):=FALSE]
  
  # everyone
  #smallD[!is.na(get(var_refHospsource)), (var_temp_refHosp):=FALSE]
  
  # control
  smallD[get(var_refHospsource)==TRUE, (var_temp_refHosp):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  smallD[,(var_refHosp):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_refHosp):=get(var_temp_refHosp)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_refHosp):=get(var_temp_manperf) & 
           get(var_temp_refHosp)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_refHosp):=NULL]
  
}
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_Hosp_35_35)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_Hosp_35_35)
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_Hosp_32_32)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_Hosp_32_32)

checkHosp <- smallD[!is.na(TrialOne_manRef_Hosp_32_32) &
                      ident_dhis2_control==F, c("TrialOne_manperf_32_32",
                                                "TrialOne_refHosp_32_32",
                                                "TrialOne_manRef_Hosp_32_32")]


########## Ref HR for any reason at any time point #########
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:0), width=2, flag="0")
  
  #output variable
  var_refHR <- sprintf("TrialOne_manRef_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_refHR <- "temp_refHR"
  
  #id source
  var_refHRsource <- sprintf("TrialOne_refHR_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_refHR):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  smallD[get(var_refHRsource)==TRUE, (var_temp_manperf):=FALSE]
  
  
  # control
  smallD[get(var_refHRsource)==TRUE, (var_temp_refHR):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  smallD[,(var_refHR):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_refHR):=get(var_temp_refHR)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_refHR):=get(var_temp_manperf) & 
           get(var_temp_refHR)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_refHR):=NULL]
  
}
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_HR_35_35)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_HR_35_35)

xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_HR_20_20)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_HR_20_20)

checkHR <- smallD[!is.na(TrialOne_manRef_HR_20_20) &
                    ident_dhis2_control==F, c("TrialOne_manperf_20_20",
                                              "TrialOne_refHR_20_20",
                                              "TrialOne_manRef_HR_20_20")]


##### save data set for analysis of annual report ##### 
if(IS_GAZA==F){
  
  
 saveRDS(smallD,
        file.path(FOLDER_DATA_CLEAN,
                  "annual reports",
                  "annualreportdata.RDS"))
  
  
  
} else {
  
  saveRDS(smallD,
          file.path(FOLDER_DATA_CLEAN_GAZA,
                    "annual reports",
                    "annualreportdata.RDS"))
  
  
}

