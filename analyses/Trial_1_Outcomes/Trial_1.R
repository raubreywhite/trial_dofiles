
####################  Trial 1 Outcomes #################### 

###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)
###### SETUP ENDS ######

#Load data in
d <- LoadDataFileFromNetwork()

# renaming arms
d[ident_dhis2_control==F, prettyExposure:="Trial Arm B"]
d[ident_dhis2_control==T, prettyExposure:="Trial Arm A"]

 # defining dataset
smallD <- smallD <- d[bookdate >= "2017-01-15"&
                        bookdate<="2017-09-15" &
                        ident_TRIAL_1==T,]

### use gestational ages from creatin further variables
smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                             breaks=c(-500,0,104,
                                      125,160,167,202,
                                      216,237,244,265,293),
                             include.lowest=T)]

# MAKE BOOK VISIT FOR ANEMIA
smallD[,booklabhb:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]



# Discrepancy Variable anexamsfh variable
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,angestage_0:=bookgestage]
vars <- stringr::str_subset(names(smallD), "^anexamsfh_")

vars <- stringr::str_remove(vars, "anexamsfh_")

#anexamsfh stuff
for(i in vars){
  print(i)
  anexamsfh <-sprintf("anexamsfh_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrep <-  sprintf("sfhDiscrep_%s",i)
  
  smallD[,(sfhDiscrep):=as.numeric(NA)]
  
  smallD[!is.na(get(angestage)) &
           !is.na(get(anexamsfh)), (sfhDiscrep):=abs(get(anexamsfh)-get(angestage))]
  
}

# SFH discrepancy with ancongestagesizevisitweek
vars <- stringr::str_subset(names(smallD), "^anconancgestationaageatvisitweeks_")
vars <- stringr::str_remove(vars, "anconancgestationaageatvisitweeks_")

#anconancgestationaageatvisitweeks var
for(i in vars){
  print(i)
  anconangestageweeks <-sprintf("anconancgestationaageatvisitweeks_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrepCon <-  sprintf("sfhDiscrepCon_%s",i)
  
  smallD[,(sfhDiscrepCon):=as.numeric(NA)]
  
  smallD[!is.na(get(angestage)) &
           !is.na(get(anconangestageweeks)), 
              (sfhDiscrepCon):=abs(get(anconangestageweeks)-get(angestage))]
  
}


openxlsx::write.xlsx(smallD[,c("bookgestage",
                               "anconancgestationaageatvisitweeks_1")], 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "booking_and_visits",
                       sprintf("%s_bookgA_ancongA.xlsx", lubridate::today())))


# anT1 in weeks to calculate sfhDiscrep via anexamsfh and anT1gestagedays to weeks
smallD[,anT1gestagedays_0:=bookgestagedays]
vars <- stringr::str_subset(names(smallD), "^anT1gestagedays_")
vars <- stringr::str_remove(vars, "^anT1gestagedays_")
for (i in vars){
  anT1gestagedays <- sprintf("anT1gestagedays_%s",i)
  anT1gAweeks <- sprintf("anT1gAweeks_%s",i)
 
  smallD[, (anT1gAweeks):=floor(get(anT1gestagedays)/7)]
}

gAscheck <- smallD[,c("bookgestagedays",
                      "bookgestage",
                      "anT1gAweeks_0",
                      "anT1gAweeks_1",
                      "anT1gestagedays_1",
                      "angestage_1",
                      "anT1gAweeks_2",
                      "anT1gestagedays_2",
                      "angestage_2",
                      "anT1gAweeks_3",
                      "angestage_3",
                      "anT1gestagedays_3")]


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

smallD[!is.na(anT1gAweeks) &
         !is.na(get(anexamsfh)), (sfhDiscrepAnt1gasCat):=abs(get(anexamsfh)-get(anT1gAweeks))>2]

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


VisitVariables <- function(smallD,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="anT1gestagedays" ){

  if(!is.null(TruevaluesMin) & !is.null(TruevaluesMax) & !is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NOT NULL")
  }
  
  if(is.null(TruevaluesMin) & is.null(TruevaluesMax) & is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NULL")
  }
  
  
  # pull out a list of all of the gestage variables
  
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
nrow(smallD[labhb_1>=7 & labhb_1<=11])
smallD[,labT1gestagedays_0:=NULL]
smallD[,labhb_0:=NULL]
xtabs(~smallD$TrialOne_labhb_anemia_mild_mod_15_17, addNA=T)



### Lab RBS Normal ####
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,laburglu_0:=booklabhb]
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
smallD[,laburglu_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_pos",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="POS",
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,laburglu_0:=NULL]
xtabs(~smallD$TrialOne_laburglu_pos_15_17)

# normal bloodglu values
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabhb]
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
smallD[,labbloodglu_0:=booklabhb]
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
xtabs(~smallD$TrialOne_labbloodglu_high_15_17)


# Lab FBS Normal 
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_exists_15_17)



# Lab FBS High 
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabhb]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_high",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=105,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_high_00_14)

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


####SFH Discrepancies####
#AN SFH measurements
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,anTgestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexamsfh_exists",
  variableOfInterestPattern="anexamsfh",
  TruevaluesMin=5,
  TruevaluesMax=44,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexamsfh_0:=NULL]
smallD[,anTgestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anexamsfh_exists_00_14)


# sfhDiscrepAnt1gasCat
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,anTgestagedays_0:=bookgestagedays]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepBinary",
  variableOfInterestPattern="sfhDiscrepAnt1gasCat",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="TRUE",
  gestagedaysVariable = "anT1gestagedays")
smallD[,anexamsfh_0:=NULL]
smallD[,anTgestagedays_0:=NULL]
xtabs(~smallD$TrialOne_sfhDiscrepBinary_15_17)



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


smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepCon",
  variableOfInterestPattern="sfhDiscrepCon",
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
xtabs(~smallD$TrialOne_refHR_00_14)

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
xtabs(~smallD$TrialOne_refHosp_00_14)


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
xtabs(~smallD$TrialOne_manperf_00_14)



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
  
  #control
  smallD[ident_dhis2_control==T,(var_manhb):=get(var_temp_manhb)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_manhb):=get(var_temp_manhb) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_manhb):=NULL]
}
xtabs(~smallD$TrialOne_manhb_mildmodhbret_32_32)

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
xtabs(~smallD$TrialOne_manlga_Hosp_32_32)

########## lmpstatusKnown #########
varsT1all <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_")]
#Notes:
#calculate gA based on how system calculates it. 
#check code to make sure everything is recalculated
#any us dates prior to lmp dates should be reverted backwards from that date

lmpstatusKnown<-smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                          ArmB=sum(ident_dhis2_control==F, na.rm=TRUE)), 
                       keyby=.(booklmpknown)]


################################ HBO ################################

#gestationalage at delivery
nrow(smallD[!is.na(merged_gestagedeliv)])
nrow(smallD[merged_abortion==T])

missinggA<-smallD[is.na(mahima_hospenteredgestage_1),
                  
                  c("bookdate",
                    "ident_dhis2_control",
                    "booklmp",
                    "merged_datedeliv",
                    "merged_gestagedeliv",
                    "cpodate_1",
                    "cpopregoutcome_1", 
                    "prettyExposure",
                    "USorLMPdate",
                    "mahima_gestageatbirthwk_1")]

table<- missinggA[,.(
  BookdateNa=sum(is.na(bookdate)),
  MahimagestagebirthNA=sum(is.na(mahima_gestageatbirthwk_1)),
  USorLMPdateNa=sum(is.na(USorLMPdate)),
  DoBNa=sum(is.na(merged_datedeliv)),
  gAdelivNa=sum(is.na(merged_gestagedeliv)),
  BooklmpNA=sum(is.na(booklmp)),
  CPOdateNA=sum(is.na(cpodate_1)),
  CPOpregNA=sum(is.na(cpopregoutcome_1))),
  keyby=.(prettyExposure)]

#TO DO: figure out why some are missing gAs-either not calculated, not entered, etc

# Creating categories for the at birth variables
smallD[,USorLMPdateCombogAdays:= as.numeric(difftime(
  mahima_dateofbirth_1,
  USorLMPdate,
  units ="days"))]

smallD[,gAatBirth_cats:=cut((USorLMPdateCombogAdays),
                            breaks=c(-1000,0,14,104,119,
                                     125,154,167, 196,
                                     216,231,244,259,266,280,294,314,500000),
                            include.lowest=T)]

smallD[,gAatBirth_mostGAs_cats:=cut(((7*mahima_gestageatbirthwk_1)),
                                    breaks=c(-1000,0,14,104,119,
                                             125,154,167, 196,
                                             216,231,244,259,266,280,294,314,500000),
                                    include.lowest=T)]





#added 37-38, 38-40,40-42, above 45 weeks
#38 weeks=266 days
#40 weeks=280 days
#42 weeks=294 days
#45 weeks=315 days

######### HTN at Birth ##########
#cleaning systolic bp, move to top level cleaning sheet
smallD[,merged_bpdiast := stringr::str_replace(as.numeric(merged_bpdiast), 
                                               "[0-9][0-9][0-9]/$","")]

smallD[,merged_bpsyst:=as.numeric(merged_bpsyst)]

###making severe htn variable
smallD[,sevHTNatBirth:=NA]
smallD[!is.na(merged_bpsyst) &
         !is.na(merged_bpdiast),sevHTNatBirth:=FALSE]
smallD[(merged_bpsyst>=160 & merged_bpsyst<=300) |
         (merged_bpdiast>=110 & merged_bpdiast<=300),sevHTNatBirth:=TRUE ]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(sevHTNatBirth)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("SevHTNatBirth_%s.xlsx", 
                               lubridate::today())))



######### HBG at Birth ##########
unique(smallD$merged_birthhemo)

#cleaning 
smallD[,merged_birthhemo:=as.numeric(merged_birthhemo)]
unique(smallD$merged_birthhemo)

#variable for moderate or severe anemia
smallD[,SevOrModHbatBirth:=NA]
smallD[!is.na(merged_birthhemo),SevOrModHbatBirth:=FALSE]
smallD[merged_birthhemo>=3 & 
         merged_birthhemo<9,SevOrModHbatBirth:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(SevOrModHbatBirth)]

openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("SevModHBG_%s.xlsx", 
                               lubridate::today())))



######### Birth Weight ##########
unique(smallD$merged_pregbweight)

#cleaning 
smallD[,merged_pregbweight:=as.numeric(merged_pregbweight)]
unique(smallD$merged_pregbweight)

#2 options to clean them, when its less than 5, multiply it by 100
#when its >=10 and <40, but for this option it may not be the greatest idea because it could just be a shifted value from some other data variable in data entry

smallD[merged_pregbweight<7 &
         merged_pregbweight>0, 
       merged_pregbweight:=1000*merged_pregbweight]

smallD[merged_pregbweight<50 & merged_pregbweight>16, 
       merged_pregbweight:=100*merged_pregbweight]
#getting summary data
sapply(smallD$merged_pregbweight, mean, na.rm=TRUE)

summary(smallD[merged_pregbweight<6000 & merged_pregbweight>500]$merged_pregbweight)



######### Presentaion ##########
unique(smallD$merged_presentationdeliv)

smallD[,has_merged_presentation_breech:=FALSE]

smallD[merged_presentationdeliv %in% c("Breech",
                                       "Transverse",
                                       "TRANSVERSE",
                                       "Trasverse",
                                       "Breach",
                                       "TRANSVER BOTHE",
                                       "BREECH"),
       has_merged_presentation_breech:=TRUE]


######### Date Deliv ##########
unique(smallD$merged_gestagedeliv)
nrow(smallD[is.na(merged_gestagedeliv)])


######### GA ##########

##LmpT1 for the new LMP that we calculated

#cleaning
unique(smallD$merged_datedeliv)
nrow(smallD[is.na(merged_datedeliv)])

nrow(smallD[is.na(USorLMPdate)])

nrow(smallD[is.na(mahima_dateofbirth_1)])

nrow(smallD[is.na(mahima_gestageatbirthwk_1)])

nrow(smallD[is.na(mahima_hospenteredgestage_1)])


# Deliveries by gestational age category
# we should include outcome with this

#smallD[,mergedGABirth_cats:=cut(mahima_gestageatbirthwk_1,
#                        breaks=c(0,15,17),
#                       include.lowest = TRUE]

nam <- names(smallD)[stringr::str_detect(names(smallD),"^mahima_dateofbirth_[0-9]*$")]
num <- stringr::str_replace(nam,"mahima_dateofbirth_","")
for(i in num){
  print(i)
  
  smallD[,(sprintf("mahima_gestageatbirthwk_%s",i)):=round(as.numeric(difftime(
    get(sprintf("mahima_dateofbirth_%s", i)),
    USorLMPdate,
    units="weeks")),digits=1)]
}

nrow(smallD[is.na(mahima_gestageatbirthwk_1)])

smallD[is.na(mahima_gestageatbirthwk_1),
       mahima_gestageatbirthwk_1:=mahima_hospenteredgestage_1]


smallD[is.na(USorLMPdate),
       mahima_gestageatbirthwk_1:=mahima_hospenteredgestage_1]


#checking out who still has missing data and why
NomahimagA <- smallD[is.na(mahima_gestageatbirthwk_1), 
                     c("uniqueid",
                       "matching",
                       "merged_datedeliv",
                       "mahima_hospenteredgestage_1",
                       "merged_gestagedeliv",
                       "USorLMPdate")]

openxlsx::write.xlsx(NomahimagA, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("No_mahima_gA_%s.xlsx", lubridate::today())))

#making small data set to get birth weight data stuff
sgaLga <- smallD[,c("mahima_gestageatbirthwk_1","merged_pregbweight")]

openxlsx::write.xlsx(sgaLga, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("SgaLgaDataSet_%s.xlsx", lubridate::today())))


#making sgA variables
smallD[,sga:=as.logical(NA)]
smallD[!is.na(merged_pregbweight) & !is.na(USorLMPdateCombogAdays),sga:=FALSE]

#identifying sgas
smallD[sga==FALSE &
         merged_pregbweight>0 & 
         merged_pregbweight<2394 & 
         USorLMPdateCombogAdays>=259 & 
         USorLMPdateCombogAdays<=265, sga:=TRUE]
smallD[sga==FALSE &
         merged_pregbweight<2550 & 
         USorLMPdateCombogAdays>=266 & 
         USorLMPdateCombogAdays<=272, sga:=TRUE]
smallD[sga==FALSE &
         merged_pregbweight<2696 &
         USorLMPdateCombogAdays>=273 & 
         USorLMPdateCombogAdays<=279, sga:=TRUE]
smallD[sga==FALSE &
         merged_pregbweight<2831 & 
         USorLMPdateCombogAdays>=280 & 
         USorLMPdateCombogAdays<=286, sga:=TRUE]
smallD[sga==FALSE &
         merged_pregbweight<2952 & 
         USorLMPdateCombogAdays>=287 &
         USorLMPdateCombogAdays<=293, sga:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(sga)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Sga_%s.xlsx", 
                               lubridate::today())))


#making lgA variables
smallD[,lga:=NA]

#everyone who isnt missing gA and weight is given a false
smallD[!is.na(merged_pregbweight) & !is.na(USorLMPdateCombogAdays), lga:=FALSE]

#identifying lgas
smallD[lga==FALSE &
         merged_pregbweight>3259 & 
         USorLMPdateCombogAdays>=259 &
         USorLMPdateCombogAdays<=265, lga:=TRUE]
smallD[lga==FALSE &
         merged_pregbweight>3471 & 
         USorLMPdateCombogAdays>=266 & 
         USorLMPdateCombogAdays<=272, lga:=TRUE]
smallD[lga==FALSE &
         merged_pregbweight>3670 & 
         USorLMPdateCombogAdays>=273 &
         USorLMPdateCombogAdays<=279, lga:=TRUE]
smallD[lga==FALSE &
         merged_pregbweight>3854 & 
         USorLMPdateCombogAdays>=280 & 
         USorLMPdateCombogAdays<=286, lga:=TRUE]
smallD[lga==FALSE &
         merged_pregbweight>4018 &
         USorLMPdateCombogAdays>=287 & 
         USorLMPdateCombogAdays<=293, lga:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(lga)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Lga_%s.xlsx", 
                               lubridate::today())))

######### ANC Detection of sga ##########
smallD[,anc_detection_sga:=FALSE]


for(i in 20:37){
    var <- sprintf("TrialOne_us_iugrSuspected_%s_%s",i,i)
    
    smallD[get(var)==T, anc_detection_sga:=TRUE ]
    
}
xtabs(~smallD$anc_detection_sga)

# undetected sga at birth
smallD[is.na(sga), sga_undetected_at_birth:=as.logical(NA)]
smallD[sga==T & anc_detection_sga==T, sga_undetected_at_birth:=FALSE]
smallD[anc_detection_sga==FALSE & sga==T,sga_undetected_at_birth:=TRUE]
xtabs(~smallD$sga_undetected_at_birth)

######### Indication for CS and Malpresentation ##########

#cleaning
unique(smallD$merged_indic_csection)
smallD[,has_malpresentation:=NA]

smallD[!is.na(merged_indic_csection), has_malpresentation:=FALSE]

#creating lower cases in all of them
smallD[,merged_indic_csection_lowercase:= stringr::str_to_lower(merged_indic_csection)]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"breech"),
       has_malpresentation:=TRUE]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"transfe lie"),
       has_malpresentation:=TRUE]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"transverse"),
       has_malpresentation:=TRUE]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"trnasverse"),
       has_malpresentation:=TRUE]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"malpresentation"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"breach"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"male position"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"transver"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"transfer"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"transvearse"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"breec"),
       has_malpresentation:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"breehch"),
       has_malpresentation:=TRUE]

###################### Presentation at Term and Detection ###################### 
#### Pres at term 36+ #####
smallD[,hasan36plusweeks:=FALSE]
#smallD[,hasanexampalp36:= FALSE]
vars <- stringr::str_subset(names(smallD),"^angestage_")
for (i in vars){
  print(i)
  smallD[get(i)>=36 & get(i)<=40, hasan36plusweeks:=TRUE]
  
}

#fetal presentation at term by ultrasound
smallD[,presatterm:=as.character(NA)]

vars <- stringr::str_subset(names(smallD),"^uspres_")


for (var_pres in vars){
  
  vargestage <-stringr::str_replace(var_pres,"uspres", "usgestage")
  
  smallD[hasan36plusweeks==TRUE &
           get(vargestage)>=36 &
           get(vargestage)<=40 &
           !is.na(get(var_pres)) &
           get(var_pres)!="",
         presatterm:=get(var_pres)]
}


# anexampalp at term
smallD[,anexampalp_0:=bookexampalp]
smallD[,anexampalp36:=as.character(NA)]
vars_gestage <- stringr::str_subset(names(smallD),"^angestage_[0-9]+")

for (var_gestage in vars_gestage){
  
  var_exampalp <-stringr::str_replace(var_gestage,"angestage", "anexampalp")
  
  smallD[hasan36plusweeks==TRUE &
           get(var_gestage)>=36 &
           get(var_gestage)<=40 &
           !is.na(get(var_exampalp)) &
           get(var_exampalp)!="",
         anexampalp36:=get(var_exampalp)]
}

# 
smallD[!is.na(presatterm),
       anc_detection_malpres_1:=FALSE]
smallD[anc_detection_malpres_1==FALSE & 
         presatterm %in% c("Breech","Trasverse"), anc_detection_malpres_1:=TRUE ]


#making anc detection malpres with vars we used from loop

smallD[,anc_detection_malpres_2:=as.logical(NA)]
#need to define false
for(i in 35:38){
  var <- sprintf("TrialOne_us_malpresvar_%s_%s",i,i)
  
  smallD[anc_detection_malpres_2==FALSE & 
           get(var)==TRUE, anc_detection_malpres_2:=TRUE ]
  
}

smallD[,anc_detection_malpres_3:=as.logical(NA)]
for(i in 35:38){
  var <- sprintf("TrialOne_anexampalpmal_%s_%s",i,i)
  
  smallD[anc_detection_malpres_3==FALSE & 
           get(var)==TRUE, anc_detection_malpres_3:=TRUE ]
  
}


#malpresentation at birth undetected

smallD[is.na(has_malpresentation), malpres_undetected_1:=as.logical(NA)]
smallD[is.na(has_malpresentation), malpres_undetected_2:=as.logical(NA)]
smallD[is.na(has_malpresentation), malpres_undetected_3:=as.logical(NA)]

smallD[anc_detection_malpres_1==F, malpres_undetected_1:=FALSE]
smallD[anc_detection_malpres_2==F, malpres_undetected_2:=FALSE]
smallD[anc_detection_malpres_3==F, malpres_undetected_3:=FALSE]

smallD[malpres_undetected_1==F & has_malpresentation==T,malpres_undetected_1:=TRUE]
smallD[malpres_undetected_2==F & has_malpresentation==T,malpres_undetected_2:=TRUE]
smallD[malpres_undetected_3==F & has_malpresentation==T,malpres_undetected_3:=TRUE]


######### Multiplepreg Var ##########

smallD[,multiplepreg:=as.numeric(NA)]
smallD[,multiplepreg:=1]

smallD[stringr::str_detect(merged_indic_csection_lowercase,"twins"),
       multiplepreg:=2]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"twin"),
       multiplepreg:=2]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"triplets"),
       multiplepreg:=3]

#making paperhbo lowercase to use in multiplepreg var paperhbo
smallD[,lowercasepaperhbo_notes_1:= stringr::str_to_lower(paperhbo_notes_1)]
smallD[,lowercasepaperhbo_notes_2:= stringr::str_to_lower(paperhbo_notes_2)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_1,"twins"),
       multiplepreg:=2]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_1,"twin"),
       multiplepreg:=2]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_2,"twins"),
       multiplepreg:=2]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_2,"twin"),
       multiplepreg:=2]


#twins variable from avicenna
smallD[,lowercaseAvicnotes_1:= stringr::str_to_lower(acsdatatext_1)]
smallD[,lowercaseAvicnotes_2:= stringr::str_to_lower(acsdatatext_2)]
smallD[,lowercaseAvicnotes_3:= stringr::str_to_lower(acsdatatext_3)]


smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"twin"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"twins"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"triplet"), multiplepreg:=3]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"triplets"), multiplepreg:=3]    

smallD[multiplepreg==FALSE & stringr::str_detect(lowercaseAvicnotes_2,"twin"), multiplepreg:=2] 

smallD[multiplepreg==FALSE & stringr::str_detect(lowercaseAvicnotes_2,"twins"), multiplepreg:=2]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_2,"triplet"), multiplepreg:=3]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_2,"triplets"), multiplepreg:=3]    

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_3,"triplet"), multiplepreg:=3]     
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_3,"treplit"), multiplepreg:=3]     
nrow(smallD[multiplepreg==3])

#twins var dhis2
smallD[,lowercaseDhis2hbo_1:= stringr::str_to_lower(dhis2hbousrecommendcomment_1)]
smallD[,lowercaseDhis2hbo_2:= stringr::str_to_lower(dhis2hbousrecommendcomment_2)]
smallD[,lowercaseDhis2hbo_3:= stringr::str_to_lower(dhis2hbousrecommendcomment_3)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseDhis2hbo_1,"twins"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseDhis2hbo_1,"twin"), multiplepreg:=2]

#twins var dhis2
smallD[,lowercasehbocon_1:= stringr::str_to_lower(hboconreasonforcs_1)]
smallD[,lowercasehbocon_2:= stringr::str_to_lower(hboconreasonforcs_2)]
smallD[,lowercasehbocon_3:= stringr::str_to_lower(hboconreasonforcs_3)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"twins"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"twin"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"triplets"), multiplepreg:=3]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"twins"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"twin"), multiplepreg:=2]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"triplet"), multiplepreg:=3]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_3,"triplet"), multiplepreg:=3]


#in strings that we want we use the %in%
#smallD[merged_indic_csection %in% c("Breech","TWINS , BOTH BREECH IN LABOUR"#,"breach", "transverse"), var:=true

######### LBW and Macrosomia  ##########
# extremely low birth weight
smallD[,elbw:=NA]
smallD[!is.na(merged_pregbweight),elbw:=FALSE]
smallD[USorLMPdateCombogAdays>168 &
         USorLMPdateCombogAdays<=314 &
         merged_pregbweight <1000, elbw:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(elbw)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Elbw_%s.xlsx", 
                               lubridate::today())))
# vlbw: verylow birth weight
smallD[,vlbw:=NA]
smallD[!is.na(merged_pregbweight),vlbw:=FALSE]
smallD[USorLMPdateCombogAdays>168 &
         USorLMPdateCombogAdays<=314 &
         merged_pregbweight <1500, vlbw:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(vlbw)]

# lbw:low birth weight
smallD[,lbw:=NA]
smallD[!is.na(merged_pregbweight),lbw:=FALSE]
smallD[USorLMPdateCombogAdays>168 &
         USorLMPdateCombogAdays<=314 &
         merged_pregbweight <2500, lbw:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(lbw)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Lbw_%s.xlsx", 
                               lubridate::today())))

# macrosomia
smallD[,macrosomia:=as.logical(NA)]
smallD[!is.na(merged_pregbweight), macrosomia:=FALSE]
smallD[USorLMPdateCombogAdays>168 &
         USorLMPdateCombogAdays<=314 &
         merged_pregbweight>4000, macrosomia:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(macrosomia)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Macrosomia_%s.xlsx", 
                               lubridate::today())))

######### Outcome Var ##########
#everyone starts of as missing
smallD[,TrialOne_primarypregoutc:=as.logical(NA)]

varshbooutcome <- smallD[,c("SevOrModHbatBirth",
                            "sevHTNatBirth",
                            "sga_undetected_at_birth",
                            "lga",
                            "malpres_undetected_1",
                            "malpres_undetected_2",
                            "malpres_undetected_3")]

varlist <- names(varshbooutcome)

# anyone who has all FALSE is set to False
smallD[SevOrModHbatBirth==F &
         sevHTNatBirth==F &
         sga_undetected_at_birth==F &
         lga==F &
         malpres_undetected_1==F &
         malpres_undetected_2==F &
         malpres_undetected_3==F,
         TrialOne_primarypregoutc:= FALSE]

#at least one value is true, set to True regardless of other values
for(i in varlist){
  #print(i)
  smallD[get(i)==T, TrialOne_primarypregoutc:=T]
}


#################### Vars to add to other data sets #################### 
# GA cats at delivery for process outcomes
smallD[,birthgAcats:=cut(USorLMPdateCombogAdays,
                         breaks=c(0,104,125,160,167,202,216,237,244,265,308),
                         include.lowest=T)]

#make these into vars and add them in vars keep
varsanevent <- names(smallD)[stringr::str_detect(names(smallD),"^anevent")]
varsangAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^angestage")]
varsangAdays <- names(smallD)[stringr::str_detect(names(smallD),"^anT1gestagedays")]
varslabevent <- names(smallD)[stringr::str_detect(names(smallD),"^labevent")]
varslabgAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^labgestage")]
varslabgAdays <- names(smallD)[stringr::str_detect(names(smallD),"^labT1gestagedays")]
varslabhb <- names(smallD)[stringr::str_detect(names(smallD),"^labhb")]
varslabother <- names(smallD)[stringr::str_detect(names(smallD),"^labother")]
varslaburpro <- names(smallD)[stringr::str_detect(names(smallD),"^laburpro")]
varsmanevent <- names(smallD)[stringr::str_detect(names(smallD),"^manevent")]
varsmangAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^mangestage")]
varsgAdays <- names(smallD)[stringr::str_detect(names(smallD),"^manT1gestagedays")]
varsmantypex <- names(smallD)[stringr::str_detect(names(smallD),"^mantypex")]
varsmantypey <- names(smallD)[stringr::str_detect(names(smallD),"^mantypey")]
varsRefhosp<- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_refHosp_")]
varsRefHR <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_refHR_")]
varsT1anvis <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_anvisitnew")]
varsHBscr <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_labhb_")]
varsT1bpsyst <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_anbpsyst_")]
varsT1bpdia <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_anbpdiast_")]

varsT1labur <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_laburglu_")]

varsT1labblglu <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_labbloodglu_")]
varsT1labfastglu <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_labfastbloodglu_")]
varslabrbs <- names(smallD)[stringr::str_detect(names(smallD),"^labbloodglu")]
varslabfbs <- names(smallD)[stringr::str_detect(names(smallD),"^labfastbloodglu")]
varslaburglu<- names(smallD)[stringr::str_detect(names(smallD),"^laburglu")]
varslabogct <- names(smallD)[stringr::str_detect(names(smallD),"^labogct")]

varsanexamsfh <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_anexamsfh_")]

varsanexampalpmal <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_anexampalpmal_")]

varsanexmalpres <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_malpresanexam_")]

varsSFHdisc <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_sfhDiscrep")]
varsSFHdiscExists <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_sfhDiscrepExists")]

varsmanperf <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manperf_")]
varsUs <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_us_")]
varsusgAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^usgestage")]
varsusgAdays <- names(smallD)[stringr::str_detect(names(smallD),"^usT1gestagedays")]
varsUSevents <- varsmanevent <- names(smallD)[stringr::str_detect(names(smallD),"^usevent")]


### Management vars ###
manhb <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manhb_")]
manhtn <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manhtn_")]
manRBG <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manRBGHigh_")]
malpres <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manmalpres_")]
iugr <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_maniugr_")]
lga <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manlga_")]


#################### Anonymized BirthOutcome Data Set ####################
varsanexpalp <- names(smallD)[stringr::str_detect(names(smallD),"^anexampalp")]
varsuspres <- names(smallD)[stringr::str_detect(names(smallD),"^uspres")]
varsusgestage <- names(smallD)[stringr::str_detect(names(smallD),"^usgestage")]
varsusgestagedays <- names(smallD)[stringr::str_detect(names(smallD),"^usT1gestagedays")]
varsangestage <- names(smallD)[stringr::str_detect(names(smallD),"^angestage")]
varsangestagedays <- names(smallD)[stringr::str_detect(names(smallD),"^anT1gestagedays")]
varsusiugr <- names(smallD)[stringr::str_detect(names(smallD),"^usiugr")]
varsuslga <- names(smallD)[stringr::str_detect(names(smallD),"^uslga")]
varsanbpsyst <- names(smallD)[stringr::str_detect(names(smallD),"^anbpsyst")]
varsanbpdiast <- names(smallD)[stringr::str_detect(names(smallD),"^anbpdiast")]


smallD[prettyExposure=="Trial Arm A", prettyExposure:="D"]
smallD[prettyExposure=="Trial Arm B", prettyExposure:="C"]

varskeep <- c("prettyExposure",
              "uniqueid",
              "agecat",
              "agepregnancycat",
              "incomecat",
              "education",
              "educationcat",
              "str_TRIAL_1_Cluster",
              "bookorgdistricthashed",
              "bookgestage",
              "bookgestagedays_cats",
              "age",	
              "agepregnancy",	
              "avgincome",
              "avgincomecat",	
              "bookhistdm",	
              "bookhistcs",	
              "bookhistgdm",	
              "bookhistperi",	
              "bookhistpph",	
              "bookhistaph",	
              "bookhistabort",	
              "bookhistpreecl",	
              "bookheight",	
              "bookweight",	
              "bookbmi", 
              "bookbmicat",
              "bookevent",	
              "bookgestage",	
              "bookgestagedays",	
              "booklabhb",
              "gAatBirth_cats",
              "TrialOne_primarypregoutc",
              varsT1anvis,
              varsangAdays,
              varsangAweeks,
              varsanevent,
              "merged_birthhemo",
              "SevOrModHbatBirth",
              "merged_bpsyst",
              "merged_bpdiast",
              "sevHTNatBirth",
              "merged_modedeliv",
              "merged_pregoutcome",
              "merged_pregoutcome_1",
              "merged_pregoutcome_2",
              "merged_pregoutcome_3",
              "merged_indic_csection",
              "merged_presentationdeliv",
              "has_malpresentation",
              "merged_pregbweight",
              "merged_birthweight_1",
              "merged_birthweight_2",
              "merged_birthweight_3",
              "mahima_hospenteredgestage_1",
              "mahima_gestageatbirthwk_1",
              "macrosomia",
              "elbw",
              "vlbw",
              "lbw",
              "USorLMPdateCombogAdays",
              "lga",
              "sga",
              "anc_detection_sga",
              "sga_undetected_at_birth",
              "TrialOne_us_iugrSuspected_00_14",
              "TrialOne_us_iugrSuspected_15_17",
              "TrialOne_us_iugrSuspected_18_22",
              "TrialOne_us_iugrSuspected_23_23",
              "TrialOne_us_iugrSuspected_24_28",
              "TrialOne_us_iugrSuspected_29_30",
              "TrialOne_us_iugrSuspected_29_30",
              "TrialOne_us_iugrSuspected_31_33",
              "TrialOne_us_iugrSuspected_34_34",
              "TrialOne_us_iugrSuspected_35_37",
              "multiplepreg",
              "lowercasepaperhbo_notes_1", 
              "lowercasepaperhbo_notes_2",
              "lowercaseAvicnotes_1",
              "lowercaseAvicnotes_2",
              "lowercaseAvicnotes_3",
              "lowercaseDhis2hbo_1",
              "lowercaseDhis2hbo_2",
              "lowercaseDhis2hbo_3",
              "lowercasehbocon_1",
              "lowercasehbocon_2",
              "lowercasehbocon_3",
              varsanexpalp,
              varsanexampalpmal,
              varsuspres,
              "anc_detection_malpres_1",
              "anc_detection_malpres_2",
              "anc_detection_malpres_3",
              "malpres_undetected_1",
              "malpres_undetected_2",
              "malpres_undetected_3",
               varsUSevents,
              varsusgestage,
              varsusgestagedays,
              varsangestage,
              varsangestagedays,
              varsusiugr,
              varsuslga,
              varsUs
              
)

hbooutcomes <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(hbooutcomes,file.path(FOLDER_DATA_CLEAN,
                                           "Trial_1_Outcomes",
                                           sprintf("%s_BirthOutcomes.xlsx", 
                                                   lubridate::today())))



#################### Export ANC Process Outcome Data Sets ####################

varskeepAll <- c("prettyExposure",	
                 "uniqueid",	
                 "age",	
                 "agecat",
                 "agepregnancy",
                 "agepregnancycat",
                 "avgincome",
                 "avgincomecat",	
                 "education", 
                 "educationcat",
                 "str_TRIAL_1_Cluster",	
                 "bookgestagedays_cats",
                 "bookorgdistricthashed",	
                 "bookhistdm",	
                 "bookhistcs",	
                 "bookhistgdm",	
                 "bookhistperi",	
                 "bookhistpph",	
                 "bookhistaph",	
                 "bookhistabort",	
                 "bookhistpreecl",	
                 "bookheight",	
                 "bookweight",	
                 "bookbmi", 
                 "bookbmicat",
                 "bookevent",	
                 "bookgestage",	
                 "bookgestagedays",	
                 "booklabhb",
                 "birthgAcats",
                 "mahima_gestageatbirthwk_1",
                 "mahima_gA_1_us_cats",
                 "mahima_gA_1_us",
                 varsT1anvis,
                 varsangAdays,
                 varsangAweeks,
                 varsanevent)
                 
                 
varshb <-c(varslabevent, 
           varslabgAweeks, 
           varslabgAdays,
           varslabhb, 
           varsHBscr,
           manhb)



varsbp <-c("bookbpsyst",
           "bookbpdiast",
           varsanbpsyst,
           varsanbpdiast,
           varsT1bpsyst,
           varsT1bpdia,
           varslabevent,
           varslabgAweeks, 
           varslabgAdays,
           varslaburpro,
           varslabother,
           varsUSevents,
           varsusgestage,
           varsusgestagedays,
           manhtn)


varsgdm <-c(varslabevent, 
            varslabgAweeks, 
            varslabgAdays,
            varsT1labur,
            varsT1labblglu,
            varsT1labfastglu,
            varslabrbs, 
            varslabfbs,
            varslaburglu,
            varslabogct,
            varslabother,
            manRBG)


varsmalpres <- c(varsUSevents,
                 varsUs,
                 varsuspres,
                 malpres,
                 varsanexampalpmal,
                 varsanexmalpres,
                 varsUSevents,
                 varsusgestage,
                 varsusgestagedays,
                 "anexampalp36",
                 "presatterm"
                 )

varsfgr <- c(varsusgAweeks,
             varsusgAdays,
             varsUSevents,
             varsusiugr,
             varsuslga,
             varsUs,
             varsSFHdisc,
             varsSFHdiscExists,
             varsanexamsfh,
             iugr,
             lga)

varsman <-c(varsmanevent,
            varsmangAweeks,
            varsmantypey,
            varsmantypex,
            varsgAdays,
            varsRefHR,
            varsRefhosp,
            varsmanperf)


###### Attendance data set  ######
smallD[prettyExposure=="D", prettyExposure:="E"]
smallD[prettyExposure=="C", prettyExposure:="F"]
varskeep <- c(varskeepAll)
attendance <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(attendance,file.path(FOLDER_DATA_CLEAN,
                                          "Trial_1_Outcomes",
                                          sprintf("%s_Attendance.xlsx", 
                                                  lubridate::today())))

###### Anemia  data set  ###### 
smallD[prettyExposure=="E", prettyExposure:="G"]
smallD[prettyExposure=="F", prettyExposure:="H"]
varskeep <- c(varskeepAll,
              varshb,
              varsman)
hb <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(hb,file.path(FOLDER_DATA_CLEAN,
                                  "Trial_1_Outcomes",
                                  sprintf("%s_Anemia.xlsx", 
                                          lubridate::today())))

###### Hypertension data set  ###### 
smallD[prettyExposure=="G", prettyExposure:="J"]
smallD[prettyExposure=="H", prettyExposure:="I"]
varskeep <- c(varskeepAll,
              varsbp,
              varsman)
bp <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(bp,file.path(FOLDER_DATA_CLEAN,
                                  "Trial_1_Outcomes",
                                  sprintf("%s_HTN.xlsx", 
                                          lubridate::today())))

###### GDM data set  ###### 
smallD[prettyExposure=="J", prettyExposure:="K"]
smallD[prettyExposure=="I", prettyExposure:="L"]
varskeep <- c(varskeepAll,
              varsgdm,
              varsman)
gdm <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(gdm,file.path(FOLDER_DATA_CLEAN,
                                   "Trial_1_Outcomes",
                                   sprintf("%s_GDM.xlsx", 
                                           lubridate::today())))
###### Malpres data set  ###### 
smallD[prettyExposure=="K", prettyExposure:="A"]
smallD[prettyExposure=="L", prettyExposure:="B"]
varskeep <- c(varskeepAll,
              varsmalpres,
              varsman)
malpres <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(malpres,file.path(FOLDER_DATA_CLEAN,
                                       "Trial_1_Outcomes",
                                       sprintf("%s_Malpres.xlsx", 
                                               lubridate::today())))




###### FGR data set  ###### 
smallD[prettyExposure=="A", prettyExposure:="N"]
smallD[prettyExposure=="B", prettyExposure:="M"]
varskeep <- c(varskeepAll,
              varsfgr,
              varsman)
fgr <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(fgr,file.path(FOLDER_DATA_CLEAN,
                                   "Trial_1_Outcomes",
                                   sprintf("%s_FGR.xlsx", 
                                           lubridate::today())))



#### Anonymize Data Set ####
#### Choose Variables Data Set ####
#each outcome will have its own data set with the variables we made above
#rename ident_dhis2_control to something else in each data set
#keep lab values in smaller data sets 



########################  random samples **** QC **** ######################## 
set.seed(7)
randomsample <- smallD[!is.na(TrialOne_labhb_anemia_sev_00_14)==T][sample(1:.N,5)]
#export this




## SevAnemia

# 00_14
smallD[,TrialOne_man_sev_anemia_00_14:=NA]
smallD[TrialOne_labhb_anemia_sev_00_14==T,TrialOne_man_sev_anemia_00_14:=FALSE]
smallD[TrialOne_labhb_anemia_sev_00_14==T &
         TrialOne_refHosp_00_14==T,
       TrialOne_man_sev_anemia_00_14:=TRUE]

# 15_17
smallD[,TrialOne_man_sev_anemia_15_17:=NA]
smallD[TrialOne_labhb_anemia_sev_15_17==T,TrialOne_man_sev_anemia_15_17:=FALSE]
smallD[TrialOne_labhb_anemia_sev_15_17==T &
         TrialOne_refHosp_15_17==T,
       TrialOne_man_sev_anemia_15_17:=TRUE]

# 18_22
smallD[,TrialOne_man_sev_anemia_18_22:=NA]
smallD[TrialOne_labhb_anemia_sev_18_22==T,TrialOne_man_sev_anemia_18_22:=FALSE]
smallD[TrialOne_labhb_anemia_sev_18_22==T &
         TrialOne_refHosp_18_22==T,
       TrialOne_man_sev_anemia_18_22:=TRUE]


# 24_28
smallD[,TrialOne_man_sev_anemia_24_28:=NA]
smallD[TrialOne_labhb_anemia_sev_24_28==T,TrialOne_man_sev_anemia_24_28:=FALSE]
smallD[TrialOne_labhb_anemia_sev_24_28==T &
         TrialOne_refHosp_24_28==T,
       TrialOne_man_sev_anemia_24_28:=TRUE]

# 29_30
smallD[,TrialOne_man_sev_anemia_29_30:=NA]
smallD[TrialOne_labhb_anemia_sev_29_30==T,TrialOne_man_sev_anemia_29_30:=FALSE]
smallD[TrialOne_labhb_anemia_sev_29_30==T &
         TrialOne_refHosp_29_30==T,
       TrialOne_man_sev_anemia_29_30:=TRUE]

# 31_33
smallD[,TrialOne_man_sev_anemia_31_33:=NA]
smallD[TrialOne_labhb_anemia_sev_31_33==T,TrialOne_man_sev_anemia_31_33:=FALSE]
smallD[TrialOne_labhb_anemia_sev_31_33==T &
         TrialOne_refHosp_31_33==T,
       TrialOne_man_sev_anemia_31_33:=TRUE]

# 34_34
smallD[,TrialOne_man_sev_anemia_34_34:=NA]
smallD[TrialOne_labhb_anemia_sev_34_34==T,TrialOne_man_sev_anemia_34_34:=FALSE]
smallD[TrialOne_labhb_anemia_sev_34_34==T &
         TrialOne_refHosp_34_34==T,
       TrialOne_man_sev_anemia_34_34:=TRUE]

# 35_37
smallD[,TrialOne_man_sev_anemia_35_37:=NA]
smallD[TrialOne_labhb_anemia_sev_35_37==T,TrialOne_man_sev_anemia_35_37:=FALSE]
smallD[TrialOne_labhb_anemia_sev_35_37==T &
         TrialOne_refHosp_35_37==T,
       TrialOne_man_modSev_HTN_35_37:=TRUE]


## Moderate/SevereHTN 
 # Chronic Mod/Sev Htn
# 00_14
smallD[,TrialOne_man_modSev_HTN_00_14:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_00_14==T,TrialOne_man_modSev_HTN_00_14:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_00_14==T &
         TrialOne_refHosp_00_14==T,
       TrialOne_man_modSev_HTN_00_14:=TRUE]

# 15_17
smallD[,TrialOne_man_modSev_HTN_15_17:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_15_17==T,TrialOne_man_modSev_HTN_15_17:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_15_17==T &
         TrialOne_refHosp_15_17==T,
       TrialOne_man_modSev_HTN_15_17:=TRUE]

# 18_22
smallD[,TrialOne_man_modSev_HTN_18_22:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_18_22==T,TrialOne_man_modSev_HTN_18_22:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_18_22==T &
         TrialOne_refHosp_18_22==T,
       TrialOne_man_modSev_HTN_18_22:=TRUE]

 # Mod/Sev Gestational HTN
# 24_28
smallD[,TrialOne_man_modSev_HTN_24_28:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_24_28==T,TrialOne_man_modSev_HTN_24_28:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_24_28==T &
         TrialOne_refHosp_24_28==T,
       TrialOne_man_modSev_HTN_24_28:=TRUE]

# 29_30
smallD[,TrialOne_man_modSev_HTN_29_30:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_29_30==T,TrialOne_man_modSev_HTN_29_30:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_29_30==T &
         TrialOne_refHosp_29_30==T,
       TrialOne_man_modSev_HTN_29_30:=TRUE]

# 31_33
smallD[,TrialOne_man_modSev_HTN_31_33:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_31_33==T,TrialOne_man_modSev_HTN_31_33:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_31_33==T &
         TrialOne_refHosp_31_33==T,
       TrialOne_man_modSev_HTN_31_33:=TRUE]
# 34_34
smallD[,TrialOne_man_modSev_HTN_34_34:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_34_34==T,TrialOne_man_modSev_HTN_34_34:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_34_34==T &
         TrialOne_refHosp_34_34==T,
       TrialOne_man_modSev_HTN_34_34:=TRUE]

# 35_37
smallD[,TrialOne_man_modSev_HTN_35_37:=NA]
smallD[TrialOne_anbpdiast_modSevHTN_35_37==T,TrialOne_man_modSev_HTN_35_37:=FALSE]
smallD[TrialOne_anbpdiast_modSevHTN_35_37==T &
         TrialOne_refHosp_35_37==T,
       TrialOne_man_modSev_HTN_35_37:=TRUE]

## DM managements
#TO DO: should the laburglu=="POS" or should it just exist
#24_28
smallD[,TrialOne_man_gdm_24_28:=NA]
smallD[TrialOne_laburglu_pos_24_28==T & 
         (TrialOne_labbloodglu_high_24_28==T |
         TrialOne_labfastbloodglu_high_24_28==T),
         TrialOne_man_gdm_24_28:=FALSE]
smallD[TrialOne_laburglu_pos_24_28==T &
         TrialOne_refHR_24_28==T,
       TrialOne_man_gdm_24_28:=TRUE]
#29_30
smallD[,TrialOne_man_gdm_29_30:=NA]
smallD[TrialOne_laburglu_pos_29_30==T & 
         (TrialOne_labbloodglu_high_29_30==T |
            TrialOne_labfastbloodglu_high_29_30==T),
       TrialOne_man_gdm_29_30:=FALSE]
smallD[TrialOne_laburglu_pos_29_30==T &
         TrialOne_refHR_29_30==T,
       TrialOne_man_gdm_29_30:=TRUE]
#31_33
smallD[,TrialOne_man_gdm_31_33:=NA]
smallD[TrialOne_laburglu_pos_31_33==T & 
         (TrialOne_labbloodglu_high_31_33==T |
            TrialOne_labfastbloodglu_high_31_33==T),
       TrialOne_man_gdm_31_33:=FALSE]
smallD[TrialOne_laburglu_pos_31_33==T &
         TrialOne_refHR_31_33==T,
       TrialOne_man_gdm_31_33:=TRUE]

#34_34
smallD[,TrialOne_man_gdm_34_34:=NA]
smallD[TrialOne_laburglu_pos_34_34==T & 
         (TrialOne_labbloodglu_high_34_34==T |
            TrialOne_labfastbloodglu_high_34_34==T),
       TrialOne_man_gdm_34_34:=FALSE]
smallD[TrialOne_laburglu_pos_34_34==T &
         TrialOne_refHR_34_34==T,
       TrialOne_man_gdm_34_34:=TRUE]

#35_37
smallD[,TrialOne_man_gdm_35_37:=NA]
smallD[TrialOne_laburglu_pos_35_37==T & 
         (TrialOne_labbloodglu_high_35_37==T |
            TrialOne_labfastbloodglu_high_35_37==T),
       TrialOne_man_gdm_35_37:=FALSE]
smallD[TrialOne_laburglu_pos_35_37==T &
         TrialOne_refHR_35_37==T,
       TrialOne_man_gdm_35_37:=TRUE]


############################# TABLES ##################################

#Booking visits by categories
bookings<- smallD[,.(ArmA=sum(ident_dhis2_control==T),
                     ArmB=sum(ident_dhis2_control==F)),
                  keyby=.(bookgestagedays_cats)]


openxlsx::write.xlsx(bookings, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Bookings_%s.xlsx", lubridate::today())))

#An visits

anvisits <- smallD[,.(BookedBefore15wks=sum(TrialOne_anvisitnew_00_14==T,na.rm=TRUE),
                     Visits_15_17=sum(TrialOne_anvisitnew_15_17,na.rm=TRUE),

                     Visits_18_22=sum(TrialOne_anvisitnew_18_22,na.rm=TRUE),
                     Visits_23_23=sum(TrialOne_anvisitnew_23_23,na.rm=TRUE),
                     Visits_24_28=sum(TrialOne_anvisitnew_24_28,na.rm=TRUE),
                     Visits_29_30=sum(TrialOne_anvisitnew_29_30,na.rm=TRUE),
                     Visits_31_33=sum(TrialOne_anvisitnew_31_33,na.rm=TRUE),
                     Visits_34_34=sum(TrialOne_anvisitnew_34_34,na.rm=TRUE),
                     Visits_35_37=sum(TrialOne_anvisitnew_35_37,na.rm=TRUE)),
                      keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(anvisits, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Anvisits_and_Bookings_combined_%s.xlsx", 
                               lubridate::today())))


#BP

bpsyst<- smallD[,.(BpBefore15wks=sum(TrialOne_anbpsyst_present_00_14==T,
                                            na.rm=TRUE),
                Screened_15_17=sum(TrialOne_anbpsyst_present_15_17,
                                 na.rm=TRUE),
                       Screened_18_22=sum(TrialOne_anbpsyst_present_18_22,na.rm=TRUE),
                      Screened_23_23=sum(TrialOne_anbpsyst_present_23_23,na.rm=TRUE),
                      Screened_24_28=sum(TrialOne_anbpsyst_present_24_28,na.rm=TRUE),
                      Screened_29_30=sum(TrialOne_anbpsyst_present_29_30,na.rm=TRUE),
                      Screened_31_33=sum(TrialOne_anbpsyst_present_31_33,na.rm=TRUE),
                      Screened_34_34=sum(TrialOne_anbpsyst_present_34_34,na.rm=TRUE),
                      Screened_35_37=sum(TrialOne_anbpsyst_present_35_37,na.rm=TRUE)),
                   keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(bpsyst, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("BP_Screened_Visits_%s.xlsx", 
                               lubridate::today())))

# BP/HTN
###fix this
bpsyst<- smallD[,.(ChronicHTN=sum(TrialOne_anbpsyst_present_00_14==T,
                                     na.rm=TRUE),
                   Screened_15_17=sum(TrialOne_anbpsyst_present_15_17,
                                      na.rm=TRUE),
                   Screened_18_22=sum(TrialOne_anbpsyst_present_18_22,na.rm=TRUE),
                   Screened_23_23=sum(TrialOne_anbpsyst_present_23_23,na.rm=TRUE),
                   Screened_24_28=sum(TrialOne_anbpsyst_present_24_28,na.rm=TRUE),
                   Screened_29_30=sum(TrialOne_anbpsyst_present_29_30,na.rm=TRUE),
                   Screened_31_33=sum(TrialOne_anbpsyst_present_31_33,na.rm=TRUE),
                   Screened_34_34=sum(TrialOne_anbpsyst_present_34_34,na.rm=TRUE),
                   Screened_35_37=sum(TrialOne_anbpsyst_present_35_37,na.rm=TRUE)),
                keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(bpsyst, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("BP_Screened_Visits_%s.xlsx", 
                               lubridate::today())))

#LabHb and Anemia
labhb<- smallD[,.(screenedlabhb15wks=sum(TrialOne_labhb_exists_00_14==T,
                                         na.rm=TRUE),
                   screenedlabhb_15_17=sum(TrialOne_labhb_exists_15_17,
                                    na.rm=TRUE),
                  screenedlabhb_18_22=sum(TrialOne_labhb_exists_18_22,na.rm=TRUE),
                   screenedlabhb_23_23=sum(TrialOne_labhb_exists_23_23,na.rm=TRUE),
                   screenedlabhb_24_28=sum(TrialOne_labhb_exists_24_28,na.rm=TRUE),
                   screenedlabhb_29_30=sum(TrialOne_labhb_exists_29_30,na.rm=TRUE),
                   screenedlabhb_31_33=sum(TrialOne_labhb_exists_31_33,na.rm=TRUE),
                   screenedlabhb_34_34=sum(TrialOne_labhb_exists_34_34,na.rm=TRUE),
                   screenedlabhb_35_37=sum(TrialOne_labhb_exists_35_37,na.rm=TRUE)),
                keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(labhb, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Anemia_Screened_%s.xlsx", 
                               lubridate::today())))

labhb_sev<- smallD[,.(sev_anemia_15wks=sum(TrialOne_labhb_anemia_sev_00_14==T,
                                         na.rm=TRUE),
                  sev_anemia_15_17=sum(TrialOne_labhb_anemia_sev_15_17,
                                          na.rm=TRUE),
                  sev_anemia_18_22=sum(TrialOne_labhb_anemia_sev_18_22,na.rm=TRUE),
                  sev_anemia_23_23=sum(TrialOne_labhb_anemia_sev_23_23,na.rm=TRUE),
                  sev_anemia_24_28=sum(TrialOne_labhb_anemia_sev_24_28,na.rm=TRUE),
                  sev_anemia_29_30=sum(TrialOne_labhb_anemia_sev_29_30,na.rm=TRUE),
                  sev_anemia_31_33=sum(TrialOne_labhb_anemia_sev_31_33,na.rm=TRUE),
                  sev_anemia_34_34=sum(TrialOne_labhb_anemia_sev_34_34,na.rm=TRUE),
                  sev_anemia_35_37=sum(TrialOne_labhb_anemia_sev_35_37,na.rm=TRUE)),
               keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(labhb_sev, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Sev_anemia_%s.xlsx", 
                               lubridate::today())))

labhb_mild_mod<- smallD[,.(
  mild_mod_anemia_15wks=sum(TrialOne_labhb_anemia_mild_mod_00_14==T,na.rm=TRUE),
  mild_mod_anemia_15_17=sum(TrialOne_labhb_anemia_mild_mod_15_17,na.rm=TRUE),
  mild_mod_anemia_18_22=sum(TrialOne_labhb_anemia_mild_mod_18_22, na.rm=TRUE),
  mild_mod_anemia_23_23=sum(TrialOne_labhb_anemia_mild_mod_23_23,na.rm=TRUE),
  mild_mod_anemia_24_28=sum(TrialOne_labhb_anemia_mild_mod_24_28, na.rm=TRUE),
  mild_mod_anemia_29_30=sum(TrialOne_labhb_anemia_mild_mod_29_30,na.rm=TRUE),
  mild_mod_anemia_31_33=sum(TrialOne_labhb_anemia_mild_mod_31_33,na.rm=TRUE),
  mild_mod_anemia_34_34=sum(TrialOne_labhb_anemia_mild_mod_34_34,na.rm=TRUE),
  mild_mod_anemia_35_37=sum(TrialOne_labhb_anemia_mild_mod_35_37,na.rm=TRUE)),
                   keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(labhb_mild_mod, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("mild_mod_anemia_%s.xlsx", 
                               lubridate::today())))


#Lab urglu
has_laburglu<- smallD[,.(
  has_labbloodglu_15wks=sum(TrialOne_laburglu_exists_00_14==T,na.rm=TRUE),
  has_labbloodglu_15_17=sum(TrialOne_laburglu_exists_15_17,na.rm=TRUE),
  has_labbloodglu_18_22=sum(TrialOne_laburglu_exists_18_22, na.rm=TRUE),
  has_labbloodglu_23_23=sum(TrialOne_laburglu_exists_23_23,na.rm=TRUE),
  has_labbloodglu_24_28=sum(TrialOne_laburglu_exists_24_28, na.rm=TRUE),
  has_labbloodglu_29_30=sum(TrialOne_laburglu_exists_29_30,na.rm=TRUE),
  has_labbloodglu_31_33=sum(TrialOne_laburglu_exists_31_33,na.rm=TRUE),
  has_labbloodglu_34_34=sum(TrialOne_laburglu_exists_34_34,na.rm=TRUE),
  has_labbloodglu_35_37=sum(TrialOne_laburglu_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(has_laburglu, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Urglu_screenings_%s.xlsx", 
                               lubridate::today())))


#Lab bloodglu
has_labbloodglu<- smallD[,.(
 has_labbloodglu_15wks=sum(TrialOne_labbloodglu_exists_00_14==T,na.rm=TRUE),
 has_labbloodglu_15_17=sum(TrialOne_labbloodglu_exists_15_17,na.rm=TRUE),
 has_labbloodglu_18_22=sum(TrialOne_labbloodglu_exists_18_22, na.rm=TRUE),
 has_labbloodglu_23_23=sum(TrialOne_labbloodglu_exists_23_23,na.rm=TRUE),
 has_labbloodglu_24_28=sum(TrialOne_labbloodglu_exists_24_28, na.rm=TRUE),
 has_labbloodglu_29_30=sum(TrialOne_labbloodglu_exists_29_30,na.rm=TRUE),
 has_labbloodglu_31_33=sum(TrialOne_labbloodglu_exists_31_33,na.rm=TRUE),
 has_labbloodglu_34_34=sum(TrialOne_labbloodglu_exists_34_34,na.rm=TRUE),
 has_labbloodglu_35_37=sum(TrialOne_labbloodglu_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(has_labbloodglu, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("bloodglu_screenings_%s.xlsx", 
                               lubridate::today())))
#high labbloodglu values
has_High_bloodglu<- smallD[,.(
  has_highbloodglu_15wks=sum(TrialOne_labbloodglu_high_00_14==T,na.rm=TRUE),
  has_highbloodglu_15_17=sum(TrialOne_labbloodglu_high_15_17,na.rm=TRUE),
  has_highbloodglu_18_22=sum(TrialOne_labbloodglu_high_18_22, na.rm=TRUE),
  has_highbloodglu_23_23=sum(TrialOne_labbloodglu_high_23_23,na.rm=TRUE),
  has_highbloodglu_24_28=sum(TrialOne_labbloodglu_high_24_28, na.rm=TRUE),
  has_highbloodglu_29_30=sum(TrialOne_labbloodglu_high_29_30,na.rm=TRUE),
  has_highbloodglu_31_33=sum(TrialOne_labbloodglu_high_31_33,na.rm=TRUE),
  has_highbloodglu_34_34=sum(TrialOne_labbloodglu_high_34_34,na.rm=TRUE),
  has_highbloodglu_35_37=sum(TrialOne_labbloodglu_high_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(has_High_bloodglu, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("High_bloodglu_screenings_%s.xlsx", 
                               lubridate::today())))

#Lab fastbloodglu

has_labfastbloodglu<- smallD[,.(
  has_labfastbloodglu_15wks=sum(TrialOne_labfastbloodglu_exists_00_14==T,na.rm=TRUE),
  has_labfastbloodglu_15_17=sum(TrialOne_labfastbloodglu_exists_15_17,na.rm=TRUE),
  has_labfastbloodglu_18_22=sum(TrialOne_labfastbloodglu_exists_18_22, na.rm=TRUE),
  has_labfastbloodglu_23_23=sum(TrialOne_labfastbloodglu_exists_23_23,na.rm=TRUE),
  has_labfastbloodglu_24_28=sum(TrialOne_labfastbloodglu_exists_24_28, na.rm=TRUE),
  has_labfastbloodglu_29_30=sum(TrialOne_labfastbloodglu_exists_29_30,na.rm=TRUE),
  has_labfastbloodglu_31_33=sum(TrialOne_labfastbloodglu_exists_31_33,na.rm=TRUE),
  has_labfastbloodglu_34_34=sum(TrialOne_labfastbloodglu_exists_34_34,na.rm=TRUE),
  has_labfastbloodglu_35_37=sum(TrialOne_labfastbloodglu_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(has_labbloodglu, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("bloodfastglu_screenings_%s.xlsx", 
                               lubridate::today())))
#high labfastbloodglu values
has_High_bloodglu<- smallD[,.(
  has_highfastbloodglu_15wks=sum(TrialOne_labfastbloodglu_high_00_14==T,na.rm=TRUE),
  has_highfastbloodglu_15_17=sum(TrialOne_labfastbloodglu_high_15_17,na.rm=TRUE),
   has_highfastbloodglu_18_22=sum(TrialOne_labfastbloodglu_high_18_22, na.rm=TRUE),
  has_highfastbloodglu_23_23=sum(TrialOne_labfastbloodglu_high_23_23,na.rm=TRUE),
  has_highfastbloodglu_24_28=sum(TrialOne_labfastbloodglu_high_24_28, na.rm=TRUE),
  has_highfastbloodglu_29_30=sum(TrialOne_labfastbloodglu_high_29_30,na.rm=TRUE),
  has_highfastbloodglu_31_33=sum(TrialOne_labfastbloodglu_high_31_33,na.rm=TRUE),
  has_highfastbloodglu_34_34=sum(TrialOne_labfastbloodglu_high_34_34,na.rm=TRUE),
  has_highbloodglu_35_37=sum(TrialOne_labfastbloodglu_high_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(has_High_bloodglu, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("High_bloodglu_screenings_%s.xlsx", 
                               lubridate::today())))



#US exists

us_exists<- smallD[,.(
  us_exists_15wks=sum(TrialOne_us_exists_00_14==T,na.rm=TRUE),
  has_us_15_17=sum(TrialOne_us_exists_15_17,na.rm=TRUE),
  has_us_18_22=sum(TrialOne_us_exists_18_22, na.rm=TRUE),
  has_us_23_23=sum(TrialOne_us_exists_23_23,na.rm=TRUE),
  has_us_24_28=sum(TrialOne_us_exists_24_28, na.rm=TRUE),
  has_us_29_30=sum(TrialOne_us_exists_29_30,na.rm=TRUE),
  has_us_31_33=sum(TrialOne_us_exists_31_33,na.rm=TRUE),
  has_us_34_34=sum(TrialOne_us_exists_34_34,na.rm=TRUE),
  has_us_35_37=sum(TrialOne_us_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(us_exists, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Us_screenings_%s.xlsx", 
                               lubridate::today())))



#US IUGR
us_iugrSusp<- smallD[,.(
  iugrsuspected_15wks=sum(TrialOne_us_iugrSuspected_00_14==T,na.rm=TRUE),
  iugrsuspected_15_17=sum(TrialOne_us_iugrSuspected_15_17,na.rm=TRUE),
  iugrsuspected_18_22=sum(TrialOne_us_iugrSuspected_18_22, na.rm=TRUE),
  iugrsuspected_23_23=sum(TrialOne_us_iugrSuspected_23_23,na.rm=TRUE),
  iugrsuspected_24_28=sum(TrialOne_us_iugrSuspected_24_28, na.rm=TRUE),
  iugrsuspected_29_30=sum(TrialOne_us_iugrSuspected_29_30,na.rm=TRUE),
  iugrsuspected_31_33=sum(TrialOne_us_iugrSuspected_31_33,na.rm=TRUE),
  iugrsuspected_34_34=sum(TrialOne_us_iugrSuspected_34_34,na.rm=TRUE),
  iugrsuspected_35_37=sum(TrialOne_us_iugrSuspected_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(us_iugrSusp, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Suspected_IUGR_%s.xlsx", 
                               lubridate::today())))
#US LGA
us_lgaSusp<- smallD[,.(
  lgaSuspected_15wks=sum(TrialOne_anexamsfh_exists_00_14==T,na.rm=TRUE),
  lgaSuspected_15_17=sum(TrialOne_anexamsfh_exists_15_17,na.rm=TRUE),
  lgaSuspected_18_22=sum(TrialOne_anexamsfh_exists_18_22, na.rm=TRUE),
  lgaSuspected_23_23=sum(TrialOne_anexamsfh_exists_23_23,na.rm=TRUE),
  lgaSuspected_24_28=sum(TrialOne_anexamsfh_exists_24_28, na.rm=TRUE),
  lgaSuspected_29_30=sum(TrialOne_anexamsfh_exists_29_30,na.rm=TRUE),
  lgaSuspected_31_33=sum(TrialOne_anexamsfh_exists_31_33,na.rm=TRUE),
  lgaSuspected_34_34=sum(TrialOne_anexamsfh_exists_34_34,na.rm=TRUE),
  lgaSuspected_35_37=sum(TrialOne_anexamsfh_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(us_lgaSusp, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("Suspected_LGA_%s.xlsx", 
                               lubridate::today()))) 


# anexamsfh
anexamsfh<- smallD[,.(
  anexamsfh_exists_15wks=sum(TrialOne_anexamsfh_exists_00_14==T,na.rm=TRUE),
  anexamsfh_exists_15_17=sum(TrialOne_anexamsfh_exists_15_17,na.rm=TRUE),
 anexamsfh_exists_18_22=sum(TrialOne_anexamsfh_exists_18_22, na.rm=TRUE),
  anexamsfh_exists_23_23=sum(TrialOne_anexamsfh_exists_23_23,na.rm=TRUE),
  anexamsfh_exists_24_28=sum(TrialOne_anexamsfh_exists_24_28, na.rm=TRUE),
  anexamsfh_exists_29_30=sum(TrialOne_anexamsfh_exists_29_30,na.rm=TRUE),
  anexamsfh_exists_31_33=sum(TrialOne_anexamsfh_exists_31_33,na.rm=TRUE),
  anexamsfh_exists_34_34=sum(TrialOne_anexamsfh_exists_34_34,na.rm=TRUE),
  anexamsfh_exists_35_37=sum(TrialOne_anexamsfh_exists_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(anexamsfh, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("anexamsfh_%s.xlsx", 
                               lubridate::today()))) 

####Referrals####
#ref to HR
refHR<- smallD[,.(
  refHR_15wks=sum(TrialOne_refHR_00_14==T,na.rm=TRUE),
  refHR_15_17=sum(TrialOne_refHR_15_17,na.rm=TRUE),
  refHR_18_22=sum(TrialOne_refHR_18_22, na.rm=TRUE),
  refHR_23_23=sum(TrialOne_refHR_23_23,na.rm=TRUE),
  refHR_24_28=sum(TrialOne_refHR_24_28, na.rm=TRUE),
  refHR_29_30=sum(TrialOne_refHR_29_30,na.rm=TRUE),
  refHR_31_33=sum(TrialOne_refHR_31_33,na.rm=TRUE),
  refHR_34_34=sum(TrialOne_refHR_34_34,na.rm=TRUE),
  refHR_35_37=sum(TrialOne_refHR_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(refHR, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("refHR_%s.xlsx", 
                               lubridate::today())))

#refHosp
refHosp<- smallD[,.(
  refHosp_15wks=sum(TrialOne_refHosp_00_14==T,na.rm=TRUE),
  refHosp_15_17=sum(TrialOne_refHosp_15_17,na.rm=TRUE),
  refHosp_18_22=sum(TrialOne_refHosp_18_22, na.rm=TRUE),
  refHosp_23_23=sum(TrialOne_refHosp_23_23,na.rm=TRUE),
  refHosp_24_28=sum(TrialOne_refHosp_24_28, na.rm=TRUE),
  refHosp_29_30=sum(TrialOne_refHosp_29_30,na.rm=TRUE),
  refHosp_31_33=sum(TrialOne_refHosp_31_33,na.rm=TRUE),
  refHosp_34_34=sum(TrialOne_refHosp_34_34,na.rm=TRUE),
  refHosp_35_37=sum(TrialOne_refHosp_35_37,na.rm=TRUE)),
  keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(refHosp, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "booking_and_visits",
                       sprintf("refHosp_%s.xlsx", 
                               lubridate::today())))

