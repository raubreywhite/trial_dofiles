
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
  
  smallD[,(var_us_gestage):=floor(as.numeric(difftime(get(var_us_date),
                                                      lmpT1, 
                                                      units="days")))]
  
}


#ANC gAs
an_date <- stringr::str_subset(names(smallD), "^andate_[0-9]+")
anT1_gA <- stringr::str_replace(an_date, "andate","anT1gestagedays")

for(i in seq_along(an_date)){
  var_an_gestage <- anT1_gA[i]
  var_an_date<- an_date[i] 
  
  smallD[,(var_an_gestage):=floor(as.numeric(difftime(get(var_an_date),
                                                      lmpT1, 
                                                      units="days")))]
  
}

#Lab gAs
##NEED TO ROUND OR USE FLOOR
lab_date <- stringr::str_subset(names(smallD), "^labdate_[0-9]+")
labT1_gA <- stringr::str_replace(lab_date, "labdate","labT1gestagedays")

for(i in seq_along(lab_date)){
  var_lab_gestage <- labT1_gA[i]
  var_lab_date<- lab_date[i] 
  
  smallD[,(var_lab_gestage):=floor(as.numeric(difftime(get(var_lab_date),
                                                       lmpT1, 
                                                       units="days")))]
  
}

#Man gAs
man_date <- stringr::str_subset(names(smallD), "^mandate_[0-9]+")
manT1_gA <- stringr::str_replace(man_date, "mandate","manT1gestagedays")

for(i in seq_along(man_date)){
  var_man_gestage <- manT1_gA[i]
  var_man_date<- man_date[i] 
  
  smallD[,(var_man_gestage):=floor(as.numeric(difftime(get(var_man_date),
                                                       lmpT1, 
                                                       units="days")))]
  
}


##making categories of days for booking

smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                       breaks=c(-500,104,119,
                                125,154,167, 196,
                                216,231,244,259),
                       include.lowest=T)]



# MAKE BOOK VISIT FOR ANEMIA
smallD[,booklabhb:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]



# Discrepancy Variable anexamsfh variable
smallD[,anexamsfh_0:=bookexamsfh]
smallD[,angestage_0:=bookgestage]
vars <- stringr::str_subset(names(smallD), "^anexamsfh_")

vars <- stringr::str_remove(vars, "anexamsfh_")


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
  smallD[,angestagedays_0:=bookgestagedays]
  smallD[,anbpdiast_0:=bookbpdiast]
  smallD[,anbpsyst_0:=bookbpsyst]
  smallD[,anexamsfh_0:=bookexamsfh]
  smallD[,labhb_0:=booklabhb]
  
  
  
  # pull out a list of all of the gestage variables
  
  gestagedaysVariablewithcarrot <- sprintf("^%s",gestagedaysVariable)
  listOfGestAgeVars <- names(smallD)[stringr::str_detect(names(smallD),gestagedaysVariablewithcarrot)]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, gestagedaysVariable,variableOfInterestPattern)

  
  for(i in 1:length(days)){
    # name of new variable
    var <- sprintf("TrialOne_%s_%s",variableOfInterestName,names(days)[i])
    # initialize all as FALSE if has booking variable
    smallD[!is.na(ident_dhis2_booking),(var):=FALSE]
    
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
  smallD[,angestagedays_0:=NULL]
  smallD[,anbpdiast_0:=NULL]
  smallD[,anbpsyst_0:=NULL]
 
}



######### Managements ############
#creating new variable for weeks for managements
mandays<- list(
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
  "20_20"=20*7+c(0:6),
  "21_21"=21*7+c(0:6),
  "22_22"=22*7+c(0:6),
  "23_23"=23*7+c(0:6),
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
  "40_40"=41*7+c(0:6)
  

)


for(i in 0:37){
  #i=23
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(3,4), width=2, flag="0")
  
  #output variable
  var_manhb <- sprintf("TrialOne_manhb_%s_%s", week_current, week_current)
  
  #id source
  var_badhb <- sprintf("TrialOne_labhb_anemia_sev_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_manhb):=as.logical(NA)]
  # is false, if you have a bad hb
  smallD[get(var_badhb)==TRUE, (var_man_hb):=TRUE]
  for(week_later in weeks_later){
    var_secondcheck <- sprintf("TrialOne_labhb_exists_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_man_hb)==FALSE  & get(var_secondcheck)==TRUE, (var_man_hb):=TRUE]
  }
}

# join the weeks together

smallD[,TrialOne_manhb_07_12:=pmax(
  TrialOne_manhb_07_07,
  TrialOne_manhb_08_08,
  TrialOne_manhb_09_09,
  TrialOne_manhb_10_10,
  TrialOne_manhb_11_11,
  TrialOne_manhb_12_12,
  na.rm=T)
  ]


#EXAMPLE:
  
  library(data.table)
x <- data.table(
  x1=c(NA,NA,NA,T,T,F,F),
  x2=c(NA,T,F,T,F,T,F)
)
x[,y:=pmax(x1,x2,na.rm=T)]
x



###### identifying outcomes #######

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
  "38_41"=c(266:293)
)

###ANC Visits####

VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="anT1gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT1gestagedays")

###ANC BP SYT ####

# BP SYST Present
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_present",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=60,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

# BP Diast Present
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_present",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=40,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

# BP Syst High
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_high",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

# BP Syst MildHTN
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_mildHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=149,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

# BP Syst ModSevHTN
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_modSevHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=150,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")


# BP Diast High
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_high",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

# BP Diast MildHTN
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_mildHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=99,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

# BP Diast Mod/SevHTN
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_modSevHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=100,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")


###ANC Anemia ####
# lab hb exists
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_exists",
  variableOfInterestPattern="labhb",
  TruevaluesMin=4,
  TruevaluesMax=20,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

nrow(smallD[labhb_1>=4 & labhb_1<=20])
xtabs(~smallD$TrialOne_labhb_exists_15_17)


# sev anemia
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_sev",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=6.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

# mild and moderate anemia
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_mild_mod",
  variableOfInterestPattern="labhb",
  TruevaluesMin=7,
  TruevaluesMax=10.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")


### Lab RBS Normal ####
# normal blood glucose
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_exists",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("POS", "NEG"),
  gestagedaysVariable = "labT1gestagedays")

# lab urglu pos
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_pos",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="POS",
  gestagedaysVariable = "labT1gestagedays")

# normal bloodglu values
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_exists",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

# high blood glucose
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_high",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=140,
  TruevaluesMax=500,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labT1gestagedays")

# Lab FBS Normal 
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

# Lab FBS High 
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_high",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=105,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")


#### US visits ####
# Has US visit
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_exists",
  variableOfInterestPattern="usT1gestagedays",
  TruevaluesMin=10,
  TruevaluesMax=300,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable ="usT1gestagedays")


# US suspected IUGR
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_iugrSuspected",
  variableOfInterestPattern="usiugr",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable ="usT1gestagedays")

# US expected LGA
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_lgaSuspected",
  variableOfInterestPattern="uslga",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usT1gestagedays")

# US pres-malpresentation
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_malpres",
  variableOfInterestPattern="us_malpres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete="Yes",
  gestagedaysVariable = "usT1gestagedays")

# US pres-malpresentation
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_malpresvar",
  variableOfInterestPattern="uspres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete= c("Trasverse","Breech"),
  gestagedaysVariable = "usT1gestagedays")


####SFH Discrepancies####
#AN SFH measurements
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexamsfh_exists",
  variableOfInterestPattern="anexamsfh",
  TruevaluesMin=5,
  TruevaluesMax=44,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")


######## Re Check These SFH Discrep variables!!!!!################
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepExists",
  variableOfInterestPattern="sfhDiscrep",
  TruevaluesMin=2.1,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="sfhDiscrepCon",
  variableOfInterestPattern="sfhDiscrepCon",
  TruevaluesMin=2.1,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

#anexampalp malpres
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="malpresanexam",
  variableOfInterestPattern="malpresanexam_",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = "Yes",
  gestagedaysVariable = "anT1gestagedays")

#anexampalp source
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexampalpmal",
  variableOfInterestPattern="anexampalp",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("Trasverse","Breech"),
  gestagedaysVariable = "anT1gestagedays")


####Referrals####
# Ref to HR
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHR",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHighRisk",
  gestagedaysVariable = "manT1gestagedays")

# Ref to Hosp
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "manT1gestagedays")

######## Managements ########

######################## Export ANC Outcome Data Sets ########################
#make these into vars and add them in vars keep
varsanevent <- names(smallD)[stringr::str_detect(names(smallD),"^anevent")]
varsangAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^angestage")]
varsangAdays <- names(smallD)[stringr::str_detect(names(smallD),"^anT1gestagedays")]
varslabevent <- names(smallD)[stringr::str_detect(names(smallD),"^labevent")]
varslabgAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^labgestage")]
varslabgAdays <- names(smallD)[stringr::str_detect(names(smallD),"^labT1gestagedays")]
varslabhb <- names(smallD)[stringr::str_detect(names(smallD),"^labhb")]
varsmanevent <- names(smallD)[stringr::str_detect(names(smallD),"^manevent")]
varsmangAweeks <- names(smallD)[stringr::str_detect(names(smallD),"^mangestage")]
varsgAdays <- names(smallD)[stringr::str_detect(names(smallD),"^manT1gestagedays")]
varsmantypex <- names(smallD)[stringr::str_detect(names(smallD),"^mantypex")]
varsmantypey <- names(smallD)[stringr::str_detect(names(smallD),"^mantypey")]
varsRefhosp<- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_refHosp_")]
varsRefHR <- names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_refHR_")]
	

varskeep <- c("prettyExposure",	
               "uniqueid",	
               "age",	
               "agepregnancy",	
               "avgincome",
               "avgincomecat",	
               "education", 
               "educationcat",
               "str_TRIAL_1_Cluster",	
               "bookorgdistricthashed",	
               "bookhistdm",	
               "bookhistcs",	
               "bookhistgdm",	
               "bookhistperi",	
               "bookhistph",	
               "bookhistaph",	
               "bookhistabort",	
               "bookhistpreecl",	
               "bookheight",	
               "bookwight",	
               "bookbmi", 
               "bookbmicat",
               "bookevent",	
               "bookgestage",	
               "bookgestagedays",	
               "booklabhb"	
               	
)

######## Making Variables for managment ###############

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


###### random samples **** QC **** #######
set.seed(7)
randomsample <- smallD[!is.na(TrialOne_labhb_anemia_sev_00_14)==T][sample(1:.N,5)]
#export this

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




########## Managements #########

  
##visits by 
#TO DO: managements and laburglu in a different code , referral to HR or refhosp 
#has to be onewith in one week after a lab test 

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
smallD[,sga:=NA]
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
######### Indication for CS ##########

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


##Twins Variable
smallD[,multiplepreg:=FALSE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"twins"),
       multiplepreg:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"twin"),
       multiplepreg:=TRUE]
smallD[stringr::str_detect(merged_indic_csection_lowercase,"triplets"),
       multiplepreg:=TRUE]

#making paperhbo lowercase to use in multiplepreg var paperhbo
smallD[,lowercasepaperhbo_notes_1:= stringr::str_to_lower(paperhbo_notes_1)]
smallD[,lowercasepaperhbo_notes_2:= stringr::str_to_lower(paperhbo_notes_2)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_1,"twins"),
       multiplepreg:=TRUE]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_1,"twin"),
       multiplepreg:=TRUE]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_2,"twins"),
       multiplepreg:=TRUE]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasepaperhbo_notes_2,"twin"),
       multiplepreg:=TRUE]

#twins var from dhis2hosp birthoutcomes

#twins variable from avicenna
smallD[,lowercaseAvicnotes_1:= stringr::str_to_lower(acsdatatext_1)]
smallD[,lowercaseAvicnotes_2:= stringr::str_to_lower(acsdatatext_2)]
smallD[,lowercaseAvicnotes_3:= stringr::str_to_lower(acsdatatext_3)]


smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"twin"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"twins"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"triplet"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_1,"triplets"), multiplepreg:=TRUE]    

smallD[multiplepreg==FALSE & stringr::str_detect(lowercaseAvicnotes_2,"twin"), multiplepreg:=TRUE] 

smallD[multiplepreg==FALSE & stringr::str_detect(lowercaseAvicnotes_2,"twins"), multiplepreg:=TRUE]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_2,"triplet"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_2,"triplets"), multiplepreg:=TRUE]    

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_3,"triplet"), multiplepreg:=TRUE]     
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseAvicnotes_3,"treplit"), multiplepreg:=TRUE]     
nrow(smallD[multiplepreg==T])

#twins var dhis2
smallD[,lowercaseDhis2hbo_1:= stringr::str_to_lower(dhis2hbousrecommendcomment_1)]
smallD[,lowercaseDhis2hbo_2:= stringr::str_to_lower(dhis2hbousrecommendcomment_2)]
smallD[,lowercaseDhis2hbo_3:= stringr::str_to_lower(dhis2hbousrecommendcomment_3)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseDhis2ho_1,"twins"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercaseDhis2ho_1,"twin"), multiplepreg:=TRUE]

#twins var dhis2
smallD[,lowercasehbocon_1:= stringr::str_to_lower(hboconreasonforcs_1)]
smallD[,lowercasehbocon_2:= stringr::str_to_lower(hboconreasonforcs_2)]
smallD[,lowercasehbocon_3:= stringr::str_to_lower(hboconreasonforcs_3)]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"twins"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"twin"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_1,"triplets"), multiplepreg:=TRUE]

smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"twins"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"twin"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_2,"triplet"), multiplepreg:=TRUE]
smallD[multiplepreg==FALSE &
         stringr::str_detect(lowercasehbocon_3,"triplet"), multiplepreg:=TRUE]




#in strings that we want we use the %in%
#smallD[merged_indic_csection %in% c("Breech","TWINS , BOTH BREECH IN LABOUR"#,"breach", "transverse"), var:=true


######### LBW and Macrosomia ##########
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

# lbw:low birth weight
smallD[,lbw:=NA]
smallD[!is.na(merged_pregbweight),lbw:=FALSE]
smallD[USorLMPdateCombogAdays>168 &
         USorLMPdateCombogAdays<=314 &
         merged_pregbweight <1500, lbw:=TRUE]

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
         merged_pregbweight <1500, macrosomia:=TRUE]

smalld <- smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                    ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                 keyby=.(macrosomia)]


openxlsx::write.xlsx(smalld, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "hbo_outcomes",
                       sprintf("Macrosomia_%s.xlsx", 
                               lubridate::today())))

#### Anonymized BirthOutcome Data Set#####
varsanexpalp <- names(smallD)[stringr::str_detect(names(smallD),"^anexampalp")]
varsuspres <- names(smallD)[stringr::str_detect(names(smallD),"^uspres")]
varsusgestage <- names(smallD)[stringr::str_detect(names(smallD),"^usgestage")]
varsusgestagedays <- names(smallD)[stringr::str_detect(names(smallD),"^usT1gestagedays")]
varsangestage <- names(smallD)[stringr::str_detect(names(smallD),"^angestage")]
varsangestagedays <- names(smallD)[stringr::str_detect(names(smallD),"^anT1gestagedays")]
varsusiugr <- names(smallD)[stringr::str_detect(names(smallD),"^usiugr")]
varsuslga <- names(smallD)[stringr::str_detect(names(smallD),"^uslga")]

smallD[prettyExposure=="Trial Arm A", prettyExposure:="D"]
smallD[prettyExposure=="Trial Arm B", prettyExposure:="C"]

 varskeep <- c("prettyExposure",
               "uniqueid",
               "agecat",
               "agepregnancycat",
               "incomecat",
               "educationcat",
               "str_TRIAL_1_Cluster",
               "bookorgdistricthashed",
               "bookgestage",
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
               "TrialOne_malpresanexam_35_37",
               "TrialOne_malpresanexam_38_41",
               "TrialOne_us_malpres_35_37",
               "TrialOne_us_malpres_38_41",
               "has_malpresentation",
               "merged_pregbweight",
               "merged_birthweight_1",
               "merged_birthweight_2",
               "merged_birthweight_3",
               "mahima_hospenteredgestage_1",
               "mahima_gestageatbirthwk_1",
               "macrosomia",
               "elbw",
               "lbw",
               "USorLMPdateCombogAdays",
              "lga",
               "sga",
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
              varsuspres,
              varsusgestage,
              varsusgestagedays,
              varsangestage,
              varsangestagedays,
              varsusiugr,
              varsuslga
              
              )
 
 hbooutcomes <-smallD[,varskeep,with=F]
 
 openxlsx::write.xlsx(hbooutcomes,file.path(FOLDER_DATA_CLEAN,
                                      "Trial_1_Outcomes",
                                      sprintf("%s_BirthOutcomes.xlsx", 
                                              lubridate::today())))


 #### Anonymize Data Set ####
 #### Choose Variables Data Set ####
 #each outcome will have its own data set with the variables we made above
 #rename ident_dhis2_control to something else in each data set
 #keep lab values in smaller data sets 
