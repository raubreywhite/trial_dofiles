# define data set

t2reboot <- d[bookdate>"2019-12-01" & ident_TRIAL_2_and_3==T,]
t2reboot[,smsyes:=areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits]

####### creating variables we need ####### 
# making bookgestage cats
t2reboot[,bookgestagecat:=cut(bookgestage,
                              breaks=c(0,14,17,22,23,28,30,33,34,37,40),
                              include.lowest=T)]

# timely anc variables
t2reboot[,ancbefore15:=as.logical(NA)]
t2reboot[,anc15to17:=as.logical(NA)]
t2reboot[,anc18to22:=as.logical(NA)]
t2reboot[,anc23:=as.logical(NA)]
t2reboot[,anc24to28:=as.logical(NA)]
t2reboot[,anc29to30:=as.logical(NA)]
t2reboot[,anc31to33:=as.logical(NA)]
t2reboot[,anc34:=as.logical(NA)]
t2reboot[,anc35to37:=as.logical(NA)]
t2reboot[,anc38to40:=as.logical(NA)]

vars_gestage <- stringr::str_subset(names(t2reboot),"^angestage_[0-9]+")
vars_anevent <- stringr::str_subset(names(t2reboot),"^anevent_[0-9]+")

for(i in vars_gestage){
  t2reboot[get(vars_gestage)>0 & get(vars_gestage)<=14 &
             !is.na(vars_anevent),ancbefore15:=TRUE]
  
  t2reboot[get(vars_gestage)>=15 & get(vars_gestage)<=17 & 
             !is.na(vars_anevent),anc15to17:=TRUE]
  
  t2reboot[get(vars_gestage)>=18 & get(vars_gestage)<=22 &
             !is.na(vars_anevent),anc18to22:=TRUE]
  
  t2reboot[get(vars_gestage)==23 &!is.na(vars_anevent),anc23:=TRUE]
  
  t2reboot[get(vars_gestage)>=24 & get(vars_gestage)<=28 & 
             !is.na(vars_anevent),anc24to28:=TRUE]
  
  t2reboot[get(vars_gestage)>=29 & get(vars_gestage)<=30 &
             !is.na(vars_anevent),anc29to30:=TRUE]
  
  t2reboot[get(vars_gestage)>=31 & get(vars_gestage)<=33 & 
             !is.na(vars_anevent),anc31to33:=TRUE]
  
  t2reboot[get(vars_gestage)==34 & !is.na(vars_anevent),anc34:=TRUE]
  
  t2reboot[get(vars_gestage)>=35 & get(vars_gestage)<=37 & 
             !is.na(vars_anevent),anc35to37:=TRUE]
  
  t2reboot[get(vars_gestage)>=38 & get(vars_gestage)<=40 & 
             !is.na(vars_anevent),anc38to40:=TRUE]
  

  }

t2nums <- t2reboot[,.(N=.N,
                      Booked=sum(ident_dhis2_booking==T, na.rm=T),
                      ANCvisits=sum(ident_dhis2_an==T, na.rm=T),
                      BookedSMSyes=sum(smsyes==1,na.rm=T),
                      BookedSMSonly=sum(ident_TRIAL_2==T & 
                                          ident_TRIAL_3==F, na.rm=T),
                      BookedinSMSclinic=sum(ident_TRIAL_2==T, na.rm=T),
                      BookedQIDonly=sum(ident_TRIAL_2==F & ident_TRIAL_3==T),
                      BookdQID=sum(ident_TRIAL_3==T, na.rm=T),
                      BookedBoth=sum(ident_TRIAL_3==T & ident_TRIAL_2==T),
                      BookedControl=sum(ident_TRIAL_2_3_Control, na.rm=T)
                      
)]

openxlsx::write.xlsx(t2nums,file.path(FOLDER_DATA_RESULTS,
                                        "T2",
                                        sprintf("%s_recruit_update_by_arm.xlsx",
                                                lubridate::today())))

# bookgestage cats
t2bookcats <- t2reboot[,.(N=.N),
                       keyby=.(bookgestagecat)]

openxlsx::write.xlsx(t2bookcats,file.path(FOLDER_DATA_RESULTS,
                                        "T2",
                                        sprintf("%s_recruit_update_bookgestage.xlsx",
                                                lubridate::today())))


#anc visits timely and not timely

vars <- names(t2reboot)[stringr::str_detect(names(t2reboot),"^anevent_[0-9]+")]
t2reboot[,anevent_x:=0]

print(vars)

for(i in vars){
  t2reboot[!is.na(get(i)), anevent_x:=anevent_x + 1]
}

sum(t2reboot[ident_dhis2_control==F]$anevent_x,na.rm=T)


# visits per clinic

t2visits <- t2reboot[,.(N=.N,
                        bookingvisits=sum(!is.na(bookevent), na.rm=T),
                        ancvisits=sum(anevent_x, na.rm=T),
                        ancb415=sum(ancbefore15, na.rm=T),
                        anc15to17=sum(anc15to17, na.rm=T),
                        anc18to22=sum(anc18to22, na.rm=T),
                        anc24to28=sum(anc24to28, na.rm=T),
                        anc31to33=sum(anc31to33, na.rm=T),
                        anc35to37=sum(anc35to37, na.rm=T)),
                     keyby=.(str_TRIAL_2_Cluster)]

openxlsx::write.xlsx(t2visits,file.path(FOLDER_DATA_RESULTS,
                                         "T2",
                                         sprintf("%s_recruit_update_visits_by_clinic.xlsx",
                                                 lubridate::today())))


t2visits <- t2reboot[,.(N=.N,
                        bookingvisits=sum(!is.na(bookevent), na.rm=T),
                        ancvisits=sum(anevent_x, na.rm=T),
                        ancb415=sum(ancbefore15, na.rm=T),
                        anc15to17=sum(anc15to17, na.rm=T),
                        anc18to22=sum(anc18to22, na.rm=T),
                        anc24to28=sum(anc24to28, na.rm=T),
                        anc31to33=sum(anc31to33, na.rm=T),
                        anc35to37=sum(anc35to37, na.rm=T))]

openxlsx::write.xlsx(t2visits,file.path(FOLDER_DATA_RESULTS,
                                        "T2",
                                        sprintf("%s_recruit_update_visits.xlsx",
                                                lubridate::today())))

# by bookgestage

t2visits <- t2reboot[,.(N=.N,
                        bookingvisits=sum(!is.na(bookevent), na.rm=T),
                        ancvisits=sum(anevent_x, na.rm=T),
                        ancb415=sum(ancbefore15, na.rm=T),
                        anc15to17=sum(anc15to17, na.rm=T),
                        anc18to22=sum(anc18to22, na.rm=T),
                        anc24to28=sum(anc24to28, na.rm=T),
                        anc31to33=sum(anc31to33, na.rm=T),
                        anc35to37=sum(anc35to37, na.rm=T)),
                     keyby=.(bookgestagecat)]

openxlsx::write.xlsx(t2visits,file.path(FOLDER_DATA_RESULTS,
                                        "T2",
                                        sprintf("%s_recruit_update_visits_by_bookgestage.xlsx",
                                                lubridate::today())))


####################### Process outcomes ####################### 

t2reboot[,bookgestagedays_cats:=cut(bookgestagedays,
                                  breaks=c(-500,0,104,
                                           125,160,167,202,
                                           216,237,244,265,293),
                                  include.lowest=T)]

# MAKE BOOK VISIT FOR ANEMIA
t2reboot[,booklabhb:=as.numeric(NA)]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]

# MAKE BOOK VISIT FOR Laburglu
t2reboot[,booklaburglu:=as.character(NA)]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]

t2reboot[,booklaburglu:=NULL]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7,
       booklaburglu:=laburglu_1]
xtabs(~t2reboot$booklaburglu)
str(t2reboot$booklaburglu)
unique(t2reboot$booklaburglu)

t2reboot[,booklaburglu:=NULL]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]
xtabs(~t2reboot$booklaburglu)


# MAKE BOOK VISIT FOR LABBLOODGLU
t2reboot[,booklabbloodglu:=as.integer(NA)]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7,booklabbloodglu:=labbloodglu_1]
xtabs(~t2reboot$booklabbloodglu, addNA=T)

# MAKE BOOK VISIT FOR LABBLOODGLU_HIGH
t2reboot[,booklabbloodglu_high:=as.logical(NA)]
t2reboot[!is.na(booklabbloodglu),booklabbloodglu_high:=FALSE]
t2reboot[booklabbloodglu>=140 & booklabbloodglu<500,booklabbloodglu_high:=TRUE]
xtabs(~t2reboot$booklabbloodglu_high, addNA=T)

# MAKE BOOK VISIT FOR LABFASTBLOODGLU
t2reboot[,booklabfastbloodglu:=as.numeric(NA)]
t2reboot[abs(labT1gestagedays_1-bookgestagedays)<7,booklabfastbloodglu:=labfastbloodglu_1]
xtabs(~t2reboot$booklabfastbloodglu)

# MAKE BOOK VISIT FOR LABfastBLOODGLU_HIGH
t2reboot[,booklabfastbloodglu_high:=as.logical(NA)]
t2reboot[!is.na(booklabfastbloodglu),booklabfastbloodglu_high:=FALSE]
t2reboot[booklabfastbloodglu>126 ,booklabfastbloodglu_high:=TRUE]
xtabs(~t2reboot$booklabfastbloodglu_high, addNA=T)

# Discrepancy Variable anexamsfh variable
t2reboot[,anexamsfh_0:=bookexamsfh]
t2reboot[,angestage_0:=bookgestage]
vars <- stringr::str_subset(names(t2reboot), "^anexamsfh_")

vars <- stringr::str_remove(vars, "anexamsfh_")

#anexamsfh stuff
for(i in vars){
  print(i)
  anexamsfh <-sprintf("anexamsfh_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrep <-  sprintf("sfhDiscrep_%s",i)
  
  t2reboot[,(sfhDiscrep):=as.numeric(NA)]
  
  t2reboot[!is.na(get(angestage)) &
           !is.na(get(anexamsfh)), (sfhDiscrep):=abs(get(anexamsfh)-get(angestage))]
  
}

# SFH discrepancy with ancongestagesizevisitweek
vars <- stringr::str_subset(names(t2reboot), "^anconancgestationaageatvisitweeks_")
vars <- stringr::str_remove(vars, "anconancgestationaageatvisitweeks_")

#anconancgestationaageatvisitweeks var
for(i in vars){
  print(i)
  anconangestageweeks <-sprintf("anconancgestationaageatvisitweeks_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrepCon <-  sprintf("sfhDiscrepCon_%s",i)
  
  t2reboot[,(sfhDiscrepCon):=as.numeric(NA)]
  
  t2reboot[!is.na(get(angestage)) &
           !is.na(get(anconangestageweeks)), 
         (sfhDiscrepCon):=abs(get(anconangestageweeks)-get(angestage))]
  
}


# SFH discrepancy with ancongestagesizevisitweek
vars <- stringr::str_subset(names(t2reboot), "^anconancgestationaageatvisitweeks_")
vars <- stringr::str_remove(vars, "anconancgestationaageatvisitweeks_")

#anconancgestationaageatvisitweeks var
for(i in vars){
  print(i)
  anconangestageweeks <-sprintf("anconancgestationaageatvisitweeks_%s",i)
  angestage <- sprintf("angestage_%s",i)
  sfhDiscrepCon <-  sprintf("sfhDiscrepCon_%s",i)
  
  t2reboot[,(sfhDiscrepCon):=as.numeric(NA)]
  
  t2reboot[!is.na(get(angestage)) &
           !is.na(get(anconangestageweeks)), 
         (sfhDiscrepCon):=abs(get(anconangestageweeks)-get(angestage))]
  
}

# anT1 in weeks to calculate sfhDiscrep via anexamsfh and anT1gestagedays to weeks
t2reboot[,anT1gestagedays_0:=bookgestagedays]
vars <- stringr::str_subset(names(t2reboot), "^anT1gestagedays_")
vars <- stringr::str_remove(vars, "^anT1gestagedays_")
for (i in vars){
  anT1gestagedays <- sprintf("anT1gestagedays_%s",i)
  anT1gAweeks <- sprintf("anT1gAweeks_%s",i)
  
  t2reboot[, (anT1gAweeks):=floor(get(anT1gestagedays)/7)]
}


# Discrepancy Variable anexamsfh variable
t2reboot[,anexamsfh_0:=bookexamsfh]

vars <- stringr::str_subset(names(t2reboot), "^anexamsfh_")
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
  
  t2reboot[!is.na(anT1gAweeks) &
           !is.na(get(anexamsfh)), (sfhDiscrepAnt1gas):=abs(get(anexamsfh)-get(anT1gAweeks))]
  
  t2reboot[!is.na(anT1gAweeks) &
           !is.na(get(anexamsfh)), (sfhDiscrepAnt1gasCat):=abs(get(anexamsfh)-get(anT1gAweeks))>2]
  
}

# an exam malpresentation into one variable
vars_source <- names(d)[stringr::str_detect(names(t2reboot),"^anexampalp_")]
vars_outcome <- stringr::str_replace(vars_source, "anexampalp", "malpresanexam_")

for(i in 1:length(vars_source)){
  var_source <- vars_source[i]
  var_outcome <- vars_outcome[i]
  t2reboot[get(var_source) %in% c("Trasverse", "Breech"), (var_outcome):="Yes"]
}

# uspres malpresentation variable
vars_source <- names(d)[stringr::str_detect(names(t2reboot),"^uspres_")]
vars_outcome <- stringr::str_replace(vars_source, "uspres_", "us_malpres_")

for(i in 1:length(vars_source)){
  var_source <- vars_source[i]
  var_outcome <- vars_outcome[i]
  t2reboot[get(var_source) %in% c("Trasverse", "Breech"), (var_outcome):="Yes"]
  
}




VisitVariables <- function(t2reboot,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="anT1gestagedays" ){
  
  if(!is.null(TruevaluesMin) & !is.null(TruevaluesMax) & !is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NOT NULL")
  }
  
  if(is.null(TruevaluesMin) & is.null(TruevaluesMax) & is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NULL")
  }
  
  
  # pull out a list of all of the gestage variables
  #browser()
  gestagedaysVariablewithcarrot <- sprintf("^%s",gestagedaysVariable)
  listOfGestAgeVars <- names(t2reboot)[stringr::str_detect(names(t2reboot),gestagedaysVariablewithcarrot)]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, gestagedaysVariable,variableOfInterestPattern)
  
  
  for(i in 1:length(days)){
    # name of new variable
    var <- sprintf("TrialOne_%s_%s",variableOfInterestName,names(days)[i])
    # initialize all as FALSE if has booking variable
    t2reboot[!is.na(ident_dhis2_booking),(var):=FALSE]
    #xtabs(~t2reboot[[var]])
    
    # loop through the "gestage"/"bp" variables
    for(j in 1:length(listOfGestAgeVars)){
      gestageVar <- listOfGestAgeVars[j]
      interestVar <- listOfInterestVars[j]
      
      #asking discrete question
      if(!is.null(TruevaluesDiscrete)){
        
        t2reboot[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar) %in% TruevaluesDiscrete ,(var):=TRUE]
        
      }else{ #asking non discrete questions
        
        
        t2reboot[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar)>=TruevaluesMin & get(interestVar)<=TruevaluesMax, (var):=TRUE]
        
      }
      
      
      
    }
  }
  
  return(t2reboot)
  
  
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
t2reboot[,anT1gestagedays_0:=bookgestagedays]

t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="anT1gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
xtabs(~t2reboot$TrialOne_anvisitnew_00_00)

###ANC BP SYT ####
# BP SYST Present
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpsyst_0:=bookbpsyst]

t2reboot<-VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpsyst_present",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=60,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpsyst_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpsyst_present_00_00)

# BP Diast Present
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpdiast_0:=bookbpdiast]

t2reboot<- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpdiast_present",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=40,
  TruevaluesMax=170,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpdiast_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpdiast_present_00_14)

# BP Syst High
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpsyst_0:=bookbpsyst]

t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpsyst_high",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpsyst_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpsyst_high_00_14)

# BP Syst MildHTN
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpsyst_0:=bookbpsyst]

t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpsyst_mildHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=149,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpsyst_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpsyst_mildHTN_00_14)

# BP Syst ModSevHTN
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpsyst_0:=bookbpsyst]

t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpsyst_modSevHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=150,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpsyst_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpsyst_modSevHTN_00_14)

# BP Diast High
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpdiast_0:=bookbpdiast]

t2reboot <-VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpdiast_high",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")

t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpdiast_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpdiast_high_00_14)


# BP Diast MildHTN
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpdiast_0:=bookbpdiast]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpdiast_mildHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=90,
  TruevaluesMax=99,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpdiast_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpdiast_mildHTN_00_14)


# BP Diast Mod/SevHTN
t2reboot[,anT1gestagedays_0:=bookgestagedays]
t2reboot[,anbpdiast_0:=bookbpdiast]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="anbpdiast_modSevHTN",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=100,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "anT1gestagedays")
t2reboot[,anT1gestagedays_0:=NULL]
t2reboot[,anbpdiast_0:=NULL]
xtabs(~t2reboot$TrialOne_anbpdiast_modSevHTN_00_14)


### ANC Anemia ####
# lab hb exists
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labhb_0:=booklabhb]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labhb_exists",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=20,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

nrow(t2reboot[labhb_1>=4 & labhb_1<=20])
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labhb_0:=NULL]
xtabs(~t2reboot$TrialOne_labhb_exists_15_17)

#normal hb
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labhb_0:=booklabhb]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labhb_normal",
  variableOfInterestPattern="labhb",
  TruevaluesMin=11,
  TruevaluesMax=20,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labhb_0:=NULL]
xtabs(~t2reboot$TrialOne_labhb_normal_15_17, addNA=T)

# sev anemia
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labhb_0:=booklabhb]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labhb_anemia_sev",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=6.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")

nrow(t2reboot[labhb_1>=1 & labhb_1<7])
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labhb_0:=NULL]
xtabs(~t2reboot$TrialOne_labhb_anemia_sev_15_17)


# mild and moderate anemia
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labhb_0:=booklabhb]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labhb_anemia_mild_mod",
  variableOfInterestPattern="labhb",
  TruevaluesMin=7,
  TruevaluesMax=10.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
nrow(t2reboot[labhb_1>=7 & labhb_1<11])
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labhb_0:=NULL]
nrow(t2reboot[labgestage_1<=15 & labgestage_1<=17 & labhb_1>7 & labhb_1<11])
xtabs(~t2reboot$TrialOne_labhb_anemia_mild_mod_15_17, addNA=T)



### Lab RBS Normal ####
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,laburglu_0:=booklaburglu]
# normal urine glucose
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="laburglu_exists",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("POS", "NEG"),
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,laburglu_0:=NULL]
xtabs(~t2reboot$TrialOne_laburglu_exists_15_17)

# lab urglu pos
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,laburglu_0:=booklaburglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="laburglu_pos",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete =c("POS"),
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,laburglu_0:=NULL]
nrow(t2reboot[laburglu_1=="POS" & labgestage_1>0 & labgestage_1<=14])
xtabs(~t2reboot$TrialOne_laburglu_pos_00_14)


# labbloodglu exist
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labbloodglu_0:=booklabbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labbloodglu_exists",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labbloodglu_exists_15_17)

# high blood glucose
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labbloodglu_0:=booklabbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labbloodglu_high",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=140,
  TruevaluesMax=500,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labbloodglu_high_00_14)
xtabs(~t2reboot$TrialOne_labbloodglu_high_18_22)


# Lab FBS exists
#http://perinatology.com/Reference/Reference%20Ranges/Glucose,%20fasting.htm
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labfastbloodglu_0:=booklabfastbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labfastbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labfastbloodglu_exists_15_17)

# Lab FBS Normal
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labfastbloodglu_0:=booklabfastbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labfastbloodglu_normal",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=71,
  TruevaluesMax=91,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labfastbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labfastbloodglu_normal_15_17)

# Lab FBS likely GDM
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labfastbloodglu_0:=booklabfastbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labfastbloodglu_likelyGDM",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=92,
  TruevaluesMax=125,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labfastbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labfastbloodglu_likelyGDM_24_28)


# Lab FBS High 
t2reboot[,labT1gestagedays_0:=bookgestagedays]
t2reboot[,labfastbloodglu_0:=booklabfastbloodglu]
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="labfastbloodglu_high",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=126,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
t2reboot[,labT1gestagedays_0:=NULL]
t2reboot[,labfastbloodglu_0:=NULL]
xtabs(~t2reboot$TrialOne_labfastbloodglu_high_24_28)

#### US visits ####
# Has US visit
t2reboot <-VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_exists",
  variableOfInterestPattern="usT1gestagedays",
  TruevaluesMin=10,
  TruevaluesMax=300,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_exists_00_14)

# US suspected IUGR
t2reboot <-VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_iugrSuspected",
  variableOfInterestPattern="usiugr",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = 1,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_iugrSuspected_00_14)

# US expected LGA
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_lgaSuspected",
  variableOfInterestPattern="uslga",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_lgaSuspected_00_14)

# US pres-malpresentation
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_malpres",
  variableOfInterestPattern="us_malpres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete="Yes",
  gestagedaysVariable = "usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_malpres_00_14)

# US pres-malpresentation
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_malpresvar",
  variableOfInterestPattern="uspres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete= c("Trasverse","Breech"),
  gestagedaysVariable = "usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_malpresvar_00_14)

#uspres_checked
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="us_pres_checked",
  variableOfInterestPattern="uspres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete= c("Trasverse","Breech","Cephalic","Unknown"),
  gestagedaysVariable = "usT1gestagedays")
xtabs(~t2reboot$TrialOne_us_pres_checked_00_14, addNA=T)

### removed sfh discrepancies and anexampalp code from here



####Referrals####
# Ref to HR
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="refHR",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHighRisk",
  gestagedaysVariable = "manT1gestagedays")
nrow(t2reboot[mantypex_1=="RefHighRisk" & manT1gestagedays_1>=15 & manT1gestagedays_1<=17])
nrow(t2reboot[mantypex_1=="RefHighRisk" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_refHR_00_14)
xtabs(~t2reboot[ident_dhis2_control==F]$TrialOne_refHR_00_14)
xtabs(~t2reboot$TrialOne_refHR_35_37)

# Ref to Hosp
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "manT1gestagedays")
nrow(t2reboot[mantypex_1=="RefHosp" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_refHosp_00_14)
xtabs(~t2reboot[ident_dhis2_control==F]$mantypex_1, addNA=T)

# RefDiabetes
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="refDiab",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefDiabetes",
  gestagedaysVariable = "manT1gestagedays")
nrow(t2reboot[mantypex_1=="RefDiabetes" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~t2reboot$TrialOne_refDiab_00_14)


# Management Performed
t2reboot <- VisitVariables(
  t2reboot=t2reboot,
  days=days,
  variableOfInterestName="manperf",
  variableOfInterestPattern="manperf",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "manT1gestagedays")
xtabs(~t2reboot$TrialOne_manperf_18_22)

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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_manhb):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badhb)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badhb)==TRUE, (var_temp_manhb):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manhb)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manhb):=TRUE]
  }
  #making var for sev anemia 
  t2reboot[,(var_manhb):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_manhb):=get(var_temp_manhb)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_manhb):=get(var_temp_manhb) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_manhb):=NULL]
}
xtabs(~t2reboot$TrialOne_manhb_24_24)



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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_manhb):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badhb)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badhb)==TRUE, (var_temp_manhb):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_labhb_exists_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second anemia check
    #var_secondcheck <- sprintf("TrialOne_labhb_exists_%s_%s", 
    #    week_later, 
    #   week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manhb)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manhb):=TRUE]
  }
  #making var for sev anemia 
  t2reboot[,(var_manhb):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_manhb):=get(var_temp_manhb)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_manhb):=get(var_temp_manhb) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_manhb):=NULL]
}
xtabs(~t2reboot$TrialOne_manhb_mildmodhbret_32_32)

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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_manght):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badght)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badght)==TRUE, (var_temp_manght):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second anemia check
    var_secondcheck <- sprintf("TrialOne_anbpsyst_present_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manght)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manght):=TRUE]
  }
  #making var for sev anemia 
  t2reboot[,(var_manght):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_manght):=get(var_temp_manght)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_manght):=get(var_temp_manght) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_manght):=NULL]
}
xtabs(~t2reboot$TrialOne_manhtn_ModSev_18_18)

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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  t2reboot[,(var_mangdm):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_mangdm):=NULL]
  
}
xtabs(~t2reboot$TrialOne_manRBGHigh_Hosp_24_24)


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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHR_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  t2reboot[,(var_mangdm):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_mangdm):=NULL]
  
}
xtabs(~t2reboot$TrialOne_manRBGHigh_HR_24_24)

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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  t2reboot[,(var_mangdm):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_mangdm):=NULL]
  
}
xtabs(~t2reboot$TrialOne_manRBGHigh_HR_24_24)


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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_manpres):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badpres)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badpres)==TRUE, (var_temp_manpres):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manpres)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manpres):=TRUE]
  }
  #making var for high blood glu 
  t2reboot[,(var_manpres):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_manpres):=get(var_temp_manpres)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_manpres):=get(var_temp_manpres) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_manpres):=NULL]
  
}
xtabs(~t2reboot$TrialOne_manmalpres_us_36_36)


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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_manpres):=as.logical(NA)]
  
  # is false, if you have a bad hb
  t2reboot[get(var_badpres)==TRUE, (var_temp_manperf):=FALSE]
  t2reboot[get(var_badpres)==TRUE, (var_temp_manpres):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manpres)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_manpres):=TRUE]
  }
  #making var for high blood glu 
  t2reboot[,(var_manpres):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_manpres):=get(var_temp_manpres)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_manpres):=get(var_temp_manpres) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_manpres):=NULL]
  
}
xtabs(~t2reboot$TrialOne_manmalpres_anexam_35_35)

### iugr and lga stuff for managements was remoed from here

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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_refHosp):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  t2reboot[get(var_refHospsource)==TRUE, (var_temp_manperf):=FALSE]
  
  # everyone
  #t2reboot[!is.na(get(var_refHospsource)), (var_temp_refHosp):=FALSE]
  
  # control
  t2reboot[get(var_refHospsource)==TRUE, (var_temp_refHosp):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  t2reboot[,(var_refHosp):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_refHosp):=get(var_temp_refHosp)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_refHosp):=get(var_temp_manperf) & 
           get(var_temp_refHosp)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_refHosp):=NULL]
  
}
xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_manRef_Hosp_35_35)
xtabs(~t2reboot[ident_dhis2_control==F]$TrialOne_manRef_Hosp_35_35)
xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_manRef_Hosp_32_32)
xtabs(~t2reboot[ident_dhis2_control==F]$TrialOne_manRef_Hosp_32_32)

checkHosp <- t2reboot[!is.na(TrialOne_manRef_Hosp_32_32) &
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
  t2reboot[,(var_temp_manperf):=as.logical(NA)]
  t2reboot[,(var_temp_refHR):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  t2reboot[get(var_refHRsource)==TRUE, (var_temp_manperf):=FALSE]
  
  
  # control
  t2reboot[get(var_refHRsource)==TRUE, (var_temp_refHR):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    t2reboot[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  t2reboot[,(var_refHR):=as.logical(NA)]
  
  #control
  t2reboot[ident_dhis2_control==T,(var_refHR):=get(var_temp_refHR)]
  
  #intervention
  t2reboot[ident_dhis2_control==F,(var_refHR):=get(var_temp_manperf) & 
           get(var_temp_refHR)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  t2reboot[,(var_temp_manperf):=NULL]
  t2reboot[,(var_temp_refHR):=NULL]
  
}
xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_manRef_HR_35_35)
xtabs(~t2reboot[ident_dhis2_control==F]$TrialOne_manRef_HR_35_35)

xtabs(~t2reboot[ident_dhis2_control==T]$TrialOne_manRef_HR_20_20)
xtabs(~t2reboot[ident_dhis2_control==F]$TrialOne_manRef_HR_20_20)

checkHR <- t2reboot[!is.na(TrialOne_manRef_HR_20_20) &
                    ident_dhis2_control==F, c("TrialOne_manperf_20_20",
                                              "TrialOne_refHR_20_20",
                                              "TrialOne_manRef_HR_20_20")]






##################### Process Outcomes #################


########## Anemia ########## 
# Define opportunities at 3 different cut off points

## booked before 24
t2reboot[,Opportunity_anemia_screening_1:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]",
                                   "(125,160]",
                                   "(160,167]"),
       Opportunity_anemia_screening_1:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_1, addNA=T)


## booked 24 or has visit 
t2reboot[,Opportunity_anemia_screening_2:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(167,202]")| 
         TrialOne_anvisitnew_24_28==T,
       Opportunity_anemia_screening_2:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_2, addNA=T)

# booked 29-34 weeks or has visit
t2reboot[,Opportunity_anemia_screening_3:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]",
                                   "(237,244]"),
       Opportunity_anemia_screening_3:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_3, addNA=T)


## booked or visit at 35-37 weeks
t2reboot[,Opportunity_anemia_screening_4:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(244,265]") |
         TrialOne_anvisitnew_35_37==T, 
       Opportunity_anemia_screening_4:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_4, addNA=T)



## severe anemia at booking and at any other visit after that
t2reboot[,Opportunity_anemia_screening_5:=as.numeric(NA)]
t2reboot[TrialOne_labhb_anemia_sev_00_14==T|
         TrialOne_labhb_anemia_sev_15_17==T|
         TrialOne_labhb_anemia_sev_18_22==T|
         TrialOne_labhb_anemia_sev_23_23==T,Opportunity_anemia_screening_5:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_5, addNA=T)


## mild mod anemia
t2reboot[,Opportunity_anemia_screening_6:=as.numeric(NA)]
t2reboot[TrialOne_labhb_anemia_mild_mod_00_14==T|
         TrialOne_labhb_anemia_mild_mod_15_17==T|
         TrialOne_labhb_anemia_mild_mod_18_22==T|
         TrialOne_labhb_anemia_mild_mod_23_23==T,
       Opportunity_anemia_screening_6:=1]

xtabs(~t2reboot$Opportunity_anemia_screening_6, addNA=T)



# ADJUSTING OPPORTUNITIES FOR THOSE WHO HAVE BEEN REFERRED
## Before 24 weeks

#variable for man sev anemia anytime before 24 weeks
t2reboot[,manhbsev:=(TrialOne_manhb_00_00 |
                     TrialOne_manhb_01_01 |
                     TrialOne_manhb_02_02 |
                     TrialOne_manhb_03_03 |
                     TrialOne_manhb_04_04 |
                     TrialOne_manhb_05_05 |
                     TrialOne_manhb_06_06 |
                     TrialOne_manhb_07_07 |
                     TrialOne_manhb_08_08 |
                     TrialOne_manhb_09_09 |
                     TrialOne_manhb_10_10 |
                     TrialOne_manhb_11_11 |
                     TrialOne_manhb_12_12 |
                     TrialOne_manhb_13_13 |
                     TrialOne_manhb_14_14 |
                     TrialOne_manhb_15_15 |
                     TrialOne_manhb_16_16 |
                     TrialOne_manhb_17_17 |
                     TrialOne_manhb_18_18 |
                     TrialOne_manhb_19_19 |
                     TrialOne_manhb_20_20 |
                     TrialOne_manhb_21_21 |
                     TrialOne_manhb_22_22 |
                     TrialOne_manhb_23_23)]
xtabs(~t2reboot$manhbsev, addNA=T)


t2reboot[,RefHr:=as.logical(NA)]
t2reboot[Opportunity_anemia_screening_1==1, RefHr:=FALSE]
t2reboot[(TrialOne_manRef_HR_00_00==T|
          TrialOne_manRef_HR_01_01==T|
          TrialOne_manRef_HR_02_02==T|
          TrialOne_manRef_HR_03_03==T|
          TrialOne_manRef_HR_04_04==T|
          TrialOne_manRef_HR_05_05==T|
          TrialOne_manRef_HR_06_06==T|
          TrialOne_manRef_HR_07_07==T|
          TrialOne_manRef_HR_08_08==T|
          TrialOne_manRef_HR_09_09==T|
          TrialOne_manRef_HR_10_10==T|
          TrialOne_manRef_HR_11_11==T|
          TrialOne_manRef_HR_12_12==T|
          TrialOne_manRef_HR_13_13==T|
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
       RefHr:=TRUE]
xtabs(~t2reboot$RefHr, addNA=T)

## At 24-28 weeks
t2reboot[Opportunity_anemia_screening_2==1 &
         (TrialOne_anvisitnew_24_24 & 
            (RefHr==T))|
         (TrialOne_anvisitnew_25_25 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28 & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T)), 
       Opportunity_anemia_screening_2:=Opportunity_anemia_screening_2-1]

xtabs(~t2reboot$Opportunity_anemia_screening_2, addNA=T)

# 35-37 weeks
t2reboot[Opportunity_anemia_screening_4==1 &
         (TrialOne_anvisitnew_29_30==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T))|
         (TrialOne_anvisitnew_31_33==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T))|
         (TrialOne_anvisitnew_34_34==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T|
               TrialOne_manRef_HR_31_31==T|
               TrialOne_manRef_HR_32_32==T|
               TrialOne_manRef_HR_33_33==T)), 
       Opportunity_anemia_screening_4:=Opportunity_anemia_screening_4-1]
xtabs(~t2reboot$Opportunity_anemia_screening_4, addNA=T)

#define different time cats for success
t2reboot[, HbonTime_1a:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_1==1, HbonTime_1a:=FALSE]

t2reboot[, HbonTime_1b:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_1==1 & 
         booklabhb<7 & booklabhb>=2,HbonTime_1b:=FALSE]


t2reboot[, HbonTime_1c:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_1==1 &
         booklabhb>=7 & booklabhb<11,HbonTime_1c:=FALSE ]


# Hbontime_2
t2reboot[,HbonTime_2a:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_2==1, HbonTime_2a:=FALSE]

t2reboot[, HbonTime_2b:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_sev_24_28==T, HbonTime_2b:=FALSE]

t2reboot[,HbonTime_2c:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_mild_mod_24_28==T, HbonTime_2c:=FALSE]

# 29-34 weeks
# Hbontime_3
t2reboot[, HbonTime_3a:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_3==1 & 
         (!is.na(booklabhb)), HbonTime_3a:=FALSE]

t2reboot[, HbonTime_3b:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_3==1 &
         (booklabhb<7 & booklabhb>2), HbonTime_3b:=FALSE]

t2reboot[, HbonTime_3c:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_3==1 &
         (booklabhb<11 & booklabhb>=7), HbonTime_3c:=FALSE]

t2reboot[, HbonTime_4a:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_4==1, HbonTime_4a:=FALSE]

t2reboot[, HbonTime_4b:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_sev_35_37==T,HbonTime_4b:=FALSE]

t2reboot[, HbonTime_4c:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_mild_mod_35_37==T, HbonTime_4c:=FALSE]

t2reboot[, HbonTime_5:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_5==1, HbonTime_5:=FALSE]

t2reboot[, HbonTime_6:= as.logical(NA)]
t2reboot[Opportunity_anemia_screening_6==1, HbonTime_6:=FALSE]



#hb on time 1, 2, 3, vars
#Screen at bookings before 24 weeks??
#check booklabhb values if normal etc

# booked before 24 weeks
t2reboot[HbonTime_1a==F & booklabhb>=11 & 
         booklabhb<=18, HbonTime_1a:=TRUE]
xtabs(~t2reboot$HbonTime_1a, addNA=T)

t2reboot[HbonTime_1b==F & 
         manhbsev==T,HbonTime_1b:=TRUE]
xtabs(~t2reboot$HbonTime_1b, addNA=T)


t2reboot[HbonTime_1c==F & 
         (TrialOne_manhb_mildmodhbret_00_00==T|
            TrialOne_manhb_mildmodhbret_01_01==T|
            TrialOne_manhb_mildmodhbret_02_02==T|
            TrialOne_manhb_mildmodhbret_03_03==T|
            TrialOne_manhb_mildmodhbret_04_04==T|
            TrialOne_manhb_mildmodhbret_05_05==T|
            TrialOne_manhb_mildmodhbret_06_06==T|
            TrialOne_manhb_mildmodhbret_07_07==T|
            TrialOne_manhb_mildmodhbret_08_08==T|
            TrialOne_manhb_mildmodhbret_09_09==T|
            TrialOne_manhb_mildmodhbret_10_10==T|
            TrialOne_manhb_mildmodhbret_11_11==T|
            TrialOne_manhb_mildmodhbret_12_12==T|
            TrialOne_manhb_mildmodhbret_13_13==T|
            TrialOne_manhb_mildmodhbret_14_14==T|
            TrialOne_manhb_mildmodhbret_15_15==T|
            TrialOne_manhb_mildmodhbret_16_16==T|
            TrialOne_manhb_mildmodhbret_17_17==T|
            TrialOne_manhb_mildmodhbret_18_18==T|
            TrialOne_manhb_mildmodhbret_19_19==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T|
            TrialOne_manhb_mildmodhbret_23_23==T),HbonTime_1c:=TRUE]

xtabs(~t2reboot$HbonTime_1c, addNA=T)

#24-28 screenings
t2reboot[HbonTime_2a==F & 
         TrialOne_labhb_normal_24_28==T, HbonTime_2a:=TRUE]

t2reboot[HbonTime_2b==F & 
         TrialOne_manhb_24_24==T|
         TrialOne_manhb_25_25==T|
         TrialOne_manhb_26_26==T|
         TrialOne_manhb_27_27==T|
         TrialOne_manhb_28_28==T, HbonTime_2b:=TRUE]

t2reboot[HbonTime_2c==F & 
         TrialOne_manhb_mildmodhbret_24_24==T|
         TrialOne_manhb_mildmodhbret_25_25==T|
         TrialOne_manhb_mildmodhbret_26_26==T|
         TrialOne_manhb_mildmodhbret_27_27==T|
         TrialOne_manhb_mildmodhbret_28_28==T, HbonTime_2c:=TRUE]

#booked 29-30, 31-33, 34
t2reboot[HbonTime_3a==F & Opportunity_anemia_screening_3==1 &
         (booklabhb<=18 & booklabhb>11), HbonTime_3a:=TRUE]


t2reboot[HbonTime_3c==1 & 
         (TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T|
            TrialOne_manhb_mildmodhbret_31_31==T|
            TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T), 
       HbonTime_3c:=TRUE]

t2reboot[HbonTime_3b==F & 
         (TrialOne_manhb_29_29==T|
            TrialOne_manhb_30_30==T|
            TrialOne_manhb_31_31==T|
            TrialOne_manhb_32_32==T|
            TrialOne_manhb_33_33==T|
            TrialOne_manhb_34_34==T), 
       HbonTime_3b:=TRUE]


# 35-37 screenings
t2reboot[HbonTime_4a==F & 
         TrialOne_labhb_normal_35_37==T, HbonTime_4a:=TRUE]

t2reboot[HbonTime_4b==F & 
         TrialOne_manhb_35_35==T|
         TrialOne_manhb_36_36==T|
         TrialOne_manhb_37_37==T, HbonTime_4b:=TRUE]

t2reboot[HbonTime_4c==F &
         TrialOne_manhb_mildmodhbret_35_35==T|
         TrialOne_manhb_mildmodhbret_36_36==T|
         TrialOne_manhb_mildmodhbret_37_37==T, HbonTime_4c:=TRUE]

# severe anemia outside of time windows
t2reboot[HbonTime_5==F & 
         (TrialOne_manhb_00_00==T|
            TrialOne_manhb_01_01==T|
            TrialOne_manhb_02_02==T|
            TrialOne_manhb_03_03==T|
            TrialOne_manhb_04_04==T|
            TrialOne_manhb_05_05==T|
            TrialOne_manhb_06_06==T|
            TrialOne_manhb_07_07==T|
            TrialOne_manhb_08_08==T|
            TrialOne_manhb_09_09==T|
            TrialOne_manhb_10_10==T|
            TrialOne_manhb_11_11==T|
            TrialOne_manhb_12_12==T|
            TrialOne_manhb_13_13==T|
            TrialOne_manhb_14_14==T|
            TrialOne_manhb_15_15==T|
            TrialOne_manhb_16_16==T|
            TrialOne_manhb_17_17==T|
            TrialOne_manhb_18_18==T|
            TrialOne_manhb_19_19==T|
            TrialOne_manhb_20_20==T|
            TrialOne_manhb_21_21==T|
            TrialOne_manhb_22_22==T|
            TrialOne_manhb_23_23==T|
            TrialOne_manhb_29_29==T|
            TrialOne_manhb_30_30==T|
            TrialOne_manhb_31_31==T|
            TrialOne_manhb_32_32==T|
            TrialOne_manhb_33_33==T|
            TrialOne_manhb_34_34==T),HbonTime_5:=TRUE]

#mild/mod anem retest
t2reboot[HbonTime_6==F &
         (TrialOne_manhb_mildmodhbret_00_00==T|
            TrialOne_manhb_mildmodhbret_01_01==T|
            TrialOne_manhb_mildmodhbret_02_02==T|
            TrialOne_manhb_mildmodhbret_03_03==T|
            TrialOne_manhb_mildmodhbret_04_04==T|
            TrialOne_manhb_mildmodhbret_05_05==T|
            TrialOne_manhb_mildmodhbret_06_06==T|
            TrialOne_manhb_mildmodhbret_07_07==T|
            TrialOne_manhb_mildmodhbret_08_08==T|
            TrialOne_manhb_mildmodhbret_09_09==T|
            TrialOne_manhb_mildmodhbret_10_10==T|
            TrialOne_manhb_mildmodhbret_11_11==T|
            TrialOne_manhb_mildmodhbret_12_12==T|
            TrialOne_manhb_mildmodhbret_13_13==T|
            TrialOne_manhb_mildmodhbret_14_14==T|
            TrialOne_manhb_mildmodhbret_15_15==T|
            TrialOne_manhb_mildmodhbret_16_16==T|
            TrialOne_manhb_mildmodhbret_17_17==T|
            TrialOne_manhb_mildmodhbret_18_18==T|
            TrialOne_manhb_mildmodhbret_19_19==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T|
            TrialOne_manhb_mildmodhbret_23_23==T|
            TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T|
            TrialOne_manhb_mildmodhbret_31_31==T|
            TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T),
       HbonTime_6:=TRUE]


prelimHB <- t2reboot[,.(N=.N,
                      Opportun_1=sum(Opportunity_anemia_screening_1, na.rm=T),
                      Success_1a=sum(HbonTime_1a, na.rm=T),
                      Success_1aFalse=sum(HbonTime_1a==FALSE, na.rm=T),
                      Success_1b=sum(HbonTime_1b, na.rm=T),
                      Success_1bFalse=sum(HbonTime_1b==FALSE, na.rm=T),
                      Success_1c=sum(HbonTime_1c, na.rm=T),
                      Success_1cFalse=sum(HbonTime_1c==FALSE, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2a=sum(HbonTime_2a, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2b=sum(HbonTime_2b, na.rm=T),
                      Success_2bFalse=sum(HbonTime_2b==F, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2c=sum(HbonTime_2c, na.rm=T),
                      Success_2cFalse=sum(HbonTime_2c==F, na.rm=T),
                      Opportun_3=sum(Opportunity_anemia_screening_3, na.rm=T),
                      Success_3a=sum(HbonTime_3a, na.rm=T),
                      Success_3b=sum(HbonTime_3b, na.rm=T),
                      Success_3bFales=sum(HbonTime_3b==FALSE, na.rm=T),
                      Success_3c=sum(HbonTime_3c, na.rm=T),
                      Sucess_3cFalse=sum(HbonTime_3c==F, na.rm=T),
                      Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                      Success_4a=sum(HbonTime_4a, na.rm=T),
                      Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                      Success_4b=sum(HbonTime_4b, na.rm=T),
                      Screening4bF=sum(HbonTime_4b==F, na.rm=T),
                      Success_4c=sum(HbonTime_4c, na.rm=T),
                      Screening4cF=sum(HbonTime_4c==F, na.rm=T),
                      Opportun_5=sum(Opportunity_anemia_screening_5, na.rm=T),
                      Success_5=sum(HbonTime_5, na.rm=T),
                      success_5F=sum(HbonTime_5==F),
                      Opportun_6=sum(Opportunity_anemia_screening_6, na.rm=T),
                      Success_6=sum(HbonTime_6, na.rm=T),
                      success_6F=sum(HbonTime_6==F))]

openxlsx::write.xlsx(prelimHB,file.path(FOLDER_DATA_RESULTS,
                                        "T2",
                                        sprintf("%s_T2_recruitment_prelim_Hb.xlsx",
                                                lubridate::today()))) 



########## Attendance ########## 
# making vars
t2reboot[,refHRhosp:= FALSE]
t2reboot[(TrialOne_manRef_HR_00_00==T|
          TrialOne_manRef_HR_01_01==T|
          TrialOne_manRef_HR_02_02==T|
          TrialOne_manRef_HR_03_03==T|
          TrialOne_manRef_HR_04_04==T|
          TrialOne_manRef_HR_05_05==T|
          TrialOne_manRef_HR_06_06==T|
          TrialOne_manRef_HR_07_07==T|
          TrialOne_manRef_HR_08_08==T|
          TrialOne_manRef_HR_09_09==T|
          TrialOne_manRef_HR_10_10==T|
          TrialOne_manRef_HR_11_11==T|
          TrialOne_manRef_HR_12_12==T|
          TrialOne_manRef_HR_13_13==T|
          TrialOne_manRef_HR_14_14==T)|
         (TrialOne_manRef_Hosp_00_00==T|
            TrialOne_manRef_Hosp_01_01==T|
            TrialOne_manRef_Hosp_02_02==T|
            TrialOne_manRef_Hosp_03_03==T|
            TrialOne_manRef_Hosp_04_04==T|
            TrialOne_manRef_Hosp_05_05==T|
            TrialOne_manRef_Hosp_06_06==T|
            TrialOne_manRef_Hosp_07_07==T|
            TrialOne_manRef_Hosp_08_08==T|
            TrialOne_manRef_Hosp_09_09==T|
            TrialOne_manRef_Hosp_10_10==T|
            TrialOne_manRef_Hosp_11_11==T|
            TrialOne_manRef_Hosp_12_12==T|
            TrialOne_manRef_Hosp_13_13==T|
            TrialOne_manRef_Hosp_14_14==T),refHRhosp:=TRUE]

xtabs(~t2reboot$refHRhosp, addNA=T)

## Define Opportunities

# oppt 16 week visit
t2reboot[,Opp_1:= as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(0,104]"),Opp_1:=1]
t2reboot[bookgestagedays_cats %in% c("(0,104]") &
         refHRhosp==T,Opp_1:=0]
xtabs(~t2reboot$Opp_1, addNA=T)



# oppt 18-22 visit
t2reboot[,Opp_2:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(104,125]")| Opp_1==1, Opp_2:=1]

xtabs(~t2reboot$Opp_2, addNA=T)

#removing opportunities
t2reboot[Opp_2==1 & 
         (TrialOne_manRef_HR_15_15==T|TrialOne_manRef_Hosp_15_15==T)|
         (TrialOne_manRef_HR_16_16==T|TrialOne_manRef_Hosp_16_16==T)|
         (TrialOne_manRef_HR_17_17==T|TrialOne_manRef_Hosp_17_17==T),
       Opp_2:=Opp_2-1]

xtabs(~t2reboot$Opp_2, addNA=T)


# 24-28 week visit
t2reboot[,Opp_3:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(125,160]",
                                   "(160,167]") | Opp_2==1, Opp_3:=1]

xtabs(~t2reboot$Opp_3, addNA=T)

# removing opportunities
t2reboot[Opp_3==1 & ((TrialOne_manRef_HR_18_18==T|TrialOne_manRef_Hosp_18_18==T)|
                     (TrialOne_manRef_HR_19_19==T|TrialOne_manRef_Hosp_19_19==T)|
                     (TrialOne_manRef_HR_20_20==T|TrialOne_manRef_Hosp_20_20==T)|
                     (TrialOne_manRef_HR_21_21==T |TrialOne_manRef_Hosp_21_21==T)|
                     (TrialOne_manRef_HR_22_22==T|TrialOne_manRef_Hosp_22_22==T)|
                     (TrialOne_manRef_HR_23_23==T|TrialOne_manRef_Hosp_23_23==T)), 
       Opp_3:=Opp_3-1]
xtabs(~t2reboot$Opp_3, addNA=T)



# 31-33 week visit
t2reboot[,Opp_4:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(160,167]",
                                   "(167,202]",
                                   "(202,216]")|Opp_3== 1, Opp_4:=1]

xtabs(~t2reboot$Opp_4, addNA=T)

# removing opportunities 
t2reboot[Opp_4==1 &
         ((TrialOne_manRef_HR_24_24==T|TrialOne_manRef_Hosp_24_24==T)|
            (TrialOne_manRef_HR_25_25==T|TrialOne_manRef_Hosp_25_25==T)|
            (TrialOne_manRef_HR_26_26==T|TrialOne_manRef_Hosp_26_26==T)|
            (TrialOne_manRef_HR_27_27==T|TrialOne_manRef_Hosp_27_27==T)|
            (TrialOne_manRef_HR_28_28==T|TrialOne_manRef_Hosp_28_28==T)|
            (TrialOne_manRef_HR_29_29==T|TrialOne_manRef_Hosp_29_29==T)|
            (TrialOne_manRef_HR_30_30==T|TrialOne_manRef_Hosp_30_30==T)), 
       Opp_4:=Opp_4-1]

xtabs(~t2reboot$Opp_4, addNA=T)

# 35-37 week visit
t2reboot[,Opp_5:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(216,237]",
                                   "(237,244]") | Opp_4==1, Opp_5:=1]
xtabs(~t2reboot$Opp_5, addNA=T)

t2reboot[Opp_5==1 &
         ((TrialOne_manRef_HR_31_31==T|TrialOne_manRef_Hosp_31_31==T)|
            (TrialOne_manRef_HR_32_32==T|TrialOne_manRef_Hosp_32_32==T)|
            (TrialOne_manRef_HR_33_33==T|TrialOne_manRef_Hosp_33_33==T)|
            (TrialOne_manRef_HR_34_34==T|TrialOne_manRef_Hosp_34_34==T)), 
       Opp_5:=Opp_5-1]
xtabs(~t2reboot$Opp_5, addNA=T)




################ successes ##########
# 15-17 week visit
t2reboot[,Succ_1:=as.logical(NA)]
t2reboot[Opp_1==1, Succ_1:=FALSE]
t2reboot[Succ_1==F & 
         TrialOne_anvisitnew_15_17==T, Succ_1:=TRUE]

xtabs(~t2reboot$Succ_1, addNA=T)

# 18-22 week visit
t2reboot[,Succ_2:=as.logical(NA)]
t2reboot[Opp_2==1, Succ_2:=FALSE]
t2reboot[Succ_2==F & TrialOne_anvisitnew_18_22==T, Succ_2:=TRUE]

xtabs(~t2reboot$Succ_2, addNA=T)

# 24-28 week visit
t2reboot[,Succ_3:=as.logical(NA)]
t2reboot[Opp_3==1, Succ_3:=as.logical(FALSE)]
t2reboot[Succ_3==F & TrialOne_anvisitnew_24_28==T, Succ_3:=TRUE]

xtabs(~t2reboot$Succ_3, addNA=T)

# 31-33 week visit
t2reboot[,Succ_4:=as.logical(NA)]
t2reboot[Opp_4==1, Succ_4:=FALSE]
t2reboot[Succ_4==F & TrialOne_anvisitnew_31_33==T, Succ_4:=TRUE]

xtabs(~t2reboot$Succ_4, addNA=T)

# 35-37
t2reboot[,Succ_5:=as.logical(NA)]
t2reboot[Opp_5==1, Succ_5:=FALSE]
t2reboot[Succ_5==F & TrialOne_anvisitnew_35_37==T, Succ_5:=TRUE]

xtabs(~t2reboot$Succ_5, addNA=T)

prelimAtt <- t2reboot[,.(N=.N,
                       bookedb414=sum(bookgestagedays_cats=="(0,104]", na.rm = T),
                       ANC15_17Opps=sum(Opp_1,na.rm=T),
                       ANC15_17=sum(Succ_1, na.rm=T),
                       ANC15_17FALSE=sum(Succ_1==F, na.rm=T),
                       booked1515=sum(bookgestagedays_cats=="(104,125]", na.rm = T),
                       ANC18_22Opps=sum(Opp_2, na.rm=T),
                       ANC18_22=sum(Succ_2, na.rm=T),
                       ANC18_22FALSE=sum(Succ_2==F, na.rm=T),
                       booked1822=sum(bookgestagedays_cats=="(125,160]", na.rm = T),
                       booked2323=sum(bookgestagedays_cats=="(160,167]", na.rm = T),
                       ANC2428Opps=sum(!is.na(Opp_3), na.rm=T),
                       ANC24_28TRUE=sum(Succ_3, na.rm=T),
                       ANC24_28FALSE=sum(Succ_3==F, na.rm=T),
                       booked2428=sum(bookgestagedays_cats=="(167,202]", na.rm = T),
                       booked2930=sum(bookgestagedays_cats=="(202,216]", na.rm = T),
                       ANC31_33Opps=sum(Opp_4, na.rm=T),
                       ANC31_33=sum(Succ_4, na.rm=T),
                       ANC31_33FALSE=sum(Succ_4==F, na.rm=T),
                       Booked31_33=sum(bookgestagedays_cats=="(216,237]", na.rm = T),
                       Booked34_34=sum(bookgestagedays_cats=="(237,244]", na.rm = T),
                       ANC3537Opps=sum(Opp_5, na.rm=T),
                       ANC3537=sum(Succ_5, na.rm=T),
                       Booked35_37=sum(bookgestagedays_cats=="(244,265]",
                                       na.rm = T))]

openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS,
                                         "T2",
                                         sprintf("%s_T2_recruit_prelim_Attendance.xlsx",
                                                 lubridate::today()))) 


########## GDM ########## 

###Redefining opportinites
t2reboot[,Opportunity_GDM_screening_1:=as.numeric(NA)]
t2reboot[,Opportunity_GDM_screening_2:=as.numeric(NA)]
t2reboot[,Opportunity_GDM_screening_3:=as.numeric(NA)]
t2reboot[,Opportunity_GDM_screening_4:=as.numeric(NA)]
#t2reboot[,Opportunity_GDM_Screening_5:=as.numeric(NA)]

# before 24
t2reboot[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_1:=1]
#24-28
t2reboot[bookgestagedays_cats %in% c("(167,202]")|
         TrialOne_anvisitnew_24_28==T,Opportunity_GDM_screening_2:=1]
# after 28
t2reboot[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]", 
                                   "(237,244]",
                                   "(244,265]"), Opportunity_GDM_screening_3:=1]

# high rbs anywhere outside of the 24-28
t2reboot[(TrialOne_labbloodglu_high_00_14==T|
          TrialOne_labbloodglu_high_15_17==T|
          TrialOne_labbloodglu_high_18_22==T|
          TrialOne_labbloodglu_high_23_23==T|
          TrialOne_labbloodglu_high_29_30==T|
          TrialOne_labbloodglu_high_31_33==T|
          TrialOne_labbloodglu_high_34_34==T|
          TrialOne_labbloodglu_high_35_37==T), Opportunity_GDM_screening_4:=1]

xtabs(~t2reboot$Opportunity_GDM_screening_1, addNA=T)
xtabs(~t2reboot$Opportunity_GDM_screening_2, addNA=T)
xtabs(~t2reboot$Opportunity_GDM_screening_3, addNA=T)
xtabs(~t2reboot$Opportunity_GDM_screening_4, addNA=T)




## Remove opportunities for people who were referred to HR or Hosp
#refHRHospmanRBG_1 rename to RefHr
t2reboot[,RefHr:=as.logical(NA)]
t2reboot[Opportunity_anemia_screening_1==1, RefHr:=FALSE]
t2reboot[(TrialOne_manRef_HR_00_00==T|
          TrialOne_manRef_HR_01_01==T|
          TrialOne_manRef_HR_02_02==T|
          TrialOne_manRef_HR_03_03==T|
          TrialOne_manRef_HR_04_04==T|
          TrialOne_manRef_HR_05_05==T|
          TrialOne_manRef_HR_06_06==T|
          TrialOne_manRef_HR_07_07==T|
          TrialOne_manRef_HR_08_08==T|
          TrialOne_manRef_HR_09_09==T|
          TrialOne_manRef_HR_10_10==T|
          TrialOne_manRef_HR_11_11==T|
          TrialOne_manRef_HR_12_12==T|
          TrialOne_manRef_HR_13_13==T|
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
       RefHr:=TRUE]
xtabs(~t2reboot$RefHr, addNA=T)

#refHrHosp_2 rename to refHr_2
t2reboot[,refHr_2:=(
  TrialOne_refHR_29_29==T|
    TrialOne_refHR_30_30==T|
    TrialOne_refHR_31_31==T|
    TrialOne_refHR_32_32==T|
    TrialOne_refHR_33_33==T|
    TrialOne_refHR_34_34==T|
    TrialOne_refHR_35_35==T|
    TrialOne_refHR_36_36==T|
    TrialOne_refHR_35_37==T)]


t2reboot[Opportunity_GDM_screening_2==1 &
         (TrialOne_anvisitnew_24_24 & 
            (RefHr==T))|
         (TrialOne_anvisitnew_25_25 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27 & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28 & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T)), 
       Opportunity_GDM_screening_2:=Opportunity_GDM_screening_2-1]

# checks
xtabs(~t2reboot$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
t2reboot[,screenb424:=as.logical(NA)]
t2reboot[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
t2reboot[screenb424==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenb424:=T]
xtabs(~t2reboot$screenb424, addNA=T)

scrb424 <- t2reboot[,.(A=sum(ident_dhis2_control==T),
                     B=sum(ident_dhis2_control==F)),
                  keyby=.(screenb424)]

##Defining Successes 
t2reboot[,GDMscreeningontime_1A:=as.logical(NA)]
t2reboot[,GDMscreeningontime_1B:=as.logical(NA)]
t2reboot[,GDMscreeningontime_1C:=as.logical(NA)]
t2reboot[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
t2reboot[screenb424==T, 
       GDMscreeningontime_1:=TRUE]

xtabs(~t2reboot$GDMscreeningontime_1, addNA=T)


t2reboot[,GDMscreeningontime_1A:=as.logical(NA)]
t2reboot[Opportunity_GDM_screening_1==1 & 
         booklaburglu=="NEG", 
       GDMscreeningontime_1A:=TRUE]

t2reboot[,GDMscreeningontime_1B:=as.logical(NA)]
t2reboot[Opportunity_GDM_screening_1==1 &
         booklaburglu=="POS" & 
         !is.na(booklabbloodglu), GDMscreeningontime_1B:=TRUE]

##### Need to add: and referred for 1C!!!! ##### 
t2reboot[,GDMscreeningontime_1C:=as.logical(NA)]
t2reboot[booklabbloodglu_high==T &
         !is.na(booklabbloodglu), GDMscreeningontime_1C:=TRUE]



#24-28 weeks
t2reboot[,GDMscreeningontime_2:=as.logical(NA)]
t2reboot[Opportunity_GDM_screening_2==1 &
         (TrialOne_labbloodglu_exists_24_24==F &
            TrialOne_labbloodglu_exists_25_25==F &
            TrialOne_labbloodglu_exists_26_26==F &
            TrialOne_labbloodglu_exists_27_27==F &
            TrialOne_labbloodglu_exists_28_28==F) &
         (TrialOne_labfastbloodglu_exists_24_24==F &
            TrialOne_labfastbloodglu_exists_25_25==F &
            TrialOne_labfastbloodglu_exists_26_26==F &
            TrialOne_labfastbloodglu_exists_27_27==F &
            TrialOne_labfastbloodglu_exists_28_28==F), GDMscreeningontime_2:=F]
t2reboot[Opportunity_GDM_screening_2==1 & 
         (TrialOne_labbloodglu_exists_24_24==T|
            TrialOne_labbloodglu_exists_25_25==T|
            TrialOne_labbloodglu_exists_26_26==T|
            TrialOne_labbloodglu_exists_27_27==T|
            TrialOne_labbloodglu_exists_28_28==T) &
         (TrialOne_labbloodglu_high_24_24==F|
            TrialOne_labbloodglu_high_25_25==F|
            TrialOne_labbloodglu_high_26_26==F|
            TrialOne_labbloodglu_high_27_27==F|
            TrialOne_labbloodglu_high_28_28==F)|
         (TrialOne_labfastbloodglu_exists_24_24==T|
            TrialOne_labfastbloodglu_exists_25_25==T|
            TrialOne_labfastbloodglu_exists_26_26==T|
            TrialOne_labfastbloodglu_exists_27_27==T|
            TrialOne_labfastbloodglu_exists_28_28==T),GDMscreeningontime_2:=TRUE]
xtabs(~t2reboot$GDMscreeningontime_2, addNA=T)


#Screening after 28 weeks: Creating one var for 3 possibilities
t2reboot[,screenafter28:=as.logical(NA)]
t2reboot[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]"),
       screenafter28:=F]
t2reboot[screenafter28==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~t2reboot$screenafter28, addNA=T)

##Defining Success
t2reboot[,GDMscreeningontime_3:=as.logical(NA)]
t2reboot[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
t2reboot[screenafter28==T,GDMscreeningontime_3:=TRUE]
xtabs(~t2reboot$GDMscreeningontime_3, addNA=T)

#management fo high RBG outside of time windows
t2reboot[, GDMscreeningontime_4:=as.logical(NA)]
t2reboot[Opportunity_GDM_screening_4==1, GDMscreeningontime_4:= FALSE]
t2reboot[GDMscreeningontime_4==F & 
         (RefHr==T|refHr_2==T),GDMscreeningontime_4:=TRUE]


prelimGDM <- t2reboot[,.(N=.N,
                       Opportun_1=sum(Opportunity_GDM_screening_1==T, na.rm=T),
                       Success_1A=sum(GDMscreeningontime_1A==T, na.rm=T),
                       Success_1B=sum(GDMscreeningontime_1B==T, na.rm=T),
                       Success_1C=sum(GDMscreeningontime_1C==T, na.rm=T),
                       Screenb424=sum(screenb424==T, na.rm=T),
                       Screenb424False=sum(screenb424==F, na.rm=T),
                       Opportun_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2=sum(GDMscreeningontime_2==T, na.rm=T),
                       Opportun_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       screenafter28=sum(screenafter28==T, na.rm=T),
                       screenafter28False=sum(screenafter28==F, na.rm=T),
                       screenbtwn=sum(GDMscreeningontime_4==T, na.rm=T),
                       screenbtwnFalse=sum(GDMscreeningontime_4==F, na.rm=T),
                       Opportun_4=sum(Opportunity_GDM_screening_4==T, na.rm=T),
                       Succ_4=sum(GDMscreeningontime_4, na.rm=T))]



openxlsx::write.xlsx(prelimGDM,file.path(FOLDER_DATA_RESULTS,
                                         "T2",
                                         sprintf("%s_T2_recruitment_prelim_GDM.xlsx",
                                                 lubridate::today()))) 


############## HTN ############## 

# making vars
# refHRhosp variable made in attendance outcome

## Define Opportunities

# before 16 weeks
t2reboot[,bp_1a:= as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(0,104]"),
         bp_1a:=1]

xtabs(~t2reboot$bp_1a, addNA=T)


# oppt 16 week visit
t2reboot[,bp_1:= as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(104,125]")| TrialOne_anvisitnew_15_17==T,
         bp_1:=1]
t2reboot[bookgestagedays_cats %in% c("(104,125]") &
           refHRhosp==T,bp_1:=0]
xtabs(~t2reboot$bp_1, addNA=T)


# oppt 18-22 visit
t2reboot[,bp_2:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(125,160]")|
           TrialOne_anvisitnew_18_22==T, bp_2:=1]

xtabs(~t2reboot$bp_2, addNA=T)

#removing opportunities
t2reboot[bp_2==1 & 
           (TrialOne_manRef_HR_15_15==T|TrialOne_manRef_Hosp_15_15==T)|
           (TrialOne_manRef_HR_16_16==T|TrialOne_manRef_Hosp_16_16==T)|
           (TrialOne_manRef_HR_17_17==T|TrialOne_manRef_Hosp_17_17==T),
         bp_2:=bp_2-1]

xtabs(~t2reboot$bp_2, addNA=T)


# 24-28 week visit
t2reboot[,bp_3:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(160,167]",
                                     "(167,202]") |
              TrialOne_anvisitnew_24_28==T, bp_3:=1]

xtabs(~t2reboot$bp_3, addNA=T)

# removing opportunities
t2reboot[bp_3==1 & ((TrialOne_manRef_HR_18_18==T|TrialOne_manRef_Hosp_18_18==T)|
                       (TrialOne_manRef_HR_19_19==T|TrialOne_manRef_Hosp_19_19==T)|
                       (TrialOne_manRef_HR_20_20==T|TrialOne_manRef_Hosp_20_20==T)|
                       (TrialOne_manRef_HR_21_21==T |TrialOne_manRef_Hosp_21_21==T)|
                       (TrialOne_manRef_HR_22_22==T|TrialOne_manRef_Hosp_22_22==T)|
                       (TrialOne_manRef_HR_23_23==T|TrialOne_manRef_Hosp_23_23==T)), 
         bp_3:=bp_3-1]
xtabs(~t2reboot$bp_3, addNA=T)



# 31-33 week visit
t2reboot[,bp_4:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(202,216]",
                                     "(216,237]")|
                              TrialOne_anvisitnew_31_33==T, bp_4:=1]

xtabs(~t2reboot$bp_4, addNA=T)

# removing opportunities 
t2reboot[bp_4==1 &
           ((TrialOne_manRef_HR_24_24==T|TrialOne_manRef_Hosp_24_24==T)|
              (TrialOne_manRef_HR_25_25==T|TrialOne_manRef_Hosp_25_25==T)|
              (TrialOne_manRef_HR_26_26==T|TrialOne_manRef_Hosp_26_26==T)|
              (TrialOne_manRef_HR_27_27==T|TrialOne_manRef_Hosp_27_27==T)|
              (TrialOne_manRef_HR_28_28==T|TrialOne_manRef_Hosp_28_28==T)|
              (TrialOne_manRef_HR_29_29==T|TrialOne_manRef_Hosp_29_29==T)|
              (TrialOne_manRef_HR_30_30==T|TrialOne_manRef_Hosp_30_30==T)), 
         bp_4:=bp_4-1]

xtabs(~t2reboot$bp_4, addNA=T)

# 35-37 week visit
t2reboot[,bp_5:=as.numeric(NA)]
t2reboot[bookgestagedays_cats %in% c("(237,244]", "(244,265]") |
           TrialOne_anvisitnew_35_37, bp_5:=1]
xtabs(~t2reboot$bp_5, addNA=T)

t2reboot[bp_5==1 &
           ((TrialOne_manRef_HR_31_31==T|TrialOne_manRef_Hosp_31_31==T)|
              (TrialOne_manRef_HR_32_32==T|TrialOne_manRef_Hosp_32_32==T)|
              (TrialOne_manRef_HR_33_33==T|TrialOne_manRef_Hosp_33_33==T)|
              (TrialOne_manRef_HR_34_34==T|TrialOne_manRef_Hosp_34_34==T)), 
         bp_5:=bp_5-1]
xtabs(~t2reboot$bp_5, addNA=T)




################ successes ##########

# before 15 weeks
# 15-17 week visit
t2reboot[,Succ_1a:=as.logical(NA)]
t2reboot[bp_1a==1, Succ_1a:=FALSE]
t2reboot[Succ_1a==F & 
           TrialOne_anbpsyst_present_00_14==T, Succ_1a:=TRUE]

xtabs(~t2reboot$Succ_1a, addNA=T)
xtabs(~t2reboot$bp_1a, addNA=T)


# 15-17 week visit
t2reboot[,Succ_1:=as.logical(NA)]
t2reboot[bp_1==1, Succ_1:=FALSE]
t2reboot[Succ_1==F & 
           TrialOne_anbpsyst_present_15_17==T, Succ_1:=TRUE]


xtabs(~t2reboot$bp_1, addNA=T)
xtabs(~t2reboot$Succ_1, addNA=T)

# 18-22 week visit
t2reboot[,Succ_2:=as.logical(NA)]
t2reboot[bp_2==1, Succ_2:=FALSE]
t2reboot[Succ_2==F & TrialOne_anbpsyst_present_18_22==T, Succ_2:=TRUE]

xtabs(~t2reboot$bp_2, addNA=T)
xtabs(~t2reboot$Succ_2, addNA=T)

# 24-28 week visit
t2reboot[,Succ_3:=as.logical(NA)]
t2reboot[bp_3==1, Succ_3:=as.logical(FALSE)]
t2reboot[Succ_3==F & TrialOne_anbpsyst_present_24_28==T, Succ_3:=TRUE]

xtabs(~t2reboot$bp_3, addNA=T)
xtabs(~t2reboot$Succ_3, addNA=T)

# 31-33 week visit
t2reboot[,Succ_4:=as.logical(NA)]
t2reboot[bp_4==1, Succ_4:=FALSE]
t2reboot[Succ_4==F & TrialOne_anbpsyst_present_31_33==T, Succ_4:=TRUE]

xtabs(~t2reboot$bp_4, addNA=T)
xtabs(~t2reboot$Succ_4, addNA=T)

# 35-37
t2reboot[,Succ_5:=as.logical(NA)]
t2reboot[bp_5==1, Succ_5:=FALSE]
t2reboot[Succ_5==F & TrialOne_anbpsyst_present_35_37==T, Succ_5:=TRUE]

xtabs(~t2reboot$bp_5, addNA=T)
xtabs(~t2reboot$Succ_5, addNA=T)


prelimHTN <- t2reboot[,.(N=.N,
                         Screenb415=sum(bp_1a==T, na.rm=T),
                         Success_1A=sum(Succ_1a==T, na.rm=T),
                         opport_15_17=sum(bp_1==T, na.rm=T),
                         Success_1=sum(Succ_1==T, na.rm=T),
                         opport_18_22=sum(bp_2==T, na.rm=T),
                         Success_2=sum(Succ_2==T, na.rm=T),
                         opport_24_28=sum(bp_3==T, na.rm=T),
                         Success_3=sum(Succ_3==T, na.rm=T),
                         opport_31_33=sum(bp_4==T, na.rm=T),
                         Success_4=sum(Succ_4==T, na.rm=T),
                         opport_35_37=sum(bp_5==T, na.rm=T),
                         Success_5=sum(Succ_5==T, na.rm=T))]



openxlsx::write.xlsx(prelimHTN,file.path(FOLDER_DATA_RESULTS,
                                         "T2",
                                         sprintf("%s_T2_recruitment_prelim_HTN.xlsx",
                                                 lubridate::today()))) 



