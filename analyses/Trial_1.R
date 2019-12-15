###Trial 1 Outcomes###

 # defining dataset
smallD <- smallD <- d[bookdate >= "2017-01-15"&
                        bookdate<="2017-09-15" &
                        ident_TRIAL_1==T,]
 # renaming arms
d[ident_dhis2_control==F, prettyExposure:="Trial Arm B"]
d[ident_dhis2_control==T, prettyExposure:="Trial Arm A"]

##making categories

  #booking
  #TO DO: check lower cut off
smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                       breaks=c(-500,104,119,
                                125,154,167, 196,
                                216,231,244,259),
                       include.lowest=T)]


#need to create a dummy gestational age variable

# MAKE BOOK VISIT FOR ANEMIA
smallD[,booklabhb:=as.numeric(NA)]
smallD[abs(labgestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]

smallD[,bookanexamsfh:=as.numeric(NA)]
smallD[abs(labgestagedays_1-bookgestagedays)<7,bookanexamsfh:=anexamsfh_1]


VisitVariables <- function(smallD,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="angestagedays" ){

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

days <- list(
  "00_14"=c(-500:104),
  "15_17"=c(105:119),
  "17_18"=c(120:125),
  "18_22"=c(126:154),
  "23_23"=c(155:167),
  "24_28"=c(168:196),
  "29_30"=c(197:210),
  "31_33"=c(217:231),
  "34_34"=c(232:244),
  "35_37"=c(245:259)
)

###ANC Visits####

VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="angestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="angestagedays")

###ANC BP SYT ####

# BP SYST Present
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_present",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=60,
  TruevaluesMax=170,
  gestagedaysVariable = "angestagedays")

# BP Diast Present
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpdiast_present",
  variableOfInterestPattern="anbpdiast",
  TruevaluesMin=40,
  TruevaluesMax=170,
  gestagedaysVariable = "angestagedays")

# BP Syst High
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_high",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "angestagedays")

# BP Syst MOd/Sev HTN
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anbpsyst_sevHTN",
  variableOfInterestPattern="anbpsyst",
  TruevaluesMin=140,
  TruevaluesMax=170,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "angestagedays")

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
  gestagedaysVariable = "labgestagedays")

# sev anemia
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_sev",
  variableOfInterestPattern="labhb",
  TruevaluesMin=1,
  TruevaluesMax=6.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labgestagedays")

# mild and moderate anemia
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labhb_anemia_mild_mod",
  variableOfInterestPattern="labhb",
  TruevaluesMin=7,
  TruevaluesMax=10.9,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labgestagedays")




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
  gestagedaysVariable = "labgestagedays")

#normal bloodglu values
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_exists",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labgestagedays")

# high blood glucose
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_high",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=140,
  TruevaluesMax=500,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labgestagedays")

# Lab FBS Normal 
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labgestagedays")


#### US visits ####
# Has US visit
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_exists",
  variableOfInterestPattern="usgestagedays",
  TruevaluesMin=10,
  TruevaluesMax=300,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usgestagedays")

#US AN SFH measurements
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anexamsfh_exists",
  variableOfInterestPattern="anexamsfh",
  TruevaluesMin=5,
  TruevaluesMax=44,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "angestagedays")

#US expected IUGR
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_iugrSuspected",
  variableOfInterestPattern="usiugr",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usgestagedays")

#US expected LGA
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_lgaSuspected",
  variableOfInterestPattern="uslga",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usgestagedays")

####Referrals ####
#Ref to HR
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHR",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHighRisk",
  gestagedaysVariable = "mangestagedays")

#Ref to Hosp
VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "mangestagedays")

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
                     Visits_17_18=sum(TrialOne_anvisitnew_17_18,na.rm=TRUE),
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
                      Screened_17_18=sum(TrialOne_anbpsyst_present_17_18,na.rm=TRUE),
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
                   screenedlabhb_17_18=sum(TrialOne_labhb_exists_17_18,na.rm=TRUE),
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
                  sev_anemia_17_18=sum(TrialOne_labhb_anemia_sev_17_18,na.rm=TRUE),
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
  mild_mod_anemia_17_18=sum(TrialOne_labhb_anemia_mild_mod_17_18,na.rm=TRUE),
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
  has_labbloodglu_17_18=sum(TrialOne_laburglu_exists_17_18,na.rm=TRUE),
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
 has_labbloodglu_17_18=sum(TrialOne_labbloodglu_exists_17_18,na.rm=TRUE),
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
  has_highbloodglu_17_18=sum(TrialOne_labbloodglu_high_17_18,na.rm=TRUE),
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
#US exists

us_exists<- smallD[,.(
  us_exists_15wks=sum(TrialOne_us_exists_00_14==T,na.rm=TRUE),
  has_us_15_17=sum(TrialOne_us_exists_15_17,na.rm=TRUE),
  has_us_17_18=sum(TrialOne_us_exists_17_18,na.rm=TRUE),
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
  iugrsuspected_17_18=sum(TrialOne_us_iugrSuspected_17_18,na.rm=TRUE),
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
  lgaSuspected_17_18=sum(TrialOne_anexamsfh_exists_17_18,na.rm=TRUE),
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
  anexamsfh_exists_17_18=sum(TrialOne_anexamsfh_exists_17_18,na.rm=TRUE),
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
  refHR_17_18=sum(TrialOne_refHR_17_18,na.rm=TRUE),
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
  refHosp_17_18=sum(TrialOne_refHosp_17_18,na.rm=TRUE),
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

##visits by 
#TO DO: managements and laburglu in a different code , referral to HR or refhosp 
#has to be onewith in one week after a lab test 

#calculate gA based on how system calculates it. 
#check code to make sure everything is recalculated
#any us dates prior to lmp dates should be reverted backwards from that date

lmpstatusKnown<-smallD[,.(ArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                          ArmB=sum(ident_dhis2_control==F, na.rm=TRUE)), 
                       keyby=.(booklmpknown)]


####### HBO #######

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
smallD[,sevHTNatBirth:=FALSE]
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
smallD[,SevOrModHbatBirth:=FALSE]
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




######### Indication for CS ##########

#cleaning
unique(smallD$merged_indic_csection)
smallD[,has_malpresentation:=FALSE]

#creating lower cases in all of them
smallD[,merged_indic_csection_lowercase:= stringr::str_to_lower(merged_indicationforcsection)]

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


#in strings that we want we use the %in%
#smallD[merged_indic_csection %in% c("Breech","TWINS , BOTH BREECH IN LABOUR"#,"breach", "transverse"), var:=true


