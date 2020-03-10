
#import data set from process outcomes

#smallD <- fread("C:/data processing/data_clean/Trial_1_Outcomes/2020-03-07_GDM.xlsx")

smallD <- smallD

########## GDM ########## 

###Redefining opportinites
smallD[,Opportunity_GDM_screening_1:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_2:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_3:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_4:=as.numeric(NA)]

# before 24
smallD[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_1:=1]
#24-28
smallD[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]",
                                   "(167,202]") |
              TrialOne_anvisitnew_24_28==T,Opportunity_GDM_screening_2:=1]
# after 28
smallD[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]", 
                                   "(237,244]",
                                   "(244,265]"), Opportunity_GDM_screening_3:=1]

# high rbs anywhere outside of the 24-28
smallD[(booklabbloodglu_high==F | is.na(booklabbloodglu_high)) & 
         (TrialOne_labbloodglu_high_00_14==T|
          TrialOne_labbloodglu_high_15_17==T|
          TrialOne_labbloodglu_high_18_22==T|
          TrialOne_labbloodglu_high_23_23==T|
          TrialOne_labbloodglu_high_29_30==T|
          TrialOne_labbloodglu_high_31_33==T|
          TrialOne_labbloodglu_high_34_34==T|
          TrialOne_labbloodglu_high_35_37==T), Opportunity_GDM_screening_4:=1]

xtabs(~smallD$Opportunity_GDM_screening_1, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_2, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_3, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_4, addNA=T)




## Remove opportunities for people who were referred to HR or Hosp
smallD[,refHRmanRBG_1:=(
  TrialOne_manRBGHigh_HR_01_01==T|
    TrialOne_manRBGHigh_HR_02_02==T|
    TrialOne_manRBGHigh_HR_03_03==T|
    TrialOne_manRBGHigh_HR_04_04==T|
    TrialOne_manRBGHigh_HR_05_05==T|
    TrialOne_manRBGHigh_HR_06_06==T|
    TrialOne_manRBGHigh_HR_07_07==T|
    TrialOne_manRBGHigh_HR_08_08==T|
    TrialOne_manRBGHigh_HR_09_09==T|
    TrialOne_manRBGHigh_HR_10_10==T|
    TrialOne_manRBGHigh_HR_11_11==T|
    TrialOne_manRBGHigh_HR_12_12==T|
    TrialOne_manRBGHigh_HR_13_13==T|
    TrialOne_manRBGHigh_HR_14_14==T|
    TrialOne_manRBGHigh_HR_15_15==T|
    TrialOne_manRBGHigh_HR_16_16==T|
    TrialOne_manRBGHigh_HR_17_17==T|
    TrialOne_manRBGHigh_HR_18_18==T|
    TrialOne_manRBGHigh_HR_19_19==T|
    TrialOne_manRBGHigh_HR_20_20==T|
    TrialOne_manRBGHigh_HR_21_21==T|
    TrialOne_manRBGHigh_HR_22_22==T|
    TrialOne_manRBGHigh_HR_23_23==T)]

smallD[(TrialOne_anvisitnew_24_24 & refHRmanRBG_1==T)|
         (TrialOne_anvisitnew_25_25 & 
            (refHRmanRBG_1==T|
               TrialOne_manRBGHigh_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26 & 
            (refHRmanRBG_1==T|
               TrialOne_manRBGHigh_HR_24_24==T|
               TrialOne_manRBGHigh_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27 & 
            (refHRmanRBG_1==T|
               TrialOne_manRBGHigh_HR_24_24==T|
               TrialOne_manRBGHigh_HR_25_25==T|
               TrialOne_manRBGHigh_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28 & 
            (refHRmanRBG_1==T|
               TrialOne_manRBGHigh_HR_24_24==T|
               TrialOne_manRBGHigh_HR_25_25==T|
               TrialOne_manRBGHigh_HR_26_26==T|
               TrialOne_manRBGHigh_HR_27_27==T)),
       Opportunity_GDM_screening_2:=Opportunity_GDM_screening_2-1]

# checks
xtabs(~smallD$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
smallD[,screenb424:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
smallD[screenb424==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
        (!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenb424:=T]
xtabs(~smallD$screenb424, addNA=T)

scrb424 <- smallD[,.(A=sum(ident_dhis2_control==T),
                        B=sum(ident_dhis2_control==F)),
                     keyby=.(screenb4242)]

##Defining Successes 
smallD[,GDMscreeningontime_1:=as.logical(NA)]
smallD[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
smallD[screenb424==T, 
       GDMscreeningontime_1:=TRUE]

xtabs(~smallD$GDMscreeningontime_1, addNA=T)


#24-28 weeks
smallD[,GDMscreeningontime_2:=as.logical(NA)]
smallD[Opportunity_GDM_screening_2==1 &
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
smallD[Opportunity_GDM_screening_2==1 & 
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
xtabs(~smallD$GDMscreeningontime_2, addNA=T)
         

#Screening after 28 weeks: Creating one var for 3 possibilities
smallD[,screenafter28:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]"),
       screenafter28:=F]
smallD[screenafter28==F &
          (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~smallD$screenafter28, addNA=T)

##Defining Success
smallD[,GDMscreeningontime_3:=as.logical(NA)]
smallD[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
smallD[screenafter28==T,GDMscreeningontime_3:=TRUE]
xtabs(~smallD$GDMscreeningontime_3, addNA=T)


prelimGDM <- smallD[,.(N=.N,
                       Opportun_1=sum(Opportunity_GDM_screening_1==T, na.rm=T),
                       Success_1=sum(GDMscreeningontime_1==T, na.rm=T),
                       Screenb424=sum(screenb424==T, na.rm=T),
                       Screenb424False=sum(screenb424==F, na.rm=T),
                       Opportun_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2=sum(GDMscreeningontime_2==T, na.rm=T),
                       Opportun_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       screenafter28=sum(screenafter28==T, na.rm=T),
                       screenafter28False=sum(screenafter28==F, na.rm=T)),
                       keyby=.(ident_dhis2_control)]
                              #str_TRIAL_1_Cluster)]
                               

openxlsx::write.xlsx(prelimGDM,file.path(FOLDER_DATA_RESULTS,
                                            "process_outcomes",
                                            "T1",
                                        sprintf("%s_prelim_GDM_Cluster.xlsx",
                                                lubridate::today))) 
 
#need to do rbs>140 outside windows
#Random blood sugar >=140 mg/dl and referred
#OGCT >=140 mg/dl
#OGCT >=140 mg/dl and referred


############ OLD CODE ###############

##Defining Opportunities
smallD[,Opportunity_GDM_screening:= as.numeric(NA)]
smallD[bookgestagedays_cats=="[-500,0]", Opportunity_GDM_screening:=as.numeric(NA)]

# before 15-17
smallD[bookgestagedays_cats %in% c("(0,104]"), Opportunity_GDM_screening:=2]

# booked 15-17
smallD[bookgestagedays_cats %in% c("(104,125]"), Opportunity_GDM_screening:=2]

#booked 18-22
smallD[bookgestagedays_cats %in% c("(125,160]"), Opportunity_GDM_screening:=2]

#booked 23-23
smallD[bookgestagedays_cats %in% c("(160,167]"), Opportunity_GDM_screening:=2]

#booked 24-28
smallD[bookgestagedays_cats %in% c("(167,202]"), Opportunity_GDM_screening:=1]

#booked 29-30
smallD[bookgestagedays_cats %in% c("(202,216]"), Opportunity_GDM_screening:=1]

#booked 31-33
smallD[bookgestagedays_cats %in% c("(216,237]"), Opportunity_GDM_screening:=1]

#booked 34_34
smallD[bookgestagedays_cats %in% c("(237,244]"), Opportunity_GDM_screening:=1]

#booked 35-37
smallD[bookgestagedays_cats %in% c("(244,265]"), Opportunity_GDM_screening:=1]

#check 
xtabs(~smallD$Opportunity_GDM_screening, addNA=T)

smallD[,GDMscreeningontime:=as.numeric(NA)]
smallD[screenb424==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallD$GDMscreeningontime, addNA = T)

# after 28 weeks
smallD[screenafter28==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallD$GDMscreeningontime, addNA = T)

### make man vars for ogct and rbs testing over 140 ###
## FIX THIS ## 
## Referal for High At booking
smallD[,booklabbloodgluhigh_refer:=as.logical(NA)]
smallD[!is.na(booklabbloodglu), booklabbloodgluhigh_refer:=FALSE]

varsmanRBGHigh <-names(smallD)[stringr::str_detect(names(smallD),"^TrialOne_manRBGHigh_")]

for (i in varsmanRBGHigh){smallD[get(i)==T, proprefDiab:=T]}
#fix this statement
smallD[booklabbloodglu_high==T & proprefDiab==T, booklabbloodgluhigh_refer:=TRUE]

#booked before 24 weeks
b415weeks <- smallD[bookgestagedays_cats %in% c( "(0,104]","(104,125]")]
## High at 24-28 weeks
b415weeks[,TrialOne_bloodsugar_24_28_high:=as.logical(NA)]
b415weeks[TrialOne_labbloodglu_exists_24_24==T|
         TrialOne_labbloodglu_exists_25_25==T|
         TrialOne_labbloodglu_exists_26_26==T|
         TrialOne_labbloodglu_high_27_27==T,TrialOne_bloodsugar_24_28_high:=FALSE]

smallD[TrialOne_labbloddglu_high_24_28_high==F & 
         (TrialOne_labbloodglu_high_24_24==T|
            TrialOne_labbloodglu_high_25_25==T|
            TrialOne_labbloodglu_high_26_26==T|
            TrialOne_labbloodglu_high_27_27==T),TrialOne_bloodsugar_24_28_high:=T]



## High at 24-28 weeks
smallD[,TrialOne_bloodsugar_24_28_high:=as.logical(NA)]
smallD[TrialOne_labbloodglu_exists_24_24==T|
          TrialOne_labbloodglu_exists_25_25==T|
          TrialOne_labbloodglu_exists_26_26==T|
          TrialOne_labbloodglu_exists_27_27==T,TrialOne_bloodsugar_24_28_high:=FALSE]

smallD[TrialOne_bloodsugar_24_28_high==F & 
         (TrialOne_labbloodglu_high_24_24==T|
          TrialOne_labbloodglu_high_25_25==T|
          TrialOne_labbloodglu_high_26_26==T|
          TrialOne_labbloodglu_high_27_27==T),TrialOne_bloodsugar_24_28_high:=T]





