
#import data set from process outcomes

#smallDgdm <- fread("C:/data processing/data_clean/Trial_1_Outcomes/2020-03-07_GDM.xlsx")

smallDgdm <- smallD

########## GDM ########## 

###Redefining opportinites
smallDgdm[,Opportunity_GDM_screening_1:=as.numeric(NA)]
smallDgdm[,Opportunity_GDM_screening_2:=as.numeric(NA)]
smallDgdm[,Opportunity_GDM_screening_3:=as.numeric(NA)]
smallDgdm[,Opportunity_GDM_screening_4:=as.numeric(NA)]

# before 24
smallDgdm[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_1:=1]
#24-28
smallDgdm[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]",
                                   "(167,202]") |
              TrialOne_anvisitnew_24_28==T,Opportunity_GDM_screening_2:=1]
# after 28
smallDgdm[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]", 
                                   "(237,244]",
                                   "(244,265]"), Opportunity_GDM_screening_3:=1]

# high rbs anywhere outside of the 24-28
smallDgdm[(booklabbloodglu_high==F | is.na(booklabbloodglu_high)) & 
         (TrialOne_labbloodglu_high_00_14==T|
          TrialOne_labbloodglu_high_15_17==T|
          TrialOne_labbloodglu_high_18_22==T|
          TrialOne_labbloodglu_high_23_23==T|
          TrialOne_labbloodglu_high_29_30==T|
          TrialOne_labbloodglu_high_31_33==T|
          TrialOne_labbloodglu_high_34_34==T|
          TrialOne_labbloodglu_high_35_37==T), Opportunity_GDM_screening_4:=1]

xtabs(~smallDgdm$Opportunity_GDM_screening_1, addNA=T)
xtabs(~smallDgdm$Opportunity_GDM_screening_2, addNA=T)
xtabs(~smallDgdm$Opportunity_GDM_screening_3, addNA=T)
xtabs(~smallDgdm$Opportunity_GDM_screening_4, addNA=T)




## Remove opportunities for people who were referred to HR or Hosp
smallDgdm[(TrialOne_anvisitnew_24_24 & 
         (TrialOne_refHR_00_14==T|
          TrialOne_refHR_15_17==T|
          TrialOne_refHR_18_22==T|
          TrialOne_refHR_23_23==T))|
         (TrialOne_anvisitnew_25_25 & 
            (TrialOne_refHR_00_14==T|
               TrialOne_refHR_15_17==T|
               TrialOne_refHR_18_22==T|
               TrialOne_refHR_23_23==T|
               TrialOne_refHR_24_24==T))|
         (TrialOne_anvisitnew_26_26 & 
            (TrialOne_refHR_00_14==T|
               TrialOne_refHR_15_17==T|
               TrialOne_refHR_18_22==T|
               TrialOne_refHR_23_23==T|
               TrialOne_refHR_24_24==T|
               TrialOne_refHR_25_25==T))|
         (TrialOne_anvisitnew_27_27 & 
            (TrialOne_refHR_00_14==T|
               TrialOne_refHR_15_17==T|
               TrialOne_refHR_18_22==T|
               TrialOne_refHR_23_23==T|
               TrialOne_refHR_24_24==T|
               TrialOne_refHR_25_25==T|
               TrialOne_refHR_26_26==T))|
         (TrialOne_anvisitnew_28_28 & 
            (TrialOne_refHR_00_14==T|
               TrialOne_refHR_15_17==T|
               TrialOne_refHR_18_22==T|
               TrialOne_refHR_23_23==T|
               TrialOne_refHR_24_24==T|
               TrialOne_refHR_25_25==T|
               TrialOne_refHR_26_26==T|
               TrialOne_refHR_27_27==T)),
       Opportunity_GDM_screening_2:=Opportunity_GDM_screening_2-1]

# checks
xtabs(~smallDgdm$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
smallDgdm[,screenb424:=as.logical(NA)]
smallDgdm[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
smallDgdm[screenb424==F &
         booklabbloodglu_high==F &
        ((!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu))),
       screenb424:=T]
xtabs(~smallDgdm$screenb424, addNA=T)

##Defining Successes 
smallDgdm[,GDMscreeningontime_1:=as.logical(NA)]
smallDgdm[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
smallDgdm[screenb424==T & (!is.na(booklaburglu) & booklabbloodglu_high==F), 
       GDMscreeningontime_1:=TRUE]

xtabs(~smallDgdm$GDMscreeningontime_1, addNA=T)


#24-28 weeks
smallDgdm[,GDMscreeningontime_2a:=as.logical(NA)]
smallDgdm[Opportunity_GDM_screening_2==1 & 
            (TrialOne_labbloodglu_exists_24_24==T|
              TrialOne_labbloodglu_exists_25_25==T|
              TrialOne_labbloodglu_exists_26_26==T|
              TrialOne_labbloodglu_exists_27_27==T|
              TrialOne_labbloodglu_exists_28_28==T) &
            (TrialOne_labfastbloodglu_exists_24_24==T|
              TrialOne_labfastbloodglu_exists_25_25==T|
              TrialOne_labfastbloodglu_exists_26_26==T|
              TrialOne_labfastbloodglu_exists_27_27==T|
              TrialOne_labfastbloodglu_exists_28_28==T),GDMscreeningontime_2a:=TRUE]

xtabs(~smallDgdm$GDMscreeningontime_2a, addNA=T)

smallDgdm[,GDMscreeningontime_2b:=as.logical(NA)]
smallDgdm[Opportunity_GDM_screening_2==1 & 
            (TrialOne_labbloodglu_exists_24_24==T|
               TrialOne_labbloodglu_exists_25_25==T|
               TrialOne_labbloodglu_exists_26_26==T|
               TrialOne_labbloodglu_exists_27_27==T|
               TrialOne_labbloodglu_exists_28_28==T) &
            (TrialOne_labbloodglu_high_24_24==F|
               TrialOne_labbloodglu_high_25_25==F|
               TrialOne_labbloodglu_high_26_26==F|
               TrialOne_labbloodglu_high_27_27==F|
               TrialOne_labbloodglu_high_28_28==F) &
            (TrialOne_labfastbloodglu_exists_24_24==T|
               TrialOne_labfastbloodglu_exists_25_25==T|
               TrialOne_labfastbloodglu_exists_26_26==T|
               TrialOne_labfastbloodglu_exists_27_27==T|
               TrialOne_labfastbloodglu_exists_28_28==T),GDMscreeningontime_2b:=TRUE]
xtabs(~smallDgdm$GDMscreeningontime_2b, addNA=T)
         

#Screening after 28 weeks: Creating one var for 3 possibilities
smallDgdm[,screenafter28:=as.logical(NA)]
smallDgdm[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]"),
       screenafter28:=F]
smallDgdm[screenafter28==F &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~smallDgdm$screenafter28, addNA=T)

##Defining Success
smallDgdm[,GDMscreeningontime_3:=logical(NA)]
smallDgdm[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
smallDgdm[screenafter28==T & 
         booklabbloodglu_high==F, 
       GDMscreeningontime_3:=TRUE]
xtabs(~smallDgdm$GDMscreeningontime_3, addNA=T)


prelimGDM <- smallDgdm[,.(N=.N,
                       Opportun_1=sum(Opportunity_GDM_screening_1==T, na.rm=T),
                       Success_1=sum(GDMscreeningontime_1==T, na.rm=T),
                       Screenb424=sum(screenb424==T, na.rm=T),
                       Screenb424False=sum(screenb424==F, na.rm=T),
                       Opportun_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2a=sum(GDMscreeningontime_2a==T, na.rm=T),
                       Success_2a=sum(GDMscreeningontime_2b==T, na.rm=T),
                       Opportun_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       screenafter28=sum(screenafter28==T, na.rm=T),
                       screenafter28False=sum(screenafter28==F, na.rm=T)),
                       keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(prelimGDM,file.path(FOLDER_DATA_RESULTS,
                                            "process_outcomes",
                                            "T1",
                                            sprintf("%s_prelim_GDM.xlsx", 
                                                    lubridate::today())))



############ OLD CODE ###############

##Defining Opportunities
smallDgdm[,Opportunity_GDM_screening:= as.numeric(NA)]
smallDgdm[bookgestagedays_cats=="[-500,0]", Opportunity_GDM_screening:=as.numeric(NA)]

# before 15-17
smallDgdm[bookgestagedays_cats %in% c("(0,104]"), Opportunity_GDM_screening:=2]

# booked 15-17
smallDgdm[bookgestagedays_cats %in% c("(104,125]"), Opportunity_GDM_screening:=2]

#booked 18-22
smallDgdm[bookgestagedays_cats %in% c("(125,160]"), Opportunity_GDM_screening:=2]

#booked 23-23
smallDgdm[bookgestagedays_cats %in% c("(160,167]"), Opportunity_GDM_screening:=2]

#booked 24-28
smallDgdm[bookgestagedays_cats %in% c("(167,202]"), Opportunity_GDM_screening:=1]

#booked 29-30
smallDgdm[bookgestagedays_cats %in% c("(202,216]"), Opportunity_GDM_screening:=1]

#booked 31-33
smallDgdm[bookgestagedays_cats %in% c("(216,237]"), Opportunity_GDM_screening:=1]

#booked 34_34
smallDgdm[bookgestagedays_cats %in% c("(237,244]"), Opportunity_GDM_screening:=1]

#booked 35-37
smallDgdm[bookgestagedays_cats %in% c("(244,265]"), Opportunity_GDM_screening:=1]

#check 
xtabs(~smallDgdm$Opportunity_GDM_screening, addNA=T)

smallDgdm[,GDMscreeningontime:=as.numeric(NA)]
smallDgdm[screenb424==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallDgdm$GDMscreeningontime, addNA = T)

# after 28 weeks
smallDgdm[screenafter28==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallDgdm$GDMscreeningontime, addNA = T)

### make man vars for ogct and rbs testing over 140 ###
## FIX THIS ## 
## Referal for High At booking
smallDgdm[,booklabbloodgluhigh_refer:=as.logical(NA)]
smallDgdm[!is.na(booklabbloodglu), booklabbloodgluhigh_refer:=FALSE]

varsmanRBGHigh <-names(smallDgdm)[stringr::str_detect(names(smallDgdm),"^TrialOne_manRBGHigh_")]

for (i in varsmanRBGHigh){smallDgdm[get(i)==T, proprefDiab:=T]}
#fix this statement
smallDgdm[booklabbloodglu_high==T & proprefDiab==T, booklabbloodgluhigh_refer:=TRUE]

#booked before 24 weeks
b415weeks <- smallDgdm[bookgestagedays_cats %in% c( "(0,104]","(104,125]")]
## High at 24-28 weeks
b415weeks[,TrialOne_bloodsugar_24_28_high:=as.logical(NA)]
b415weeks[TrialOne_labbloodglu_exists_24_24==T|
         TrialOne_labbloodglu_exists_25_25==T|
         TrialOne_labbloodglu_exists_26_26==T|
         TrialOne_labbloodglu_high_27_27==T,TrialOne_bloodsugar_24_28_high:=FALSE]

smallDgdm[TrialOne_labbloddglu_high_24_28_high==F & 
         (TrialOne_labbloodglu_high_24_24==T|
            TrialOne_labbloodglu_high_25_25==T|
            TrialOne_labbloodglu_high_26_26==T|
            TrialOne_labbloodglu_high_27_27==T),TrialOne_bloodsugar_24_28_high:=T]



## High at 24-28 weeks
smallDgdm[,TrialOne_bloodsugar_24_28_high:=as.logical(NA)]
smallDgdm[TrialOne_labbloodglu_exists_24_24==T|
          TrialOne_labbloodglu_exists_25_25==T|
          TrialOne_labbloodglu_exists_26_26==T|
          TrialOne_labbloodglu_exists_27_27==T,TrialOne_bloodsugar_24_28_high:=FALSE]

smallDgdm[TrialOne_bloodsugar_24_28_high==F & 
         (TrialOne_labbloodglu_high_24_24==T|
          TrialOne_labbloodglu_high_25_25==T|
          TrialOne_labbloodglu_high_26_26==T|
          TrialOne_labbloodglu_high_27_27==T),TrialOne_bloodsugar_24_28_high:=T]





