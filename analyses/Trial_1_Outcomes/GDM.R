#import data set from process outcomes

########## GDM ########## 
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



## Remove opportunities for people who were referred to HR or Hosp
smallD[(TrialOne_anvisitnew_24_24 & 
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
xtabs(~smallD$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
smallD[,screenb424:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
smallD[screenb424==F &
         booklabbloodglu_high==F &
        ((!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu))),
       screenb424:=T]
xtabs(~smallD$screenb424, addNA=T)

##Defining Successes 
smallD[,GDMscreeningontime:=as.numeric(NA)]
smallD[screenb424==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallD$GDMscreeningontime, addNA = T)

smallD[,GDMscreeningontime_1:=as.numeric(NA)]
smallD[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
smallD[screenb424==T & booklaburglu_high==F, 
       GDMscreeningontime_1:=TRUE]

xtabs(~smallD$GDMscreeningontime_1, addNA=T)


#24-28 weeks
smallD[,GDMscreeningontime_2:=as.numeric(NA)]

smallD[(TrialOne_labbloodglu_exists_24_24==T|
         TrialOne_labbloodglu_exists_25_25==T|
         TrialOne_labbloodglu_exists_26_26==T|
         TrialOne_labbloodglu_exists_27_27==T|
         TrialOne_labbloodglu_exists_28_28==T)|
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
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~smallD$screenafter28, addNA=T)

##Defining Success
# after 28 weeks
smallD[screenafter28==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallD$GDMscreeningontime, addNA = T)

smallD[,GDMscreeningontime_3:=as.numeric(NA)]
smallD[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
smallD[screenafter28==T & 
         booklaburglu_high==F, 
       GDMscreeningontime_3:=TRUE]
xtabs(~smallD$GDMscreeningontime_3, addNA=T)


prelimGDM <- smallD[,.(Opportunities_1=sum(Opportunity_GDM_screening_1==T),
                       Success_1=sum(GDMscreeningontime_1==T, na.rm=T),
                       Opportunities_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2=sum(GDMscreeningontime_2==T, na.rm=T),
                       Opportunities_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       Opportunities_4=sum(Opportunity_GDM_screening_4==T, na.rm=T),
                       Success_4=sum(GDMscreeningontime_4==T, na.rm=T))]










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





