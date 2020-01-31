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
smallD[bookgestagedays_cats %in% c( "(125,160]" ), Opportunity_GDM_screening:=2]

#booked 23-23
smallD[bookgestagedays_cats %in% c("(160,167]"), Opportunity_GDM_screening:=2]

#booked 24-28
smallD[bookgestagedays_cats %in% c( "(167,202]" ), Opportunity_GDM_screening:=1]

#booked 29-30
smallD[bookgestagedays_cats %in% c( "(202,216]" ), Opportunity_GDM_screening:=1]

#booked 31-33
smallD[bookgestagedays_cats %in% c( "(216,237]" ), Opportunity_GDM_screening:=1]

#booked 34_34
smallD[bookgestagedays_cats %in% c( "(237,244]" ), Opportunity_GDM_screening:=1]

#booked 35-37
smallD[bookgestagedays_cats %in% c( "(244,265]" ), Opportunity_GDM_screening:=1]

#check 
xtabs(~smallD$Opportunity_GDM_screening, addNA=T)


## Remove opportunities for people who were referred to HR or Hosp
smallD[TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-0]

smallD[TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-0]

smallD[TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-0]

smallD[TrialOne_refHosp_29_30==T | TrialOne_refHR_29_30==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-0]

smallD[TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-0]

smallD[TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-1]

smallD[TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-1]

smallD[TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-1]

smallD[TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14==T,
       Opportunity_GDM_screening:=Opportunity_GDM_screening-1]

# checks
xtabs(~smallD$Opportunity_GDM_screening, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
smallD[,screenb424:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]") &
        (!is.na(booklaburglu)| !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenb424:=T]
xtabs(~smallD$screenb424, addNA=T)


#Screening after 28 weeks: Creating one var for 3 possibilities
smallD[,screenafter28:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]") &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~smallD$screenafter28, addNA=T)

##Defining Successes
smallD[,GDMscreeningontime:=0]

# 0-23+6
smallD[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]") &
       booklabbloodglu_high==FALSE & 
         screenb424==T, 
       GDMscreeningontime:=GDMscreeningontime+1]
xtabs(~smallD$GDMscreeningontime, addNA = T)


###### TO DO: SCREENING AT 24-28 WEEKS ######

## High At booking
##
