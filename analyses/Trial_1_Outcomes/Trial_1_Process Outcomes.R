#################################  Process Outcomes ################################

########## Attendance ########## 
#here we want the bookgestagedays to be counted as 1, 2,3,4
smallD[bookgestagedays_cats=="[-500,0]", OpportunityofVisits:=as.numeric(NA)]

# before 15-17
smallD[bookgestagedays_cats %in% c("(0,104]"), OpportunityofVisits:=6]

# booked 15-17
smallD[bookgestagedays_cats %in% c("(104,125]"), OpportunityofVisits:=5]

#booked 18-22
smallD[bookgestagedays_cats %in% c( "(125,160]" ), OpportunityofVisits:=4]

#booked 23-23
smallD[bookgestagedays_cats %in% c("(160,167]"), OpportunityofVisits:=4]

#booked 24-28
smallD[bookgestagedays_cats %in% c( "(167,202]" ), OpportunityofVisits:=3]

#booked 29-30
smallD[bookgestagedays_cats %in% c( "(202,216]" ), OpportunityofVisits:=2]

#booked 31-33
smallD[bookgestagedays_cats %in% c( "(216,237]" ), OpportunityofVisits:=2]

#booked 34_34
smallD[bookgestagedays_cats %in% c( "(237,244]" ), OpportunityofVisits:=1]

#booked 35-37
smallD[bookgestagedays_cats %in% c( "(244,265]" ), OpportunityofVisits:=1]

#check 
xtabs(~smallD$OpportunityofVisits)

#id women referred at some point in time to remove the the opportunities she may have
smallD[TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37,
       OpportunityofVisits:=OpportunityofVisits-0]

smallD[TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34,
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33,
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28,
       OpportunityofVisits:=OpportunityofVisits-2]

smallD[TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23,
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22,
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17,
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14,
       OpportunityofVisits:=OpportunityofVisits-5]

#check 
xtabs(~smallD$OpportunityofVisits, addNA = T)

#Attendance (Success)
smallD[,AttendedonTime:=0]
smallD[TrialOne_anvisitnew_15_17==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_18_22==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_24_28==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_31_33==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_35_37==T, AttendedonTime:=AttendedonTime+1]

xtabs(~smallD$AttendedonTime, addNA = T)


########## Hypertension ########## 
#Number of opportunities should be same as opportunities of attendance
smallD[,OpportunityBPScreening:= OpportunityofVisits]

#BP Screenings (sucesses)
smallD[,BPonTime:=0]
smallD[TrialOne_anbpsyst_present_15_17==T & TrialOne_anbpdiast_present_15_17==T, 
        BPonTime:=BPonTime+1]
smallD[TrialOne_anbpsyst_present_18_22==T & TrialOne_anbpdiast_present_18_22==T, 
        BPonTime:=BPonTime+1]
smallD[TrialOne_anbpsyst_present_24_28==T & TrialOne_anbpdiast_present_24_28==T, 
        BPonTime:=BPonTime+1]
smallD[TrialOne_anbpsyst_present_31_33==T & TrialOne_anbpdiast_present_31_33==T, 
       BPonTime:=BPonTime+1]
smallD[TrialOne_anbpsyst_present_35_37==T & TrialOne_anbpdiast_present_35_37==T, 
        BPonTime:=BPonTime+1]
#check 
xtabs(~smallD$BPonTime, addNA = T)

########## Anemia ########## 
# Define opportunities for everyone assuming No one has anemia
smallD[bookgestagedays_cats=="[-500,0]", Opportunity_anemia_screening:=as.numeric(NA)]

# before 15-17
smallD[bookgestagedays_cats %in% c("(0,104]"), Opportunity_anemia_screening:=3]

# booked 15-17
smallD[bookgestagedays_cats %in% c("(104,125]"), Opportunity_anemia_screening:=3]

#booked 18-22
smallD[bookgestagedays_cats %in% c( "(125,160]" ), Opportunity_anemia_screening:=3]

#booked 23-23
smallD[bookgestagedays_cats %in% c("(160,167]"), Opportunity_anemia_screening:=2]

#booked 24-28
smallD[bookgestagedays_cats %in% c( "(167,202]" ), Opportunity_anemia_screening:=2]

#booked 29-30
smallD[bookgestagedays_cats %in% c( "(202,216]" ), Opportunity_anemia_screening:=2]

#booked 31-33
smallD[bookgestagedays_cats %in% c( "(216,237]" ), Opportunity_anemia_screening:=1]

#booked 34_34
smallD[bookgestagedays_cats %in% c( "(237,244]" ), Opportunity_anemia_screening:=1]

#booked 35-37
smallD[bookgestagedays_cats %in% c( "(244,265]" ), Opportunity_anemia_screening:=1]

xtabs(~smallD$Opportunity_anemia_screening, addNA=T)


#id women referred at some point in time to remove the the opportunities she may have
smallD[TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-0]

smallD[TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-0]

smallD[TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-1]

smallD[TrialOne_refHosp_29_30==T | TrialOne_refHR_29_30==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-1]

smallD[TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-2]

smallD[TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-2]

smallD[TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-2]

smallD[TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-2]

smallD[TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14==T,
       Opportunity_anemia_screening:=Opportunity_anemia_screening-2]

# checks
xtabs(~smallD$Opportunity_anemia_screening, addNA=T)


###### Successful Anemia Screenings for NORMAL CONDITIONs ###### 
smallD[,HbonTime:=0]

#Screen at bookings before 24 weeks??
# before 15-17
smallD[bookgestagedays_cats %in% c("(0,104]") & 
         TrialOne_labhb_normal_00_14==T, HbonTime:=HbonTime+1]

# booked 15-17
smallD[bookgestagedays_cats %in% c("(104,125]") & 
         (TrialOne_labhb_normal_14_14==T|
         TrialOne_labhb_normal_15_17==T |
         TrialOne_labhbnormal_18_18==T), HbonTime:=HbonTime+1]

#booked 18-22
smallD[bookgestagedays_cats %in% c( "(125,160]" ) &
         (TrialOne_labhb_normal_17_17==T |
         TrialOne_labhb_normal_18_22==T), HbonTime:=HbonTime+1]


#24-28 screenings
smallD[TrialOne_labhb_normal_23_23==T |
         TrialOne_labhb_normal_24_28==T, 
       HbonTime:=HbonTime+1]

#booked 29-30, 31-33
smallD[bookgestagedays_cats %in% c("(202,216]","(216,237]" ) & 
         (TrialOne_labhb_normal_29_30==T|
         TrialOne_labhb_normal_31_33==T), 
       HbonTime:=HbonTime+1]


#booked 34_34
smallD[bookgestagedays_cats %in% c("(237,244]") &
         TrialOne_labhb_normal_34_34==T, HbonTime:=HbonTime+1]


#35-37
smallD[TrialOne_labhb_normal_34_34==T |
         TrialOne_labhb_normal_35_37==T, HbonTime:=HbonTime+1]

# Severe anemia
#if you have severe anemia at any point, refer to hospital and does not get opportunities in the future
# before 15 weeks
smallD[TrialOne_labhb_anemia_sev_00_14==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_sev_00_14==T & TrialOne_manhb_00_14==T,HbonTime:=HbonTime+1 ]




