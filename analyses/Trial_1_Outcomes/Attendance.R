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

#need to seperate control and intervention seperately
#for intervention add the trialmanperf

#control
smallD[ident_dhis2_control==T & (TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37),
       OpportunityofVisits:=OpportunityofVisits-0]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28),
       OpportunityofVisits:=OpportunityofVisits-2]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17),
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[ident_dhis2_control==T & (TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14),
       OpportunityofVisits:=OpportunityofVisits-5]


# Intervention
smallD[ident_dhis2_control==F & (TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37),
       OpportunityofVisits:=OpportunityofVisits-0]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28),
       OpportunityofVisits:=OpportunityofVisits-2]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17),
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[ident_dhis2_control==F & (TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14),
       OpportunityofVisits:=OpportunityofVisits-5]



#check 
xtabs(~smallD$OpportunityofVisits, addNA = T)

##-1opportunity for visits-need to check this out

#intervention

#Attendance (Success)
smallD[,AttendedonTime:=0]
smallD[TrialOne_anvisitnew_15_17==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_18_22==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_24_28==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_31_33==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_35_37==T, AttendedonTime:=AttendedonTime+1]

xtabs(~smallD$AttendedonTime, addNA = T)

#save data set in clean folder
#will use it for hypertension