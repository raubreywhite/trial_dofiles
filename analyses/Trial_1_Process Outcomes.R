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
mallD[bookgestagedays_cats %in% c("(160,167]"), OpportunityofVisits:=4]

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

#Attendance (Success)
smallD[,AttendedonTime:=0]
smallD[TrialOne_anvisitnew_15_17==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_18_22==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_24_28==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_31_33==T, AttendedonTime:=AttendedonTime+1]
smallD[TrialOne_anvisitnew_35_37==T, AttendedonTime:=AttendedonTime+1]


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

########## Anemia ########## 
# Define opportunities





