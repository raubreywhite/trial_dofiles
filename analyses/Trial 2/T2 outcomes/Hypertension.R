### Need to import data set saved from attendance script

# need opportunity of visits first

#need to seperate control and intervention seperately
#for intervention add the trialmanperf

#control
smallD[(TrialOne_refHosp_35_37==T | TrialOne_refHR_35_37),
       OpportunityofVisits:=OpportunityofVisits-0]

smallD[(TrialOne_refHosp_34_34==T | TrialOne_refHR_34_34),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[(TrialOne_refHosp_31_33==T | TrialOne_refHR_31_33),
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[(TrialOne_refHosp_24_28==T | TrialOne_refHR_24_28),
       OpportunityofVisits:=OpportunityofVisits-2]

smallD[(TrialOne_refHosp_23_23==T | TrialOne_refHR_23_23),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[(TrialOne_refHosp_18_22==T | TrialOne_refHR_18_22),
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[(TrialOne_refHosp_15_17==T | TrialOne_refHR_15_17),
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[(TrialOne_refHosp_00_14==T | TrialOne_refHR_00_14),
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



########## Hypertension ########## 
#define chronic?

#Number of opportunities should be same as opportunities of attendance
smallD[,OpportunityBPScreening:= OpportunityofVisits]

#BP Screenings (sucesses) for anyone who has been screened
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

# Moderate or severe hypertension
#refer to hospital or HR
smallD[TrialOne_anbpsyst_modSevHTN_00_14==T & TrialOne_anbpdiast_modSevHTN_00_14==T,
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[TrialOne_anbpsyst_modSevHTN_15_17==T & TrialOne_anbpdiast_modSevHTN_15_17==T,
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[TrialOne_anbpsyst_modSevHTN_18_22==T & TrialOne_anbpdiast_modSevHTN_18_22==T,
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[TrialOne_anbpsyst_modSevHTN_23_23==T & TrialOne_anbpdiast_modSevHTN_23_23==T,
       OpportunityofVisits:=OpportunityofVisits-3]

smallD[TrialOne_anbpsyst_modSevHTN_24_28==T & TrialOne_anbpdiast_modSevHTN_24_28==T,
       OpportunityofVisits:=OpportunityofVisits-2]

smallD[TrialOne_anbpsyst_modSevHTN_31_33==T & TrialOne_anbpdiast_modSevHTN_31_33==T,
       OpportunityofVisits:=OpportunityofVisits-1]

smallD[TrialOne_anbpsyst_modSevHTN_35_37==T & TrialOne_anbpdiast_modSevHTN_35_37==T,
       OpportunityofVisits:=OpportunityofVisits-4]




#proper managment (who and when do we want this to happen??)
smallD[, newVar:= as.logical(NA)]
smallD[TrialOne_manhtn_ModSev_18_18==T, newVar:=TRUE]




##### Need to take into consideration cases with MILD bp values and then rescreened ####
##Mild GHT
#must have other tests like refer to ultrasound, proteinuria, liver function test, kidney #function test, cbc, etc


