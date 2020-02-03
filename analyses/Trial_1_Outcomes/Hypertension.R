### Need to import data set saved from attendance script


########## Hypertension ########## 
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


##### Need to take into consideration cases with high bp values and then rescreened ####
# Moderate or severe hypertension
#refer to hospital or HR
smallD[TrialOne_anbpsyst_modSevHTN_00_14==T & TrialOne_anbpdiast_modSevHTN_00_14==T,
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[TrialOne_anbpsyst_modSevHTN_15_17==T & TrialOne_anbpdiast_modSevHTN_15_17==T,
       OpportunityofVisits:=OpportunityofVisits-4]

smallD[TrialOne_anbpsyst_modSevHTN_18_22==T & TrialOne_anbpdiast_modSevHTN_18_22==T,
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

##Mild GHT
#must have other tests like refer to ultrasound, proteinuria, liver function test, kidney #function test, cbc, etc
