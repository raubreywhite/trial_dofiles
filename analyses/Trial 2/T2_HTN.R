### Need to import data set saved from attendance script

# indicators for trial 2 qid arm for bp

########## Hypertension ########## 
#define chronic?

#Number of opportunities should be same as opportunities as HTN in TRIAL One 
# but here need to add those that have a visit as well
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




#proper managment (who and when do we want this to happen??)
smallD[, newVar:= as.logical(NA)]
smallD[TrialOne_manhtn_ModSev_18_18==T, newVar:=TRUE]




##### Need to take into consideration cases with MILD bp values and then rescreened ####
##Mild GHT
#must have other tests like refer to ultrasound, proteinuria, liver function test, kidney #function test, cbc, etc


