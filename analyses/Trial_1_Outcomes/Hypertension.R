### Need to import data set saved from attendance script


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
