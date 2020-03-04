### Need to import smallD
#check this code and how to read it in despite the date
anemia <- fread("C:/data processing/data_clean/Trial_1_Outcomes/"Anemia.xlsx",encoding="UTF-8"")


########## Anemia ########## 

## No Anemia Cases ##
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
#might have toe remove this
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
            TrialOne_labhb_normal_15_17==T), HbonTime:=HbonTime+1]

#booked 18-22
smallD[bookgestagedays_cats %in% c( "(125,160]" ) &
         (TrialOne_labhb_normal_18_22==T), HbonTime:=HbonTime+1]

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
smallD[TrialOne_labhb_normal_35_37==T, HbonTime:=HbonTime+1]

## Severe anemia ##
# before 15 weeks
smallD[TrialOne_labhb_anemia_sev_00_14==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_sev_00_14==T & 
         (TrialOne_manhb_00_00==T |
            TrialOne_manhb_01_01==T |
            TrialOne_manhb_02_02==T |
            TrialOne_manhb_03_03==T |
            TrialOne_manhb_04_04==T |
            TrialOne_manhb_05_05==T |
            TrialOne_manhb_06_06==T |
            TrialOne_manhb_07_07==T |
            TrialOne_manhb_08_08==T |
            TrialOne_manhb_09_09==T |
            TrialOne_manhb_10_10==T |
            TrialOne_manhb_11_11==T |
            TrialOne_manhb_12_12==T |
            TrialOne_manhb_13_13==T |
            TrialOne_manhb_14_14==T),HbonTime:=HbonTime+1]

#15-17
smallD[TrialOne_labhb_anemia_sev_15_17==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_sev_15_17==T & 
         (TrialOne_manhb_15_15==T |
          TrialOne_manhb_16_16==T |
          TrialOne_manhb_17_17==T),HbonTime:=HbonTime+1]

#18-22
smallD[TrialOne_labhb_anemia_sev_18_22==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_sev_18_22==T & 
         (TrialOne_manhb_18_18==T|
            TrialOne_manhb_19_19==T |
            TrialOne_manhb_20_20==T |
            TrialOne_manhb_21_21==T |
            TrialOne_manhb_22_22==T),HbonTime:=HbonTime+1]

#23-23
smallD[TrialOne_labhb_anemia_sev_23_23==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_sev_23_23==T & TrialOne_manhb_23_23==T,HbonTime:=HbonTime+1]

#24-28
smallD[TrialOne_labhb_anemia_sev_24_28==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_sev_24_28==T & 
         (TrialOne_manhb_24_24==T |
          TrialOne_manhb_25_25==T |
          TrialOne_manhb_26_26==T |
          TrialOne_manhb_27_27==T |
          TrialOne_manhb_28_28==T),HbonTime:=HbonTime+1]

#29-30
smallD[TrialOne_labhb_anemia_sev_29_30==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_sev_29_30==T &
         (TrialOne_manhb_29_29==T |
          TrialOne_manhb_30_30==T),HbonTime:=HbonTime+1]

#31-33
smallD[TrialOne_labhb_anemia_sev_31_33==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-0]
smallD[TrialOne_labhb_anemia_sev_31_33==T & 
         (TrialOne_manhb_31_31==T |
          TrialOne_manhb_32_32==T |
          TrialOne_manhb_33_33==T),HbonTime:=HbonTime+1]

#34-34
smallD[TrialOne_labhb_anemia_sev_34_34==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-0]
smallD[TrialOne_labhb_anemia_sev_34_34==T & TrialOne_manhb_34_34==T,HbonTime:=HbonTime+1]

#35-37
smallD[TrialOne_labhb_anemia_sev_35_37==T,
       Opportunity_anemia_screening := Opportunity_anemia_screening-0]
smallD[TrialOne_labhb_anemia_sev_35_37==T & 
         (TrialOne_manhb_35_35==T |
          TrialOne_manhb_36_36==T |
          TrialOne_manhb_37_37==T),HbonTime:=HbonTime+1]

xtabs(~smallD$HbonTime)

prelimresults <- smallD[,.(
  N=.N,
  Success=sum(HbonTime, na.rm=T),
  Opportunitites=sum(Opportunity_anemia_screening, na.rm=T)),
  keyby=.(bookgestagedays_cats, ident_dhis2_control)]


#mild/mod anemia
# detection of mild/mod, decrease the opportunity depending on where it was diagnosed, 
#check if managed. 
#If managed and decreases leave opportunity as is
#if managed and improves, opportunity for next gA window increases
 

##### Should add the result of the screening somewhere #####
#00-14
smallD[TrialOne_labhb_anemia_mild_mod_00_14==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_mild_mod_00_14==T &
         (TrialOne_manhb_mildmodhbret_18_18==T|
         TrialOne_manhb_mildmodhbret_19_19==T|
         TrialOne_manhb_mildmodhbret_20_20==T|
           TrialOne_manhb_mildmodhbret_21_21==T),
            Opportunity_anemia_screening:= Opportunity_anemia_screening-2]


#15-17
smallD[TrialOne_labhb_anemia_mild_mod_15_17==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_mild_mod_15_17==T &
         (TrialOne_manhb_mildmodhbret_18_18==T|
            TrialOne_manhb_mildmodhbret_19_19==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+2]
#18-22
smallD[TrialOne_labhb_anemia_mild_mod_18_22==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-2]
smallD[TrialOne_labhb_anemia_mild_mod_18_22==T &
         (TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+2]
#24-28
smallD[TrialOne_labhb_anemia_mild_mod_24_28==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_mild_mod_24_28==T &
         (TrialOne_manhb_mildmodhbret_27_27==T|
            TrialOne_manhb_mildmodhbret_28_28==T|
            TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]
#29-30
smallD[TrialOne_labhb_anemia_mild_mod_29_30==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_mild_mod_29_30==T &
         (TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T |
            TrialOne_manhb_mildmodhbret_35_35==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]
#31-33
smallD[TrialOne_labhb_anemia_mild_mod_31_33==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_mild_mod_31_33==T &
         (TrialOne_manhb_mildmodhbret_34_34==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]


#34-34





