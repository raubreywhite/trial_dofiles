### Need to import smallD
#check this code and how to read it in despite the date
#anemia <- fread("C:/data processing/data_clean/Trial_1_Outcomes/"Anemia.xlsx",encoding="UTF#-8"")


########## Anemia ########## 
# Define opportunities at 3 different cut off points

## booked before 24
smallD[,Opportunity_anemia_screening_1:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]",
                                   "(125,160]",
                                   "(160,167]"),
                          Opportunity_anemia_screening_1:=1]
## booked 24 or has visit 
smallD[,Opportunity_anemia_screening_2:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(167,202]")| 
                        TrialOne_anvisitnew_24_28==T,
                         Opportunity_anemia_screening_2:=1]

# booked 29-34 weeks
smallD[,Opportunity_anemia_screening_3:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]",
                                   "(237,244]"),
                          Opportunity_anemia_screening_3:=1]

## booked or visit at 35-37 weeks
smallD[,Opportunity_anemia_screening_4:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(244,265]") |
                          TrialOne_anvisitnew_35_37==T, 
                          Opportunity_anemia_screening_4:=1]

## severe anemia at booking and at any other visit after that
smallD[,Opportunity_anemia_screening_5:=as.numeric(NA)]
smallD[booklabhb<7 & booklabhb>2 &
         (TrialOne_labhb_anemia_sev_00_14==T|
            TrialOne_labhb_anemia_sev_15_17==T|
            TrialOne_labhb_anemia_sev_18_22==T|
            TrialOne_labhb_anemia_sev_23_23==T|
            TrialOne_labhb_anemia_sev_29_30==T|
            TrialOne_labhb_anemia_sev_31_33==T|
            TrialOne_labhb_anemia_sev_34_34==T),Opportunity_anemia_screening_5:=1]
## mild mod anemia
smallD[,Opportunity_anemia_screening_6:=as.numeric(NA)]
smallD[booklabhb<11 & booklabhb>=7 &
         (TrialOne_labhb_anemia_mild_mod_00_14==T|
            TrialOne_labhb_anemia_mild_mod_15_17==T|
            TrialOne_labhb_anemia_mild_mod_18_22==T|
            TrialOne_labhb_anemia_mild_mod_23_23==T|
            TrialOne_labhb_anemia_mild_mod_29_30==T|
            TrialOne_labhb_anemia_mild_mod_31_33==T|
            TrialOne_labhb_anemia_mild_mod_34_34==T),
       Opportunity_anemia_screening_6:=1]

# ADJUSTING OPPORTUNITIES FOR THOSE WHO HAVE BEEN REFERRED
## Before 24 weeks

#variable for man sev anemia anytime before 24 weeks
smallD[,manhbsev:=(TrialOne_manhb_00_00 |
                     TrialOne_manhb_01_01 |
                     TrialOne_manhb_02_02 |
                     TrialOne_manhb_03_03 |
                     TrialOne_manhb_04_04 |
                     TrialOne_manhb_05_05 |
                     TrialOne_manhb_06_06 |
                     TrialOne_manhb_07_07 |
                     TrialOne_manhb_08_08 |
                     TrialOne_manhb_09_09 |
                     TrialOne_manhb_10_10 |
                     TrialOne_manhb_11_11 |
                     TrialOne_manhb_12_12 |
                     TrialOne_manhb_13_13 |
                     TrialOne_manhb_14_14 |
                     TrialOne_manhb_15_15 |
                     TrialOne_manhb_16_16 |
                     TrialOne_manhb_17_17 |
                     TrialOne_manhb_18_18 |
                     TrialOne_manhb_19_19 |
                     TrialOne_manhb_20_20 |
                     TrialOne_manhb_21_21 |
                     TrialOne_manhb_22_22 |
                     TrialOne_manhb_23_23)]
xtabs(~smallD$manhbsev, addNA=T)


smallD[,RefHr:=as.logical(NA)]
smallD[Opportunity_anemia_screening_1==1, RefHr:=FALSE]
smallD[(TrialOne_manRef_HR_00_00==T|
          TrialOne_manRef_HR_01_01==T|
          TrialOne_manRef_HR_02_02==T|
          TrialOne_manRef_HR_03_03==T|
          TrialOne_manRef_HR_04_04==T|
          TrialOne_manRef_HR_05_05==T|
          TrialOne_manRef_HR_06_06==T|
          TrialOne_manRef_HR_07_07==T|
          TrialOne_manRef_HR_08_08==T|
          TrialOne_manRef_HR_09_09==T|
          TrialOne_manRef_HR_10_10==T|
          TrialOne_manRef_HR_11_11==T|
          TrialOne_manRef_HR_12_12==T|
          TrialOne_manRef_HR_13_13==T|
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
          RefHr:=TRUE]
xtabs(~smallD$RefHr, addNA=T)

## At 24-28 weeks
smallD[Opportunity_anemia_screening_2==1 &
        (TrialOne_anvisitnew_24_24 & 
          (RefHr==T))|
        (TrialOne_anvisitnew_25_25 & 
          (RefHr==T|TrialOne_manRef_HR_24_24==T))|
        (TrialOne_anvisitnew_26_26 & 
          (RefHr==T|TrialOne_manRef_HR_24_24==T|
             TrialOne_manRef_HR_25_25==T))|
        (TrialOne_anvisitnew_27_27 & 
          (RefHr==T|TrialOne_manRef_HR_24_24==T|
             TrialOne_manRef_HR_25_25==T|
             TrialOne_manRef_HR_26_26==T))|
        (TrialOne_anvisitnew_28_28 & 
          (RefHr==T|
        TrialOne_manRef_HR_24_24==T|
        TrialOne_manRef_HR_25_25==T|
        TrialOne_manRef_HR_26_26==T|
        TrialOne_manRef_HR_27_27==T)), 
          Opportunity_anemia_screening_2:=Opportunity_anemia_screening_2-1]

xtabs(~smallD$Opportunity_anemia_screening_2, addNA=T)

# 35-37 weeks
smallD[Opportunity_anemia_screening_4==1 &
         (TrialOne_anvisitnew_29_30==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T))|
         (TrialOne_anvisitnew_31_33==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T))|
         (TrialOne_anvisitnew_34_34==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T|
               TrialOne_manRef_HR_31_31==T|
               TrialOne_manRef_HR_32_32==T|
               TrialOne_manRef_HR_33_33==T)), 
       Opportunity_anemia_screening_4:=Opportunity_anemia_screening_4-1]
xtabs(~smallD$Opportunity_anemia_screening_3, addNA=T)

#define different time cats for success
smallD[, HbonTime_1a:= as.logical(NA)]
smallD[Opportunity_anemia_screening_1==1, HbonTime_1a:=FALSE]

smallD[, HbonTime_1b:= as.logical(NA)]
smallD[Opportunity_anemia_screening_1==1 &
         (TrialOne_labhb_anemia_sev_00_14==T|
          TrialOne_labhb_anemia_sev_15_17==T|
          TrialOne_labhb_anemia_sev_18_22==T|
          TrialOne_labhb_anemia_sev_23_23==T), HbonTime_1b:=FALSE]

smallD[, HbonTime_1c:= as.logical(NA)]
smallD[Opportunity_anemia_screening_1==1 &
         (TrialOne_labhb_anemia_mild_mod_00_14==T|
            TrialOne_labhb_anemia_mild_mod_15_17==T|
            TrialOne_labhb_anemia_mild_mod_18_22==T|
            TrialOne_labhb_anemia_mild_mod_23_23==T), HbonTime_1c:=FALSE]


# Hbontime_2
smallD[, HbonTime_2a:= as.logical(NA)]
smallD[Opportunity_anemia_screening_2==1, HbonTime_2a:=FALSE]

smallD[, HbonTime_2b:= as.logical(NA)]
smallD[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_sev_24_28==T, HbonTime_2b:=FALSE]

smallD[, HbonTime_2c:= as.logical(NA)]
smallD[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_mild_mod_24_28==T, HbonTime_2c:=FALSE]

# Hbontime_3
smallD[, HbonTime_3a:= as.logical(NA)]
smallD[Opportunity_anemia_screening_3==1 & 
         (!is.na(booklabhb)), HbonTime_3a:=FALSE]

smallD[, HbonTime_3b:= as.logical(NA)]
smallD[Opportunity_anemia_screening_3==1 &
         (TrialOne_labhb_anemia_sev_29_30==T|
         TrialOne_labhb_anemia_sev_31_33==T|
         TrialOne_labhb_anemia_sev_34_34==T), HbonTime_3b:=FALSE]

smallD[, HbonTime_3c:= as.logical(NA)]
smallD[Opportunity_anemia_screening_3==1 &
         (TrialOne_labhb_anemia_mild_mod_29_30==T|
         TrialOne_labhb_anemia_mild_mod_31_33==T|
         TrialOne_labhb_anemia_mild_mod_34_34==T), HbonTime_3c:=FALSE]

smallD[, HbonTime_4a:= as.logical(NA)]
smallD[Opportunity_anemia_screening_4==1, HbonTime_4a:=FALSE]

smallD[, HbonTime_4b:= as.logical(NA)]
smallD[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_sev_35_37==T,HbonTime_4b:=FALSE]

smallD[, HbonTime_4c:= as.logical(NA)]
smallD[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_mild_mod_35_37==T, HbonTime_4c:=FALSE]

smallD[, HbonTime_5:= as.logical(NA)]
smallD[Opportunity_anemia_screening_5==1, HbonTime_5:=FALSE]

smallD[, HbonTime_6:= as.logical(NA)]
smallD[Opportunity_anemia_screening_6==1, HbonTime_6:=FALSE]



#hb on time 1, 2, 3, vars
#Screen at bookings before 24 weeks??
#check booklabhb values if normal etc

# booked before 24 weeks
smallD[HbonTime_1a==F & booklabhb>=11 & 
         booklabhb<=18, HbonTime_1a:=TRUE]
xtabs(~smallD$HbonTime_1a, addNA=T)

smallD[HbonTime_1b==F & 
         booklabhb>=2 & booklabhb<=7,HbonTime_1b:=TRUE]
xtabs(~smallD$HbonTime_1b, addNA=T)


smallD[HbonTime_1c==F & 
         booklabhb>=9 & booklabhb<=10.9,HbonTime_1c:=TRUE]
xtabs(~smallD$HbonTime_1c, addNA=T)

#24-28 screenings
smallD[HbonTime_2a==F & 
         TrialOne_labhb_normal_24_28==T, HbonTime_2a:=TRUE]

smallD[HbonTime_2b==F & 
         TrialOne_manhb_24_24==T|
         TrialOne_manhb_25_25==T|
         TrialOne_manhb_26_26==T|
         TrialOne_manhb_27_27==T|
         TrialOne_manhb_28_28==T, HbonTime_2b:=TRUE]

smallD[HbonTime_2c==F & 
         TrialOne_manhb_mildmodhbret_24_24==T|
         TrialOne_manhb_mildmodhbret_25_25==T|
         TrialOne_manhb_mildmodhbret_26_26==T|
         TrialOne_manhb_mildmodhbret_27_27==T|
         TrialOne_manhb_mildmodhbret_28_28==T, HbonTime_2c:=TRUE]

#booked 29-30, 31-33, 34
smallD[HbonTime_3a==F & Opportunity_anemia_screening_3==1 &
         (booklabhb<=18 & booklabhb>11), HbonTime_3a:=TRUE]


smallD[HbonTime_3c==1 & 
         (TrialOne_manhb_mildmodhbret_29_29==T|
         TrialOne_manhb_mildmodhbret_30_30==T|
         TrialOne_manhb_mildmodhbret_31_31==T|
         TrialOne_manhb_mildmodhbret_32_32==T|
         TrialOne_manhb_mildmodhbret_33_33==T|
         TrialOne_manhb_mildmodhbret_34_34==T), 
       HbonTime_3c:=TRUE]

smallD[HbonTime_3b==F & 
         (TrialOne_manhb_29_29==T|
             TrialOne_manhb_30_30==T|
             TrialOne_manhb_31_31==T|
             TrialOne_manhb_32_32==T|
             TrialOne_manhb_33_33==T|
             TrialOne_manhb_34_34==T), 
              HbonTime_3b:=TRUE]


# 35-37 screenings
smallD[HbonTime_4a==F & 
          TrialOne_labhb_normal_35_37==T, HbonTime_4a:=TRUE]

smallD[HbonTime_4b==F & 
         TrialOne_manhb_35_35==T|
         TrialOne_manhb_36_36==T|
         TrialOne_manhb_37_37==T, HbonTime_4b:=TRUE]

smallD[HbonTime_4c==F &
         TrialOne_manhb_mildmodhbret_25_25==T|
         TrialOne_manhb_mildmodhbret_26_26==T|
         TrialOne_manhb_mildmodhbret_27_27==T|
         TrialOne_manhb_mildmodhbret_28_28==T, HbonTime_4c:=TRUE]

# severe anemia outside of time windows
smallD[HbonTime_5==F & 
         (TrialOne_manhb_00_00==T|
         TrialOne_manhb_01_01==T|
         TrialOne_manhb_02_02==T|
         TrialOne_manhb_03_03==T|
         TrialOne_manhb_04_04==T|
         TrialOne_manhb_05_05==T|
         TrialOne_manhb_06_06==T|
         TrialOne_manhb_07_07==T|
         TrialOne_manhb_08_08==T|
         TrialOne_manhb_09_09==T|
         TrialOne_manhb_10_10==T|
         TrialOne_manhb_11_11==T|
         TrialOne_manhb_12_12==T|
         TrialOne_manhb_13_13==T|
         TrialOne_manhb_14_14==T|
         TrialOne_manhb_15_15==T|
         TrialOne_manhb_16_16==T|
         TrialOne_manhb_17_17==T|
         TrialOne_manhb_18_18==T|
         TrialOne_manhb_19_19==T|
         TrialOne_manhb_20_20==T|
         TrialOne_manhb_21_21==T|
         TrialOne_manhb_22_22==T|
         TrialOne_manhb_23_23==T),HbonTime_5:=TRUE]

#mild/mod anem retest
smallD[HbonTime_6==F &
         (TrialOne_labhb_anemia_mild_mod_00_14==T|
            TrialOne_manhb_mildmodhbret_00_00==T|
            TrialOne_manhb_mildmodhbret_01_01==T|
            TrialOne_manhb_mildmodhbret_02_02==T|
            TrialOne_manhb_mildmodhbret_03_03==T|
            TrialOne_manhb_mildmodhbret_04_04==T|
            TrialOne_manhb_mildmodhbret_05_05==T|
            TrialOne_manhb_mildmodhbret_06_06==T|
            TrialOne_manhb_mildmodhbret_07_07==T|
            TrialOne_manhb_mildmodhbret_08_08==T|
            TrialOne_manhb_mildmodhbret_09_09==T|
            TrialOne_manhb_mildmodhbret_10_10==T|
            TrialOne_manhb_mildmodhbret_11_11==T|
            TrialOne_manhb_mildmodhbret_12_12==T|
            TrialOne_manhb_mildmodhbret_13_13==T|
            TrialOne_manhb_mildmodhbret_14_14==T|
            TrialOne_manhb_mildmodhbret_15_15==T|
            TrialOne_manhb_mildmodhbret_16_16==T|
            TrialOne_manhb_mildmodhbret_17_17==T|
            TrialOne_manhb_mildmodhbret_18_18==T|
            TrialOne_manhb_mildmodhbret_19_19==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T|
            TrialOne_manhb_mildmodhbret_23_23==T|
            TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T|
            TrialOne_manhb_mildmodhbret_31_31==T|
            TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T),
       HbonTime_6:=TRUE]


prelimHB <- smallD[,.(N=.N,
                       Opportun_1=sum(Opportunity_anemia_screening_1, na.rm=T),
                       Success_1a=sum(HbonTime_1a, na.rm=T),
                       Success_1aFalse=sum(HbonTime_1a==FALSE, na.rm=T),
                       Success_1b=sum(HbonTime_1b, na.rm=T),
                       Success_1bFalse=sum(HbonTime_1b==FALSE, na.rm=T),
                       Success_1c=sum(HbonTime_1c, na.rm=T),
                       Success_1cFalse=sum(HbonTime_1c==FALSE, na.rm=T),
                       Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                       Success_2a=sum(HbonTime_2a, na.rm=T),
                       Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                       Success_2b=sum(HbonTime_2b, na.rm=T),
                       Success_2bFalse=sum(HbonTime_2b==F, na.rm=T),
                       Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                       Success_2c=sum(HbonTime_2c, na.rm=T),
                       Success_2cFalse=sum(HbonTime_2c==F, na.rm=T),
                       Opportun_3=sum(Opportunity_anemia_screening_3, na.rm=T),
                       Success_3a=sum(HbonTime_3a, na.rm=T),
                       Success_3b=sum(HbonTime_3b, na.rm=T),
                       Success_3bFales=sum(HbonTime_3b==FALSE, na.rm=T),
                       Success_3c=sum(HbonTime_3c, na.rm=T),
                       Sucess_3cFalse=sum(HbonTime_3c==F, na.rm=T),
                       Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                       Success_4a=sum(HbonTime_4a, na.rm=T),
                       Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                       Success_4b=sum(HbonTime_4b, na.rm=T),
                       Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                       Success_4c=sum(HbonTime_4c, na.rm=T),
                       Opportun_5=sum(Opportunity_anemia_screening_5, na.rm=T),
                       Success_5=sum(HbonTime_5, na.rm=T),
                      Opportun_6=sum(Opportunity_anemia_screening_6, na.rm=T),
                      Success_6=sum(HbonTime_6, na.rm=T)),
                    keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(prelimHB,file.path(FOLDER_DATA_RESULTS,
                                         "T1",
                                         sprintf("%s_prelim_Hb.xlsx",
                                                 lubridate::today()))) 
###### Anemia  data set  ###### 
HbSucc <- names(smallD)[stringr::str_detect(names(smallD),"^HbonTime_")]
HbOpp <- names(smallD)[stringr::str_detect(names(smallD),"Opportunity_anemia_")]

smallD[ident_dhis2_control==T, prettyExposure:="G"]
smallD[ident_dhis2_control==F, prettyExposure:="H"]
varskeep <- c(varskeepAll,
              varshb,
              varsman,
              HbSucc,
              HbOpp)
hb <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(hb,file.path(FOLDER_DATA_RESULTS,
                                  "T1",
                                  sprintf("%s_Anemia_Outcomes_dataset.xlsx", 
                                          lubridate::today())))


######################################################################################
######################################################################################
############################## ORIGINAL THOUGHT PROCESS ############################## 

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
smallD[bookgestagedays_cats %in% c("(216,237]"), Opportunity_anemia_screening:=1]

#booked 34_34
smallD[bookgestagedays_cats %in% c("(237,244]"), Opportunity_anemia_screening:=1]

#booked 35-37
smallD[bookgestagedays_cats %in% c("(244,265]"), Opportunity_anemia_screening:=1]

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

#hb on time 1, 2, 3, vars
#Screen at bookings before 24 weeks??
#check booklabhb values if normal etc
# before 15-17
smallD[bookgestagedays_cats %in% c("(0,104]") & 
         TrialOne_labhb_normal_00_14==T, HbonTime:=HbonTime+1]

smallD[HbonTime_1==F &
         bookgestagedays_cats %in% c("(0,104]") & 
         TrialOne_labhb_normal_00_14==T, HbonTime_1:=TRUE]


# booked 15-17
smallD[bookgestagedays_cats %in% c("(104,125]") & 
         (TrialOne_labhb_normal_14_14==T|
            TrialOne_labhb_normal_15_17==T), HbonTime:=HbonTime+1]

smallD[HbonTime_1==F &
         bookgestagedays_cats %in% c("(0,104]") & 
         TrialOne_labhb_normal_14_14==T |
         TrialOne_labhb_normal_15_17==T, HbonTime_1:=TRUE]

#booked 18-22
smallD[bookgestagedays_cats %in% c( "(125,160]" ) &
         (TrialOne_labhb_normal_18_22==T), HbonTime:=HbonTime+1]
smallD[HbonTime_1==F &
         bookgestagedays_cats %in% c("(125,160]" ) & 
         TrialOne_labhb_normal_18_22==T, HbonTime_1:=TRUE]

#24-28 screenings
smallD[TrialOne_labhb_normal_23_23==T |
         TrialOne_labhb_normal_24_28==T, 
       HbonTime:=HbonTime+1]

smallD[HbonTime_2==F & 
         TrialOne_labhb_normal_23_23==T |
         TrialOne_labhb_normal_24_28==T, 
       HbonTime_2:=TRUE]

#booked 29-30, 31-33
smallD[bookgestagedays_cats %in% c("(202,216]","(216,237]" ) & 
         (TrialOne_labhb_normal_29_30==T|
            TrialOne_labhb_normal_31_33==T), 
       HbonTime:=HbonTime+1]

smallD[HbonTime_3==F & 
         (!is.na(bookhb) |TrialOne_labhb_normal_29_30==T)|
         (!is.na(booklabhb)|TrialOne_labhb_normal_31_33==T), 
       HbonTime_3:=TRUE]

#booked 34_34
smallD[bookgestagedays_cats %in% c("(237,244]") &
         TrialOne_labhb_normal_34_34==T, HbonTime:=HbonTime+1]



# 35-37 screenings
smallD[TrialOne_labhb_normal_35_37==T, HbonTime:=HbonTime+1]

smallD[HbonTime_3==F & TrialOne_labhb_normal_35_37==T, HbonTime_3:=TRUE]

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

#23-23
smallD[TrialOne_labhb_anemia_mild_mod_23_23==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_mild_mod_23_23==T &
         (TrialOne_manhb_mildmodhbret_26_26==T |
           TrialOne_manhb_mildmodhbret_27_27==T|
            TrialOne_manhb_mildmodhbret_28_28==T|
            TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]

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
            TrialOne_manhb_mildmodhbret_34_34==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]
#31-33
smallD[TrialOne_labhb_anemia_mild_mod_31_33==T,Opportunity_anemia_screening:= Opportunity_anemia_screening-1]
smallD[TrialOne_labhb_anemia_mild_mod_31_33==T &
         (TrialOne_manhb_mildmodhbret_34_34==T),
       Opportunity_anemia_screening:= Opportunity_anemia_screening+1]


#34-34

#35-37





