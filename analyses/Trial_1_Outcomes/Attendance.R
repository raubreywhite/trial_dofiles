#################################  Attendance ################################
# making vars
smallD[, refHrhosp:= as.logical(NA)]
smallD[,refHRhosp:=(TrialOne_manRef_HR_00_00==T|
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
          TrialOne_manRef_HR_14_14==T)|
         (TrialOne_manRef_Hosp_00_00==T|
         TrialOne_manRef_Hosp_01_01==T|
         TrialOne_manRef_Hosp_02_02==T|
         TrialOne_manRef_Hosp_03_03==T|
         TrialOne_manRef_Hosp_04_04==T|
         TrialOne_manRef_Hosp_05_05==T|
         TrialOne_manRef_Hosp_06_06==T|
         TrialOne_manRef_Hosp_07_07==T|
         TrialOne_manRef_Hosp_08_08==T|
         TrialOne_manRef_Hosp_09_09==T|
         TrialOne_manRef_Hosp_10_10==T|
         TrialOne_manRef_Hosp_11_11==T|
         TrialOne_manRef_Hosp_12_12==T|
         TrialOne_manRef_Hosp_13_13==T|
         TrialOne_manRef_Hosp_14_14==T)
         ]

## Define Opportunities

# before 15-17
smallD[,Opp_1:= as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]"),Opp_1:=1]

# booked 15-17
smallD[,Opp_2:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(104,125]") &
         refHRhosp==F, Opp_2:=1]


#booked 18-22
smallD[,Opp_3:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(125,160]") &
         refHRhosp==F &
         TrialOne_manRef_HR_15_15==F &
         TrialOne_manRef_Hosp_15_15==F &
         TrialOne_manRef_HR_16_16==F &
         TrialOne_manRef_Hosp_16_16==F &
         TrialOne_manRef_HR_17_17==F &
         TrialOne_manRef_Hosp_17_17==F, Opp_3:=1]



#booked 23-23
smallD[,Opp_4:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(160,167]") &
         refHRhosp==F &
         TrialOne_manRef_HR_15_15==F &
         TrialOne_manRef_Hosp_15_15==F &
         TrialOne_manRef_HR_16_16==F &
         TrialOne_manRef_Hosp_16_16==F &
         TrialOne_manRef_HR_17_17==F &
         TrialOne_manRef_Hosp_17_17==F &
         TrialOne_manRef_HR_18_18==F &
         TrialOne_manRef_Hosp_18_18==F &
         TrialOne_manRef_HR_19_19==F &
         TrialOne_manRef_Hosp_19_19==F &
         TrialOne_manRef_HR_20_20==F &
         TrialOne_manRef_Hosp_20_20==F &
         TrialOne_manRef_HR_21_21==F &
         TrialOne_manRef_Hosp_21_21==F &
         TrialOne_manRef_HR_22_22==F &
         TrialOne_manRef_Hosp_22_22==F, Opp_4:=1]

#booked 24-28
smallD[,Opp_5:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(167,202]") &
         refHRhosp==F &
         TrialOne_manRef_HR_15_15==F &
         TrialOne_manRef_Hosp_15_15==F &
         TrialOne_manRef_HR_16_16==F &
         TrialOne_manRef_Hosp_16_16==F &
         TrialOne_manRef_HR_17_17==F &
         TrialOne_manRef_Hosp_17_17==F &
         TrialOne_manRef_HR_18_18==F &
         TrialOne_manRef_Hosp_18_18==F &
         TrialOne_manRef_HR_19_19==F &
         TrialOne_manRef_Hosp_19_19==F &
         TrialOne_manRef_HR_20_20==F &
         TrialOne_manRef_Hosp_20_20==F &
         TrialOne_manRef_HR_21_21==F &
         TrialOne_manRef_Hosp_21_21==F &
         TrialOne_manRef_HR_22_22==F &
         TrialOne_manRef_Hosp_22_22==F &
         TrialOne_manRef_HR_23_23 &
         TrialOne_manRef_Hosp_23_23, Opp_5:=1]



#booked 29-30
smallD[, Opp_6:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(202,216]") &
         refHRhosp==F &
         TrialOne_manRef_HR_15_15==F &
         TrialOne_manRef_Hosp_15_15==F &
         TrialOne_manRef_HR_16_16==F &
         TrialOne_manRef_Hosp_16_16==F &
         TrialOne_manRef_HR_17_17==F &
         TrialOne_manRef_Hosp_17_17==F &
         TrialOne_manRef_HR_18_18==F &
         TrialOne_manRef_Hosp_18_18==F &
         TrialOne_manRef_HR_19_19==F &
         TrialOne_manRef_Hosp_19_19==F &
         TrialOne_manRef_HR_20_20==F &
         TrialOne_manRef_Hosp_20_20==F &
         TrialOne_manRef_HR_21_21==F &
         TrialOne_manRef_Hosp_21_21==F &
         TrialOne_manRef_HR_22_22==F &
         TrialOne_manRef_Hosp_22_22==F &
         TrialOne_manRef_HR_23_23==F &
         TrialOne_manRef_Hosp_23_23==F &
         TrialOne_manRef_HR_24_24==F &
         TrialOne_manRef_Hosp_24_24==F &
         TrialOne_manRef_HR_25_25==F &
         TrialOne_manRef_Hosp_25_25==F &
         TrialOne_manRef_HR_26_26==F &
         TrialOne_manRef_Hosp_26_26==F &
         TrialOne_manRef_HR_27_27==F &
         TrialOne_manRef_Hosp_27_27==F &
         TrialOne_manRef_HR_28_28==F &
         TrialOne_manRef_Hosp_28_28==F, Opp_6:=1]

#booked 31-33
smallD[,Opp_7:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(216,237]") &
         refHRhosp==F &
         TrialOne_manRef_HR_15_15==F &
         TrialOne_manRef_Hosp_15_15==F &
         TrialOne_manRef_HR_16_16==F &
         TrialOne_manRef_Hosp_16_16==F &
         TrialOne_manRef_HR_17_17==F &
         TrialOne_manRef_Hosp_17_17==F &
         TrialOne_manRef_HR_18_18==F &
         TrialOne_manRef_Hosp_18_18==F &
         TrialOne_manRef_HR_19_19==F &
         TrialOne_manRef_Hosp_19_19==F &
         TrialOne_manRef_HR_20_20==F &
         TrialOne_manRef_Hosp_20_20==F &
         TrialOne_manRef_HR_21_21==F &
         TrialOne_manRef_Hosp_21_21==F &
         TrialOne_manRef_HR_22_22==F &
         TrialOne_manRef_Hosp_22_22==F &
         TrialOne_manRef_HR_23_23==F &
         TrialOne_manRef_Hosp_23_23==F &
         TrialOne_manRef_HR_24_24==F &
         TrialOne_manRef_Hosp_24_24==F &
         TrialOne_manRef_HR_25_25==F &
         TrialOne_manRef_Hosp_25_25==F &
         TrialOne_manRef_HR_26_26==F &
         TrialOne_manRef_Hosp_26_26==F &
         TrialOne_manRef_HR_27_27==F &
         TrialOne_manRef_Hosp_27_27==F &
         TrialOne_manRef_HR_28_28==F &
         TrialOne_manRef_Hosp_28_28==F&
         TrialOne_manRef_HR_29_29==F &
         TrialOne_manRef_Hosp_29_29==F&
         TrialOne_manRef_HR_30_30==F, Opp_7:=1]

####### up to here #####
# booked 34 weeks
smallD[,Opp_8:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(237,244]") &
         TrialOne_refHR_00_14==F &
         TrialOne_refHosp_00_14==F &
         TrialOne_refHR_15_17==F &
         TrialOne_refHosp_15_17==F &
         TrialOne_refHR_18_22==F &
         TrialOne_refHosp_18_22==F &
         TrialOne_refHR_18_22==F &
         TrialOne_refHosp_18_22==F &
         TrialOne_refHR_23_23==F &
         TrialOne_refHosp_23_23==F &
         TrialOne_refHR_24_28==F &
         TrialOne_refHosp_24_28==F &
         TrialOne_refHR_29_30==F &
         TrialOne_refHosp_29_30==F &
         TrialOne_refHR_31_33==F &
         TrialOne_refHosp_31_33==F, Opp_8:=1]


#booked 35-37
smallD[,Opp_9:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(244,265]") & 
       TrialOne_refHR_00_14==F &
         TrialOne_refHosp_00_14==F &
         TrialOne_refHR_15_17==F &
         TrialOne_refHosp_15_17==F &
         TrialOne_refHR_18_22==F &
         TrialOne_refHosp_18_22==F &
         TrialOne_refHR_18_22==F &
         TrialOne_refHosp_18_22==F &
         TrialOne_refHR_23_23==F &
         TrialOne_refHosp_23_23==F &
         TrialOne_refHR_24_28==F &
         TrialOne_refHosp_24_28==F &
         TrialOne_refHR_29_30==F &
         TrialOne_refHosp_29_30==F &
         TrialOne_refHR_31_33==F &
         TrialOne_refHosp_31_33==F &
         TrialOne_refHosp_34_34==F &
         TrialOne_refHR_34_34==F, Opp_9:=1]


################ successes ##########
# 15-17 week visit
smallD[,Succ_1:=as.logical(NA)]
smallD[Opp_1==1, Succ_1:=FALSE]
smallD[Succ_1==F & 
        TrialOne_anvisitnew_15_17==T, Succ_1:=TRUE]

# 18-22 week visit
smallD[,Succ_2:=as.logical(NA)]
smallD[Opp_1==1|Opp_2==1, Succ_2:=FALSE]
smallD[Succ_2==F & TrialOne_anvisitnew_18_22==T, Succ_2:=TRUE]

# 24-28 week visit
smallD[,Succ_3:=as.logical(NA)]
smallD[Opp_1==1|Opp_2==1|Opp_3==1|Opp_4==1, Succ_3:=as.logical(FALSE)]
smallD[Succ_3==F & TrialOne_anvisitnew_24_28==T, Succ_3:=TRUE]

# 31-33 week visit
smallD[,Succ_4:=as.logical(NA)]
smallD[Opp_1==1|Opp_2==1|Opp_3==1|Opp_4==1|Opp_5==1|Opp_6==1, Succ_4:=FALSE]
smallD[Succ_4==F & TrialOne_anvisitnew_31_33==T, Succ_4:=TRUE]

# 35-37
smallD[,Succ_5:=as.logical(NA)]
smallD[Opp_1==1|Opp_2==1|Opp_3==1|Opp_4==1|Opp_5==1|Opp_6==1|Opp_7==1, Succ_5:=FALSE]
smallD[Succ_5==F & TrialOne_anvisitnew_35_37==T, Succ_5:=TRUE]

prelimAtt <- smallD[,.(N=.N,
                       Bookby14=sum(Opp_1,na.rm=T),
                       ANC15_17=sum(Succ_1, na.rm=T),
                       ANC15_17Denom=sum(!is.na(Succ_1), na.rm=T),
                       Booked15_17=sum(Opp_2, na.rm=T),
                       ANC18_22=sum(Succ_2, na.rm=T),
                       ANC18_22Denom=sum(!is.na(Succ_2), na.rm=T),
                       Booked18_22=sum(Opp_3, na.rm=T),
                       Booked23_23=sum(Opp_4, na.rm=T),
                       ANC24_28=sum(Succ_3, na.rm=T),
                       ANC24_28Denom=sum(!is.na(Succ_3), na.rm=T),
                       Booked24_28=sum(Opp_5, na.rm=T),
                       Booked29_30=sum(Opp_6, na.rm=T),
                       ANC31_33=sum(Succ_4, na.rm=T),
                       ANC31_33Denom=sum(!is.na(Succ_4), na.rm=T),
                       Booked31_33=sum(Opp_7, na.rm=T),
                       Booked34_34=sum(Opp_8, na.rm=T),
                       ANC35_37=sum(Succ_5, na.rm=T),
                       ANC35_37Denom=sum(!is.na(Succ_5), na.rm=T),
                       Booked35_37=sum(Opp_9, na.rm=T)),
                    keyby=.(ident_dhis2_control)]

openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS,
                                        "T1",
                                        sprintf("%s_prelim_Attendance.xlsx",
                                                lubridate::today()))) 

###### Attendance data set  ######
AttSucc <- names(smallD)[stringr::str_detect(names(smallD),"^Succ_")]
AttOpp <- names(smallD)[stringr::str_detect(names(smallD),"^Opp_")]

smallD[ident_dhis2_control==F, prettyExposure:="E"]
smallD[ident_dhis2_control==T, prettyExposure:="F"]
varskeep <- c(varskeepAll,AttOpp,AttSucc)
attendance <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(attendance,file.path(FOLDER_DATA_RESULTS,
                                          "T1",
                                          sprintf("%s_Attendance_outcomes.xlsx", 
                                                  lubridate::today())))

################################## OLD CODE ################################## 
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