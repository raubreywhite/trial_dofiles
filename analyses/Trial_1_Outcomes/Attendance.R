#################################  Attendance ################################
# making vars
smallD[,refHRhosp:= FALSE]
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
         TrialOne_manRef_Hosp_14_14==T),refHRhosp:=TRUE]
         
xtabs(~smallD$refHRhosp, addNA=T)

## Define Opportunities

# oppt 16 week visit
smallD[,Opp_1:= as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]") &
         refHRhosp==F,Opp_1:=1]
xtabs(~smallD$Opp_1, addNA=T)


# oppt 18-22 visit
smallD[,Opp_2:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(104,125]")| Opp_1==1, Opp_2:=1]

xtabs(~smallD$Opp_2, addNA=T)

  #removing opportunities
smallD[Opp_2==1 & 
         (TrialOne_manRef_HR_15_15==T|TrialOne_manRef_Hosp_15_15==T)|
         (TrialOne_manRef_HR_16_16==T|TrialOne_manRef_Hosp_16_16==T)|
         (TrialOne_manRef_HR_17_17==T|TrialOne_manRef_Hosp_17_17==T),
       Opp_2:=Opp_2-1]

xtabs(~smallD$Opp_2, addNA=T)


# 24-28 week visit
smallD[,Opp_3:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(125,160]",
                                   "(160,167]") | Opp_2==1, Opp_3:=1]

xtabs(~smallD$Opp_3, addNA=T)
         
 # removing opportunities
smallD[Opp_3==1 & ((TrialOne_manRef_HR_18_18==T|TrialOne_manRef_Hosp_18_18==T)|
                  (TrialOne_manRef_HR_19_19==T|TrialOne_manRef_Hosp_19_19==T)|
                  (TrialOne_manRef_HR_20_20==T|TrialOne_manRef_Hosp_20_20==T)|
                  (TrialOne_manRef_HR_21_21==T |TrialOne_manRef_Hosp_21_21==T)|
                  (TrialOne_manRef_HR_22_22==T|TrialOne_manRef_Hosp_22_22==T)|
                  (TrialOne_manRef_HR_23_23==T|TrialOne_manRef_Hosp_23_23==T)), 
                      Opp_3:=Opp_3-1]
xtabs(~smallD$Opp_3, addNA=T)



# 31-33 week visit
smallD[,Opp_4:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(160,167]",
                                   "(167,202]",
                                   "(202,216]")|Opp_3== 1, Opp_4:=1]

xtabs(~smallD$Opp_4, addNA=T)
 
# removing opportunities 
 smallD[Opp_4==1 &
         ((TrialOne_manRef_HR_24_24==T|TrialOne_manRef_Hosp_24_24==T)|
         (TrialOne_manRef_HR_25_25==T|TrialOne_manRef_Hosp_25_25==T)|
         (TrialOne_manRef_HR_26_26==T|TrialOne_manRef_Hosp_26_26==T)|
         (TrialOne_manRef_HR_27_27==T|TrialOne_manRef_Hosp_27_27==T)|
         (TrialOne_manRef_HR_28_28==T|TrialOne_manRef_Hosp_28_28==T)|
         (TrialOne_manRef_HR_29_29==T|TrialOne_manRef_Hosp_29_29==T)|
         (TrialOne_manRef_HR_30_30==T|TrialOne_manRef_Hosp_30_30==T)), 
              Opp_4:=Opp_4-1]
 
xtabs(~smallD$Opp_4, addNA=T)

# 35-37 week visit
smallD[,Opp_5:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(216,237]",
                                   "(237,244]") | Opp_4==1, Opp_5:=1]
xtabs(~smallD$Opp_5, addNA=T)

smallD[Opp_5==1 &
           ((TrialOne_manRef_HR_31_31==T|TrialOne_manRef_Hosp_31_31==T)|
           (TrialOne_manRef_HR_32_32==T|TrialOne_manRef_Hosp_32_32==T)|
           (TrialOne_manRef_HR_33_33==T|TrialOne_manRef_Hosp_33_33==T)|
           (TrialOne_manRef_HR_34_34==T|TrialOne_manRef_Hosp_34_34==T)), 
           Opp_5:=Opp_5-1]
xtabs(~smallD$Opp_5, addNA=T)




################ successes ##########
# 15-17 week visit
smallD[,Succ_1:=as.logical(NA)]
smallD[Opp_1==1, Succ_1:=FALSE]
smallD[Succ_1==F & 
        TrialOne_anvisitnew_15_17==T, Succ_1:=TRUE]

xtabs(~smallD$Succ_1, addNA=T)

# 18-22 week visit
smallD[,Succ_2:=as.logical(NA)]
smallD[Opp_2==1, Succ_2:=FALSE]
smallD[Succ_2==F & TrialOne_anvisitnew_18_22==T, Succ_2:=TRUE]

xtabs(~smallD$Succ_2, addNA=T)

# 24-28 week visit
smallD[,Succ_3:=as.logical(NA)]
smallD[Opp_3==1, Succ_3:=as.logical(FALSE)]
smallD[Succ_3==F & TrialOne_anvisitnew_24_28==T, Succ_3:=TRUE]

xtabs(~smallD$Succ_3, addNA=T)

# 31-33 week visit
smallD[,Succ_4:=as.logical(NA)]
smallD[Opp_4==1, Succ_4:=FALSE]
smallD[Succ_4==F & TrialOne_anvisitnew_31_33==T, Succ_4:=TRUE]

xtabs(~smallD$Succ_4, addNA=T)

# 35-37
smallD[,Succ_5:=as.logical(NA)]
smallD[Opp_5==1, Succ_5:=FALSE]
smallD[Succ_5==F & TrialOne_anvisitnew_35_37==T, Succ_5:=TRUE]

xtabs(~smallD$Succ_5, addNA=T)

prelimAtt <- smallD[,.(N=.N,
                       bookedb414=sum(bookgestagedays_cats=="(0,104]", na.rm = T),
                       ANC15_17Opps=sum(Opp_1,na.rm=T),
                       ANC15_17=sum(Succ_1, na.rm=T),
                       ANC15_17FALSE=sum(Succ_1==F, na.rm=T),
                       booked1515=sum(bookgestagedays_cats=="(104,125]", na.rm = T),
                       ANC18_22Opps=sum(Opp_2, na.rm=T),
                       ANC18_22=sum(Succ_2, na.rm=T),
                       ANC18_22FALSE=sum(Succ_2==F, na.rm=T),
                       booked1822=sum(bookgestagedays_cats=="(125,160]", na.rm = T),
                       booked2323=sum(bookgestagedays_cats=="(160,167]", na.rm = T),
                       ANC2428Opps=sum(!is.na(Opp_3), na.rm=T),
                       ANC24_28TRUE=sum(Succ_3, na.rm=T),
                       ANC24_28FALSE=sum(Succ_3==F, na.rm=T),
                       booked2428=sum(bookgestagedays_cats=="(167,202]", na.rm = T),
                       booked2930=sum(bookgestagedays_cats=="(202,216]", na.rm = T),
                       ANC31_33Opps=sum(Opp_4, na.rm=T),
                       ANC31_33=sum(Succ_4, na.rm=T),
                       ANC31_33FALSE=sum(Succ_4==F, na.rm=T),
                       Booked31_33=sum(bookgestagedays_cats=="(216,237]", na.rm = T),
                       Booked34_34=sum(bookgestagedays_cats=="(237,244]", na.rm = T),
                       ANC3537Opps=sum(Opp_5, na.rm=T),
                       ANC3537=sum(Succ_5, na.rm=T),
                       Booked35_37=sum(bookgestagedays_cats=="(244,265]", na.rm = T)),
                       
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