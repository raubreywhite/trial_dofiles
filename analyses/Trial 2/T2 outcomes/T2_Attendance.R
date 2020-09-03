


#################################  Attendance ################################

# Load in smallD dataset
# if(IS_GAZA==F){
#   
#   # Load in data set
#   fread("C:/data processing/data_clean/T2/WB/T2_dataset_%s_%s.rds", encoding="UTF-8")
#   fileTag <- "WB"
# }else{
#   
#   fileTag <-"GAZA"
#  }

# making vars
smallD[,refHRhosp:= FALSE]
smallD[(T2_manRef_HR_00_00==T|
          T2_manRef_HR_01_01==T|
          T2_manRef_HR_02_02==T|
          T2_manRef_HR_03_03==T|
          T2_manRef_HR_04_04==T|
          T2_manRef_HR_05_05==T|
          T2_manRef_HR_06_06==T|
          T2_manRef_HR_07_07==T|
          T2_manRef_HR_08_08==T|
          T2_manRef_HR_09_09==T|
          T2_manRef_HR_10_10==T|
          T2_manRef_HR_11_11==T|
          T2_manRef_HR_12_12==T|
          T2_manRef_HR_13_13==T|
          T2_manRef_HR_14_14==T)|
         (T2_manRef_Hosp_00_00==T|
            T2_manRef_Hosp_01_01==T|
            T2_manRef_Hosp_02_02==T|
            T2_manRef_Hosp_03_03==T|
            T2_manRef_Hosp_04_04==T|
            T2_manRef_Hosp_05_05==T|
            T2_manRef_Hosp_06_06==T|
            T2_manRef_Hosp_07_07==T|
            T2_manRef_Hosp_08_08==T|
            T2_manRef_Hosp_09_09==T|
            T2_manRef_Hosp_10_10==T|
            T2_manRef_Hosp_11_11==T|
            T2_manRef_Hosp_12_12==T|
            T2_manRef_Hosp_13_13==T|
            T2_manRef_Hosp_14_14==T),refHRhosp:=TRUE]

xtabs(~smallD$refHRhosp, addNA=T)

## Define Opportunities

# oppt 16 week visit
smallD[,Opp_1:= as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]"),Opp_1:=1]
smallD[bookgestagedays_cats %in% c("(0,104]") &
         refHRhosp==T,Opp_1:=0]
xtabs(~smallD$Opp_1, addNA=T)



# oppt 18-22 visit
smallD[,Opp_2:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(104,125]")| Opp_1==1, Opp_2:=1]

xtabs(~smallD$Opp_2, addNA=T)

#removing opportunities
smallD[Opp_2==1 & 
         (T2_manRef_HR_15_15==T|T2_manRef_Hosp_15_15==T)|
         (T2_manRef_HR_16_16==T|T2_manRef_Hosp_16_16==T)|
         (T2_manRef_HR_17_17==T|T2_manRef_Hosp_17_17==T),
       Opp_2:=Opp_2-1]

xtabs(~smallD$Opp_2, addNA=T)


# 24-28 week visit
smallD[,Opp_3:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(125,160]",
                                   "(160,167]") | Opp_2==1, Opp_3:=1]

xtabs(~smallD$Opp_3, addNA=T)

# removing opportunities
smallD[Opp_3==1 & ((T2_manRef_HR_18_18==T|T2_manRef_Hosp_18_18==T)|
                     (T2_manRef_HR_19_19==T|T2_manRef_Hosp_19_19==T)|
                     (T2_manRef_HR_20_20==T|T2_manRef_Hosp_20_20==T)|
                     (T2_manRef_HR_21_21==T |T2_manRef_Hosp_21_21==T)|
                     (T2_manRef_HR_22_22==T|T2_manRef_Hosp_22_22==T)|
                     (T2_manRef_HR_23_23==T|T2_manRef_Hosp_23_23==T)), 
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
         ((T2_manRef_HR_24_24==T|T2_manRef_Hosp_24_24==T)|
            (T2_manRef_HR_25_25==T|T2_manRef_Hosp_25_25==T)|
            (T2_manRef_HR_26_26==T|T2_manRef_Hosp_26_26==T)|
            (T2_manRef_HR_27_27==T|T2_manRef_Hosp_27_27==T)|
            (T2_manRef_HR_28_28==T|T2_manRef_Hosp_28_28==T)|
            (T2_manRef_HR_29_29==T|T2_manRef_Hosp_29_29==T)|
            (T2_manRef_HR_30_30==T|T2_manRef_Hosp_30_30==T)), 
       Opp_4:=Opp_4-1]

xtabs(~smallD$Opp_4, addNA=T)

# 35-37 week visit
smallD[,Opp_5:=as.numeric(NA)]
smallD[bookgestagedays_cats %in% c("(216,237]",
                                   "(237,244]") | Opp_4==1, Opp_5:=1]
xtabs(~smallD$Opp_5, addNA=T)

smallD[Opp_5==1 &
         ((T2_manRef_HR_31_31==T|T2_manRef_Hosp_31_31==T)|
            (T2_manRef_HR_32_32==T|T2_manRef_Hosp_32_32==T)|
            (T2_manRef_HR_33_33==T|T2_manRef_Hosp_33_33==T)|
            (T2_manRef_HR_34_34==T|T2_manRef_Hosp_34_34==T)), 
       Opp_5:=Opp_5-1]
xtabs(~smallD$Opp_5, addNA=T)




################ successes ##########
# 15-17 week visit
smallD[,Succ_1:=as.logical(NA)]
smallD[Opp_1==1, Succ_1:=FALSE]
smallD[Succ_1==F & 
         T2_anvisitnew_15_17==T, Succ_1:=TRUE]

xtabs(~smallD$Succ_1, addNA=T)

# 18-22 week visit
smallD[,Succ_2:=as.logical(NA)]
smallD[Opp_2==1, Succ_2:=FALSE]
smallD[Succ_2==F & T2_anvisitnew_18_22==T, Succ_2:=TRUE]

xtabs(~smallD$Succ_2, addNA=T)

# 24-28 week visit
smallD[,Succ_3:=as.logical(NA)]
smallD[Opp_3==1, Succ_3:=as.logical(FALSE)]
smallD[Succ_3==F & T2_anvisitnew_24_28==T, Succ_3:=TRUE]

xtabs(~smallD$Succ_3, addNA=T)

# 31-33 week visit
smallD[,Succ_4:=as.logical(NA)]
smallD[Opp_4==1, Succ_4:=FALSE]
smallD[Succ_4==F & T2_anvisitnew_31_33==T, Succ_4:=TRUE]

xtabs(~smallD$Succ_4, addNA=T)

# 35-37
smallD[,Succ_5:=as.logical(NA)]
smallD[Opp_5==1, Succ_5:=FALSE]
smallD[Succ_5==F & T2_anvisitnew_35_37==T, Succ_5:=TRUE]

xtabs(~smallD$Succ_5, addNA=T)

prelimAtt <- smallD[,.(Total_Num_Women_Booked=.N,
                       bookedb414=sum(bookgestagedays_cats=="(0,104]", na.rm = T),
                       ANC15_17_Opportunities=sum(Opp_1,na.rm=T),
                       ANC15_17_Attended=sum(Succ_1, na.rm=T),
                       ANC15_17_NotAttended=sum(Succ_1==F, na.rm=T),
                      
                       ANC18_2_Opportunities=sum(Opp_2, na.rm=T),
                       ANC18_22_Attended=sum(Succ_2, na.rm=T),
                       ANC18_22_NotAttended=sum(Succ_2==F, na.rm=T),
                       booked_18_22_weeks=sum(bookgestagedays_cats=="(125,160]", na.rm = T),
                       booked_23_23_weeks=sum(bookgestagedays_cats=="(160,167]", na.rm = T),
                       ANC24_28_Opportunities=sum(!is.na(Opp_3), na.rm=T),
                       ANC24_28_Attended=sum(Succ_3, na.rm=T),
                       ANC24_28_NotAttended=sum(Succ_3==F, na.rm=T),
                       booked24_28_weeks=sum(bookgestagedays_cats=="(167,202]", 
                                             na.rm = T),
                       booked2930=sum(bookgestagedays_cats=="(202,216]", 
                                      na.rm = T),
                       ANC31_33_Opportunities=sum(Opp_4, na.rm=T),
                       ANC31_33_Attended=sum(Succ_4, na.rm=T),
                       ANC31_33_NotAttended=sum(Succ_4==F, na.rm=T),
                       Booked31_33_weeks=sum(bookgestagedays_cats=="(216,237]",
                                             na.rm = T),
                       Booked34_34_weeks=sum(bookgestagedays_cats=="(237,244]", 
                                             na.rm = T),
                       ANC35_37_Opportunities=sum(Opp_5, na.rm=T),
                       ANC35_37_Attended=sum(Succ_5, na.rm=T),
                       Booked35_37_weeks=sum(bookgestagedays_cats=="(244,265]", 
                                             na.rm = T)),
                    
                    keyby=.(prettyExposure,
                            str_TRIAL_2_Cluster,
                            str_TRIAL_2_ClusSize,
                            bookyearmonth)]


if(IS_GAZA==F){

openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS,
                                         "T2",
                                         "outcomes",
                                         sprintf("%s_prelim_Attendance_Clusters_%s.xlsx",
                                                 lubridate::today(),fileTag)))
  
} else {
  
  
  openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS_GAZA,
                                           "T2",
                                           "outcomes",
                                           sprintf("%s_prelim_Attendance_%s.xlsx",
                                                   lubridate::today(),fileTag)))
  
  
  }







###### Attendance data set  ######
AttSucc <- names(smallD)[stringr::str_detect(names(smallD),"^Succ_")]
AttOpp <- names(smallD)[stringr::str_detect(names(smallD),"^Opp_")]

#smallD[ident_dhis2_control==F, prettyExposure:="E"]
#smallD[ident_dhis2_control==T, prettyExposure:="F"]
varskeep <- c(varskeepAll,AttOpp,AttSucc)
attendance <-smallD[,varskeep,with=F]

openxlsx::write.xlsx(attendance,file.path(FOLDER_DATA_RESULTS,
                                          "T1",
                                          sprintf("%s_Attendance_outcomes.xlsx", 
                                                  lubridate::today())))





######## Variables for Power calculation for QID and SMS ######## 
# visits 
# pregnancies
# opportunities

# averages across Jan-Mar 2020 *2
# average across July-Aug 15, 2020 * 4
# by arm (control, sms, qid)



powercalcvars <- smallD[,.(numpreg=sum(ident_dhis2_booking),
                           opportunities=sum())]

