


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

#smallD <- d[bookyear>=2019]


# first opportunity
smallD[,Opp_1:= as.numeric(NA)]
smallD[T2_anT2visit_00_14==T,Opp_1:=1]
xtabs(~smallD$Opp_1)

# second opportunity
smallD[,Opp_2:=as.numeric(NA)]
smallD[T2_anT2visit_15_17==TRUE, Opp_2:=1]
xtabs(~smallD$Opp_2)


# third opportunity
smallD[,Opp_3:=as.numeric(NA)]
smallD[T2_anT2visit_18_22==T, Opp_3:=1]
xtabs(~smallD$Opp_3)


# fourth opportunity
smallD[,Opp_4:=as.numeric(NA)]
smallD[T2_anT2visit_24_28==T, Opp_4:=1]
xtabs(~smallD$Opp_4)


# fifth opportunity
smallD[,Opp_5:=as.numeric(NA)]
smallD[T2_anT2visit_31_33==T, Opp_5:=1]
xtabs(~smallD$Opp_5)


# sixth opportunity
smallD[,Opp_6:=as.numeric(NA)]
smallD[T2_anT2visit_35_37==T, Opp_6:=1]
xtabs(~smallD$Opp_6)


########

uglytable <- smallD[(ident_TRIAL_2_and_3==T &
                      !is.na(precovid)),.(
                    N=.N,
                    "Before 15 weeks Opportunity"=sum(Opp_1, na.rm=T),
                    "15-17 Week Opportunity"=sum(Opp_2, na.rm=T),
                    "18-22 Week Opportunity"=sum(Opp_3, na.rm=T),
                    "24-28 Week Opportunity"=sum(Opp_4, na.rm=T),
                    "31-33 Week Opportunity"=sum(Opp_5, na.rm=T),
                    "35-37 Week Opportunity"=sum(Opp_6, na.rm=T)),
                    
                    keyby=.(TrialArm, precovid)]



if(IS_GAZA==F){
openxlsx::write.xlsx(uglytable, 
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "outcomes",
                               sprintf(
                                 "BP_opportunities_(via Attendance)_%s_%s.xlsx",
                                 fileTag,lubridate::today())))

} else {
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(FOLDER_DATA_RESULTS_GAZA,
                                 "T2",
                                 "outcomes",
                                 sprintf(
                                   "BP_opportunities_(via Attendance)_%s_%s.xlsx",
                                   fileTag,lubridate::today())))
  
  
  
  
}




