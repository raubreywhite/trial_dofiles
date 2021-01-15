### Need to import smallD
#check this code and how to read it in despite the date
#anemia <- fread("C:/data processing/data_clean/Trial_1_Outcomes/"Anemia.xlsx",encoding="UTF#-8"")

## number of rows, 
# check management dates and risk data and other dates for other dates


T2 <- smallD[!is.na(firstvisitinT2) & !is.na(TrialArm)]
nrow(T2)



################
# 15-17 weeks

# anyone booked before 23 weeks
################

# need to remove anyone who had anemia prior to 24-28 weeks
# 00-23 weeks
T2[,denom_anemia_00_23:=as.logical(NA)]
T2[(T2_anvisitnew_00_14==T & T2_labhb_anemia_mild_mod_00_14==F & T2_labhb_anemia_sev_00_14==F) |
      (T2_anvisitnew_15_17==T & T2_labhb_anemia_mild_mod_15_17==F & T2_labhb_anemia_sev_15_17==F)|
      (T2_anvisitnew_18_22==T & T2_labhb_anemia_mild_mod_18_22==F & T2_labhb_anemia_sev_18_22==F)|
      (T2_anvisitnew_23_23==T & T2_labhb_anemia_mild_mod_23_23==F & T2_labhb_anemia_sev_23_23==F), 
                    denom_anemia_00_23:=TRUE ]

xtabs(~T2$denom_anemia_00_23, addNA=T)

# 00-23
T2[,num_anemia_00_23:=as.logical(NA)]
T2[denom_anemia_00_23==T,num_anemia_00_23:=FALSE]
T2[num_anemia_15_17==F &
     (T2_labhb_exists_00_14==T |
        T2_labhb_exists_15_17==T|
        T2_labhb_exists_18_22==T |
        T2_labhb_exists_23_23==T),num_anemia_00_23:=TRUE]

xtabs(~T2$num_anemia_00_23, addNA=T)


###########
#management
###########

T2[,manmildmodanemia_00_23:=as.logical(NA)]
T2[denom_anemia_00_23==T & 
     (T2_labhb_anemia_mild_mod_15_17==T|
        T2_labhb_anemia_mild_mod_00_14==T|
        T2_labhb_anemia_mild_mod_18_22==T|
        T2_labhb_anemia_mild_mod_23_23==T),manmildmodanemia_00_23:=F]


T2[manmildmodanemia_00_23==F &
     (T2_riskMildModAne_00_14==T|
        T2_riskMildModAne_15_17==T|
        T2_riskMildModAne_18_22==T|
        T2_riskMildModAne_23_23==T) &
     (T2_manhb_mildmodhbret_03_03==T|
        T2_manhb_mildmodhbret_04_04==T|
        T2_manhb_mildmodhbret_05_05==T|
        T2_manhb_mildmodhbret_06_06==T|
        T2_manhb_mildmodhbret_07_07==T|
        T2_manhb_mildmodhbret_08_08==T|
        T2_manhb_mildmodhbret_09_09==T|
        T2_manhb_mildmodhbret_10_10==T|
        T2_manhb_mildmodhbret_11_11==T|
        T2_manhb_mildmodhbret_12_12==T|
        T2_manhb_mildmodhbret_13_13==T|
        T2_manhb_mildmodhbret_14_14==T|
        T2_manhb_mildmodhbret_15_15==T|
        T2_manhb_mildmodhbret_16_16==T|
        T2_manhb_mildmodhbret_17_17==T|
        T2_manhb_mildmodhbret_18_18==T|
        T2_manhb_mildmodhbret_19_19==T|
        T2_manhb_mildmodhbret_20_20==T|
        T2_manhb_mildmodhbret_21_21==T|
        T2_manhb_mildmodhbret_22_22==T|
        T2_manhb_mildmodhbret_23_23==T|
        T2_manhb_mildmodhbret_24_24==T|
        T2_manhb_mildmodhbret_25_25==T|
        T2_manhb_mildmodhbret_26_26==T|
        T2_manhb_mildmodhbret_27_27==T|
        T2_manhb_mildmodhbret_28_28==T),manmildmodanemia_00_23:=T]

xtabs(~T2$manmildmodanemia_00_23, addNA=T)


# 15-17 weeks severe anemia
T2[,mansevanemia_00_23:=as.logical(NA)]
T2[denom_anemia_00_23==F & 
     (T2_labhb_anemia_sev_15_17==T|
        T2_labhb_anemia_sev_00_14==T|
        T2_labhb_anemia_sev_18_22==T|
        T2_labhb_anemia_sev_23_23==T),mansevanemia_00_23:=F]
xtabs(~T2$mansevanemia_00_23, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_00_23==F &
     (T2_manhb_03_03==T |
        T2_manhb_04_04==T |
        T2_manhb_05_05==T |
        T2_manhb_06_06==T |
        T2_manhb_07_07==T |
        T2_manhb_08_08==T |
        T2_manhb_09_09==T |
        T2_manhb_10_10==T |
        T2_manhb_11_11==T |
        T2_manhb_12_12==T |
        T2_manhb_13_13==T |
        T2_manhb_14_14==T |
        T2_manhb_15_15==T |
        T2_manhb_16_16==T |
        T2_manhb_17_17==T |
        T2_manhb_18_18==T |
        T2_manhb_19_19==T |
        T2_manhb_20_20==T |
        T2_manhb_21_21==T |
        T2_manhb_22_22==T |
        T2_manhb_23_23==T |
        T2_manhb_24_24==T),mansevanemia_00_23:=T]

xtabs(~T2$mansevanemia_00_23)


# anyone who booked before 23 weeks and was screened, managed, etc

################
#24-28 weeks
################

# 24-28 weeks
T2[,denom_anemia_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T & (T2_labhb_anemia_mild_mod_00_14==F|
                               T2_labhb_anemia_mild_mod_15_17==F|
                               T2_labhb_anemia_mild_mod_18_22==F|
                               T2_labhb_anemia_mild_mod_23_23==F|
                               T2_labhb_anemia_sev_00_14==F |
                               T2_labhb_anemia_sev_15_17==F |
                               T2_labhb_anemia_sev_18_22==F |
                               T2_labhb_anemia_sev_23_23==F), denom_anemia_24_28:=TRUE ]

xtabs(~T2$denom_anemia_24_28)

# 24-28
T2[,num_anemia_24_28:=as.logical(NA)]
T2[denom_anemia_24_28==T,num_anemia_24_28:=FALSE]
T2[num_anemia_24_28==F &
     T2_labhb_exists_24_28==T,num_anemia_24_28:=TRUE]

xtabs(~T2$num_anemia_24_28, addNA=T)


##########
#management
##########
T2[,manmildmodanemia_24_28:=as.logical(NA)]
T2[denom_anemia_24_28==T & T2_labhb_anemia_mild_mod_24_28==T,manmildmodanemia_24_28:=F]


T2[manmildmodanemia_24_28==F &
     T2_riskMildModAne_24_28==T &
     (T2_manhb_mildmodhbret_24_24==T|
        T2_manhb_mildmodhbret_25_25==T|
        T2_manhb_mildmodhbret_26_26==T|
        T2_manhb_mildmodhbret_27_27==T|
        T2_manhb_mildmodhbret_28_28==T|
        T2_manhb_mildmodhbret_29_29==T|
        T2_manhb_mildmodhbret_30_30==T|
        T2_manhb_mildmodhbret_31_31==T),manmildmodanemia_24_28:=T]

xtabs(~T2$manmildmodanemia_24_28, addNA=T)


# 24-28 weeks severe anemia
T2[,mansevanemia_24_28:=as.logical(NA)]
T2[denom_anemia_24_28==F & 
     T2_labhb_anemia_sev_24_28==T,mansevanemia_24_28:=F]
xtabs(~T2$mansevanemia_24_28, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_24_28==F &
     (T2_manhb_24_24==T |
        T2_manhb_25_25==T |
        T2_manhb_26_26==T |
        T2_manhb_27_27==T |
        T2_manhb_28_28==T |
        T2_manhb_29_29==T),mansevanemia_24_28:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$mansevanemia_24_28)


################
#29-34 weeks
################


#denom
T2[,denom_anemia_29_34:=as.logical(NA)]

T2[(T2_anvisitnew_29_30==T|
      T2_anvisitnew_31_33==T|
      T2_anvisitnew_34_34==T) &
      (T2_labhb_anemia_mild_mod_00_14==F|
                               T2_labhb_anemia_mild_mod_15_17==F|
                               T2_labhb_anemia_mild_mod_18_22==F|
                               T2_labhb_anemia_mild_mod_23_23==F|
                               T2_labhb_anemia_mild_mod_24_28==F|
                               T2_labhb_anemia_mild_mod_29_30==F|
                               T2_labhb_anemia_mild_mod_31_33==F|
                               T2_labhb_anemia_mild_mod_34_34==F|
                               T2_labhb_anemia_sev_00_14==F |
                               T2_labhb_anemia_sev_15_17==F |
                               T2_labhb_anemia_sev_18_22==F |
                               T2_labhb_anemia_sev_23_23==F|
                               T2_labhb_anemia_sev_24_28==F |
                               T2_labhb_anemia_sev_29_30==F |
                               T2_labhb_anemia_sev_31_33==F |
                               T2_labhb_anemia_sev_34_34==F), denom_anemia_29_34:=TRUE ]

xtabs(~T2$denom_anemia_29_34, addNA=T)

# 29-34
T2[,num_anemia_29_34:=as.logical(NA)]
T2[denom_anemia_29_34==T,num_anemia_29_34:=FALSE]
T2[num_anemia_29_34==F &
     T2_labhb_exists_31_33==T|
     T2_labhb_exists_29_30==T|
     T2_labhb_exists_34_34==T,num_anemia_29_34:=TRUE]

xtabs(~T2$num_anemia_29_34, addNA=T)


# 35_37 weeks severe anemia
T2[,mansevanemia_29_34:=as.logical(NA)]
T2[denom_anemia_29_34==F & 
     (T2_labhb_anemia_sev_35_37==T),mansevanemia_29_34:=F]
xtabs(~T2$mansevanemia_29_34, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_35_37==F &
     (T2_manhb_35_35==T),mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$mansevanemia_35_37)


##########
#management
##########


# 29-34 weeks severe anemia
T2[,mansevanemia_35_37:=as.logical(NA)]
T2[denom_anemia_35_37==F & 
     T2_labhb_anemia_sev_35_37==T,mansevanemia_35_37:=F]
xtabs(~T2$mansevanemia_35_37, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_35_37==F &
     (T2_manhb_35_35==T |
        T2_manhb_36_36==T |
        T2_manhb_37_37==T |
        T2_manhb_38_38==T),mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$mansevanemia_35_37)


################
#35-37 weeks
################

#denom
T2[,denom_anemia_35_37:=as.logical(NA)]

T2[T2_anvisitnew_35_37==T & (T2_labhb_anemia_mild_mod_00_14==F|
                               T2_labhb_anemia_mild_mod_15_17==F|
                               T2_labhb_anemia_mild_mod_18_22==F|
                               T2_labhb_anemia_mild_mod_23_23==F|
                               T2_labhb_anemia_mild_mod_24_28==F|
                               T2_labhb_anemia_mild_mod_29_30==F|
                               T2_labhb_anemia_mild_mod_31_33==F|
                               T2_labhb_anemia_mild_mod_34_34==F|
                               T2_labhb_anemia_sev_00_14==F |
                               T2_labhb_anemia_sev_15_17==F |
                               T2_labhb_anemia_sev_18_22==F |
                               T2_labhb_anemia_sev_23_23==F|
                               T2_labhb_anemia_sev_24_28==F |
                               T2_labhb_anemia_sev_29_30==F |
                               T2_labhb_anemia_sev_31_33==F |
                               T2_labhb_anemia_sev_34_34==F), denom_anemia_35_37:=TRUE ]

xtabs(~T2$denom_anemia_35_37, addNA=T)

# 35-37
T2[,num_anemia_35_37:=as.logical(NA)]
T2[denom_anemia_35_37==T,num_anemia_35_37:=FALSE]
T2[num_anemia_35_37==F &
     T2_labhb_exists_35_37==T,num_anemia_35_37:=TRUE]

xtabs(~T2$num_anemia_35_37, addNA=T)


# 35_37 weeks severe anemia
T2[,mansevanemia_35_37:=as.logical(NA)]
T2[denom_anemia_35_37==F & 
     T2_labhb_anemia_sev_35_37==T,mansevanemia_35_37:=F]
xtabs(~T2$mansevanemia_35_37, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_35_37==F &
     (T2_manhb_35_35==T),mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$mansevanemia_35_37)


##########
#management
##########


# 35-37 weeks severe anemia
T2[,mansevanemia_35_37:=as.logical(NA)]
T2[denom_anemia_35_37==F & 
     T2_labhb_anemia_sev_35_37==T,mansevanemia_35_37:=F]
xtabs(~T2$mansevanemia_35_37, addNA=T)

# should probably use manhb variable here
T2[mansevanemia_35_37==F &
     (T2_manhb_35_35==T |
        T2_manhb_36_36==T |
        T2_manhb_37_37==T |
        T2_manhb_38_38==T),mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$mansevanemia_35_37)



prelimHb <-T2[,.(N=.N,
                 Num_15_17_T=sum(num_anemia_15_17==T, na.rm=T),
                 Num_15_17_F=sum(num_anemia_15_17==F, na.rm=T),
                 Denom_15_17=sum(denom_anemia_15_17==T, na.rm=T),
                 ManMildMod_15_17_T=sum(manmildmodanemia_15_17==T, na.rm=T),
                 ManMildMod_15_17_F=sum(manmildmodanemia_15_17==F, na.rm=T),
                 ManSevAnemia_15_17_T=sum(mansevanemia_15_17==T, na.rm=T),
                 ManSevAnemia_15_17_F=sum(mansevanemia_15_17==F, na.rm=T),
                 Num_24_28_T=sum(num_anemia_24_28==T, na.rm=T),
                 Num_24_28_F=sum(num_anemia_24_28==F, na.rm=T),
                 Denom_24_28=sum(denom_anemia_24_28==T, na.rm=T),
                 ManMildMod_24_28_T=sum(manmildmodanemia_24_28==T, na.rm=T),
                 ManMildMod_24_28_F=sum(manmildmodanemia_24_28==F, na.rm=T),
                 ManSevAnemia_24_28_T=sum(mansevanemia_24_28==T, na.rm=T),
                 ManSevAnemia_24_28_F=sum(mansevanemia_24_28==F, na.rm=T),
                 Num_35_37_T=sum(num_anemia_35_37==T, na.rm=T),
                 Num_35_37_F=sum(num_anemia_35_37==F, na.rm=T),
                 Denom_35_37=sum(denom_anemia_35_37==T, na.rm=T),
                 ManSevAnemia_35_37_T=sum(mansevanemia_35_37==T, na.rm=T),
                 ManSevAnemia_35_37_F=sum(mansevanemia_35_37==F, na.rm=T)),
              keyby=.(TrialArm)]



if(IS_GAZA==T){
  
  openxlsx::write.xlsx(prelimHb, file.path(FOLDER_DATA_RESULTS_GAZA,
                                           "T2",
                                           "outcomes",
                                           "anemia.xlsx"))
} else{
  openxlsx::write.xlsx(prelimHb, file.path(FOLDER_DATA_RESULTS,
                                           "T2",
                                           "outcomes",
                                           "anemia.xlsx"))
  
}


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




