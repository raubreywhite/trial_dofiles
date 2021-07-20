
# SMS and QID outcomes

# create new data set to work on this 


 # adjust this so we read in these files

# adjust this so we read in these files

if(IS_GAZA==F){  
  ww <- readRDS(file.path(FOLDER_DATA_CLEAN,
                          "T2_clean",
                          sprintf("T2_complete_dataset_%s_WB.rds",CLINIC_INTERVENTION_DATE)))
  

  
  
} else {
  
  
  
  ww <- readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                          "T2_clean",
                          sprintf("T2_complete_dataset_%s_Gaza.rds",CLINIC_INTERVENTION_DATE)))
  
  
  
}



TT2 <- ww

###################################################################################################
                                            # Anemia #
###################################################################################################

# remove those with documented anemia, remove from denominator so it can carry through

################
#24-28 weeks
################

# 24-28 weeks
TT2[,T2_qidsms_Oppt_anemia_24_28:=as.logical(NA)]
TT2[denom_24_28==T, T2_qidsms_Oppt_anemia_24_28:=FALSE]
xtabs(~TT2$T2_qidsms_Oppt_anemia_24_28,addNA=T)

# do we need to add a statement about no anemia prior? In the other scripts, 
#there was a statement about being screened on time from 00-23 weeks with no anemia. 

TT2[num_24_28==T & 
      T2_qidsms_Oppt_anemia_24_28==F, T2_qidsms_Oppt_anemia_24_28:=TRUE]

xtabs(~TT2$T2_qidsms_Oppt_anemia_24_28)

# 24-28
TT2[,T2_qidsms_screeniningontime_anemia_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_anemia_24_28==T,T2_qidsms_screeniningontime_anemia_24_28:=FALSE]
TT2[T2_qidsms_screeniningontime_anemia_24_28==F &
     T2_labhb_exists_24_28==T,T2_qidsms_screeniningontime_anemia_24_28:=TRUE]

xtabs(~TT2$T2_qidsms_screeniningontime_anemia_24_28, addNA=T)


# no anemia 24- 28 weeks

TT2[,T2_qidsms_screeniningontime_no_anemia_24_28:=as.logical(NA)]
TT2[T2_qidsms_screeniningontime_anemia_24_28==TRUE, T2_qidsms_screeniningontime_no_anemia_24_28:=FALSE]
TT2[T2_qidsms_screeniningontime_no_anemia_24_28==F &
     T2_labhb_normal_24_28==T, 
    T2_qidsms_screeniningontime_no_anemia_24_28:=TRUE]

xtabs(~TT2$T2_qidsms_screeniningontime_no_anemia_24_28, addNA=T)


##########
#management
##########

TT2[,T2_qidsms_manmildmodanemia_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_anemia_24_28==T & 
      T2_riskMildModAne_24_28==T,T2_qidsms_manmildmodanemia_24_28:=F]
xtabs(~TT2$T2_qidsms_manmildmodanemia_24_28, addNA=T)

TT2[T2_qidsms_manmildmodanemia_24_28==F &
     (T2_manhb_mildmodhbret_27_27==T|
        T2_manhb_mildmodhbret_28_28==T|
        T2_manhb_mildmodhbret_29_29==T|
        T2_manhb_mildmodhbret_30_30==T|
        T2_manhb_mildmodhbret_31_31==T),T2_qidsms_manmildmodanemia_24_28:=T]

xtabs(~TT2$T2_qidsms_manmildmodanemia_24_28, addNA=T)


# 24-28 weeks severe anemia
TT2[,T2_qidsms_mansevanemia_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_anemia_24_28==T & 
     T2_riskSevAne_24_28==T,T2_qidsms_mansevanemia_24_28:=F]
xtabs(~TT2$T2_qidsms_mansevanemia_24_28, addNA=T)

# should probably use manhb variable here
TT2[T2_qidsms_mansevanemia_24_28==F &
     (T2_manhb_24_24==T |
        T2_manhb_25_25==T |
        T2_manhb_26_26==T |
        T2_manhb_27_27==T |
        T2_manhb_28_28==T),T2_qidsms_mansevanemia_24_28:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~TT2$T2_qidsms_mansevanemia_24_28)



################
#35-37 weeks
################

# same comment as in 24-28 weeks about restricting what happened before, do we need to remove everyone with sev and mod anemia prior? and does it have to be appropriate
#35-37 weeks
TT2[,T2_qidsms_Oppt_anemia_35_37:=as.logical(NA)]
TT2[denom_35_37==T, T2_qidsms_Oppt_anemia_35_37:=F]
TT2[num_35_37==T &
      T2_qidsms_Oppt_anemia_35_37==F,
    T2_qidsms_Oppt_anemia_35_37:=TRUE ]

xtabs(~TT2$T2_qidsms_Oppt_anemia_35_37)

# 35_37
TT2[,T2_qidsms_screeniningontime_anemia_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_anemia_35_37==T,T2_qidsms_screeniningontime_anemia_35_37:=FALSE]
TT2[T2_qidsms_screeniningontime_anemia_35_37==F &
     T2_labhb_exists_35_37==T,T2_qidsms_screeniningontime_anemia_35_37:=TRUE]

xtabs(~TT2$T2_qidsms_screeniningontime_anemia_35_37, addNA=T)


##########
#management
##########

# 35_37 weeks severe anemia
TT2[,T2_qidsms_mansevanemia_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_anemia_35_37==T & 
     T2_riskSevAne_35_37==T,T2_qidsms_mansevanemia_35_37:=F]
xtabs(~TT2$T2_qidsms_mansevanemia_35_37, addNA=T)

# should probably use manhb variable here
TT2[T2_qidsms_mansevanemia_35_37==F &
     (T2_manhb_35_35==T |
        T2_manhb_36_36==T |
        T2_manhb_37_37==T),T2_qidsms_mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~TT2$T2_qidsms_mansevanemia_35_37, addNA=T)



###################################################################################################
                                              # HTN #
###################################################################################################

################
#15-17 weeks
################
# T2_anvisitnew: take opportunity and success from the attendance script


#screening
TT2[,T2_qidsms_Oppt_bp_15_17:=as.logical(NA)]
TT2[denom_15_17==T,T2_qidsms_Oppt_bp_15_17:=FALSE]
xtabs(~TT2$T2_qidsms_Oppt_bp_15_17, addNA=T)

# denominator scheduled and visited ==T
# denominator scheduled and not visited==F
TT2[num_15_17==T &
     T2_qidsms_Oppt_bp_15_17==F, T2_qidsms_Oppt_bp_15_17:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_15_17,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_15_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==TRUE,T2_qidsms_bpontime_15_17:=FALSE]
TT2[T2_anbpsyst_present_15_17==T &
     T2_anbpdiast_present_15_17==T &
      T2_qidsms_bpontime_15_17==F,T2_qidsms_bpontime_15_17:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_15_17)


################
#15-17 weeks
################
#management
#T2_Oppt
TT2[,T2_qidsms_manchronichtn_15_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_15_17==T|
                            T2_anbpsyst_mildHTN_15_17==T|
                            T2_anbpsyst_modSevHTN_15_17==T|
                            T2_anbpdiast_modSevHTN_15_17==T),T2_qidsms_manchronichtn_15_17:=F]

xtabs(~TT2$T2_qidsms_manchronichtn_15_17, addNA=T)

# add the rest of the values for refHR
TT2[T2_qidsms_manchronichtn_15_17==F & (T2_refHR_15_17==T|
                                  T2_refHosp_15_17==T|
                                  T2_refSpec_15_17==T),T2_qidsms_manchronichtn_15_17:=T]

xtabs(~TT2$T2_qidsms_manchronichtn_15_17, addNA=T)


################
#18-22 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_18_22:=as.logical(NA)]
TT2[denom_18_22==T,T2_qidsms_Oppt_bp_18_22:=FALSE ]
TT2[num_18_22==T &
      T2_qidsms_Oppt_bp_18_22==F & 
     is.na(T2_qidsms_manchronichtn_15_17),T2_qidsms_Oppt_bp_18_22:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_18_22,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_18_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==TRUE,T2_qidsms_bpontime_18_22:=FALSE]
TT2[T2_anbpsyst_present_18_22==T &
     T2_anbpdiast_present_18_22==T &
      T2_qidsms_bpontime_18_22==F,T2_qidsms_bpontime_18_22:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_18_22, addNA=T)


################
#18-22 weeks
################

#management
#T2_Oppt
TT2[,T2_qidsms_manchronichtn_18_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_18_22==T|
                            T2_anbpsyst_mildHTN_18_22==T|
                            T2_anbpsyst_modSevHTN_18_22==T|
                            T2_anbpdiast_modSevHTN_18_22==T),T2_qidsms_manchronichtn_18_22:=F]

xtabs(~TT2$T2_qidsms_manchronichtn_18_22, addNA=T)


TT2[T2_qidsms_manchronichtn_18_22==F & (T2_refHR_18_22==T|
                                  T2_refHosp_18_22==T|
                                  T2_refSpec_18_22==T),T2_qidsms_manchronichtn_18_22:=T]

xtabs(~TT2$T2_qidsms_manchronichtn_18_22, addNA=T)



################
#24-28 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_24_28:=as.logical(NA)]
TT2[denom_24_28==T,T2_qidsms_Oppt_bp_24_28:=FALSE]

TT2[num_24_28==T &
     is.na(T2_qidsms_manchronichtn_18_22),T2_qidsms_Oppt_bp_24_28:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_24_28,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==TRUE,T2_qidsms_bpontime_24_28:=FALSE]
TT2[T2_anbpsyst_present_24_28==T &
     T2_anbpdiast_present_24_28==T &
      T2_qidsms_bpontime_24_28==F,T2_qidsms_bpontime_24_28:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_24_28, addNA=T)


################
#24-28 weeks
################

#management
#T2_Oppt
TT2[,T2_qidsms_manmildhtn_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_24_28==T|
                            T2_anbpsyst_mildHTN_24_28==T),T2_qidsms_manmildhtn_24_28:=F]

xtabs(~TT2$T2_qidsms_manmildhtn_24_28, addNA=T)


TT2[T2_qidsms_manmildhtn_24_28==F &
     ((T2_anbpsyst_present_25_25==T & T2_anbpdiast_present_25_25==T)|
        (T2_anbpsyst_present_26_26==T & T2_anbpdiast_present_26_26==T)|
        (T2_anbpsyst_present_27_27==T & T2_anbpdiast_present_27_27==T )|
        (T2_anbpsyst_present_28_28==T & T2_anbpdiast_present_28_28==T) |
        (T2_anbpsyst_present_29_29==T & T2_anbpdiast_present_29_29==T)),T2_qidsms_manmildhtn_24_28:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_24_28, addNA=T)


# 24-28 weeks severe anemia
TT2[,T2_qidsms_manmodsevhtn_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_24_28==T|
                            T2_anbpsyst_modSevHTN_24_28==T),T2_qidsms_manmodsevhtn_24_28:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_24_28, addNA=T)

TT2[T2_qidsms_manmodsevhtn_24_28==F & 
     (T2_manhtn_ModSev_24_24==T |
        T2_manhtn_ModSev_25_25==T |
        T2_manhtn_ModSev_26_26==T |
        T2_manhtn_ModSev_27_27==T |
        T2_manhtn_ModSev_28_28==T),T2_qidsms_manmodsevhtn_24_28:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_24_28, addNA=T)



################
#31-33 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_31_33:=as.logical(NA)]

TT2[denom_31_33==T,T2_qidsms_Oppt_bp_31_33:=FALSE]

TT2[num_31_33==T &
     is.na(T2_qidsms_manmodsevhtn_24_28) & 
     is.na(T2_qidsms_manmildhtn_24_28) &
      T2_qidsms_Oppt_bp_31_33==FALSE,T2_qidsms_Oppt_bp_31_33:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_31_33,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_31_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==TRUE,T2_qidsms_bpontime_31_33:=FALSE]
TT2[T2_anbpsyst_present_31_33==T &
     T2_anbpdiast_present_31_33==T &
      T2_qidsms_bpontime_31_33==F,T2_qidsms_bpontime_31_33:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_31_33)


################
#31-33 weeks
################

#management
#T2_Oppt
TT2[,T2_qidsms_manmildhtn_31_33:=as.logical(NA)]
TT2[T2_qidsms_bpontime_31_33==T & (T2_anbpdiast_mildHTN_31_33==T|
                            T2_anbpsyst_mildHTN_31_33==T),T2_qidsms_manmildhtn_31_33:=F]

xtabs(~TT2$T2_qidsms_manmildhtn_31_33, addNA=T)


TT2[T2_qidsms_manmildhtn_31_33==F &
     ((T2_anbpsyst_present_31_31==T & T2_anbpdiast_present_34_34==T)|
        (T2_anbpsyst_present_32_32==T & T2_anbpdiast_present_32_32==T)|
        (T2_anbpsyst_present_33_33==T & T2_anbpdiast_present_33_33==T)),T2_qidsms_manmildhtn_31_33:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_31_33, addNA=T)


# 31-33 weeks severe hypertensioni
TT2[,T2_qidsms_manmodsevhtn_31_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_31_33==T|
                            T2_anbpsyst_modSevHTN_31_33==T),T2_qidsms_manmodsevhtn_31_33:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_31_33, addNA=T)

TT2[T2_qidsms_manmodsevhtn_31_33==F & 
     (T2_manhtn_ModSev_31_31==T |
        T2_manhtn_ModSev_32_32==T |
        T2_manhtn_ModSev_33_33==T),T2_qidsms_manmodsevhtn_31_33:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_31_33, addNA=T)




################
#35-37 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_35_37:=as.logical(NA)]
TT2[denom_35_37==T,T2_qidsms_Oppt_bp_35_37:=FALSE]
TT2[num_35_37==T &
     is.na(T2_qidsms_manmildhtn_31_33) &
     is.na(T2_qidsms_manmodsevhtn_31_33)&
      T2_qidsms_Oppt_bp_35_37==F,T2_qidsms_Oppt_bp_35_37:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_35_37,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==TRUE,T2_qidsms_bpontime_35_37:=FALSE]
TT2[T2_anbpsyst_present_35_37==T &
     T2_anbpdiast_present_35_37==T &
      T2_qidsms_bpontime_35_37==F,T2_qidsms_bpontime_35_37:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_35_37)


################
#35-37 weeks
################

#management
#T2_Oppt
TT2[,T2_qidsms_manmildhtn_35_37:=as.logical(NA)]
TT2[T2_qidsms_bpontime_35_37==T & 
      (T2_anbpdiast_mildHTN_35_37==T|
          T2_anbpsyst_mildHTN_35_37==T),T2_qidsms_manmildhtn_35_37:=FALSE]

xtabs(~TT2$T2_qidsms_manmildhtn_35_37, addNA=T)


TT2[T2_qidsms_manmildhtn_35_37==F &
     ((T2_anbpsyst_present_35_35==T & T2_anbpdiast_present_35_35==T)|
        (T2_anbpsyst_present_36_36==T & T2_anbpdiast_present_36_36==T)|
        (T2_anbpsyst_present_37_37==T & T2_anbpdiast_present_37_37==T)),T2_qidsms_manmildhtn_35_37:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_35_37, addNA=T)


# 35-37 weeks severe htn
TT2[,T2_qidsms_manmodsevhtn_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_35_37==T|
                            T2_anbpsyst_modSevHTN_35_37==T),T2_qidsms_manmodsevhtn_35_37:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_35_37, addNA=T)

TT2[T2_qidsms_manmodsevhtn_35_37==F & 
     (T2_manhtn_ModSev_35_35==T |
        T2_manhtn_ModSev_36_36==T |
        T2_manhtn_ModSev_37_37==T),T2_qidsms_manmodsevhtn_35_37:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_35_37, addNA=T)



#

###################################################################################################
                                             # GDM #
###################################################################################################
# who do we look at for these ? only the 24-28 week values?

#remove referrals for people who were referred for diabetes before 24 weeks?
# and do we have to remove those with high glucose values before or just those that were referred previously?
# TT2[T2_qidsms_Opportunity_GDM_screening_2==1 &
#      (T2_labbloodglu_high_00_14==T|
#         T2_labbloodglu_high_15_17==T|
#         T2_labbloodglu_high_18_22==T|
#         T2_labbloodglu_high_23_23==T|
#         T2_labfastbloodglu_high_00_14==T|
#         T2_labfastbloodglu_high_15_17==T|
#         T2_labfastbloodglu_high_18_22==T|
#         T2_labfastbloodglu_high_23_23==T),T2_qidsms_Opportunity_GDM_screening_2:=FALSE]

## Remove opportunities for people who were referred to HR or Hosp
#T2_RefHrHospmanRBG_1 rename to T2_RefHr
TT2[,T2_qidsms_RefHr:=as.logical(NA)]
TT2[((T2_manRef_HR_00_00==T|
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
       T2_manRef_HR_14_14==T|
       T2_manRef_HR_15_15==T|
       T2_manRef_HR_16_16==T|
       T2_manRef_HR_17_17==T|
       T2_manRef_HR_18_18==T|
       T2_manRef_HR_19_19==T|
       T2_manRef_HR_20_20==T|
       T2_manRef_HR_21_21==T|
       T2_manRef_HR_22_22==T|
       T2_manRef_HR_23_23==T)|
      (T2_manRBGHigh_Diab_00_00==T|
         T2_manRBGHigh_Diab_01_01==T|
         T2_manRBGHigh_Diab_02_02==T|
         T2_manRBGHigh_Diab_03_03==T|
         T2_manRBGHigh_Diab_04_04==T|
         T2_manRBGHigh_Diab_05_05==T|
         T2_manRBGHigh_Diab_06_06==T|
         T2_manRBGHigh_Diab_07_07==T|
         T2_manRBGHigh_Diab_08_08==T|
         T2_manRBGHigh_Diab_09_09==T|
         T2_manRBGHigh_Diab_10_10==T|
         T2_manRBGHigh_Diab_11_11==T|
         T2_manRBGHigh_Diab_12_12==T|
         T2_manRBGHigh_Diab_13_13==T|
         T2_manRBGHigh_Diab_14_14==T|
         T2_manRBGHigh_Diab_15_15==T|
         T2_manRBGHigh_Diab_16_16==T|
         T2_manRBGHigh_Diab_17_17==T|
         T2_manRBGHigh_Diab_18_18==T|
         T2_manRBGHigh_Diab_19_19==T|
         T2_manRBGHigh_Diab_20_20==T|
         T2_manRBGHigh_Diab_21_21==T|
         T2_manRBGHigh_Diab_22_22==T|
         T2_manRBGHigh_Diab_23_23==T)|
      (T2_manFBSHigh_Diab_00_00==T|
         T2_manFBSHigh_Diab_01_01==T|
         T2_manFBSHigh_Diab_02_02==T|
         T2_manFBSHigh_Diab_03_03==T|
         T2_manFBSHigh_Diab_04_04==T|
         T2_manFBSHigh_Diab_05_05==T|
         T2_manFBSHigh_Diab_06_06==T|
         T2_manFBSHigh_Diab_07_07==T|
         T2_manFBSHigh_Diab_08_08==T|
         T2_manFBSHigh_Diab_09_09==T|
         T2_manFBSHigh_Diab_10_10==T|
         T2_manFBSHigh_Diab_11_11==T|
         T2_manFBSHigh_Diab_12_12==T|
         T2_manFBSHigh_Diab_13_13==T|
         T2_manFBSHigh_Diab_14_14==T|
         T2_manFBSHigh_Diab_15_15==T|
         T2_manFBSHigh_Diab_16_16==T|
         T2_manFBSHigh_Diab_17_17==T|
         T2_manFBSHigh_Diab_18_18==T|
         T2_manFBSHigh_Diab_19_19==T|
         T2_manFBSHigh_Diab_20_20==T|
         T2_manFBSHigh_Diab_21_21==T|
         T2_manFBSHigh_Diab_22_22==T|
         T2_manFBSHigh_Diab_23_23==T)|
      (T2_manFBSHigh_spec_00_00==T|
         T2_manFBSHigh_spec_01_01==T|
         T2_manFBSHigh_spec_02_02==T|
         T2_manFBSHigh_spec_03_03==T|
         T2_manFBSHigh_spec_04_04==T|
         T2_manFBSHigh_spec_05_05==T|
         T2_manFBSHigh_spec_06_06==T|
         T2_manFBSHigh_spec_07_07==T|
         T2_manFBSHigh_spec_08_08==T|
         T2_manFBSHigh_spec_09_09==T|
         T2_manFBSHigh_spec_10_10==T|
         T2_manFBSHigh_spec_11_11==T|
         T2_manFBSHigh_spec_12_12==T|
         T2_manFBSHigh_spec_13_13==T|
         T2_manFBSHigh_spec_14_14==T|
         T2_manFBSHigh_spec_15_15==T|
         T2_manFBSHigh_spec_16_16==T|
         T2_manFBSHigh_spec_17_17==T|
         T2_manFBSHigh_spec_18_18==T|
         T2_manFBSHigh_spec_19_19==T|
         T2_manFBSHigh_spec_20_20==T|
         T2_manFBSHigh_spec_21_21==T|
         T2_manFBSHigh_spec_22_22==T|
         T2_manFBSHigh_spec_23_23==T)),T2_qidsms_RefHr:=TRUE]

xtabs(~TT2$T2_qidsms_RefHr, addNA=T)



################
# 24- 28  weeks
################

# denominator


TT2[,T2_qidsms_Opportunity_GDM_screening_24_28:=as.logical(NA)]

TT2[denom_24_28==T, T2_qidsms_Opportunity_GDM_screening_24_28:=FALSE]

TT2[T2_qidsms_Opportunity_GDM_screening_24_28==FALSE &
      is.na(T2_qidsms_RefHr) &
      num_24_28==T,T2_qidsms_Opportunity_GDM_screening_24_28:=TRUE] 

xtabs(~TT2$T2_qidsms_Opportunity_GDM_screening_24_28, addNA=T)



# numerator

if(IS_GAZA==F){
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28:=as.logical(NA)]
  TT2[T2_qidsms_Opportunity_GDM_screening_24_28==TRUE &
       ((T2_labbloodglu_exists_24_28=T|
           T2_labfastbloodglu_exists_24_28==T)), T2_qidsms_GDMscreeningontime_24_28:=F]
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28, addNA=T)
  
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28_normal:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28==F,T2_qidsms_GDMscreeningontime_24_28_normal:=F]
  
  TT2[T2_qidsms_GDMscreeningontime_24_28==F &
       (T2_labbloodglu_normal_24_28==T),T2_qidsms_GDMscreeningontime_24_28_normal:=TRUE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_normal, addNA=T)
  
  
  # high rbg at 24-28 weeks
  TT2[,T2_qidsms_GDMscreeningontime_24_28_high:=as.logical(NA)]
  
  #identified as high blood sugar and not managed yet = F
  # true value = managed
  TT2[T2_qidsms_GDMscreeningontime_24_28==F &
       (T2_labbloodglu_high_24_28==T|
          T2_labfastbloodglu_high_24_28==T),T2_qidsms_GDMscreeningontime_24_28_high:=FALSE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_high, addNA=T)
  
  
  # high value managed
  
  TT2[T2_qidsms_GDMscreeningontime_24_28_high==F &
       ((T2_manRBGHigh_Diab_24_24==T|
           T2_manRBGHigh_Diab_25_25==T|
           T2_manRBGHigh_Diab_26_26==T|
           T2_manRBGHigh_Diab_27_27==T|
           T2_manRBGHigh_Diab_28_28==T) |
          (T2_manFBSHigh_Diab_24_24==T|
             T2_manFBSHigh_Diab_25_25==T|
             T2_manFBSHigh_Diab_26_26==T|
             T2_manFBSHigh_Diab_27_27==T|
             T2_manFBSHigh_Diab_28_28==T)|
          (T2_manRef_HR_24_24==T|
             T2_manRef_HR_25_25==T|
             T2_manRef_HR_26_26==T|
             T2_manRef_HR_27_27==T|
             T2_manRef_HR_28_28==T)|
          (T2_manRef_spec_24_24==T|
             T2_manRef_spec_25_25==T|
             T2_manRef_spec_26_26==T|
             T2_manRef_spec_27_27==T|
             T2_manRef_spec_28_28==T)), T2_qidsms_GDMscreeningontime_24_28_high:=T]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_high, addNA=T)
  
  
  # intermediate values,  but dont want them for WB because management is in free text
  TT2[,T2_qidsms_GDMscreeningontime_24_28_intmd:=as.logical(NA)]
  
  
  
} else {
  
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28:=as.logical(NA)]
  TT2[T2_qidsms_Opportunity_GDM_screening_24_28==TRUE &
       (T2_labfastbloodglu_exists_24_28==T|
          T2_labbloodglu_exists_24_28==T), T2_qidsms_GDMscreeningontime_24_28:=F]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28, addNA=T)
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28_normal:=as.logical(NA)]
  
  
  TT2[T2_qidsms_GDMscreeningontime_24_28==F &
       (T2_labfastbloodglu_exists_24_28==T|
          T2_labbloodglu_exists_24_28==T), T2_qidsms_GDMscreeningontime_24_28_normal:=F]
  
  TT2[T2_qidsms_Opportunity_GDM_screening_24_28==TRUE & 
        T2_qidsms_GDMscreeningontime_24_28_normal==F &
       (T2_labfastbloodglu_normal_24_28==T|
          T2_labbloodglu_normal_24_28==T),T2_qidsms_GDMscreeningontime_24_28_normal:=TRUE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_normal, addNA=T)
  
  
  
  # high rbg at 24-28 weeks
  TT2[,T2_qidsms_GDMscreeningontime_24_28_high:=as.logical(NA)]
  
  #identified as high blood sugar and not managed yet = F
  # true value = managed
  TT2[T2_qidsms_GDMscreeningontime_24_28==F &
        (T2_labbloodglu_high_24_28==T|
           T2_labfastbloodglu_high_24_28==T),T2_qidsms_GDMscreeningontime_24_28_high:=FALSE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_high, addNA=T)
  
  
  # high value managed
  
  TT2[T2_qidsms_GDMscreeningontime_24_28_high==F &
        ((T2_manRBGHigh_Diab_24_24==T|
            T2_manRBGHigh_Diab_25_25==T|
            T2_manRBGHigh_Diab_26_26==T|
            T2_manRBGHigh_Diab_27_27==T|
            T2_manRBGHigh_Diab_28_28==T) |
           (T2_manFBSHigh_Diab_24_24==T|
              T2_manFBSHigh_Diab_25_25==T|
              T2_manFBSHigh_Diab_26_26==T|
              T2_manFBSHigh_Diab_27_27==T|
              T2_manFBSHigh_Diab_28_28==T)|
           (T2_manRef_HR_24_24==T|
              T2_manRef_HR_25_25==T|
              T2_manRef_HR_26_26==T|
              T2_manRef_HR_27_27==T|
              T2_manRef_HR_28_28==T)|
           (T2_manRef_spec_24_24==T|
              T2_manRef_spec_25_25==T|
              T2_manRef_spec_26_26==T|
              T2_manRef_spec_27_27==T|
              T2_manRef_spec_28_28==T)), T2_qidsms_GDMscreeningontime_24_28_high:=T]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_high, addNA=T)
  
  # intermediate values
  # 
  TT2[,T2_qidsms_GDMscreeningontime_24_28_intmd:=as.logical(NA)]
  
  
  TT2[T2_qidsms_Opportunity_GDM_screening_24_28==TRUE &
        T2_qidsms_GDMscreeningontime_24_28==F &
       #is.na(T2_GDMscreeningontime_2) & 
       (T2_labfastbloodglu_likelyGDM_24_28==T |
          T2_labbloodglu_likelyGDM_24_28==T),T2_qidsms_GDMscreeningontime_24_28_intmd:=FALSE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_intmd, addNA=T)
  # managment is repeat FBS with in 3 weeks
  
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmd==F &
       (T2_repeatFBS_24_24==T|
          T2_repeatFBS_25_25==T|
          T2_repeatFBS_26_26==T|
          T2_repeatFBS_27_27==T|
          T2_repeatFBS_28_28==T|
          T2_repeatFBS_29_29==T|
          T2_repeatFBS_30_30==T|
          T2_repeatFBS_31_31==T),T2_qidsms_GDMscreeningontime_24_28_intmd:=T]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_intmd, addNA=T)
  
  
  
  
  
}

xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28, addNA = T)
xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_normal, addNA = T)
xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_high, addNA = T)
xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_intmd, addNA = T)



####################
# reassign values
####################


varsT2qidsms <- names(TT2)[stringr::str_detect(names(TT2),"T2_qidsms")]

for (i in varsT2qidsms){
  
  
  TT2[,(i):=as.character(get(i))]
  
  TT2[stringr::str_detect(get(i),"TRUE"), (i):="Successful"]
  TT2[stringr::str_detect(get(i),"FALSE"), (i):="Not Successful"]
  TT2[is.na(get(i)), (i):="Not Applicable"]
  
}



xtabs(~TT2$T2_qidsms_manmildhtn_35_37, addNA=T)


if(IS_GAZA==F){

                   saveRDS(TT2,
                       file.path(FOLDER_DATA_CLEAN,
                                 "T2_clean",
                                 sprintf("T2_FINAL_dataset_%s_WB.rds", 
                                         CLINIC_INTERVENTION_DATE)))
  
} else{
  
 
  
                    saveRDS(TT2,
                       file.path(FOLDER_DATA_CLEAN_GAZA,
                                 "T2_clean",
                                 sprintf("T2_FINAL_dataset_%s_Gaza.rds",
                                         CLINIC_INTERVENTION_DATE)))
}







































# ###################################################################################################
# # Attendance #
# ###################################################################################################
# 
# # anyone who was scheduled , then those who attended and werent referred to HR?
# 
# ####################
# # 15-17
# ####################
# 
# # referred before 15 weeks to remove from these visits
# 
# TT2[,refHRhosp_1:=as.logical(NA)]
# TT2[(T2_manRef_HR_00_00==T|
#        T2_manRef_HR_01_01==T|
#        T2_manRef_HR_02_02==T|
#        T2_manRef_HR_03_03==T|
#        T2_manRef_HR_04_04==T|
#        T2_manRef_HR_05_05==T|
#        T2_manRef_HR_06_06==T|
#        T2_manRef_HR_07_07==T|
#        T2_manRef_HR_08_08==T|
#        T2_manRef_HR_09_09==T|
#        T2_manRef_HR_10_10==T|
#        T2_manRef_HR_11_11==T|
#        T2_manRef_HR_12_12==T|
#        T2_manRef_HR_13_13==T|
#        T2_manRef_HR_14_14==T)|
#       (T2_manRef_Hosp_00_00==T|
#          T2_manRef_Hosp_01_01==T|
#          T2_manRef_Hosp_02_02==T|
#          T2_manRef_Hosp_03_03==T|
#          T2_manRef_Hosp_04_04==T|
#          T2_manRef_Hosp_05_05==T|
#          T2_manRef_Hosp_06_06==T|
#          T2_manRef_Hosp_07_07==T|
#          T2_manRef_Hosp_08_08==T|
#          T2_manRef_Hosp_09_09==T|
#          T2_manRef_Hosp_10_10==T|
#          T2_manRef_Hosp_11_11==T|
#          T2_manRef_Hosp_12_12==T|
#          T2_manRef_Hosp_13_13==T|
#          T2_manRef_Hosp_14_14==T),refHRhosp_1:=TRUE]
# # opportunity
# TT2[,T2_qidsms_Oppt_att_15_17:=as.logical(NA)]
# TT2[denom_15_17==T,T2_qidsms_Oppt_att_15_17:=FALSE]
# TT2[is.na(refHRhosp_1) &
#       T2_qidsms_Oppt_att_15_17==FALSE, 
#     T2_qidsms_Oppt_att_15_17:=TRUE]
# 
# # success
# TT2[,T2_qidsms_attontime_15_17:=as.logical(NA)]
# TT2[num_15_17==T,T2_qidsms_attontime_15_17:=TRUE]
# 
# 
# 
# 
# 
# ####################
# # 18-22
# ####################
# 
# TT2[,refHrhosp_2:=as.logical(NA)]
# TT2[(T2_manRef_HR_15_15==T | T2_manRef_Hosp_15_15==T) |
#       (T2_manRef_HR_16_16==T | T2_manRef_Hosp_16_16==T)|
#       (T2_manRef_HR_17_17==T | T2_manRef_Hosp_17_17==T), refHRhosp_2:=TRUE]
# 
# 
# # opportunity
# TT2[,T2_qidsms_Oppt_att_18_22:=as.logical(NA)]
# TT2[denom_18_22==T,T2_qidsms_Oppt_att_18_22:=FALSE]
# TT2[refHRhosp_2!=TRUE &
#       T2_qidsms_Oppt_att_18_22==FALSE,
#     T2_qidsms_Oppt_att_18_22:=TRUE]
# 
# # success
# TT2[,T2_qidsms_attontime_18_22:=as.logical(NA)]
# TT2[T2_qidsms_Oppt_att_18_22==T,T2_qidsms_attontime_18_22:=FALSE]
# TT2[num_18_22==T,T2_qidsms_attontime_18_22:=TRUE]
# 
# 
# 
# 
# ####################
# # 24-28
# ####################
# 
# 
# # opportunity
# TT2[,T2_qidsms_Oppt_att_24_28:=as.logical(NA)]
# TT2[denom_24_28==T,T2_qidsms_Oppt_att_24_28:=FALSE]
# TT2[(T2_manRef_HR_18_18!=T & T2_manRef_Hosp_18_18!=T) &
#       (T2_manRef_HR_19_19!=T & T2_manRef_Hosp_19_19!=T) &
#       (T2_manRef_HR_20_20!=T & T2_manRef_Hosp_20_20!=T) &
#       (T2_manRef_HR_21_21!=T & T2_manRef_Hosp_21_21!=T) &
#       (T2_manRef_HR_22_22!=T & T2_manRef_Hosp_22_22!=T) &
#       (T2_manRef_HR_23_23!=T & T2_manRef_Hosp_23_23!=T) &
#       T2_qidsms_Oppt_att_24_28==FALSE,
#     T2_qidsms_Oppt_att_24_28:=TRUE]
# 
# # success
# TT2[,T2_qidsms_attontime_24_28:=as.logical(NA)]
# TT2[T2_qidsms_Oppt_att_24_28==T,T2_qidsms_attontime_24_28:=FALSE]
# TT2[num_24_28==T,T2_qidsms_attontime_24_28:=TRUE]
# 
# 
# 
# 
# ####################
# # 31-33
# ####################
# 
# 
# # opportunity
# TT2[,T2_qidsms_Oppt_att_31_33:=as.logical(NA)]
# TT2[denom_31_33==T,T2_qidsms_Oppt_att_31_33:=FALSE]
# TT2[(T2_manRef_HR_18_18!=T & T2_manRef_Hosp_18_18!=T) &
#       (T2_manRef_HR_19_19!=T & T2_manRef_Hosp_19_19!=T) &
#       (T2_manRef_HR_20_20!=T & T2_manRef_Hosp_20_20!=T) &
#       (T2_manRef_HR_21_21!=T & T2_manRef_Hosp_21_21!=T) &
#       (T2_manRef_HR_22_22!=T & T2_manRef_Hosp_22_22!=T) &
#       (T2_manRef_HR_23_23!=T & T2_manRef_Hosp_23_23!=T) &
#       T2_qidsms_Oppt_att_31_33==FALSE,
#     T2_qidsms_Oppt_att_31_33:=TRUE]
# 
# # success
# TT2[,T2_qidsms_attontime_31_33:=as.logical(NA)]
# TT2[T2_qidsms_Oppt_att_31_33==T,T2_qidsms_attontime_31_33:=FALSE]
# TT2[num_31_33==T,T2_qidsms_attontime_31_33:=TRUE]
# 
# 
# 
# 
# ####################
# # 35-37
# ####################
# 
# 
# # opportunity
# TT2[,T2_qidsms_Oppt_att_35_37:=as.logical(NA)]
# TT2[denom_35_37==T,T2_qidsms_Oppt_att_35_37:=FALSE]
# TT2[(T2_manRef_HR_18_18!=T & T2_manRef_Hosp_18_18!=T) &
#       (T2_manRef_HR_19_19!=T & T2_manRef_Hosp_19_19!=T) &
#       (T2_manRef_HR_20_20!=T & T2_manRef_Hosp_20_20!=T) &
#       (T2_manRef_HR_21_21!=T & T2_manRef_Hosp_21_21!=T) &
#       (T2_manRef_HR_22_22!=T & T2_manRef_Hosp_22_22!=T) &
#       (T2_manRef_HR_23_23!=T & T2_manRef_Hosp_23_23!=T) &
#       T2_qidsms_Oppt_att_35_37==FALSE,
#     T2_qidsms_Oppt_att_35_37:=TRUE]
# 
# # success
# TT2[,T2_qidsms_attontime_35_37:=as.logical(NA)]
# TT2[T2_qidsms_Oppt_att_35_37==T,T2_qidsms_attontime_35_37:=FALSE]
# TT2[num_35_37==T,T2_qidsms_attontime_35_37:=TRUE]
# 
# 


