
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
TT2[T2_qidsms_screeniningontime_no_anemia_24_28==F & 
      T2_labhb_anemia_mild_mod_24_28==T,T2_qidsms_manmildmodanemia_24_28:=F]
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
TT2[T2_qidsms_screeniningontime_no_anemia_24_28==F & 
     T2_labhb_anemia_sev_24_28==T,T2_qidsms_mansevanemia_24_28:=F]
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


# no anemia

# 35_37
TT2[,T2_qidsms_screeniningontime_no_anemia_35_37:=as.logical(NA)]
TT2[T2_qidsms_screeniningontime_anemia_35_37==T,T2_qidsms_screeniningontime_no_anemia_35_37:=FALSE]
TT2[T2_qidsms_screeniningontime_no_anemia_35_37==F &
      T2_labhb_normal_35_37==T,T2_qidsms_screeniningontime_no_anemia_35_37:=TRUE]

xtabs(~TT2$T2_qidsms_screeniningontime_no_anemia_35_37, addNA=T)


##########
#management
##########

# 35_37 weeks severe anemia
TT2[,T2_qidsms_manmildmodanemia_35_37:=as.logical(NA)]
TT2[T2_qidsms_screeniningontime_no_anemia_35_37==F & 
      T2_labhb_anemia_mild_mod_35_37==T,T2_qidsms_manmildmodanemia_35_37:=F]
xtabs(~TT2$T2_qidsms_manmildmodanemia_35_37, addNA=T)

# should probably use manhb variable here
TT2[T2_qidsms_manmildmodanemia_35_37==F &
      (T2_manhb_mildmodhbret_38_38==T|
         T2_manhb_mildmodhbret_39_39==T|
         T2_manhb_mildmodhbret_40_40==T|
         T2_manhb_mildmodhbret_41_41==T),T2_qidsms_manmildmodanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~TT2$T2_qidsms_manmildmodanemia_35_37, addNA=T)






# 35_37 weeks severe anemia
TT2[,T2_qidsms_mansevanemia_35_37:=as.logical(NA)]
TT2[T2_qidsms_screeniningontime_no_anemia_35_37==F & 
     T2_labhb_anemia_sev_35_37==T,T2_qidsms_mansevanemia_35_37:=F]
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
# manag/combo
################

#management
#needs to be done week by week

# mild 15 weeks
TT2[,T2_qidsms_manmildchronichtn_15_15:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_15_15==T|
                            T2_anbpsyst_mildHTN_15_15==T),T2_qidsms_manmildchronichtn_15_15:=F]

TT2[T2_qidsms_manmildchronichtn_15_15==F & (T2_refHR_15_15==T|
                                      T2_refHosp_15_15==T|
                                      T2_refSpec_15_15==T),T2_qidsms_manmildchronichtn_15_15:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_15_15, addNA=T)

# mod/sev 15 weeks
TT2[,T2_qidsms_manmodsevchronichtn_15_15:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_15_15==T|
                            T2_anbpsyst_modSevHTN_15_15==T),T2_qidsms_manmodsevchronichtn_15_15:=F]

TT2[T2_qidsms_manmodsevchronichtn_15_15==F & (T2_refHR_15_15==T|
                                        T2_refHosp_15_15==T|
                                        T2_refSpec_15_15==T),T2_qidsms_manmodsevchronichtn_15_15:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_15_15, addNA=T)

# mild/mod/sev 16 weeks
TT2[,T2_qidsms_manmildchronichtn_16_16:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_16_16==T|
                            T2_anbpsyst_mildHTN_16_16==T),T2_qidsms_manmildchronichtn_16_16:=F]

TT2[T2_qidsms_manmildchronichtn_16_16==F & (T2_refHR_16_16==T|
                                      T2_refHosp_16_16==T|
                                      T2_refSpec_16_16==T),T2_qidsms_manmildchronichtn_16_16:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_16_16, addNA=T)

# mod/sev 15 weeks
TT2[,T2_qidsms_manmodsevchronichtn_16_16:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_16_16==T|
                            T2_anbpsyst_modSevHTN_16_16==T),T2_qidsms_manmodsevchronichtn_16_16:=F]

TT2[T2_qidsms_manmodsevchronichtn_16_16==F & (T2_refHR_16_16==T|
                                        T2_refHosp_16_16==T|
                                        T2_refSpec_16_16==T),T2_qidsms_manmodsevchronichtn_16_16:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_16_16, addNA=T)

# mild/mod/sev 17 weeks
TT2[,T2_qidsms_manmildchronichtn_17_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_17_17==T|
                            T2_anbpsyst_mildHTN_17_17==T),T2_qidsms_manmildchronichtn_17_17:=F]

TT2[T2_qidsms_manmildchronichtn_17_17==F & (T2_refHR_17_17==T|
                                      T2_refHosp_17_17==T|
                                      T2_refSpec_17_17==T),T2_qidsms_manmildchronichtn_17_17:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_17_17, addNA=T)

# mod/sev 17 weeks
TT2[,T2_qidsms_manmodsevchronichtn_17_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_17_17==T|
                            T2_anbpsyst_modSevHTN_17_17==T),T2_qidsms_manmodsevchronichtn_17_17:=F]

TT2[T2_qidsms_manmodsevchronichtn_17_17==F & (T2_refHR_17_17==T|
                                        T2_refHosp_17_17==T|
                                        T2_refSpec_17_17==T),T2_qidsms_manmodsevchronichtn_17_17:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_17_17, addNA=T)



################
# combined
################


#T2_manmildchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmildchronichtn_15_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (!is.na(T2_qidsms_manmildchronichtn_15_15)|
                            !is.na(T2_qidsms_manmildchronichtn_16_16)|
                            !is.na(T2_qidsms_manmildchronichtn_17_17)),
                        T2_qidsms_manmildchronichtn_15_17:=F]

xtabs(~TT2$T2_qidsms_manmildchronichtn_15_17, addNA=T)


TT2[T2_qidsms_manmildchronichtn_15_17==F & (T2_qidsms_manmildchronichtn_15_15==T|
                                      T2_qidsms_manmildchronichtn_16_16==T|
                                      T2_qidsms_manmildchronichtn_17_17==T),
                            T2_qidsms_manmildchronichtn_15_17:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_15_17, addNA=T)



# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmodsevchronichtn_15_17:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_15_17==T & (!is.na(T2_qidsms_manmodsevchronichtn_15_15)|
                            !is.na(T2_qidsms_manmodsevchronichtn_16_16)|
                            !is.na(T2_qidsms_manmodsevchronichtn_17_17)),
                T2_qidsms_manmodsevchronichtn_15_17:=F]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_15_17, addNA=T)


TT2[T2_qidsms_manmodsevchronichtn_15_17==F & (T2_qidsms_manmodsevchronichtn_15_15==T|
                                        T2_qidsms_manmodsevchronichtn_16_16==T|
                                        T2_qidsms_manmodsevchronichtn_17_17==T),
                    T2_qidsms_manmodsevchronichtn_15_17:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_15_17, addNA=T)



################
#18-22 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_18_22:=as.logical(NA)]
TT2[denom_18_22==T,T2_qidsms_Oppt_bp_18_22:=FALSE ]
TT2[num_18_22==T &
      T2_qidsms_Oppt_bp_18_22==F & 
     is.na(T2_qidsms_manmodsevchronichtn_15_17) &
      is.na(T2_qidsms_manmildchronichtn_15_17),T2_qidsms_Oppt_bp_18_22:=TRUE]
xtabs(~TT2$T2_qidsms_Oppt_bp_18_22,addNA=T)

# numerator
TT2[,T2_qidsms_bpontime_18_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==TRUE,T2_qidsms_bpontime_18_22:=FALSE]
TT2[T2_anbpsyst_present_18_22==T &
     T2_anbpdiast_present_18_22==T &
      T2_qidsms_bpontime_18_22==F,T2_qidsms_bpontime_18_22:=TRUE]

xtabs(~TT2$T2_qidsms_bpontime_18_22, addNA=T)


################
# mangment 18-22
################


#management

# mild 18 weeks
TT2[,T2_qidsms_manmildchronichtn_18_18:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_18_18==T|
                            T2_anbpsyst_mildHTN_18_18==T),T2_qidsms_manmildchronichtn_18_18:=F]

TT2[T2_qidsms_manmildchronichtn_18_18==F & (T2_anbpsyst_present_19_19==T &
                                      T2_anbpdiast_present_19_19==T),T2_qidsms_manmildchronichtn_18_18:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_18_18, addNA=T)

# mod/sev 18 weeks
TT2[,T2_qidsms_manmodsevchronichtn_18_18:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_18_18==T|
                            T2_anbpsyst_modSevHTN_18_18==T),T2_qidsms_manmodsevchronichtn_18_18:=F]

TT2[T2_qidsms_manmodsevchronichtn_18_18==F & (T2_refHR_18_18==T|
                                        T2_refHosp_18_18==T|
                                        T2_refSpec_18_18==T),T2_qidsms_manmodsevchronichtn_18_18:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_18_18, addNA=T)

# man 19 weeks
#mild 
TT2[,T2_qidsms_manmildchronichtn_19_19:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_19_19==T|
                            T2_anbpsyst_mildHTN_19_19==T),T2_qidsms_manmildchronichtn_19_19:=F]

TT2[T2_qidsms_manmildchronichtn_19_19==F & (T2_anbpsyst_present_20_20==T &
                                      T2_anbpdiast_present_20_20==T),T2_qidsms_manmildchronichtn_19_19:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_19_19, addNA=T)

# mod/sev 19 weeks
TT2[,T2_qidsms_manmodsevchronichtn_19_19:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_19_19==T|
                            T2_anbpsyst_modSevHTN_19_19==T),T2_qidsms_manmodsevchronichtn_19_19:=F]

TT2[T2_qidsms_manmodsevchronichtn_19_19==F & (T2_refHR_19_19==T|
                                        T2_refHosp_19_19==T|
                                        T2_refSpec_19_19==T),T2_qidsms_manmodsevchronichtn_19_19:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_19_19, addNA=T)

# man 20 weeks
#mild 20 weeks
TT2[,T2_qidsms_manmildchronichtn_20_20:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_20_20==T|
                            T2_anbpsyst_mildHTN_20_20==T),T2_qidsms_manmildchronichtn_20_20:=F]

TT2[T2_qidsms_manmildchronichtn_20_20==F & (T2_anbpsyst_present_21_21==T &
                                      T2_anbpdiast_present_21_21==T),T2_qidsms_manmildchronichtn_20_20:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_20_20, addNA=T)

# mod/sev 20 weeks
TT2[,T2_qidsms_manmodsevchronichtn_20_20:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_20_20==T|
                            T2_anbpsyst_modSevHTN_20_20==T),T2_qidsms_manmodsevchronichtn_20_20:=F]

TT2[T2_qidsms_manmodsevchronichtn_20_20==F & (T2_refHR_20_20==T|
                                        T2_refHosp_20_20==T|
                                        T2_refSpec_20_20==T),T2_qidsms_manmodsevchronichtn_20_20:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_20_20, addNA=T)

# man 21 weeks
#mild 21 weeks
TT2[,T2_qidsms_manmildchronichtn_21_21:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_21_21==T|
                            T2_anbpsyst_mildHTN_21_21==T),T2_qidsms_manmildchronichtn_21_21:=F]

TT2[T2_qidsms_manmildchronichtn_21_21==F & (T2_anbpsyst_present_22_22==T &
                                      T2_anbpdiast_present_22_22==T),T2_qidsms_manmildchronichtn_21_21:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_21_21, addNA=T)


# mod/sev 21 weeks
TT2[,T2_qidsms_manmodsevchronichtn_21_21:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_21_21==T|
                            T2_anbpsyst_modSevHTN_21_21==T),T2_qidsms_manmodsevchronichtn_21_21:=F]

TT2[T2_qidsms_manmodsevchronichtn_21_21==F & (T2_refHR_21_21==T|
                                        T2_refHosp_21_21==T|
                                        T2_refSpec_21_21==T),T2_qidsms_manmodsevchronichtn_21_21:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_21_21, addNA=T)

# man 22 weeks
#mild 22 weeks
TT2[,T2_qidsms_manmildchronichtn_22_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_22_22==T|
                            T2_anbpsyst_mildHTN_22_22==T),T2_qidsms_manmildchronichtn_22_22:=F]

TT2[T2_qidsms_manmildchronichtn_22_22==F & (T2_anbpsyst_present_23_23==T &
                                      T2_anbpdiast_present_23_23==T),T2_qidsms_manmildchronichtn_22_22:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_22_22, addNA=T)

#
TT2[,T2_qidsms_manmodsevchronichtn_22_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_22_22==T|
                            T2_anbpsyst_modSevHTN_22_22==T),T2_qidsms_manmodsevchronichtn_22_22:=F]

TT2[T2_qidsms_manmodsevchronichtn_22_22==F & (T2_refHR_22_22==T|
                                        T2_refHosp_22_22==T|
                                        T2_refSpec_22_22==T),T2_qidsms_manmodsevchronichtn_22_22:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_22_22, addNA=T)



################
# combined 18-22
################


#T2_qidsms_manmildchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmildchronichtn_18_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (!is.na(T2_qidsms_manmildchronichtn_18_18)|
                            !is.na(T2_qidsms_manmildchronichtn_19_19)|
                            !is.na(T2_qidsms_manmildchronichtn_20_20)|
                            !is.na(T2_qidsms_manmildchronichtn_21_21)|
                            !is.na(T2_qidsms_manmildchronichtn_22_22)),T2_qidsms_manmildchronichtn_18_22:=F]

xtabs(~TT2$T2_qidsms_manmildchronichtn_18_22, addNA=T)


TT2[T2_qidsms_manmildchronichtn_18_22==F & (T2_qidsms_manmildchronichtn_18_18==T|
                                      T2_qidsms_manmildchronichtn_19_19==T|
                                      T2_qidsms_manmildchronichtn_20_20==T|
                                      T2_qidsms_manmildchronichtn_21_21==T|
                                      T2_qidsms_manmildchronichtn_22_22==T),
   T2_qidsms_manmildchronichtn_18_22:=T]

xtabs(~TT2$T2_qidsms_manmildchronichtn_18_22, addNA=T)





# severe chronic htn

#T2_qidsms_manmodsevchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmodsevchronichtn_18_22:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_18_22==T & (!is.na(T2_qidsms_manmodsevchronichtn_18_18)|
                            !is.na(T2_qidsms_manmodsevchronichtn_19_19)|
                            !is.na(T2_qidsms_manmodsevchronichtn_20_20)|
                            !is.na(T2_qidsms_manmodsevchronichtn_21_21)|
                            !is.na(T2_qidsms_manmodsevchronichtn_22_22)),
              T2_qidsms_manmodsevchronichtn_18_22:=F]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_18_22, addNA=T)


TT2[T2_qidsms_manmodsevchronichtn_18_22==F & (T2_qidsms_manmodsevchronichtn_18_18==T|
                                        T2_qidsms_manmodsevchronichtn_19_19==T|
                                        T2_qidsms_manmodsevchronichtn_20_20==T|
                                        T2_qidsms_manmodsevchronichtn_21_21==T|
                                        T2_qidsms_manmodsevchronichtn_22_22==T),
                T2_qidsms_manmodsevchronichtn_18_22:=T]

xtabs(~TT2$T2_qidsms_manmodsevchronichtn_18_22, addNA=T)




################
#24-28 weeks
################

#screening
TT2[,T2_qidsms_Oppt_bp_24_28:=as.logical(NA)]
TT2[denom_24_28==T,T2_qidsms_Oppt_bp_24_28:=FALSE]

TT2[num_24_28==T &
     is.na(T2_qidsms_manmildchronichtn_18_22) &
      is.na(T2_qidsms_manmodsevchronichtn_18_22),T2_qidsms_Oppt_bp_24_28:=TRUE]
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
# 24 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_24_24:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_24_24==T|
                            T2_anbpsyst_mildHTN_24_24==T),T2_qidsms_manmildhtn_24_24:=F]

TT2[T2_qidsms_manmildhtn_24_24==F & (T2_anbpsyst_present_25_25==T &
                               T2_anbpdiast_present_25_25==T),T2_qidsms_manmildhtn_24_24:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_24_24, addNA=T)

# mod/sev  weeks
TT2[,T2_qidsms_manmodsevhtn_24_24:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_24_24==T|
                            T2_anbpsyst_modSevHTN_24_24==T),T2_qidsms_manmodsevhtn_24_24:=F]

TT2[T2_qidsms_manmodsevhtn_24_24==F & (T2_refHR_24_24==T|
                                 T2_refHosp_24_24==T|
                                 T2_refSpec_24_24==T),T2_qidsms_manmodsevhtn_24_24:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_24_24, addNA=T)

# 25 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_25_25:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_25_25==T|
                            T2_anbpsyst_mildHTN_25_25==T),T2_qidsms_manmildhtn_25_25:=F]

TT2[T2_qidsms_manmildhtn_25_25==F & (T2_anbpsyst_present_26_26==T &
                               T2_anbpdiast_present_26_26==T),T2_qidsms_manmildhtn_25_25:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_25_25, addNA=T)

# mod/sev weeks
TT2[,T2_qidsms_manmodsevhtn_25_25:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_25_25==T|
                            T2_anbpsyst_modSevHTN_25_25==T),T2_qidsms_manmodsevhtn_25_25:=F]

TT2[T2_qidsms_manmodsevhtn_25_25==F & (T2_refHR_25_25==T|
                                 T2_refHosp_25_25==T|
                                 T2_refSpec_25_25==T),T2_qidsms_manmodsevhtn_25_25:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_25_25, addNA=T)

# 26 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_26_26:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_26_26==T|
                            T2_anbpsyst_mildHTN_26_26==T),T2_qidsms_manmildhtn_26_26:=F]

TT2[T2_qidsms_manmildhtn_26_26==F & (!is.na(T2_anbpsyst_present_27_27) &
                               !is.na(T2_anbpdiast_present_27_27)),T2_qidsms_manmildhtn_26_26:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_26_26, addNA=T)


# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_26_26:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_26_26==T|
                            T2_anbpsyst_modSevHTN_26_26==T),T2_qidsms_manmodsevhtn_26_26:=F]

TT2[T2_qidsms_manmodsevhtn_26_26==F & (T2_refHR_26_26==T|
                                 T2_refHosp_26_26==T|
                                 T2_refSpec_26_26==T),T2_qidsms_manmodsevhtn_26_26:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_26_26, addNA=T)

# 27 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_27_27:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_27_27==T|
                            T2_anbpsyst_mildHTN_27_27==T),T2_qidsms_manmildhtn_27_27:=F]

TT2[T2_qidsms_manmildhtn_27_27==F & (T2_anbpsyst_present_28_28==T &
                               T2_anbpdiast_present_28_28==T),T2_qidsms_manmildhtn_27_27:=T]


xtabs(~TT2$T2_qidsms_manmildhtn_27_27, addNA=T)

# mod/sev 19 weeks
TT2[,T2_qidsms_manmodsevhtn_27_27:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_27_27==T|
                            T2_anbpsyst_modSevHTN_27_27==T),T2_qidsms_manmodsevhtn_27_27:=F]

TT2[T2_qidsms_manmodsevhtn_27_27==F & (T2_refHR_27_27==T|
                                 T2_refHosp_27_27==T|
                                 T2_refSpec_27_27==T),T2_qidsms_manmodsevhtn_27_27:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_27_27, addNA=T)


# 28 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_28_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_28_28==T|
                            T2_anbpsyst_mildHTN_28_28==T),T2_qidsms_manmildhtn_28_28:=F]

TT2[T2_qidsms_manmildhtn_28_28==F & (T2_anbpsyst_present_29_29==T &
                               T2_anbpdiast_present_29_29==T),T2_qidsms_manmildhtn_28_28:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_28_28, addNA=T)


# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_28_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_28_28==T|
                            T2_anbpsyst_modSevHTN_28_28==T),T2_qidsms_manmodsevhtn_28_28:=F]

TT2[T2_qidsms_manmodsevhtn_28_28==F & (T2_refHR_28_28==T|
                                 T2_refHosp_28_28==T|
                                 T2_refSpec_28_28==T),T2_qidsms_manmodsevhtn_28_28:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_28_28, addNA=T)



################
# combined 24-28
################


#T2_qidsms_manmildchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmildhtn_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (!is.na(T2_qidsms_manmildhtn_24_24)|
                            !is.na(T2_qidsms_manmildhtn_25_25)|
                            !is.na(T2_qidsms_manmildhtn_26_26)|
                            !is.na(T2_qidsms_manmildhtn_27_27)|
                            !is.na(T2_qidsms_manmildhtn_28_28)),T2_qidsms_manmildhtn_24_28:=F]

xtabs(~TT2$T2_qidsms_manmildhtn_24_28, addNA=T)


TT2[T2_qidsms_manmildhtn_24_28==F & (T2_qidsms_manmildhtn_24_24==T|
                               T2_qidsms_manmildhtn_25_25==T|
                               T2_qidsms_manmildhtn_26_26==T|
                               T2_qidsms_manmildhtn_27_27==T|
                               T2_qidsms_manmildhtn_28_28==T),T2_qidsms_manmildhtn_24_28:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_24_28, addNA=T)





# severe chronic htn

#T2_qidsms_manmodsevchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmodsevhtn_24_28:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_24_28==T & (!is.na(T2_qidsms_manmodsevhtn_24_24)|
                            !is.na(T2_qidsms_manmodsevhtn_25_25)|
                            !is.na(T2_qidsms_manmodsevhtn_26_26)|
                            !is.na(T2_qidsms_manmodsevhtn_27_27)|
                            !is.na(T2_qidsms_manmodsevhtn_28_28)),T2_qidsms_manmodsevhtn_24_28:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_24_28, addNA=T)


TT2[T2_qidsms_manmodsevhtn_24_28==F & (T2_qidsms_manmodsevhtn_24_24==T|
                                 T2_qidsms_manmodsevhtn_25_25==T|
                                 T2_qidsms_manmodsevhtn_26_26==T|
                                 T2_qidsms_manmodsevhtn_27_27==T|
                                 T2_qidsms_manmodsevhtn_28_28==T),T2_qidsms_manmodsevhtn_24_28:=T]

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
#31 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_31_31:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_31_31==T|
                                    T2_anbpsyst_mildHTN_31_31==T),T2_qidsms_manmildhtn_31_31:=F]

TT2[T2_qidsms_manmildhtn_31_31==F & (T2_anbpsyst_present_32_32==T &
                                       T2_anbpdiast_present_32_32==T),T2_qidsms_manmildhtn_31_31:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_31_31, addNA=T)

# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_31_31:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_31_31==T|
                                    T2_anbpsyst_modSevHTN_31_31==T),T2_qidsms_manmodsevhtn_31_31:=F]

TT2[T2_qidsms_manmodsevhtn_31_31==F & (T2_refHR_31_31==T|
                                         T2_refHosp_31_31==T|
                                         T2_refSpec_31_31==T),T2_qidsms_manmodsevhtn_31_31:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_31_31, addNA=T)

# 32 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_32_32:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_32_32==T|
                                    T2_anbpsyst_mildHTN_32_32==T),T2_qidsms_manmildhtn_32_32:=F]

TT2[T2_qidsms_manmildhtn_32_32==F & (T2_anbpsyst_present_33_33==T &
                                       T2_anbpdiast_present_33_33==T),T2_qidsms_manmildhtn_32_32:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_32_32, addNA=T)

# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_32_32:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_32_32==T|
                                    T2_anbpsyst_modSevHTN_32_32==T),T2_qidsms_manmodsevhtn_32_32:=F]

TT2[T2_qidsms_manmodsevhtn_32_32==F & (T2_refHR_32_32==T|
                                         T2_refHosp_32_32==T|
                                         T2_refSpec_32_32==T),T2_qidsms_manmodsevhtn_32_32:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_32_32, addNA=T)


# 33 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_33_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_33_33==T|
                                    T2_anbpsyst_mildHTN_33_33==T),T2_qidsms_manmildhtn_33_33:=F]

TT2[T2_qidsms_manmildhtn_33_33==F & (T2_anbpsyst_present_34_34==T &
                                       T2_anbpdiast_present_34_34==T),T2_qidsms_manmildhtn_33_33:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_33_33, addNA=T)


# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_33_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_33_33==T|
                                    T2_anbpsyst_modSevHTN_33_33==T),T2_qidsms_manmodsevhtn_33_33:=F]

TT2[T2_qidsms_manmodsevhtn_33_33==F & (T2_refHR_33_33==T|
                                         T2_refHosp_33_33==T|
                                         T2_refSpec_33_33==T),T2_qidsms_manmodsevhtn_33_33:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_33_33, addNA=T)


################
# combined 31-33
################


#T2_qidsms_manmildchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmildhtn_31_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (!is.na(T2_qidsms_manmildhtn_31_31)|
                                    !is.na(T2_qidsms_manmildhtn_32_32)|
                                    !is.na(T2_qidsms_manmildhtn_33_33)),T2_qidsms_manmildhtn_31_33:=F]

xtabs(~TT2$T2_qidsms_manmildhtn_31_33, addNA=T)


TT2[T2_qidsms_manmildhtn_31_33==F & (T2_qidsms_manmildhtn_31_31==T|
                                       T2_qidsms_manmildhtn_32_32==T|
                                       T2_qidsms_manmildhtn_33_33==T),T2_qidsms_manmildhtn_31_33:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_31_33, addNA=T)





# severe chronic htn

#T2_qidsms_manmodsevchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmodsevhtn_31_33:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_31_33==T & (!is.na(T2_qidsms_manmodsevhtn_31_31)|
                                    !is.na(T2_qidsms_manmodsevhtn_32_32)|
                                    !is.na(T2_qidsms_manmodsevhtn_33_33)),T2_qidsms_manmodsevhtn_31_33:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_31_33, addNA=T)


TT2[T2_qidsms_manmodsevhtn_31_33==F & (T2_qidsms_manmodsevhtn_31_31==T|
                                         T2_qidsms_manmodsevhtn_32_32==T|
                                         T2_qidsms_manmodsevhtn_33_33==T),T2_qidsms_manmodsevhtn_31_33:=T]

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
#35-37 weeks
#mild 
TT2[,T2_qidsms_manmildhtn_35_35:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_mildHTN_35_35==T|
                                    T2_anbpsyst_mildHTN_35_35==T),T2_qidsms_manmildhtn_35_35:=F]

TT2[T2_qidsms_manmildhtn_35_35==F & (T2_anbpsyst_present_36_36==T &
                                       T2_anbpdiast_present_36_36==T),T2_qidsms_manmildhtn_35_35:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_35_35, addNA=T)

# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_35_35:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_35_35==T|
                                    T2_anbpsyst_modSevHTN_35_35==T),T2_qidsms_manmodsevhtn_35_35:=F]

TT2[T2_qidsms_manmodsevhtn_35_35==F & (T2_refHR_35_35==T|
                                         T2_refHosp_35_35==T|
                                         T2_refSpec_35_35==T),T2_qidsms_manmodsevhtn_35_35:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_35_35, addNA=T)

# 36
#mild 
TT2[,T2_qidsms_manmildhtn_36_36:=as.logical(NA)]
TT2[T2_bpontime_35_37==T & (T2_anbpdiast_mildHTN_36_36==T|
                              T2_anbpsyst_mildHTN_36_36==T),T2_qidsms_manmildhtn_36_36:=F]

TT2[T2_qidsms_manmildhtn_36_36==F & (T2_anbpsyst_present_37_37==T &
                                       T2_anbpdiast_present_37_37==T),T2_qidsms_manmildhtn_36_36:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_36_36, addNA=T)


# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_36_36:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_36_36==T|
                                    T2_anbpsyst_modSevHTN_36_36==T),T2_qidsms_manmodsevhtn_36_36:=F]

TT2[T2_qidsms_manmodsevhtn_36_36==F & (T2_refHR_36_36==T|
                                         T2_refHosp_36_36==T|
                                         T2_refSpec_36_36==T),T2_qidsms_manmodsevhtn_36_36:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_36_36, addNA=T)



# 37
#mild 
TT2[,T2_qidsms_manmildhtn_37_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_mildHTN_37_37==T|
                                    T2_anbpsyst_mildHTN_37_37==T),T2_qidsms_manmildhtn_37_37:=F]

TT2[T2_qidsms_manmildhtn_37_37==F & (T2_anbpsyst_present_38_38==T &
                                       T2_anbpdiast_present_38_38==T),T2_qidsms_manmildhtn_37_37:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_37_37, addNA=T)

# mod/sev 
TT2[,T2_qidsms_manmodsevhtn_37_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_37_37==T|
                                    T2_anbpsyst_modSevHTN_37_37==T),T2_qidsms_manmodsevhtn_37_37:=F]

TT2[T2_qidsms_manmodsevhtn_37_37==F & (T2_refHR_37_37==T|
                                         T2_refHosp_37_37==T|
                                         T2_refSpec_37_37==T),T2_qidsms_manmodsevhtn_37_37:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_37_37, addNA=T)




################
# combined 35-37
################


#T2_qidsms_manmildchronichtn 35-37 weeks combo variable
TT2[,T2_qidsms_manmildhtn_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (!is.na(T2_qidsms_manmildhtn_35_35)|
                                    !is.na(T2_qidsms_manmildhtn_36_36)|
                                    !is.na(T2_qidsms_manmildhtn_37_37)),T2_qidsms_manmildhtn_35_37:=F]

xtabs(~TT2$T2_qidsms_manmildhtn_35_37, addNA=T)


TT2[T2_qidsms_manmildhtn_35_37==F & (T2_qidsms_manmildhtn_35_35==T|
                                       T2_qidsms_manmildhtn_36_36==T|
                                       T2_qidsms_manmildhtn_37_37==T),T2_qidsms_manmildhtn_35_37:=T]

xtabs(~TT2$T2_qidsms_manmildhtn_35_37, addNA=T)





# severe chronic htn

#T2_qidsms_manmodsevchronichtn 15-17 weeks combo variable
TT2[,T2_qidsms_manmodsevhtn_35_37:=as.logical(NA)]
TT2[T2_qidsms_Oppt_bp_35_37==T & (!is.na(T2_qidsms_manmodsevhtn_35_35)|
                                    !is.na(T2_qidsms_manmodsevhtn_36_36)|
                                    !is.na(T2_qidsms_manmodsevhtn_37_37)),T2_qidsms_manmodsevhtn_35_37:=F]

xtabs(~TT2$T2_qidsms_manmodsevhtn_35_37, addNA=T)


TT2[T2_qidsms_manmodsevhtn_35_37==F & (T2_qidsms_manmodsevhtn_35_35==T|
                                         T2_qidsms_manmodsevhtn_36_36==T|
                                         T2_qidsms_manmodsevhtn_37_37==T),T2_qidsms_manmodsevhtn_35_37:=T]

xtabs(~TT2$T2_qidsms_manmodsevhtn_35_37, addNA=T)

###################################################################################################
                                             # GDM #
###################################################################################################


###################
# screening 24-28
###################


#24-28
TT2[,T2_qidsms_Opportunity_GDM_screening_24_28:=as.logical(NA)]

TT2[denom_24_28==T, T2_qidsms_Opportunity_GDM_screening_24_28:=FALSE]

TT2[num_24_28==T &
      T2_qidsms_Opportunity_GDM_screening_24_28==F, T2_qidsms_Opportunity_GDM_screening_24_28:=TRUE]

## Remove opportunities for people who had high blood glu
TT2[T2_qidsms_Opportunity_GDM_screening_24_28==TRUE &
     (T2_labbloodglu_high_00_14==T|
        T2_labbloodglu_high_15_17==T|
        T2_labbloodglu_high_18_22==T|
        T2_labbloodglu_high_23_23==T|
        T2_labfastbloodglu_high_00_14==T|
        T2_labfastbloodglu_high_15_17==T|
        T2_labfastbloodglu_high_18_22==T|
        T2_labfastbloodglu_high_23_23==T),
    T2_qidsms_Opportunity_GDM_screening_24_28:=FALSE]


xtabs(~TT2$T2_qidsms_Opportunity_GDM_screening_24_28, addNA=T)




###################
# screening 24-28
###################
TT2[,T2_qidsms_GDMscreeningontime_24_28:=as.logical(NA)]

TT2[T2_qidsms_Opportunity_GDM_screening_24_28==T,T2_qidsms_GDMscreeningontime_24_28:=FALSE]
TT2[T2_qidsms_GDMscreeningontime_24_28==FALSE &
     (T2_labfastbloodglu_exists_24_28==T|
        T2_labbloodglu_exists_24_28==T), T2_qidsms_GDMscreeningontime_24_28:=T]
xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28, addNA=T)

# normal values
TT2[,T2_qidsms_GDMscreeningontime_24_28_normal:=as.logical(NA)]


TT2[T2_qidsms_GDMscreeningontime_24_28==T, T2_qidsms_GDMscreeningontime_24_28_normal:=F]

TT2[T2_qidsms_GDMscreeningontime_24_28_normal==F &
     (T2_labfastbloodglu_normal_24_28==T|
        T2_labbloodglu_normal_24_28==T),T2_qidsms_GDMscreeningontime_24_28_normal:=TRUE]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_normal, addNA=T)

###################
# management 24-28
###################


# highrbg 24 weeks
TT2[,T2_qidsms_GDMscreeningontime_24_24_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
     (T2_labbloodglu_high_24_24==T |
        T2_labfastbloodglu_high_24_24==T),T2_qidsms_GDMscreeningontime_24_24_manhighrbg:=F]


TT2[T2_qidsms_GDMscreeningontime_24_24_manhighrbg==F &
     (T2_manRBGHigh_Diab_24_24==T|
        T2_manRef_HR_24_24==T|
        T2_manRef_spec_24_24==T|
        T2_manFBSHigh_Diab_24_24==T), T2_qidsms_GDMscreeningontime_24_24_manhighrbg:=T]


xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_24_manhighrbg, addNA=T)

# highrbg 25 weeks
TT2[,T2_qidsms_GDMscreeningontime_25_25_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
     (T2_labbloodglu_high_25_25==T |
        T2_labfastbloodglu_high_25_25==T),T2_qidsms_GDMscreeningontime_25_25_manhighrbg:=F]


TT2[T2_qidsms_GDMscreeningontime_25_25_manhighrbg==F &
     (T2_manRBGHigh_Diab_25_25==T|
        T2_manRef_HR_25_25==T|
        T2_manRef_spec_25_25==T|
        T2_manFBSHigh_Diab_25_25==T), T2_qidsms_GDMscreeningontime_25_25_manhighrbg:=T]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_25_25_manhighrbg, addNA=T)

# highrbg 26 weeks
TT2[,T2_qidsms_GDMscreeningontime_26_26_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_GDMscreeningontime_24_28_normal==FALSE &
     (T2_labbloodglu_high_26_26==T |
        T2_labfastbloodglu_high_26_26==T),T2_qidsms_GDMscreeningontime_26_26_manhighrbg:=F]


TT2[T2_qidsms_GDMscreeningontime_26_26_manhighrbg==F &
     (T2_manRBGHigh_Diab_26_26==T|
        T2_manRef_HR_26_26==T|
        T2_manRef_spec_26_26==T|
        T2_manFBSHigh_Diab_26_26==T), T2_qidsms_GDMscreeningontime_26_26_manhighrbg:=T]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_26_26_manhighrbg, addNA=T)


# highrbg 27 weeks
TT2[,T2_qidsms_GDMscreeningontime_27_27_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
     (T2_labbloodglu_high_27_27==T |
        T2_labfastbloodglu_high_27_27==T),T2_qidsms_GDMscreeningontime_27_27_manhighrbg:=F]


TT2[T2_qidsms_GDMscreeningontime_27_27_manhighrbg==F &
     (T2_manRBGHigh_Diab_27_27==T|
        T2_manRef_HR_27_27==T|
        T2_manRef_spec_27_27==T|
        T2_manFBSHigh_Diab_27_27==T), T2_qidsms_GDMscreeningontime_27_27_manhighrbg:=T]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_27_27_manhighrbg, addNA=T)

# highrbg 28 weeks
TT2[,T2_qidsms_GDMscreeningontime_28_28_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_GDMscreeningontime_24_28_normal==FALSE &
     (T2_labbloodglu_high_28_28==T |
        T2_labfastbloodglu_high_28_28==T),T2_qidsms_GDMscreeningontime_28_28_manhighrbg:=F]


TT2[T2_qidsms_GDMscreeningontime_28_28_manhighrbg==F &
     (T2_manRBGHigh_Diab_28_28==T|
        T2_manRef_HR_28_28==T|
        T2_manRef_spec_28_28==T|
        T2_manFBSHigh_Diab_28_28==T), T2_qidsms_GDMscreeningontime_28_28_manhighrbg:=T]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_28_28_manhighrbg, addNA=T)


# combined group

TT2[,T2_qidsms_GDMscreeningontime_24_28_manhighrbg:=as.logical(NA)]
TT2[T2_qidsms_GDMscreeningontime_24_28==T &
     T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
     (!is.na(T2_qidsms_GDMscreeningontime_24_24_manhighrbg)|
        !is.na(T2_qidsms_GDMscreeningontime_25_25_manhighrbg)|
        !is.na(T2_qidsms_GDMscreeningontime_26_26_manhighrbg)|
        !is.na(T2_qidsms_GDMscreeningontime_27_27_manhighrbg)|
        !is.na(T2_qidsms_GDMscreeningontime_28_28_manhighrbg)),
   T2_qidsms_GDMscreeningontime_24_28_manhighrbg:=F]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_manhighrbg, addNA=T)


TT2[T2_qidsms_GDMscreeningontime_24_28_manhighrbg==F & 
     (T2_qidsms_GDMscreeningontime_24_24_manhighrbg==T|
        T2_qidsms_GDMscreeningontime_25_25_manhighrbg==T|
        T2_qidsms_GDMscreeningontime_26_26_manhighrbg==T|
        T2_qidsms_GDMscreeningontime_27_27_manhighrbg==T|
        T2_qidsms_GDMscreeningontime_28_28_manhighrbg==T),
   T2_qidsms_GDMscreeningontime_24_28_manhighrbg:=T]

xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_manhighrbg, addNA=T)



if(IS_GAZA){
  
  # intermediate values
  
  # intermediate values,  but dont want them for WB because management is in free text
  TT2[,T2_qidsms_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28==T &
       T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
       is.na(T2_qidsms_GDMscreeningontime_24_28_manhighrbg) &
       (T2_labbloodglu_likelyGDM_24_28==T|
          T2_labfastbloodglu_likelyGDM_24_28==T),T2_qidsms_GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_intmbg, addNA=T)
  # managment is repeat FBS with in 3 weeks
  
  # do this by one week intervals
  
  # 24 weeks
  TT2[,T2_qidsms_GDMscreeningontime_24_24_manintmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_24_24==T|
       T2_labbloodglu_likelyGDM_24_24==T),T2_qidsms_GDMscreeningontime_24_24_manintmbg:=FALSE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_24_manintmbg, addNA=T)
  
  TT2[T2_qidsms_GDMscreeningontime_24_24_manintmbg==F &
       T2_repeatFBS_27_27==T,T2_qidsms_GDMscreeningontime_24_24_manintmbg:=T]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_24_manintmbg, addNA=T)
  
  # 25 weeks
  TT2[,T2_qidsms_GDMscreeningontime_25_25_manintmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
        (T2_labfastbloodglu_likelyGDM_25_25==T|
           T2_labbloodglu_likelyGDM_25_25==T),T2_qidsms_GDMscreeningontime_25_25_manintmbg:=FALSE]
  
  TT2[T2_qidsms_GDMscreeningontime_25_25_manintmbg==F &
       T2_repeatFBS_28_28==T,T2_qidsms_GDMscreeningontime_25_25_manintmbg:=T]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_25_25_manintmbg, addNA=T)
  
  
  # 26 weeks
  TT2[,T2_qidsms_GDMscreeningontime_26_26_manintmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_26_26==T|
       T2_labbloodglu_likelyGDM_26_26==T),T2_qidsms_GDMscreeningontime_26_26_manintmbg:=FALSE]
  
  TT2[T2_qidsms_GDMscreeningontime_26_26_manintmbg==F &
       T2_repeatFBS_29_29==T,T2_qidsms_GDMscreeningontime_26_26_manintmbg:=T]
  

  xtabs(~TT2$T2_qidsms_GDMscreeningontime_26_26_manintmbg, addNA=T)
  
  
  # 27 weeks
  TT2[,T2_qidsms_GDMscreeningontime_27_27_manintmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_27_27==T |
          T2_labbloodglu_likelyGDM_27_27==T),T2_qidsms_GDMscreeningontime_27_27_manintmbg:=FALSE]
  
  TT2[T2_qidsms_GDMscreeningontime_27_27_manintmbg==F &
       T2_repeatFBS_30_30==T,T2_qidsms_GDMscreeningontime_27_27_manintmbg:=T]
  
  
  # 28 weeks
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_28_28==T |
          T2_labbloodglu_likelyGDM_28_28==T),T2_qidsms_GDMscreeningontime_28_28_manintmbg:=FALSE]
  
  TT2[T2_qidsms_GDMscreeningontime_28_28_manintmbg==F &
        T2_repeatFBS_31_31==T,T2_qidsms_GDMscreeningontime_28_28_manintmbg:=T]
  
  
  
  # combined variable
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28_intmbg==T &
       (!is.na(T2_qidsms_GDMscreeningontime_28_28_manintmbg)|
          !is.na(T2_qidsms_GDMscreeningontime_27_27_manintmbg)|
          !is.na(T2_qidsms_GDMscreeningontime_26_26_manintmbg)|
          !is.na(T2_qidsms_GDMscreeningontime_25_25_manintmbg)|
          !is.na(T2_qidsms_GDMscreeningontime_24_24_manintmbg)),
      T2_qidsms_GDMscreeningontime_24_28_manintmbg:=FALSE ]
  
  TT2[T2_qidsms_GDMscreeningontime_24_28_manintmbg==F &
       (T2_qidsms_GDMscreeningontime_24_24_manintmbg==T |
          T2_qidsms_GDMscreeningontime_25_25_manintmbg==T|
          T2_qidsms_GDMscreeningontime_26_26_manintmbg==T|
          T2_qidsms_GDMscreeningontime_27_27_manintmbg==T|
          T2_qidsms_GDMscreeningontime_28_28_manintmbg==T), 
      T2_qidsms_GDMscreeningontime_24_28_manintmbg:=TRUE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_manintmbg, addNA=T)
  
}else{
  
  # intermediate values,  but dont want them for WB because management is in free text
  TT2[,T2_qidsms_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  TT2[T2_qidsms_GDMscreeningontime_24_28==T &
       T2_qidsms_GDMscreeningontime_24_28_normal==FALSE &
       is.na(T2_qidsms_GDMscreeningontime_24_28_manhighrbg) &
       (T2_labbloodglu_likelyGDM_24_28==T),T2_qidsms_GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~TT2$T2_qidsms_GDMscreeningontime_24_28_intmbg, addNA=T)
  
  
  
  TT2[,T2_qidsms_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_24_24_manintmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_25_25_manintmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_26_26_manintmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_27_27_manintmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_28_28_manintmbg:=as.logical(NA)]
  TT2[,T2_qidsms_GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  
}



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

