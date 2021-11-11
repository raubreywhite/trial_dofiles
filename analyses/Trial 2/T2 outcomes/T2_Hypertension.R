


########## Hypertension ########## 
#define chronic?
# high blood pressure before or on 22 weeks

################
#00-14 weeks
################

#screening
T2[,T2_Oppt_bp_00_14:=as.logical(NA)]
T2[T2_anvisitnew_00_14==T,T2_Oppt_bp_00_14:=TRUE]
xtabs(~T2$T2_Oppt_bp_00_14,addNA=T)

# numerator
T2[,T2_bpontime_00_14:=as.logical(NA)]
T2[T2_Oppt_bp_00_14==TRUE,T2_bpontime_00_14:=FALSE]
T2[T2_anbpsyst_present_00_14==T &
     T2_anbpdiast_present_00_14==T &
     T2_bpontime_00_14==F,T2_bpontime_00_14:=TRUE]

xtabs(~T2$T2_bpontime_00_14)



#management
#T2_Oppt
T2[,T2_manchronichtn_00_14:=as.logical(NA)]
T2[T2_Oppt_bp_00_14==T & (T2_anbpdiast_mildHTN_00_14==T|
                          T2_anbpsyst_mildHTN_00_14==T|
                          T2_anbpsyst_modSevHTN_00_14==T|
                          T2_anbpdiast_modSevHTN_00_14==T),T2_manchronichtn_00_14:=F]



T2[T2_manchronichtn_00_14==F & (T2_refHR_00_14==T|
                                  T2_refHosp_00_14==T|
                                  T2_refSpec_00_14==T),T2_manchronichtn_00_14:=T]

xtabs(~T2$T2_manchronichtn_00_14, addNA=T)



################
#15-17 weeks
################

#screening
T2[,T2_Oppt_bp_15_17:=as.logical(NA)]
T2[T2_anvisitnew_15_17==T &
     is.na(T2_manchronichtn_00_14),T2_Oppt_bp_15_17:=TRUE]
xtabs(~T2$T2_Oppt_bp_15_17,addNA=T)

# numerator
T2[,T2_bpontime_15_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==TRUE,T2_bpontime_15_17:=FALSE]
T2[T2_anbpsyst_present_15_17==T &
     T2_anbpdiast_present_15_17==T &
     T2_bpontime_15_17==F,T2_bpontime_15_17:=TRUE]

xtabs(~T2$T2_bpontime_15_17)


################
#15-17 weeks
################

#management
#needs to be done week by week

# mild 15 weeks
T2[,T2_manmildchronichtn_15_15:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_15_15==T|
                            T2_anbpsyst_mildHTN_15_15==T),T2_manmildchronichtn_15_15:=F]

T2[T2_manmildchronichtn_15_15==F & (T2_refHR_15_15==T|
                                T2_refHosp_15_15==T|
                                T2_refSpec_15_15==T),T2_manmildchronichtn_15_15:=T]

xtabs(~T2$T2_manmildchronichtn_15_15, addNA=T)

# mod/sev 15 weeks
T2[,T2_manmodsevchronichtn_15_15:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_15_15==T|
                            T2_anbpsyst_modSevHTN_15_15==T),T2_manmodsevchronichtn_15_15:=F]

T2[T2_manmodsevchronichtn_15_15==F & (T2_refHR_15_15==T|
                                      T2_refHosp_15_15==T|
                                      T2_refSpec_15_15==T),T2_manmodsevchronichtn_15_15:=T]

xtabs(~T2$T2_manmodsevchronichtn_15_15, addNA=T)

# mild/mod/sev 16 weeks
T2[,T2_manmildchronichtn_16_16:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_16_16==T|
                            T2_anbpsyst_mildHTN_16_16==T),T2_manmildchronichtn_16_16:=F]

T2[T2_manmildchronichtn_16_16==F & (T2_refHR_16_16==T|
                                  T2_refHosp_16_16==T|
                                  T2_refSpec_16_16==T),T2_manmildchronichtn_16_16:=T]
xtabs(~T2$T2_manmildchronichtn_16_16, addNA=T)

# mod/sev 15 weeks
T2[,T2_manmodsevchronichtn_16_16:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_16_16==T|
                            T2_anbpsyst_modSevHTN_16_16==T),T2_manmodsevchronichtn_16_16:=F]

T2[T2_manmodsevchronichtn_16_16==F & (T2_refHR_16_16==T|
                                        T2_refHosp_16_16==T|
                                        T2_refSpec_16_16==T),T2_manmodsevchronichtn_16_16:=T]

xtabs(~T2$T2_manmodsevchronichtn_16_16, addNA=T)

# mild/mod/sev 17 weeks
T2[,T2_manmildchronichtn_17_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_17_17==T|
                            T2_anbpsyst_mildHTN_17_17==T),T2_manmildchronichtn_17_17:=F]

T2[T2_manmildchronichtn_17_17==F & (T2_refHR_17_17==T|
                                  T2_refHosp_17_17==T|
                                  T2_refSpec_17_17==T),T2_manmildchronichtn_17_17:=T]

xtabs(~T2$T2_manmildchronichtn_17_17, addNA=T)

# mod/sev 17 weeks
T2[,T2_manmodsevchronichtn_17_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_modSevHTN_17_17==T|
                            T2_anbpsyst_modSevHTN_17_17==T),T2_manmodsevchronichtn_17_17:=F]

T2[T2_manmodsevchronichtn_17_17==F & (T2_refHR_17_17==T|
                                        T2_refHosp_17_17==T|
                                        T2_refSpec_17_17==T),T2_manmodsevchronichtn_17_17:=T]

xtabs(~T2$T2_manmodsevchronichtn_17_17, addNA=T)


################
# combined
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildchronichtn_15_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (!is.na(T2_manmildchronichtn_15_15)|
                            !is.na(T2_manmildchronichtn_16_16)|
                            !is.na(T2_manmildchronichtn_17_17)),T2_manmildchronichtn_15_17:=F]

xtabs(~T2$T2_manmildchronichtn_15_17, addNA=T)


T2[T2_manmildchronichtn_15_17==F & (T2_manmildchronichtn_15_15==T|
                                  T2_manmildchronichtn_16_16==T|
                                  T2_manmildchronichtn_17_17==T),T2_manmildchronichtn_17_17:=T]

xtabs(~T2$T2_manmildchronichtn_15_17, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevchronichtn_15_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (!is.na(T2_manmodsevchronichtn_15_15)|
                            !is.na(T2_manmodsevchronichtn_16_16)|
                            !is.na(T2_manmodsevchronichtn_17_17)),T2_manmodsevchronichtn_15_17:=F]

xtabs(~T2$T2_manmodsevchronichtn_15_17, addNA=T)


T2[T2_manmodsevchronichtn_15_17==F & (T2_manmodsevchronichtn_15_15==T|
                                        T2_manmodsevchronichtn_16_16==T|
                                        T2_manmodsevchronichtn_17_17==T),T2_manmodsevchronichtn_15_17:=T]

xtabs(~T2$T2_manmodsevchronichtn_15_17, addNA=T)

################
#18-22 weeks
################

#screening
T2[,T2_Oppt_bp_18_22:=as.logical(NA)]
T2[T2_anvisitnew_18_22==T &
     is.na(T2_manmildchronichtn_15_17) &
     is.na(T2_manmodsevchronichtn_15_17),T2_Oppt_bp_18_22:=TRUE]
xtabs(~T2$T2_Oppt_bp_18_22,addNA=T)

# numerator
T2[,T2_bpontime_18_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==TRUE,T2_bpontime_18_22:=FALSE]
T2[T2_anbpsyst_present_18_22==T &
     T2_anbpdiast_present_18_22==T &
     T2_bpontime_18_22==F,T2_bpontime_18_22:=TRUE]

xtabs(~T2$T2_bpontime_18_22)


################
#18-22 weeks
################

#management

# mild 18 weeks
T2[,T2_manmildchronichtn_18_18:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_18_18==T|
                            T2_anbpsyst_mildHTN_18_18==T),T2_manmildchronichtn_18_18:=F]

T2[T2_manmildchronichtn_18_18==F & (T2_anbpsyst_present_19_19==T &
                                    T2_anbpdiast_present_19_19==T),T2_manmildchronichtn_18_18:=T]

xtabs(~T2$T2_manmildchronichtn_18_18, addNA=T)

# mod/sev 18 weeks
T2[,T2_manmodsevchronichtn_18_18:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_18_18==T|
                            T2_anbpsyst_modSevHTN_18_18==T),T2_manmodsevchronichtn_18_18:=F]

T2[T2_manmodsevchronichtn_18_18==F & (T2_refHR_18_18==T|
                                        T2_refHosp_18_18==T|
                                        T2_refSpec_18_18==T),T2_manmodsevchronichtn_18_18:=T]

xtabs(~T2$T2_manmodsevchronichtn_18_18, addNA=T)

# man 19 weeks
#mild 
T2[,T2_manmildchronichtn_19_19:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_19_19==T|
                            T2_anbpsyst_mildHTN_19_19==T),T2_manmildchronichtn_19_19:=F]

T2[T2_manmildchronichtn_19_19==F & (T2_anbpsyst_present_20_20==T &
                                      T2_anbpdiast_present_20_20==T),T2_manmildchronichtn_19_19:=T]

xtabs(~T2$T2_manmildchronichtn_19_19, addNA=T)

# mod/sev 19 weeks
T2[,T2_manmodsevchronichtn_19_19:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_19_19==T|
                            T2_anbpsyst_modSevHTN_19_19==T),T2_manmodsevchronichtn_19_19:=F]

T2[T2_manmodsevchronichtn_19_19==F & (T2_refHR_19_19==T|
                                        T2_refHosp_19_19==T|
                                        T2_refSpec_19_19==T),T2_manmodsevchronichtn_19_19:=T]

xtabs(~T2$T2_manmodsevchronichtn_19_19, addNA=T)

# man 20 weeks
#mild 20 weeks
T2[,T2_manmildchronichtn_20_20:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_20_20==T|
                            T2_anbpsyst_mildHTN_20_20==T),T2_manmildchronichtn_20_20:=F]

T2[T2_manmildchronichtn_20_20==F & (T2_anbpsyst_present_21_21==T &
                                      T2_anbpdiast_present_21_21==T),T2_manmildchronichtn_20_20:=T]

xtabs(~T2$T2_manmildchronichtn_20_20, addNA=T)

# mod/sev 20 weeks
T2[,T2_manmodsevchronichtn_20_20:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_20_20==T|
                            T2_anbpsyst_modSevHTN_20_20==T),T2_manmodsevchronichtn_20_20:=F]

T2[T2_manmodsevchronichtn_20_20==F & (T2_refHR_20_20==T|
                                        T2_refHosp_20_20==T|
                                        T2_refSpec_20_20==T),T2_manmodsevchronichtn_20_20:=T]

xtabs(~T2$T2_manmodsevchronichtn_20_20, addNA=T)

# man 21 weeks
#mild 21 weeks
T2[,T2_manmildchronichtn_21_21:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_21_21==T|
                            T2_anbpsyst_mildHTN_21_21==T),T2_manmildchronichtn_21_21:=F]

T2[T2_manmildchronichtn_21_21==F & (T2_anbpsyst_present_22_22==T &
                                      T2_anbpdiast_present_22_22==T),T2_manmildchronichtn_21_21:=T]

xtabs(~T2$T2_manmildchronichtn_21_21, addNA=T)


# mod/sev 21 weeks
T2[,T2_manmodsevchronichtn_21_21:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_21_21==T|
                            T2_anbpsyst_modSevHTN_21_21==T),T2_manmodsevchronichtn_21_21:=F]

T2[T2_manmodsevchronichtn_21_21==F & (T2_refHR_21_21==T|
                                        T2_refHosp_21_21==T|
                                        T2_refSpec_21_21==T),T2_manmodsevchronichtn_21_21:=T]

xtabs(~T2$T2_manmodsevchronichtn_21_21, addNA=T)

# man 22 weeks
#mild 22 weeks
T2[,T2_manmildchronichtn_22_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_22_22==T|
                            T2_anbpsyst_mildHTN_22_22==T),T2_manmildchronichtn_22_22:=F]

T2[T2_manmildchronichtn_22_22==F & (T2_anbpsyst_present_23_23==T &
                                      T2_anbpdiast_present_23_23==T),T2_manmildchronichtn_22_22:=T]

xtabs(~T2$T2_manmildchronichtn_22_22, addNA=T)

#
T2[,T2_manmodsevchronichtn_22_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_modSevHTN_22_22==T|
                            T2_anbpsyst_modSevHTN_22_22==T),T2_manmodsevchronichtn_22_22:=F]

T2[T2_manmodsevchronichtn_22_22==F & (T2_refHR_22_22==T|
                                        T2_refHosp_22_22==T|
                                        T2_refSpec_22_22==T),T2_manmodsevchronichtn_22_22:=T]

xtabs(~T2$T2_manmodsevchronichtn_22_22, addNA=T)



################
# combined 18-22
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildchronichtn_18_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (!is.na(T2_manmildchronichtn_18_18)|
                            !is.na(T2_manmildchronichtn_19_19)|
                            !is.na(T2_manmildchronichtn_20_20)|
                            !is.na(T2_manmildchronichtn_21_21)|
                            !is.na(T2_manmildchronichtn_22_22)),T2_manmildchronichtn_18_22:=F]

xtabs(~T2$T2_manmildchronichtn_18_22, addNA=T)


T2[T2_manmildchronichtn_18_22==F & (T2_manmildchronichtn_18_18==T|
                                      T2_manmildchronichtn_19_19==T|
                                      T2_manmildchronichtn_20_20==T|
                                      T2_manmildchronichtn_21_21==T|
                                      T2_manmildchronichtn_22_22==T),T2_manmildchronichtn_18_22:=T]

xtabs(~T2$T2_manmildchronichtn_18_22, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevchronichtn_18_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (!is.na(T2_manmodsevchronichtn_18_18)|
                            !is.na(T2_manmodsevchronichtn_19_19)|
                            !is.na(T2_manmodsevchronichtn_20_20)|
                            !is.na(T2_manmodsevchronichtn_21_21)|
                            !is.na(T2_manmodsevchronichtn_22_22)),T2_manmodsevchronichtn_18_22:=F]

xtabs(~T2$T2_manmodsevchronichtn_18_22, addNA=T)


T2[T2_manmodsevchronichtn_18_22==F & (T2_manmodsevchronichtn_18_18==T|
                                        T2_manmodsevchronichtn_19_19==T|
                                        T2_manmodsevchronichtn_20_20==T|
                                        T2_manmodsevchronichtn_21_21==T|
                                        T2_manmodsevchronichtn_22_22==T),T2_manmodsevchronichtn_18_22:=T]

xtabs(~T2$T2_manmodsevchronichtn_18_22, addNA=T)



################
# 23 weeks
################

#screening
T2[,T2_Oppt_bp_23_23:=as.logical(NA)]
T2[T2_anvisitnew_23_23==T &
     is.na(T2_manmodsevchronichtn_18_22) &
     is.na(T2_manmildchronichtn_18_22),T2_Oppt_bp_23_23:=TRUE]
xtabs(~T2$T2_Oppt_bp_23_23,addNA=T)

# numerator
T2[,T2_bpontime_23_23:=as.logical(NA)]
T2[T2_Oppt_bp_23_23==TRUE,T2_bpontime_23_23:=FALSE]
T2[T2_anbpsyst_present_23_23==T &
     T2_anbpdiast_present_23_23==T &
     T2_bpontime_23_23==F,T2_bpontime_23_23:=TRUE]

xtabs(~T2$T2_bpontime_23_23)


#management

# mild 23 weeks
T2[,T2_manmildchronichtn_23_23:=as.logical(NA)]
T2[T2_Oppt_bp_23_23==T & (T2_anbpdiast_mildHTN_23_23==T|
                            T2_anbpsyst_mildHTN_23_23==T),T2_manmildchronichtn_23_23:=F]

T2[T2_manmildchronichtn_23_23==F & (T2_anbpsyst_present_24_24==T &
                                      T2_anbpdiast_present_24_24==T),T2_manmildchronichtn_23_23:=T]

xtabs(~T2$T2_manmildchronichtn_23_23, addNA=T)

# mod/sev 23 weeks
T2[,T2_manmodsevchronichtn_23_23:=as.logical(NA)]
T2[T2_Oppt_bp_23_23==T & (T2_anbpdiast_modSevHTN_23_23==T|
                            T2_anbpsyst_modSevHTN_23_23==T),T2_manmodsevchronichtn_23_23:=F]

T2[T2_manmodsevchronichtn_23_23==F & (T2_refHR_23_23==T|
                                        T2_refHosp_23_23==T|
                                        T2_refSpec_23_23==T),T2_manmodsevchronichtn_23_23:=T]

xtabs(~T2$T2_manmodsevchronichtn_23_23, addNA=T)


################
#24-28 weeks
################

#screening
T2[,T2_Oppt_bp_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T &
     is.na(T2_manmildchronichtn_23_23) &
     is.na(T2_manmildchronichtn_23_23),T2_Oppt_bp_24_28:=TRUE]
xtabs(~T2$T2_Oppt_bp_24_28,addNA=T)

# numerator
T2[,T2_bpontime_24_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==TRUE,T2_bpontime_24_28:=FALSE]
T2[T2_anbpsyst_present_24_28==T &
     T2_anbpdiast_present_24_28==T &
     T2_bpontime_24_28==F,T2_bpontime_24_28:=TRUE]

xtabs(~T2$T2_bpontime_24_28)


################
#24-28 weeks
################

#management
# 24 weeks
#mild 
T2[,T2_manmildhtn_24_24:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_24_24==T|
                            T2_anbpsyst_mildHTN_24_24==T),T2_manmildhtn_24_24:=F]

T2[T2_manmildhtn_24_24==F & (T2_anbpsyst_present_25_25==T &
                               T2_anbpdiast_present_25_25==T),T2_manmildhtn_24_24:=T]

xtabs(~T2$T2_manmildhtn_24_24, addNA=T)

# mod/sev  weeks
T2[,T2_manmodsevhtn_24_24:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_24_24==T|
                            T2_anbpsyst_modSevHTN_24_24==T),T2_manmodsevhtn_24_24:=F]

T2[T2_manmodsevhtn_24_24==F & (T2_refHR_24_24==T|
                                        T2_refHosp_24_24==T|
                                        T2_refSpec_24_24==T),T2_manmodsevhtn_24_24:=T]

xtabs(~T2$T2_manmodsevhtn_24_24, addNA=T)

# 25 weeks
#mild 
T2[,T2_manmildhtn_25_25:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_25_25==T|
                            T2_anbpsyst_mildHTN_25_25==T),T2_manmildhtn_25_25:=F]

T2[T2_manmildhtn_25_25==F & (T2_anbpsyst_present_26_26==T &
                               T2_anbpdiast_present_26_26==T),T2_manmildhtn_25_25:=T]

xtabs(~T2$T2_manmildhtn_25_25, addNA=T)

# mod/sev weeks
T2[,T2_manmodsevhtn_25_25:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_25_25==T|
                            T2_anbpsyst_modSevHTN_25_25==T),T2_manmodsevhtn_25_25:=F]

T2[T2_manmodsevhtn_25_25==F & (T2_refHR_25_25==T|
                                 T2_refHosp_25_25==T|
                                 T2_refSpec_25_25==T),T2_manmodsevhtn_25_25:=T]

xtabs(~T2$T2_manmodsevhtn_25_25, addNA=T)

# 26 weeks
#mild 
T2[,T2_manmildhtn_26_26:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_26_26==T|
                            T2_anbpsyst_mildHTN_26_26==T),T2_manmildhtn_26_26:=F]

T2[T2_manmildhtn_26_26==F & (!is.na(T2_anbpsyst_present_27_27) &
                               !is.na(T2_anbpdiast_present_27_27)),T2_manmildhtn_26_26:=T]

xtabs(~T2$T2_manmildhtn_26_26, addNA=T)


# mod/sev 
T2[,T2_manmodsevhtn_26_26:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_26_26==T|
                            T2_anbpsyst_modSevHTN_26_26==T),T2_manmodsevhtn_26_26:=F]

T2[T2_manmodsevhtn_26_26==F & (T2_refHR_26_26==T|
                                 T2_refHosp_26_26==T|
                                 T2_refSpec_26_26==T),T2_manmodsevhtn_26_26:=T]

xtabs(~T2$T2_manmodsevhtn_26_26, addNA=T)

# 27 weeks
#mild 
T2[,T2_manmildhtn_27_27:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_27_27==T|
                            T2_anbpsyst_mildHTN_27_27==T),T2_manmildhtn_27_27:=F]

T2[T2_manmildhtn_27_27==F & (T2_anbpsyst_present_28_28==T &
                              T2_anbpdiast_present_28_28==T),T2_manmildhtn_27_27:=T]


xtabs(~T2$T2_manmildhtn_27_27, addNA=T)

# mod/sev 19 weeks
T2[,T2_manmodsevhtn_27_27:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_27_27==T|
                            T2_anbpsyst_modSevHTN_27_27==T),T2_manmodsevhtn_27_27:=F]

T2[T2_manmodsevhtn_27_27==F & (T2_refHR_27_27==T|
                                 T2_refHosp_27_27==T|
                                 T2_refSpec_27_27==T),T2_manmodsevhtn_27_27:=T]

xtabs(~T2$T2_manmodsevhtn_27_27, addNA=T)


# 28 weeks
#mild 
T2[,T2_manmildhtn_28_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_28_28==T|
                            T2_anbpsyst_mildHTN_28_28==T),T2_manmildhtn_28_28:=F]

T2[T2_manmildhtn_28_28==F & (T2_anbpsyst_present_29_29==T &
                               T2_anbpdiast_present_29_29==T),T2_manmildhtn_28_28:=T]

xtabs(~T2$T2_manmildhtn_28_28, addNA=T)


# mod/sev 
T2[,T2_manmodsevhtn_28_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_28_28==T|
                            T2_anbpsyst_modSevHTN_28_28==T),T2_manmodsevhtn_28_28:=F]

T2[T2_manmodsevhtn_28_28==F & (T2_refHR_28_28==T|
                                 T2_refHosp_28_28==T|
                                 T2_refSpec_28_28==T),T2_manmodsevhtn_28_28:=T]

xtabs(~T2$T2_manmodsevhtn_28_28, addNA=T)

# combine man var for 24-28 weeks
T2[,manmildhtn_24_28:=as.logical(NA)]
T2[T2_manmildhtn_28_28==T|
     T2_manmildhtn_27_27==T,manmildhtn_24_28:=T]
T2[,manmodsevhtn_24_28:=as.logical(NA)]



################
# combined 24-28
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildhtn_24_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (!is.na(T2_manmildhtn_24_24)|
                            !is.na(T2_manmildhtn_25_25)|
                            !is.na(T2_manmildhtn_26_26)|
                            !is.na(T2_manmildhtn_27_27)|
                            !is.na(T2_manmildhtn_28_28)),T2_manmildhtn_24_28:=F]

xtabs(~T2$T2_manmildhtn_24_28, addNA=T)


T2[T2_manmildhtn_24_28==F & (T2_manmildhtn_24_24==T|
                               T2_manmildhtn_25_25==T|
                               T2_manmildhtn_26_26==T|
                               T2_manmildhtn_27_27==T|
                               T2_manmildhtn_28_28==T),T2_manmildhtn_24_28:=T]

xtabs(~T2$T2_manmildhtn_24_28, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevhtn_24_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (!is.na(T2_manmodsevhtn_24_24)|
                            !is.na(T2_manmodsevhtn_25_25)|
                            !is.na(T2_manmodsevhtn_26_26)|
                            !is.na(T2_manmodsevhtn_27_27)|
                            !is.na(T2_manmodsevhtn_28_28)),T2_manmodsevhtn_24_28:=F]

xtabs(~T2$T2_manmodsevhtn_24_28, addNA=T)


T2[T2_manmodsevhtn_24_28==F & (T2_manmodsevhtn_24_24==T|
                                 T2_manmodsevhtn_25_25==T|
                                 T2_manmodsevhtn_26_26==T|
                                 T2_manmodsevhtn_27_27==T|
                                 T2_manmodsevhtn_28_28==T),T2_manmodsevhtn_24_28:=T]

xtabs(~T2$T2_manmodsevhtn_24_28, addNA=T)


################
# 29-30 weeks
################

#screening
T2[,T2_Oppt_bp_29_30:=as.logical(NA)]
T2[T2_anvisitnew_29_30==T &
     is.na(T2_manmildhtn_24_28) &
     is.na(T2_manmodsevhtn_24_28),T2_Oppt_bp_29_30:=TRUE]
xtabs(~T2$T2_Oppt_bp_29_30,addNA=T)

# numerator
T2[,T2_bpontime_29_30:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==TRUE,T2_bpontime_29_30:=FALSE]
T2[T2_anbpsyst_present_29_30==T &
     T2_anbpdiast_present_29_30==T &
     T2_bpontime_29_30==F,T2_bpontime_29_30:=TRUE]

xtabs(~T2$T2_bpontime_29_30)


#management
#29 weeks
#mild 
T2[,T2_manmildhtn_29_29:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (T2_anbpdiast_mildHTN_29_29==T|
                            T2_anbpsyst_mildHTN_29_29==T),T2_manmildhtn_29_29:=F]

T2[T2_manmildhtn_29_29==F & (T2_anbpsyst_present_30_30==T &
                               T2_anbpdiast_present_30_30==T),T2_manmildhtn_29_29:=T]

# mod/sev 
T2[,T2_manmodsevhtn_29_29:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (T2_anbpdiast_modSevHTN_29_29==T|
                            T2_anbpsyst_modSevHTN_29_29==T),T2_manmodsevhtn_29_29:=F]

T2[T2_manmodsevhtn_29_29==F & (T2_refHR_29_29==T|
                                 T2_refHosp_29_29==T|
                                 T2_refSpec_29_29==T),T2_manmodsevhtn_29_29:=T]

xtabs(~T2$T2_manmodsevhtn_29_29, addNA=T)

#30 weeks
#mild 
T2[,T2_manmildhtn_30_30:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (T2_anbpdiast_mildHTN_30_30==T|
                            T2_anbpsyst_mildHTN_30_30==T),T2_manmildhtn_30_30:=F]

T2[T2_manmildhtn_30_30==F & (T2_anbpsyst_present_31_31==T &
                               T2_anbpdiast_present_31_31==T),T2_manmildhtn_30_30:=T]

# mod/sev 
T2[,T2_manmodsevhtn_30_30:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (T2_anbpdiast_modSevHTN_30_30==T|
                            T2_anbpsyst_modSevHTN_30_30==T),T2_manmodsevhtn_30_30:=F]

T2[T2_manmodsevhtn_30_30==F & (T2_refHR_30_30==T|
                                 T2_refHosp_30_30==T|
                                 T2_refSpec_30_30==T),T2_manmodsevhtn_30_30:=T]

xtabs(~T2$T2_manmodsevhtn_30_30, addNA=T)




################
# combined 29-30
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildhtn_29_30:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (!is.na(T2_manmildhtn_29_29)|
                            !is.na(T2_manmildhtn_30_30)),T2_manmildhtn_29_30:=F]

xtabs(~T2$T2_manmildhtn_29_30, addNA=T)


T2[T2_manmildhtn_29_30==F & (T2_manmildhtn_29_29==T|
                               T2_manmildhtn_30_30==T),T2_manmildhtn_29_30:=T]

xtabs(~T2$T2_manmildhtn_29_30, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevhtn_29_30:=as.logical(NA)]
T2[T2_Oppt_bp_29_30==T & (!is.na(T2_manmodsevhtn_29_29)|
                            !is.na(T2_manmodsevhtn_30_30)),T2_manmodsevhtn_29_30:=F]

xtabs(~T2$T2_manmodsevhtn_29_30, addNA=T)


T2[T2_manmodsevhtn_29_30==F & (T2_manmodsevhtn_29_29==T|
                                 T2_manmodsevhtn_30_30==T),T2_manmodsevhtn_29_30:=T]

xtabs(~T2$T2_manmodsevhtn_29_30, addNA=T)



################
#31-33 weeks
################

#screening
T2[,T2_Oppt_bp_31_33:=as.logical(NA)]
T2[T2_anvisitnew_31_33==T &
          is.na(T2_manmodsevhtn_29_30) & 
          is.na(T2_manmildhtn_29_30),T2_Oppt_bp_31_33:=TRUE]
xtabs(~T2$T2_Oppt_bp_31_33,addNA=T)

# numerator
T2[,T2_bpontime_31_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==TRUE,T2_bpontime_31_33:=FALSE]
T2[T2_anbpsyst_present_31_33==T &
     T2_anbpdiast_present_31_33==T &
     T2_bpontime_31_33==F,T2_bpontime_31_33:=TRUE]

xtabs(~T2$T2_bpontime_31_33)


################
#31-33 weeks
################

#management

#management
#31 weeks
#mild 
T2[,T2_manmildhtn_31_31:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_31_31==T|
                            T2_anbpsyst_mildHTN_31_31==T),T2_manmildhtn_31_31:=F]

T2[T2_manmildhtn_31_31==F & (T2_anbpsyst_present_32_32==T &
                               T2_anbpdiast_present_32_32==T),T2_manmildhtn_31_31:=T]

# mod/sev 
T2[,T2_manmodsevhtn_31_31:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_31_31==T|
                            T2_anbpsyst_modSevHTN_31_31==T),T2_manmodsevhtn_31_31:=F]

T2[T2_manmodsevhtn_31_31==F & (T2_refHR_31_31==T|
                                 T2_refHosp_31_31==T|
                                 T2_refSpec_31_31==T),T2_manmodsevhtn_31_31:=T]

xtabs(~T2$T2_manmodsevhtn_31_31, addNA=T)

# 32 weeks
#mild 
T2[,T2_manmildhtn_32_32:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_32_32==T|
                            T2_anbpsyst_mildHTN_32_32==T),T2_manmildhtn_32_32:=F]

T2[T2_manmildhtn_32_32==F & (T2_anbpsyst_present_33_33==T &
                               T2_anbpdiast_present_33_33==T),T2_manmildhtn_32_32:=T]

# mod/sev 
T2[,T2_manmodsevhtn_32_32:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_32_32==T|
                            T2_anbpsyst_modSevHTN_32_32==T),T2_manmodsevhtn_32_32:=F]

T2[T2_manmodsevhtn_32_32==F & (T2_refHR_32_32==T|
                                 T2_refHosp_32_32==T|
                                 T2_refSpec_32_32==T),T2_manmodsevhtn_32_32:=T]

xtabs(~T2$T2_manmodsevhtn_32_32, addNA=T)

# 33 weeks
#mild 
T2[,T2_manmildhtn_33_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_33_33==T|
                            T2_anbpsyst_mildHTN_33_33==T),T2_manmildhtn_33_33:=F]

T2[T2_manmildhtn_33_33==F & (T2_anbpsyst_present_34_34==T &
                               T2_anbpdiast_present_34_34==T),T2_manmildhtn_33_33:=T]

# mod/sev 
T2[,T2_manmodsevhtn_33_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_33_33==T|
                            T2_anbpsyst_modSevHTN_33_33==T),T2_manmodsevhtn_33_33:=F]

T2[T2_manmodsevhtn_33_33==F & (T2_refHR_33_33==T|
                                 T2_refHosp_33_33==T|
                                 T2_refSpec_33_33==T),T2_manmodsevhtn_33_33:=T]

xtabs(~T2$T2_manmodsevhtn_33_33, addNA=T)


################
# combined 31-33
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildhtn_31_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (!is.na(T2_manmildhtn_31_31)|
                            !is.na(T2_manmildhtn_32_32)|
                            !is.na(T2_manmildhtn_33_33)),T2_manmildhtn_31_33:=F]

xtabs(~T2$T2_manmildhtn_31_33, addNA=T)


T2[T2_manmildhtn_31_33==F & (T2_manmildhtn_31_31==T|
                               T2_manmildhtn_32_32==T|
                               T2_manmildhtn_33_33==T),T2_manmildhtn_31_33:=T]

xtabs(~T2$T2_manmildhtn_31_33, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevhtn_31_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (!is.na(T2_manmodsevhtn_31_31)|
                            !is.na(T2_manmodsevhtn_32_32)|
                            !is.na(T2_manmodsevhtn_33_33)),T2_manmodsevhtn_31_33:=F]

xtabs(~T2$T2_manmodsevhtn_31_33, addNA=T)


T2[T2_manmodsevhtn_31_33==F & (T2_manmodsevhtn_31_31==T|
                                 T2_manmodsevhtn_32_32==T|
                                 T2_manmodsevhtn_33_33==T),T2_manmodsevhtn_31_33:=T]

xtabs(~T2$T2_manmodsevhtn_31_33, addNA=T)

################
# 34 weeks
################

#screening
T2[,T2_Oppt_bp_34_34:=as.logical(NA)]
T2[T2_anvisitnew_34_34==T &
     is.na(T2_manmodsevhtn_31_33) & 
     is.na(T2_manmildhtn_31_33),T2_Oppt_bp_34_34:=TRUE]
xtabs(~T2$T2_Oppt_bp_34_34,addNA=T)

# numerator
T2[,T2_bpontime_34_34:=as.logical(NA)]
T2[T2_Oppt_bp_34_34==TRUE,T2_bpontime_34_34:=FALSE]
T2[T2_anbpsyst_present_34_34==T &
     T2_anbpdiast_present_34_34==T &
     T2_bpontime_34_34==F,T2_bpontime_34_34:=TRUE]

xtabs(~T2$T2_bpontime_34_34)

#management
#34 weeks
#mild 
T2[,T2_manmildhtn_34_34:=as.logical(NA)]
T2[T2_bpontime_34_34==T & (T2_anbpdiast_mildHTN_34_34==T|
                            T2_anbpsyst_mildHTN_34_34==T),T2_manmildhtn_34_34:=F]

T2[T2_manmildhtn_34_34==F & (T2_anbpsyst_present_35_35==T &
                               T2_anbpdiast_present_35_35==T),T2_manmildhtn_34_34:=T]

# mod/sev 
T2[,T2_manmodsevhtn_34_34:=as.logical(NA)]
T2[T2_Oppt_bp_34_34==T & (T2_anbpdiast_modSevHTN_34_34==T|
                            T2_anbpsyst_modSevHTN_34_34==T),T2_manmodsevhtn_34_34:=F]

T2[T2_manmodsevhtn_34_34==F & (T2_refHR_34_34==T|
                                 T2_refHosp_34_34==T|
                                 T2_refSpec_34_34==T),T2_manmodsevhtn_34_34:=T]

xtabs(~T2$T2_manmodsevhtn_34_34, addNA=T)


################
#35-37 weeks
################

#screening
T2[,T2_Oppt_bp_35_37:=as.logical(NA)]
T2[T2_anvisitnew_35_37==T &
     is.na(T2_manmildhtn_34_34) &
     is.na(T2_manmodsevhtn_34_34),T2_Oppt_bp_35_37:=TRUE]
xtabs(~T2$T2_Oppt_bp_35_37,addNA=T)

# numerator
T2[,T2_bpontime_35_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==TRUE,T2_bpontime_35_37:=FALSE]
T2[T2_anbpsyst_present_35_37==T &
     T2_anbpdiast_present_35_37==T &
     T2_bpontime_35_37==F,T2_bpontime_35_37:=TRUE]

xtabs(~T2$T2_bpontime_35_37)


################
#35-37 weeks
################
#management
#35-37 weeks
#mild 
T2[,T2_manmildhtn_35_35:=as.logical(NA)]
T2[T2_bpontime_35_37==T & (T2_anbpdiast_mildHTN_35_35==T|
                             T2_anbpsyst_mildHTN_35_35==T),T2_manmildhtn_35_35:=F]

T2[T2_manmildhtn_35_35==F & (T2_anbpsyst_present_36_36==T &
                               T2_anbpdiast_present_36_36==T),T2_manmildhtn_35_35:=T]

xtabs(~T2$T2_manmildhtn_35_35, addNA=T)

# mod/sev 
T2[,T2_manmodsevhtn_35_35:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_35_35==T|
                            T2_anbpsyst_modSevHTN_35_35==T),T2_manmodsevhtn_35_35:=F]

T2[T2_manmodsevhtn_35_35==F & (T2_refHR_35_35==T|
                                 T2_refHosp_35_35==T|
                                 T2_refSpec_35_35==T),T2_manmodsevhtn_35_35:=T]

xtabs(~T2$T2_manmodsevhtn_35_35, addNA=T)

# 36
#mild 
T2[,T2_manmildhtn_36_36:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_mildHTN_36_36==T|
                             T2_anbpsyst_mildHTN_36_36==T),T2_manmildhtn_36_36:=F]

T2[T2_manmildhtn_36_36==F & (T2_anbpsyst_present_37_37==T &
                               T2_anbpdiast_present_37_37==T),T2_manmildhtn_36_36:=T]

xtabs(~T2$T2_manmildhtn_36_36, addNA=T)


# mod/sev 
T2[,T2_manmodsevhtn_36_36:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_36_36==T|
                            T2_anbpsyst_modSevHTN_36_36==T),T2_manmodsevhtn_36_36:=F]

T2[T2_manmodsevhtn_36_36==F & (T2_refHR_36_36==T|
                                 T2_refHosp_36_36==T|
                                 T2_refSpec_36_36==T),T2_manmodsevhtn_36_36:=T]

xtabs(~T2$T2_manmodsevhtn_36_36, addNA=T)



# 37
#mild 
T2[,T2_manmildhtn_37_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_mildHTN_37_37==T|
                             T2_anbpsyst_mildHTN_37_37==T),T2_manmildhtn_37_37:=F]

T2[T2_manmildhtn_37_37==F & (T2_anbpsyst_present_38_38==T &
                               T2_anbpdiast_present_38_38==T),T2_manmildhtn_37_37:=T]

xtabs(~T2$T2_manmildhtn_37_37, addNA=T)

# mod/sev 
T2[,T2_manmodsevhtn_37_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_37_37==T|
                            T2_anbpsyst_modSevHTN_37_37==T),T2_manmodsevhtn_37_37:=F]

T2[T2_manmodsevhtn_37_37==F & (T2_refHR_37_37==T|
                                 T2_refHosp_37_37==T|
                                 T2_refSpec_37_37==T),T2_manmodsevhtn_37_37:=T]

xtabs(~T2$T2_manmodsevhtn_37_37, addNA=T)




################
# combined 35-37
################


#T2_manmildchronichtn 15-17 weeks combo variable
T2[,T2_manmildhtn_35_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (!is.na(T2_manmildhtn_35_35)|
                            !is.na(T2_manmildhtn_36_36)|
                            !is.na(T2_manmildhtn_37_37)),T2_manmildhtn_35_37:=F]

xtabs(~T2$T2_manmildhtn_35_37, addNA=T)


T2[T2_manmildhtn_35_37==F & (T2_manmildhtn_35_35==T|
                               T2_manmildhtn_36_36==T|
                               T2_manmildhtn_37_37==T),T2_manmildhtn_35_37:=T]

xtabs(~T2$T2_manmildhtn_35_37, addNA=T)





# severe chronic htn

#T2_manmodsevchronichtn 15-17 weeks combo variable
T2[,T2_manmodsevhtn_35_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (!is.na(T2_manmodsevhtn_35_35)|
                            !is.na(T2_manmodsevhtn_36_36)|
                            !is.na(T2_manmodsevhtn_37_37)),T2_manmodsevhtn_35_37:=F]

xtabs(~T2$T2_manmodsevhtn_35_37, addNA=T)


T2[T2_manmodsevhtn_35_37==F & (T2_manmodsevhtn_35_35==T|
                                 T2_manmodsevhtn_36_36==T|
                                 T2_manmodsevhtn_37_37==T),T2_manmodsevhtn_35_37:=T]

xtabs(~T2$T2_manmodsevhtn_35_37, addNA=T)


# change variable types

varshtn <- c("T2_Oppt_bp_00_14",
             "T2_bpontime_00_14",
             "T2_manchronichtn_00_14",
             "T2_Oppt_bp_15_17",
             "T2_bpontime_15_17",
             "T2_Oppt_bp_18_22",
             "T2_bpontime_18_22",
             "T2_Oppt_bp_23_23",
             "T2_bpontime_23_23",
             "T2_Oppt_bp_24_28",
             "T2_bpontime_24_28",
             "T2_Oppt_bp_31_33",
             "T2_bpontime_31_33",
             "T2_Oppt_bp_34_34",
             "T2_bpontime_34_34",
             "T2_Oppt_bp_35_37",
             "T2_bpontime_35_37",
             "T2_manmildchronichtn_15_15",
             "T2_manmodsevchronichtn_15_15",
             "T2_manmildchronichtn_16_16",
             "T2_manmodsevchronichtn_16_16",
             "T2_manmildchronichtn_17_17",
             "T2_manmodsevchronichtn_17_17",
             "T2_manmildchronichtn_15_17",
             "T2_manmodsevchronichtn_15_17",
             "T2_manmildchronichtn_18_18",
             "T2_manmodsevchronichtn_18_18",
             "T2_manmildchronichtn_19_19",
             "T2_manmodsevchronichtn_19_19",
             "T2_manmildchronichtn_20_20",
             "T2_manmodsevchronichtn_20_20",
             "T2_manmildchronichtn_21_21",
             "T2_manmodsevchronichtn_21_21",
             "T2_manmildchronichtn_22_22",
             "T2_manmodsevchronichtn_22_22",
             "T2_manmildchronichtn_18_22",
             "T2_manmodsevchronichtn_18_22",
             "T2_manmildchronichtn_23_23",
             "T2_manmodsevchronichtn_23_23",
             "T2_manmildhtn_24_24",
             "T2_manmodsevhtn_24_24",
             "T2_manmildhtn_25_25",
             "T2_manmodsevhtn_25_25",
             "T2_manmildhtn_26_26",
             "T2_manmodsevhtn_26_26",
             "T2_manmildhtn_27_27",
             "T2_manmodsevhtn_27_27",
             "T2_manmildhtn_28_28",
             "T2_manmodsevhtn_28_28",
             "T2_manmildhtn_24_28",
             "T2_manmodsevhtn_24_28",
             "T2_manmildhtn_29_29",
             "T2_manmodsevhtn_29_29",
             "T2_manmildhtn_30_30",
             "T2_manmodsevhtn_30_30",
             "T2_manmildhtn_29_30",
             "T2_manmodsevhtn_29_30",
             "T2_manmildhtn_31_31",
             "T2_manmodsevhtn_31_31",
             "T2_manmildhtn_32_32",
             "T2_manmodsevhtn_32_32",
             "T2_manmildhtn_33_33",
             "T2_manmodsevhtn_33_33",
             "T2_manmildhtn_31_33",
             "T2_manmodsevhtn_31_33",
             "T2_manmildhtn_34_34",
             "T2_manmodsevhtn_34_34",
             "T2_manmildhtn_35_35",
             "T2_manmodsevhtn_35_35",
             "T2_manmildhtn_36_36",
             "T2_manmodsevhtn_36_36",
             "T2_manmildhtn_37_37",
             "T2_manmodsevhtn_37_37",
             "T2_manmildhtn_35_37",
             "T2_manmodsevhtn_35_37"
             
              )



for (i in varshtn){
  
  
  T2[,(i):=as.character(get(i))]
  
  T2[stringr::str_detect(get(i),"TRUE"), (i):="Successful"]
  T2[stringr::str_detect(get(i),"FALSE"), (i):="Not Successful"]
  T2[is.na(get(i)), (i):="Not Applicable"]
  
}



xtabs(~T2$T2_manmildhtn_35_37, addNA=T)
