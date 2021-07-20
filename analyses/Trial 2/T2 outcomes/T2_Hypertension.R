


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
#T2_Oppt
T2[,T2_manchronichtn_15_17:=as.logical(NA)]
T2[T2_Oppt_bp_15_17==T & (T2_anbpdiast_mildHTN_15_17==T|
                          T2_anbpsyst_mildHTN_15_17==T|
                          T2_anbpsyst_modSevHTN_15_17==T|
                          T2_anbpdiast_modSevHTN_15_17==T),T2_manchronichtn_15_17:=F]

xtabs(~T2$T2_manchronichtn_15_17, addNA=T)


T2[T2_manchronichtn_15_17==F & (T2_refHR_15_15==T|
                                  T2_refHR_16_16|
                                  T2_refHR_17_17|
                            T2_refHosp_15_15==T|
                            T2_refHosp_16_16==T|
                            T2_refHosp_17_17==T|
                            T2_refSpec_17_17==T|
                            T2_refSpec_16_16==T|
                            T2_refSpec_15_15==T),T2_manchronichtn_15_17:=T]

xtabs(~T2$T2_manchronichtn_15_17, addNA=T)


################
#18-22 weeks
################

#screening
T2[,T2_Oppt_bp_18_22:=as.logical(NA)]
T2[T2_anvisitnew_18_22==T &
     is.na(T2_manchronichtn_15_17),T2_Oppt_bp_18_22:=TRUE]
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
#T2_Oppt
T2[,T2_manchronichtn_18_22:=as.logical(NA)]
T2[T2_Oppt_bp_18_22==T & (T2_anbpdiast_mildHTN_18_22==T|
                          T2_anbpsyst_mildHTN_18_22==T|
                          T2_anbpsyst_modSevHTN_18_22==T|
                          T2_anbpdiast_modSevHTN_18_22==T),T2_manchronichtn_18_22:=F]

xtabs(~T2$T2_manchronichtn_18_22, addNA=T)


T2[T2_manchronichtn_18_22==F & (T2_refHR_18_22==T|
                               T2_refHosp_18_22==T|
                               T2_refSpec_18_22==T),T2_manchronichtn_18_22:=T]

xtabs(~T2$T2_manchronichtn_18_22, addNA=T)



################
#24-28 weeks
################

#screening
T2[,T2_Oppt_bp_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T &
     is.na(T2_manchronichtn_18_22),T2_Oppt_bp_24_28:=TRUE]
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
#T2_Oppt
T2[,T2_manmildhtn_24_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_mildHTN_24_28==T|
                          T2_anbpsyst_mildHTN_24_28==T),T2_manmildhtn_24_28:=F]

xtabs(~T2$T2_manmildhtn_24_28, addNA=T)


T2[T2_manmildhtn_24_28==F &
     ((T2_anbpsyst_present_25_25==T & T2_anbpdiast_present_25_25==T)|
        (T2_anbpsyst_present_26_26==T & T2_anbpdiast_present_26_26==T)|
        (T2_anbpsyst_present_27_27==T & T2_anbpdiast_present_27_27==T )|
        (T2_anbpsyst_present_28_28==T & T2_anbpdiast_present_28_28==T) |
        (T2_anbpsyst_present_29_29==T & T2_anbpdiast_present_29_29==T)),T2_manmildhtn_24_28:=T]

xtabs(~T2$T2_manmildhtn_24_28, addNA=T)


# 24-28 weeks severe anemia
T2[,T2_manmodsevhtn_24_28:=as.logical(NA)]
T2[T2_Oppt_bp_24_28==T & (T2_anbpdiast_modSevHTN_24_28==T|
                          T2_anbpsyst_modSevHTN_24_28==T),T2_manmodsevhtn_24_28:=F]

xtabs(~T2$T2_manmodsevhtn_24_28, addNA=T)

T2[T2_manmodsevhtn_24_28==F & 
     (T2_manhtn_ModSev_24_24==T |
        T2_manhtn_ModSev_25_25==T |
        T2_manhtn_ModSev_26_26==T |
        T2_manhtn_ModSev_27_27==T |
        T2_manhtn_ModSev_28_28==T),T2_manmodsevhtn_24_28:=T]

xtabs(~T2$T2_manmodsevhtn_24_28, addNA=T)



################
#31-33 weeks
################

#screening
T2[,T2_Oppt_bp_31_33:=as.logical(NA)]
T2[T2_anvisitnew_31_33==T &
          is.na(T2_manmodsevhtn_24_28) & 
          is.na(T2_manmildhtn_24_28),T2_Oppt_bp_31_33:=TRUE]
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
#T2_Oppt
T2[,T2_manmildhtn_31_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_mildHTN_31_33==T|
                          T2_anbpsyst_mildHTN_31_33==T),T2_manmildhtn_31_33:=F]

xtabs(~T2$T2_manmildhtn_31_33, addNA=T)


T2[T2_manmildhtn_31_33==F &
     ((T2_anbpsyst_present_31_31==T & T2_anbpdiast_present_34_34==T)|
        (T2_anbpsyst_present_32_32==T & T2_anbpdiast_present_32_32==T)|
        (T2_anbpsyst_present_33_33==T & T2_anbpdiast_present_33_33==T)),T2_manmildhtn_31_33:=T]

xtabs(~T2$T2_manmildhtn_31_33, addNA=T)


# 31-33 weeks severe hypertensioni
T2[,T2_manmodsevhtn_31_33:=as.logical(NA)]
T2[T2_Oppt_bp_31_33==T & (T2_anbpdiast_modSevHTN_31_33==T|
                          T2_anbpsyst_modSevHTN_31_33==T),T2_manmodsevhtn_31_33:=F]

xtabs(~T2$T2_manmodsevhtn_31_33, addNA=T)

T2[T2_manmodsevhtn_31_33==F & 
     (T2_manhtn_ModSev_31_31==T |
        T2_manhtn_ModSev_32_32==T |
        T2_manhtn_ModSev_33_33==T),T2_manmodsevhtn_31_33:=T]

xtabs(~T2$T2_manmodsevhtn_31_33, addNA=T)




################
#35-37 weeks
################

#screening
T2[,T2_Oppt_bp_35_37:=as.logical(NA)]
T2[T2_anvisitnew_35_37==T &
     is.na(T2_manmildhtn_31_33) &
     is.na(T2_manmodsevhtn_31_33),T2_Oppt_bp_35_37:=TRUE]
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
#T2_Oppt
T2[,T2_manmildhtn_35_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_mildHTN_35_37==T|
                          T2_anbpsyst_mildHTN_35_37==T),T2_manmildhtn_35_37:=F]

xtabs(~T2$T2_manmildhtn_35_37, addNA=T)


T2[T2_manmildhtn_35_37==F &
     ((T2_anbpsyst_present_35_35==T & T2_anbpdiast_present_35_35==T)|
        (T2_anbpsyst_present_36_36==T & T2_anbpdiast_present_36_36==T)|
        (T2_anbpsyst_present_37_37==T & T2_anbpdiast_present_37_37==T)),T2_manmildhtn_35_37:=T]

xtabs(~T2$T2_manmildhtn_35_37, addNA=T)


# 35-37 weeks severe htn
T2[,T2_manmodsevhtn_35_37:=as.logical(NA)]
T2[T2_Oppt_bp_35_37==T & (T2_anbpdiast_modSevHTN_35_37==T|
                          T2_anbpsyst_modSevHTN_35_37==T),T2_manmodsevhtn_35_37:=F]

xtabs(~T2$T2_manmodsevhtn_35_37, addNA=T)

T2[T2_manmodsevhtn_35_37==F & 
     (T2_manhtn_ModSev_35_35==T |
        T2_manhtn_ModSev_36_36==T |
        T2_manhtn_ModSev_37_37==T),T2_manmodsevhtn_35_37:=T]

xtabs(~T2$T2_manmodsevhtn_35_37, addNA=T)



# change variable types

varshtn <- c("T2_Oppt_bp_00_14",
             "T2_bpontime_00_14",
             "T2_manchronichtn_00_14",
             "T2_Oppt_bp_15_17",
             "T2_bpontime_15_17",
             "T2_manchronichtn_15_17",
             "T2_Oppt_bp_18_22",
             "T2_bpontime_18_22",
             "T2_manchronichtn_18_22",
             "T2_Oppt_bp_24_28",
             "T2_bpontime_24_28",
             "T2_manmildhtn_24_28",
             "T2_manmodsevhtn_24_28",
             "T2_Oppt_bp_31_33",
             "T2_bpontime_31_33",
             "T2_manmildhtn_31_33",
             "T2_manmodsevhtn_31_33",
             "T2_Oppt_bp_35_37",
             "T2_bpontime_35_37",
             "T2_manmildhtn_35_37",
             "T2_manmodsevhtn_35_37")



for (i in varshtn){
  
  
  T2[,(i):=as.character(get(i))]
  
  T2[stringr::str_detect(get(i),"TRUE"), (i):="Successful"]
  T2[stringr::str_detect(get(i),"FALSE"), (i):="Not Successful"]
  T2[is.na(get(i)), (i):="Not Applicable"]
  
}



xtabs(~T2$T2_manmildhtn_35_37, addNA=T)
