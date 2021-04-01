### Need to import data set saved from attendance script

# need opportunity of visits first


########## Hypertension ########## 
#define chronic?
# high blood pressure before or on 22 weeks

################
#00-14 weeks
################

#screening
T2[,denom_bp_00_14:=as.logical(NA)]
T2[T2_anvisitnew_00_14==T,denom_bp_00_14:=TRUE]
xtabs(~T2$denom_bp_00_14,addNA=T)

# numerator
T2[,bpontime_00_14:=as.logical(NA)]
T2[denom_bp_00_14==TRUE,bpontime_00_14:=FALSE]
T2[T2_anbpsyst_present_00_14==T &
     T2_anbpdiast_present_00_14==T &
     bpontime_00_14==F,bpontime_00_14:=TRUE]

xtabs(~T2$bpontime_00_14)



#management
#denom
T2[,manchronichtn_00_14:=as.logical(NA)]
T2[denom_bp_00_14==T & (T2_anbpdiast_mildHTN_00_14==T|
                          T2_anbpsyst_mildHTN_00_14==T|
                          T2_anbpsyst_modSevHTN_00_14==T|
                          T2_anbpdiast_modSevHTN_00_14==T),manchronichtn_00_14:=T]

xtabs(~T2$manchronichtn_00_14, addNA=T)



################
#15-17 weeks
################
# T2_anvisitnew: take opportunity and success from the attendance script


#screening
T2[,denom_bp_15_17:=as.logical(NA)]
T2[T2_anvisitnew_15_17==T &
     is.na(manchronichtn_00_14),denom_bp_15_17:=TRUE]
xtabs(~T2$denom_bp_15_17,addNA=T)

# numerator
T2[,bpontime_15_17:=as.logical(NA)]
T2[denom_bp_15_17==TRUE,bpontime_15_17:=FALSE]
T2[T2_anbpsyst_present_15_17==T &
     T2_anbpdiast_present_15_17==T &
     bpontime_15_17==F,bpontime_15_17:=TRUE]

xtabs(~T2$bpontime_15_17)


################
#15-17 weeks
################
#management
#denom
T2[,manchronichtn_15_17:=as.logical(NA)]
T2[denom_bp_15_17==T & (T2_anbpdiast_mildHTN_15_17==T|
                          T2_anbpsyst_mildHTN_15_17==T|
                          T2_anbpsyst_modSevHTN_15_17==T|
                          T2_anbpdiast_modSevHTN_15_17==T),manchronichtn_15_17:=F]

xtabs(~T2$manchronichtn_15_17, addNA=T)


T2[manchronichtn_15_17==F & (T2_refHR_15_15==T|
                            T2_refHosp_16_16==T|
                            T2_refSpec_17_17==T),manchronichtn_15_17:=T]

xtabs(~T2$manchronichtn_15_17, addNA=T)


################
#18-22 weeks
################

#screening
T2[,denom_bp_18_22:=as.logical(NA)]
T2[T2_anvisitnew_18_22==T &
     is.na(manchronichtn_15_17),denom_bp_18_22:=TRUE]
xtabs(~T2$denom_bp_18_22,addNA=T)

# numerator
T2[,bpontime_18_22:=as.logical(NA)]
T2[denom_bp_18_22==TRUE,bpontime_18_22:=FALSE]
T2[T2_anbpsyst_present_18_22==T &
     T2_anbpdiast_present_18_22==T &
     bpontime_18_22==F,bpontime_18_22:=TRUE]

xtabs(~T2$bpontime_18_22)


################
#18-22 weeks
################

#management
#denom
T2[,manchronichtn_18_22:=as.logical(NA)]
T2[denom_bp_15_17==T & (T2_anbpdiast_mildHTN_18_22==T|
                          T2_anbpsyst_mildHTN_18_22==T|
                          T2_anbpsyst_modSevHTN_18_22==T|
                          T2_anbpdiast_modSevHTN_18_22==T),manchronichtn_18_22:=F]

xtabs(~T2$manchronichtn_18_22, addNA=T)


T2[manchronichtn_18_22==F & (T2_refHR_18_22==T|
                               T2_refHosp_18_22==T|
                               T2_refSpec_18_22==T),manchronichtn_18_22:=T]

xtabs(~T2$manchronichtn_18_22, addNA=T)



################
#24-28 weeks
################

#screening
T2[,denom_bp_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T &
     is.na(manchronichtn_18_22),denom_bp_24_28:=TRUE]
xtabs(~T2$denom_bp_24_28,addNA=T)

# numerator
T2[,bpontime_24_28:=as.logical(NA)]
T2[denom_bp_24_28==TRUE,bpontime_24_28:=FALSE]
T2[T2_anbpsyst_present_24_28==T &
     T2_anbpdiast_present_24_28==T &
     bpontime_24_28==F,bpontime_24_28:=TRUE]

xtabs(~T2$bpontime_24_28)


################
#24-28 weeks
################

#management
#denom
T2[,manmildhtn_24_28:=as.logical(NA)]
T2[denom_bp_24_28==T & (T2_anbpdiast_mildHTN_24_28==T|
                          T2_anbpsyst_mildHTN_24_28==T),manmildhtn_24_28:=F]

xtabs(~T2$manmildhtn_24_28, addNA=T)


T2[manmildhtn_24_28==F &
     ((T2_anbpsyst_present_25_25==T & T2_anbpdiast_present_25_25==T)|
        (T2_anbpsyst_present_26_26==T & T2_anbpdiast_present_26_26==T)|
        (T2_anbpsyst_present_27_27==T & T2_anbpdiast_present_27_27==T )|
        (T2_anbpsyst_present_28_28==T & T2_anbpdiast_present_28_28==T) |
        (T2_anbpsyst_present_29_29==T & T2_anbpdiast_present_29_29==T)),manmildhtn_24_28:=T]

xtabs(~T2$manmildhtn_24_28, addNA=T)


# 24-28 weeks severe anemia
T2[,manmodsevhtn_24_28:=as.logical(NA)]
T2[denom_bp_24_28==T & (T2_anbpdiast_modSevHTN_24_28==T|
                          T2_anbpsyst_modSevHTN_24_28==T),manmodsevhtn_24_28:=F]

xtabs(~T2$manmodsevhtn_24_28, addNA=T)

T2[manmodsevhtn_24_28==F & 
     (T2_manhtn_ModSev_24_24==T |
        T2_manhtn_ModSev_25_25==T |
        T2_manhtn_ModSev_26_26==T |
        T2_manhtn_ModSev_27_27==T |
        T2_manhtn_ModSev_28_28==T),manmodsevhtn_24_28:=T]

xtabs(~T2$manmodsevhtn_24_28, addNA=T)



################
#31-33 weeks
################

#screening
T2[,denom_bp_31_33:=as.logical(NA)]
T2[T2_anvisitnew_31_33==T &
          is.na(manmodsevhtn_24_28) & 
          is.na(manmildhtn_24_28),denom_bp_31_33:=TRUE]
xtabs(~T2$denom_bp_31_33,addNA=T)

# numerator
T2[,bpontime_31_33:=as.logical(NA)]
T2[denom_bp_31_33==TRUE,bpontime_31_33:=FALSE]
T2[T2_anbpsyst_present_31_33==T &
     T2_anbpdiast_present_31_33==T &
     bpontime_31_33==F,bpontime_31_33:=TRUE]

xtabs(~T2$bpontime_31_33)


################
#31-33 weeks
################

#management
#denom
T2[,manmildhtn_31_33:=as.logical(NA)]
T2[denom_bp_31_33==T & (T2_anbpdiast_mildHTN_31_33==T|
                          T2_anbpsyst_mildHTN_31_33==T),manmildhtn_31_33:=F]

xtabs(~T2$manmildhtn_31_33, addNA=T)


T2[manmildhtn_31_33==F &
     ((T2_anbpsyst_present_31_31==T & T2_anbpdiast_present_34_34==T)|
        (T2_anbpsyst_present_32_32==T & T2_anbpdiast_present_32_32==T)|
        (T2_anbpsyst_present_33_33==T & T2_anbpdiast_present_33_33==T)),manmildhtn_31_33:=T]

xtabs(~T2$manmildhtn_31_33, addNA=T)


# 31-33 weeks severe anemia
T2[,manmodsevhtn_31_33:=as.logical(NA)]
T2[denom_bp_31_33==T & (T2_anbpdiast_modSevHTN_31_33==T|
                          T2_anbpsyst_modSevHTN_31_33==T),manmodsevhtn_31_33:=F]

xtabs(~T2$manmodsevhtn_31_33, addNA=T)

T2[manmodsevhtn_31_33==F & 
     (T2_manhtn_ModSev_31_31==T |
        T2_manhtn_ModSev_32_32==T |
        T2_manhtn_ModSev_33_33==T),manmodsevhtn_31_33:=T]

xtabs(~T2$manmodsevhtn_31_33, addNA=T)




################
#35-37 weeks
################

#screening
T2[,denom_bp_35_37:=as.logical(NA)]
T2[T2_anvisitnew_35_37==T &
     is.na(manmildhtn_31_33) &
     is.na(manmodsevhtn_31_33),denom_bp_35_37:=TRUE]
xtabs(~T2$denom_bp_35_37,addNA=T)

# numerator
T2[,bpontime_35_37:=as.logical(NA)]
T2[denom_bp_35_37==TRUE,bpontime_35_37:=FALSE]
T2[T2_anbpsyst_present_35_37==T &
     T2_anbpdiast_present_35_37==T &
     bpontime_35_37==F,bpontime_35_37:=TRUE]

xtabs(~T2$bpontime_35_37)


################
#35-37 weeks
################

#management
#denom
T2[,manmildhtn_35_37:=as.logical(NA)]
T2[denom_bp_35_37==T & (T2_anbpdiast_mildHTN_35_37==T|
                          T2_anbpsyst_mildHTN_35_37==T),manmildhtn_35_37:=F]

xtabs(~T2$manmildhtn_35_37, addNA=T)


T2[manmildhtn_35_37==F &
     ((T2_anbpsyst_present_35_35==T & T2_anbpdiast_present_38_38==T)|
        (T2_anbpsyst_present_36_36==T & T2_anbpdiast_present_36_36==T)|
        (T2_anbpsyst_present_37_37==T & T2_anbpdiast_present_37_37==T)),manmildhtn_35_37:=T]

xtabs(~T2$manmildhtn_35_37, addNA=T)


# 35-37 weeks severe htn
T2[,manmodsevhtn_35_37:=as.logical(NA)]
T2[denom_bp_35_37==T & (T2_anbpdiast_modSevHTN_35_37==T|
                          T2_anbpsyst_modSevHTN_35_37==T),manmodsevhtn_35_37:=F]

xtabs(~T2$manmodsevhtn_35_37, addNA=T)

T2[manmodsevhtn_35_37==F & 
     (T2_manhtn_ModSev_35_35==T |
        T2_manhtn_ModSev_36_36==T |
        T2_manhtn_ModSev_37_37==T),manmodsevhtn_35_37:=T]

xtabs(~T2$manmodsevhtn_35_37, addNA=T)


