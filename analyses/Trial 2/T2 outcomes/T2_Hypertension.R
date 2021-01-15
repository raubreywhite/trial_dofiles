### Need to import data set saved from attendance script

# need opportunity of visits first


########## Hypertension ########## 
#define chronic?
# high blood pressure before or on 22 weeks 
# 



################
#24-28 weeks
################

#screening
T2[,denom_bp_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T,denom_bp_24_28:=TRUE]
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


