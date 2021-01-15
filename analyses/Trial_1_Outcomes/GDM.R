
########## GDM ########## 


################
#24-28 weeks
################
#screening

#denom
T2[,denom_gdm_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T, denom_gdm_24_28:=TRUE ]

xtabs(~T2$denom_gdm_24_28)

# num
T2[,num_gdm_24_28:=as.logical(NA)]
T2[denom_gdm_24_28==T,num_gdm_24_28:=FALSE]
T2[num_gdm_24_28==F &
     (T2_labfastbloodglu_exists_24_28==T |
        T2_labbloodglu_exists_24_28==T),num_gdm_24_28:=TRUE]

xtabs(~T2$num_gdm_24_28, addNA=T)


# managements

T2[,manlikelygdm_24_28:=as.logical(NA)]
T2[denom_gdm_24_28==T &
     T2_labfastbloodglu_likelyGDM_24_28==T, manlikelygdm_24_28:=FALSE]

T2[manlikelygdm_24_28==F &
     ((T2_labfastbloodglu_exists_27_27==T|
         T2_labfastbloodglu_exists_28_28==T|
         T2_labfastbloodglu_exists_29_29==T|
         T2_labfastbloodglu_exists_30_30==T|
         T2_labfastbloodglu_exists_31_31==T|
         T2_labfastbloodglu_exists_32_32==T|
         T2_labfastbloodglu_exists_33_33==T)|(T2_labbloodglu_exists_27_27==T|
                                                T2_labbloodglu_exists_28_28==T|
                                                T2_labbloodglu_exists_29_29==T|
                                                T2_labbloodglu_exists_30_30==T|
                                                T2_labbloodglu_exists_31_31==T|
                                                T2_labbloodglu_exists_32_32==T|
                                                T2_labbloodglu_exists_33_33==T)), manlikelygdm_24_28:=T]

xtabs(~T2$manlikelygdm_24_28,addNA=T)

# man high rbg

T2[,manhighrbg_24_28:=as.logical(NA)]
T2[denom_gdm_24_28==T & T2_labbloodglu_high_24_28==T, manhighrbg_24_28:=F]

T2[manhighrbg_24_28==F &
     (T2_manRBGHigh_Diab_24_24==T|
        T2_manRBGHigh_Diab_25_25==T|
        T2_manRBGHigh_Diab_26_26==T|
        T2_manRBGHigh_Diab_27_27==T|
        T2_manRBGHigh_Diab_28_28==T ),manhighrbg_24_28:=T]

xtabs(~T2$manhighrbg_24_28,addNA=T)

