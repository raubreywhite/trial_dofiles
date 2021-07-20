
###Redefining opportinites
T2[,T2_Opportunity_GDM_screening_b4_24:=as.numeric(NA)]
T2[,T2_Opportunity_GDM_screening_24_28:=as.numeric(NA)]
T2[,T2_Opportunity_GDM_screening_after_28:=as.numeric(NA)]
T2[,T2_Opportunity_GDM_screening_high:=as.numeric(NA)]


# B4 24 weeks
################
 

# before 24
T2[bookgestagedays_cats %in% c("(0,104]",
                               "(104,125]",
                               "(125,160]",
                               "(160,167]"),T2_Opportunity_GDM_screening_b4_24:=1]

xtabs(~T2$T2_Opportunity_GDM_screening_b4_24, addNA=T)


## Remove opportunities for people who were referred to HR or Hosp
#T2_RefHrHospmanRBG_1 rename to T2_RefHr
T2[,T2_RefHr:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_b4_24==1, T2_RefHr:=FALSE]
T2[((T2_manRef_HR_00_00==T|
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
             T2_manFBSHigh_spec_23_23==T)),T2_RefHr:=TRUE]

xtabs(~T2$T2_RefHr, addNA=T)




################
# opportunities
################

#Screening before 24 weeks: Creating one var for 3 possibilities
T2[,screenb424:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_b4_24==1,
   screenb424:=F]
T2[screenb424==F &
     (T2_labfastbloodglu_exists_00_14==T|
        T2_labfastbloodglu_exists_15_17==T |
        T2_labfastbloodglu_exists_18_22==T |
        T2_labfastbloodglu_exists_23_23==T|
        T2_labbloodglu_exists_00_14==T|
        T2_labbloodglu_exists_15_17==T|
        T2_labbloodglu_exists_18_22==T|
        T2_labbloodglu_exists_23_23==T)|
     (T2_laburglu_exists_00_14==T|
        T2_laburglu_exists_15_17==T |
        T2_laburglu_exists_18_22==T|
        T2_laburglu_exists_23_23==T),screenb424:=T]

xtabs(~T2$screenb424, addNA=T)


T2[,T2_GDMscreeningontime_b4_24_normal:=as.logical(NA)]
T2[,T2_GDMscreeningontime_b4_24_posurglu:=as.logical(NA)]
T2[,T2_GDMscreeningontime_1C:=as.logical(NA)]

T2[screenb424==F, 
   T2_GDMscreeningontime_b4_24:=FALSE]
T2[screenb424==T, 
   T2_GDMscreeningontime_b4_24:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24, addNA=T)

#normal Values/ negative values
T2[,T2_GDMscreeningontime_b4_24_normal:=as.logical(NA)]


T2[T2_Opportunity_GDM_screening_b4_24==1 & 
     !is.na(booklaburglu), 
   T2_GDMscreeningontime_b4_24_normal:=FALSE]


T2[T2_Opportunity_GDM_screening_b4_24==1 & 
     booklaburglu=="NEG", 
   T2_GDMscreeningontime_b4_24_normal:=TRUE]

# high urglu or blood glu


# managements
# gdm screening if have pos urglu and have h
T2[,T2_GDMscreeningontime_b4_24_posurglu:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_b4_24==1 &
     booklaburglu=="POS",T2_GDMscreeningontime_b4_24_posurglu:=FALSE]

T2[T2_GDMscreeningontime_b4_24_posurglu==F &
     (!is.na(booklabbloodglu)|
        !is.na(booklabfastbloodglu)), T2_GDMscreeningontime_b4_24_posurglu:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24_posurglu, addNA=T)




################################
## 24-28 weeks
################################

###################
# 24- 28  weeks opp
###################

#24-28
T2[T2_anvisitnew_24_28==T |
     bookgestagedays_cats %in% c("(167,202]"),T2_Opportunity_GDM_screening_24_28:=1]

T2[T2_Opportunity_GDM_screening_24_28==1 &
     (T2_labbloodglu_high_00_14==T|
        T2_labbloodglu_high_15_17==T|
        T2_labbloodglu_high_18_22==T|
        T2_labbloodglu_high_23_23==T|
        T2_labfastbloodglu_high_00_14==T|
        T2_labfastbloodglu_high_15_17==T|
        T2_labfastbloodglu_high_18_22==T|
        T2_labfastbloodglu_high_23_23==T),
   T2_Opportunity_GDM_screening_24_28:=T2_Opportunity_GDM_screening_24_28-1]




if(IS_GAZA==F){

T2[,T2_GDMscreeningontime_24_28:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_24_28==1 &
            ((T2_labbloodglu_exists_24_28=T|
                T2_labfastbloodglu_exists_24_28==T)), T2_GDMscreeningontime_24_28:=T]
xtabs(~T2$T2_GDMscreeningontime_24_28, addNA=T)


T2[,T2_GDMscreeningontime_24_28_normal:=as.logical(NA)]
T2[T2_GDMscreeningontime_24_28==T,T2_GDMscreeningontime_24_28_normal:=F]

T2[T2_GDMscreeningontime_24_28==T &
            (T2_labbloodglu_normal_24_28==T|
               T2_labfastbloodglu_normal_24_28==T),T2_GDMscreeningontime_24_28_normal:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_24_28_normal, addNA=T)


# high rbg at 24-28 weeks
T2[,T2_GDMscreeningontime_24_28_highrbg:=as.logical(NA)]

#identified as high blood sugar and not managed yet = F
# true value = managed
T2[T2_GDMscreeningontime_24_28==T &
         (T2_labbloodglu_high_24_28==T|
            T2_labfastbloodglu_high_24_28==T),T2_GDMscreeningontime_24_28_highrbg:=FALSE]

xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA=T)


# high value managed

T2[T2_GDMscreeningontime_24_28_highrbg==F &
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
            T2_manRef_spec_28_28==T)), T2_GDMscreeningontime_24_28_highrbg:=T]

xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA=T)


# intermediate values,  but dont want them for WB because management is in free text
    T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]



} else {
  
  
  T2[,T2_GDMscreeningontime_24_28:=as.logical(NA)]
  T2[T2_Opportunity_GDM_screening_24_28==1 &
          (T2_labfastbloodglu_exists_24_28==T), T2_GDMscreeningontime_24_28:=T]
  
  
  
  T2[,T2_GDMscreeningontime_24_28_normal:=as.logical(NA)]

  
  T2[T2_GDMscreeningontime_24_28==T &
        (T2_labfastbloodglu_exists_24_28==T|
           T2_labbloodglu_exists_24_28==T), T2_GDMscreeningontime_24_28_normal:=F]
  
  T2[T2_Opportunity_GDM_screening_24_28==1 & 
           T2_GDMscreeningontime_24_28_normal==F &
          (T2_labfastbloodglu_normal_24_28==T|
             T2_labbloodglu_normal_24_28==T),T2_GDMscreeningontime_24_28_normal:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_normal, addNA=T)
  
  # high rbg at 24-28 weeks
  T2[,T2_GDMscreeningontime_24_28_highrbg:=as.logical(NA)]
  
  #identified as high blood sugar and not managed yet = F
  # true value = managed
  T2[T2_Opportunity_GDM_screening_24_28==1 & 
           #is.na(T2_GDMscreeningontime_2) &
           T2_GDMscreeningontime_24_28== TRUE &
           (T2_labfastbloodglu_high_24_28==T|
              T2_labbloodglu_high_24_28==T),T2_GDMscreeningontime_24_28_highrbg:=FALSE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA=T)
  
  
  # high value managed
  
  T2[T2_GDMscreeningontime_24_28_highrbg==F &
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
          (T2_manFBSHigh_spec_24_24==T|
             T2_manFBSHigh_spec_25_25==T|
             T2_manFBSHigh_spec_26_26==T|
             T2_manFBSHigh_spec_27_27==T|
             T2_manFBSHigh_spec_28_28==T)|
          (T2_manRef_HR_24_24==T|
             T2_manRef_HR_25_25==T|
             T2_manRef_HR_26_26==T|
             T2_manRef_HR_27_27==T|
             T2_manRef_HR_28_28==T)|
          (T2_manRef_spec_24_24==T|
             T2_manRef_spec_25_25==T|
             T2_manRef_spec_26_26==T|
             T2_manRef_spec_27_27==T|
             T2_manRef_spec_28_28==T)), T2_GDMscreeningontime_24_28_highrbg:=T]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA=T)
  
  
  # intermediate values
# 
  T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
   
   
   T2[T2_Opportunity_GDM_screening_24_28==1 &
            T2_GDMscreeningontime_24_28==TRUE &
            #is.na(T2_GDMscreeningontime_2) & 
        (T2_labfastbloodglu_likelyGDM_24_28==T |
           T2_labbloodglu_likelyGDM_24_28==T),T2_GDMscreeningontime_24_28_intmbg:=FALSE]
   
   xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA=T)
   # managment is repeat FBS with in 3 weeks
   
   T2[T2_GDMscreeningontime_24_28_intmbg==F &
            (T2_repeatFBS_24_24==T|
            T2_repeatFBS_25_25==T|
            T2_repeatFBS_26_26==T|
            T2_repeatFBS_27_27==T|
            T2_repeatFBS_28_28==T|
            T2_repeatFBS_29_29==T|
            T2_repeatFBS_30_30==T|
            T2_repeatFBS_31_31==T),T2_GDMscreeningontime_24_28_intmbg:=T]
   
   xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA=T)
   
   

  
  
}

xtabs(~T2$T2_GDMscreeningontime_24_28, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_normal, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA = T)


################
# > 28 weeks
################

#T2_RefHrHosp_2 rename to T2_RefHr_2

T2[(T2_manRef_HR_29_29==T|
      T2_manRef_HR_30_30==T|
      T2_manRef_HR_31_31==T|
      T2_manRef_HR_32_32==T|
      T2_manRef_HR_33_33==T|
      T2_manRef_HR_34_34==T|
      T2_manRef_HR_35_35==T|
      T2_manRef_HR_36_36==T|
      T2_manRef_HR_37_37==T)|
     (T2_manRBGHigh_Diab_29_29==T|
        T2_manRBGHigh_Diab_30_30==T|
        T2_manRBGHigh_Diab_31_31==T|
        T2_manRBGHigh_Diab_32_32==T|
        T2_manRBGHigh_Diab_33_33==T|
        T2_manRBGHigh_Diab_34_34==T|
        T2_manRBGHigh_Diab_35_35==T|
        T2_manRBGHigh_Diab_36_36==T|
        T2_manRBGHigh_Diab_37_37==T)|
     (T2_manFBSHigh_Diab_30_30==T|
        T2_manFBSHigh_Diab_31_31==T|
        T2_manFBSHigh_Diab_32_32==T|
        T2_manFBSHigh_Diab_33_33==T|
        T2_manFBSHigh_Diab_34_34==T|
        T2_manFBSHigh_Diab_35_35==T|
        T2_manFBSHigh_Diab_36_36==T|
        T2_manFBSHigh_Diab_37_37==T|
        T2_manFBSHigh_Diab_29_29==T)|
     (T2_manFBSHigh_Diab_30_30==T|
        T2_manFBSHigh_spec_31_31==T|
        T2_manFBSHigh_spec_32_32==T|
        T2_manFBSHigh_spec_33_33==T|
        T2_manFBSHigh_spec_34_34==T|
        T2_manFBSHigh_spec_35_35==T|
        T2_manFBSHigh_spec_36_36==T|
        T2_manFBSHigh_spec_37_37==T|
        T2_manFBSHigh_spec_29_29==T),T2_RefHr_2:=TRUE]

xtabs(~T2$T2_RefHr_2, addNA=T)



# checks
xtabs(~T2$T2_Opportunity_GDM_screening_24_28, addNA=T)



## defining opportunities
# after 28
T2[bookgestagedays_cats %in% c("(202,216]",
                               "(216,237]",
                               "(237,244]",
                               "(244,265]"), T2_Opportunity_GDM_screening_after_28:=1]
#Screening before 24 weeks: Creating one var for 3 possibilities
T2[,screenafter28:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_after_28==1,
       screenafter28:=F]
T2[screenafter28==F &
         (T2_labfastbloodglu_exists_29_30==T|
            T2_labfastbloodglu_exists_31_33==T |
            T2_labfastbloodglu_exists_34_34==T |
            T2_labfastbloodglu_exists_35_37==T|
            T2_labbloodglu_exists_29_30==T|
            T2_labbloodglu_exists_31_33==T|
            T2_labbloodglu_exists_34_34==T|
            T2_labbloodglu_exists_35_37==T),screenafter28:=T]

xtabs(~T2$screenafter28, addNA=T)


T2[,T2_GDMscreeningontime_after_28:=as.logical(NA)]
T2[,T2_GDMscreeningontime_after_28_normal:=as.logical(NA)]
T2[,T2_GDMscreeningontime_after_28_high:=as.logical(NA)]


# anyone who has a fasting or blood glu value and booked after 28 weeks
T2[screenafter28==F, 
   T2_GDMscreeningontime_after_28:=FALSE]
T2[screenafter28==T, 
   T2_GDMscreeningontime_after_28:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_after_28, addNA=T)

#normal Values/ negative values
T2[,T2_GDMscreeningontime_after_28_normal:=as.logical(NA)]


T2[T2_Opportunity_GDM_screening_after_28==1 & 
     T2_GDMscreeningontime_after_28==T,
   T2_GDMscreeningontime_after_28_normal:=FALSE]


T2[,T2_GDMscreeningontime_after_28_high:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_after_28==1 & 
     T2_GDMscreeningontime_after_28==T,
   T2_GDMscreeningontime_after_28_high:=FALSE]

T2[T2_GDMscreeningontime_after_28_normal==F &
         (T2_labfastbloodglu_normal_29_30==T|
            T2_labfastbloodglu_normal_31_33==T |
            T2_labfastbloodglu_normal_34_34==T |
            T2_labfastbloodglu_normal_35_37==T|
            T2_labbloodglu_normal_29_30==T|
            T2_labbloodglu_normal_31_33==T|
            T2_labbloodglu_normal_34_34==T|
            T2_labbloodglu_normal_35_37==T),T2_GDMscreeningontime_after_28_normal:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_after_28_normal, addNA=T)

# High fbs or rbg values
T2[T2_GDMscreeningontime_after_28_normal==F &
         (T2_labfastbloodglu_high_29_30==T|
            T2_labfastbloodglu_high_31_33==T |
            T2_labfastbloodglu_high_34_34==T |
            T2_labfastbloodglu_high_35_37==T |
            T2_labbloodglu_high_29_30==T|
            T2_labbloodglu_high_31_33==T|
            T2_labbloodglu_high_34_34==T|
            T2_labbloodglu_high_35_37==T),T2_GDMscreeningontime_after_28_high:=FALSE]

# proper management
T2[T2_GDMscreeningontime_after_28_high==F &
         T2_RefHr_2==T, T2_GDMscreeningontime_after_28_high:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_after_28_high, addNA=T)

# same in wb and gaza except dont take urine sugar
# check t2_labbloodglu_normal & t2_labfastbloodglu_normal


xtabs(~T2$T2_GDMscreeningontime_after_28, addNA=T)
xtabs(~T2$T2_GDMscreeningontime_after_28_normal, addNA=T)
xtabs(~T2$T2_GDMscreeningontime_after_28_high, addNA=T)


################################
# outside of 24-28 windo
################################


# high rbs anywhere outside of the 24-28
T2[(T2_labbloodglu_high_00_14==T|
      T2_labbloodglu_high_15_17==T|
      T2_labbloodglu_high_18_22==T|
      T2_labbloodglu_high_23_23==T|
      T2_labbloodglu_high_29_30==T|
      T2_labbloodglu_high_31_33==T|
      T2_labbloodglu_high_34_34==T|
      T2_labbloodglu_high_35_37==T) |
     (T2_labfastbloodglu_high_00_14==T|
        T2_labfastbloodglu_high_15_17==T|
        T2_labfastbloodglu_high_18_22==T|
        T2_labfastbloodglu_high_23_23==T|
        T2_labfastbloodglu_high_29_30==T|
        T2_labfastbloodglu_high_31_33==T|
        T2_labfastbloodglu_high_34_34==T|
        T2_labfastbloodglu_high_35_37==T) &
     booklabbloodglu_high==F, T2_Opportunity_GDM_screening_high:=1]

xtabs(~T2$T2_Opportunity_GDM_screening_24_28, addNA=T)
xtabs(~T2$T2_Opportunity_GDM_screening_after_28, addNA=T)
xtabs(~T2$T2_Opportunity_GDM_screening_high, addNA=T)


#management fo high RBG outside of time windows
T2[, T2_GDMscreeningontime_4:=as.logical(NA)]
T2[T2_Opportunity_GDM_screening_high==1 &
         (T2_labbloodglu_high_00_14==T|
            T2_labbloodglu_high_15_17==T|
            T2_labbloodglu_high_18_22==T|
            T2_labbloodglu_high_23_23==T|
            T2_labbloodglu_high_29_30==T|
            T2_labbloodglu_high_31_33==T|
            T2_labbloodglu_high_34_34==T|
            T2_labbloodglu_high_35_37==T) |
         (T2_labfastbloodglu_high_00_14==T|
            T2_labfastbloodglu_high_15_17==T|
            T2_labfastbloodglu_high_18_22==T|
            T2_labfastbloodglu_high_23_23==T|
            T2_labfastbloodglu_high_29_30==T|
            T2_labfastbloodglu_high_31_33==T|
            T2_labfastbloodglu_high_34_34==T|
            T2_labfastbloodglu_high_35_37==T), T2_GDMscreeningontime_4:= FALSE]

T2[T2_GDMscreeningontime_4==F & 
         (T2_RefHr==T|T2_RefHr_2==T),T2_GDMscreeningontime_4:=TRUE]


varsgdm <- c("T2_Opportunity_GDM_screening_b4_24",
          "T2_Opportunity_GDM_screening_24_28",
          "T2_Opportunity_GDM_screening_after_28",
          "T2_Opportunity_GDM_screening_high",
          "T2_RefHr",
          "T2_RefHr_2",
          "screenb424",
          "T2_GDMscreeningontime_b4_24_normal",
          "T2_GDMscreeningontime_b4_24_posurglu",
          "T2_GDMscreeningontime_b4_24",
          "T2_GDMscreeningontime_24_28",
          "T2_GDMscreeningontime_24_28_normal",
          "T2_GDMscreeningontime_24_28_highrbg",
          "T2_GDMscreeningontime_24_28_intmbg",
          "screenafter28",
          "T2_GDMscreeningontime_after_28",
          "T2_GDMscreeningontime_after_28_normal",
          "T2_GDMscreeningontime_after_28_high",
          "T2_GDMscreeningontime_4")





for (i in varsgdm){
  
  
  T2[,(i):=as.character(get(i))]
  
  T2[stringr::str_detect(get(i),"TRUE"), (i):="Successful"]
  T2[stringr::str_detect(get(i),"FALSE"), (i):="Not Successful"]
  T2[is.na(get(i)), (i):="Not Applicable"]
  
}



# save data set with outcomes to merge with attendance data
if(IS_GAZA==F){
  
  
  fileTag <- "WB"
  
  saveRDS(T2,file.path(FOLDER_DATA_CLEAN,
                           "T2_clean",
                           sprintf("T2_outcomes_dataset_%s_%s.rds",
                                   CLINIC_INTERVENTION_DATE,
                                   fileTag)))
  
  
  fwrite(T2,file.path(FOLDER_DATA_CLEAN,
                          "T2_clean",
                          sprintf("T2_outcomes_dataset_%s_%s.csv",
                                  CLINIC_INTERVENTION_DATE,
                                  fileTag)))
  
  
} else{
  
  fileTag <- "GAZA"
  
  saveRDS(T2,file.path(FOLDER_DATA_CLEAN_GAZA,
                           "T2_clean",
                           sprintf("T2_outcomes_dataset_%s_%s.rds",
                                   CLINIC_INTERVENTION_DATE,
                                   fileTag)))
  
  
  fwrite(T2,file.path(FOLDER_DATA_CLEAN_GAZA,
                          "T2_clean",
                          sprintf("T2_outcomes_dataset_%s_%s.csv",
                                  CLINIC_INTERVENTION_DATE,
                                  fileTag)))
  
  
  
  
  
  
}

           
