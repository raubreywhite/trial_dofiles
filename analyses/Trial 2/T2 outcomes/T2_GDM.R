
###Redefining opportinites


################
# B4 24 weeks
################
T2[,T2_Opportunity_GDM_screening_b4_24:=as.logical(NA)] 

# before 24
T2[bookgestagedays_cats %in% c("(0,104]",
                               "(104,125]",
                               "(125,160]",
                               "(160,167]"),T2_Opportunity_GDM_screening_b4_24:=TRUE]

xtabs(~T2$T2_Opportunity_GDM_screening_b4_24, addNA=T)


################
# opportunities
################

#Screening before 24 weeks: Creating one var for 3 possibilities
#laburglu

T2[,T2_GDMscreeningontime_b4_24_bookurglu_normal:=as.logical(NA)]

T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
    (!is.na(booklaburglu)), T2_GDMscreeningontime_b4_24_bookurglu_normal:=FALSE]


T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
     T2_GDMscreeningontime_b4_24_bookurglu_normal==FALSE &
     (booklaburglu=="NEG"), 
   T2_GDMscreeningontime_b4_24_bookurglu_normal:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24_bookurglu_normal,addNA=T)



# fastbloodglu
T2[,T2_GDMscreeningontime_b4_24_bookfastbloodglu_normal:=as.logical(NA)]

T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
     (!is.na(booklabfastbloodglu)), T2_GDMscreeningontime_b4_24_bookfastbloodglu_normal:=FALSE]


T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
     T2_GDMscreeningontime_b4_24_bookfastbloodglu_normal==FALSE &
     (booklabfastbloodglu_high=="FALSE"), 
   T2_GDMscreeningontime_b4_24_bookurglu_normal:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24_bookbloodglu_normal,addNA=T)


# booklabbloodglu
T2[,T2_GDMscreeningontime_b4_24_bookurglu_normal:=as.logical(NA)]

T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
     (!is.na(booklabbloodglu)), T2_GDMscreeningontime_b4_24_bookbloodglu_normal:=FALSE]


T2[T2_Opportunity_GDM_screening_b4_24==TRUE & 
     T2_GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
     (booklabbloodglu_high=="FALSE"), 
   T2_GDMscreeningontime_b4_24_bookbloodglu_normal:=TRUE]


xtabs(~T2$T2_GDMscreeningontime_b4_24_bookbloodglu_normal,addNA=T)

# managements
##################################### QUESTION ##################################### 
### question: are we keeping the rbs and fbs at booking as well? 
### or would it be at any time point?####
###################################################################################

# do this week by week, because management has to be up to one week later
# gdm screening if have pos urglu and have h
T2[,T2_GDMscreeningontime_b4_24_manposurglu:=as.logical(NA)]
T2[T2_GDMscreeningontime_b4_24_bookurglu_normal==FALSE &
     booklaburglu=="POS",T2_GDMscreeningontime_b4_24_manposurglu:=FALSE]

T2[T2_GDMscreeningontime_b4_24_manposurglu==F &
     (!is.na(booklabbloodglu)|
        !is.na(booklabfastbloodglu)), T2_GDMscreeningontime_b4_24_manposurglu:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24_manposurglu, addNA=T)


# management of high rbg and fbs
T2[,T2_GDMscreeningontime_b4_24_manhighrbs:=as.logical(NA)]
T2[T2_GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
     is.na(T2_GDMscreeningontime_b4_24_manposurglu) &
     (booklabbloodglu_high==T|booklabfastbloodglu_high==T),T2_GDMscreeningontime_b4_24_manhighrbs:=FALSE]

T2[T2_GDMscreeningontime_b4_24_manhighrbs==F &
     (bookhr==T), T2_GDMscreeningontime_b4_24_manhighrbs:=TRUE]

xtabs(~T2$T2_GDMscreeningontime_b4_24_manhighrbs, addNA=T)



################################
## 24-28 weeks
################################



###################
#  opportunity
###################
T2[,T2_Opportunity_GDM_screening_24_28:=as.logical(NA)]


#24-28
T2[T2_anvisitnew_24_28==T |
     bookgestagedays_cats %in% c("(167,202]"),T2_Opportunity_GDM_screening_24_28:=TRUE]

## Remove opportunities for people who had high blood glu
T2[T2_Opportunity_GDM_screening_24_28==TRUE &
     (T2_labbloodglu_high_00_14==T|
        T2_labbloodglu_high_15_17==T|
        T2_labbloodglu_high_18_22==T|
        T2_labbloodglu_high_23_23==T|
        T2_labfastbloodglu_high_00_14==T|
        T2_labfastbloodglu_high_15_17==T|
        T2_labfastbloodglu_high_18_22==T|
        T2_labfastbloodglu_high_23_23==T),
   T2_Opportunity_GDM_screening_24_28:=FALSE]


xtabs(~T2$T2_Opportunity_GDM_screening_24_28, addNA=T)
###################
# screening 24-28
###################
  
  
  T2[,T2_GDMscreeningontime_24_28:=as.logical(NA)]
  T2[T2_Opportunity_GDM_screening_24_28==TRUE &
          (T2_labfastbloodglu_exists_24_28==T), T2_GDMscreeningontime_24_28:=T]
  
  
  # normal values
  T2[,T2_GDMscreeningontime_24_28_normal:=as.logical(NA)]

  
  T2[T2_GDMscreeningontime_24_28==T &
        (T2_labfastbloodglu_exists_24_28==T), T2_GDMscreeningontime_24_28_normal:=F]
  
  T2[T2_Opportunity_GDM_screening_24_28==TRUE & 
           T2_GDMscreeningontime_24_28_normal==F &
          (T2_labfastbloodglu_normal_24_28==T),T2_GDMscreeningontime_24_28_normal:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_normal, addNA=T)
  
  ###################
  # management 24-28
  ###################
  
  
  # highrbg 24 weeks
  T2[,T2_GDMscreeningontime_24_24_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (T2_labbloodglu_high_24_24==T |
          T2_labfastbloodglu_high_24_24==T),T2_GDMscreeningontime_24_24_manhighrbg:=F]
  
  
  T2[T2_GDMscreeningontime_24_24_manhighrbg==F &
       (T2_manRBGHigh_Diab_24_24==T|
       T2_manRef_HR_24_24==T|
       T2_manRef_spec_24_24==T|
       T2_manFBSHigh_Diab_24_24==T), T2_GDMscreeningontime_24_24_manhighrbg:=T]
  
  
  # highrbg 25 weeks
  T2[,T2_GDMscreeningontime_25_25_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (T2_labbloodglu_high_25_25==T |
          T2_labfastbloodglu_high_25_25==T),T2_GDMscreeningontime_25_25_manhighrbg:=F]
  
  
  T2[T2_GDMscreeningontime_25_25_manhighrbg==F &
       (T2_manRBGHigh_Diab_25_25==T|
       T2_manRef_HR_25_25==T|
       T2_manRef_spec_25_25==T|
       T2_manFBSHigh_Diab_25_25==T), T2_GDMscreeningontime_25_25_manhighrbg:=T]
  
  
  
  # highrbg 26 weeks
  T2[,T2_GDMscreeningontime_26_26_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (T2_labbloodglu_high_26_26==T |
          T2_labfastbloodglu_high_26_26==T),T2_GDMscreeningontime_26_26_manhighrbg:=F]
  
  
  T2[T2_GDMscreeningontime_26_26_manhighrbg==F &
       (T2_manRBGHigh_Diab_26_26==T|
       T2_manRef_HR_26_26==T|
       T2_manRef_spec_26_26==T|
       T2_manFBSHigh_Diab_26_26==T), T2_GDMscreeningontime_26_26_manhighrbg:=T]
  
  
  # highrbg 27 weeks
  T2[,T2_GDMscreeningontime_27_27_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (T2_labbloodglu_high_27_27==T |
          T2_labfastbloodglu_high_27_27==T),T2_GDMscreeningontime_27_27_manhighrbg:=F]
  
  
  T2[T2_GDMscreeningontime_27_27_manhighrbg==F &
       (T2_manRBGHigh_Diab_27_27==T|
       T2_manRef_HR_27_27==T|
       T2_manRef_spec_27_27==T|
       T2_manFBSHigh_Diab_27_27==T), T2_GDMscreeningontime_27_27_manhighrbg:=T]
  
  
  # highrbg 28 weeks
  T2[,T2_GDMscreeningontime_28_28_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (T2_labbloodglu_high_28_28==T |
          T2_labfastbloodglu_high_28_28==T),T2_GDMscreeningontime_28_28_manhighrbg:=F]
  
  
  T2[T2_GDMscreeningontime_28_28_manhighrbg==F &
       (T2_manRBGHigh_Diab_28_28==T|
       T2_manRef_HR_28_28==T|
       T2_manRef_spec_28_28==T|
       T2_manFBSHigh_Diab_28_28==T), T2_GDMscreeningontime_28_28_manhighrbg:=T]
  
  
  # combined group
  
  T2[,T2_GDMscreeningontime_24_28_manhighrbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       (!is.na(T2_GDMscreeningontime_24_24_manhighrbg)|
          !is.na(T2_GDMscreeningontime_25_25_manhighrbg)|
          !is.na(T2_GDMscreeningontime_26_26_manhighrbg)|
          !is.na(T2_GDMscreeningontime_27_27_manhighrbg)|
          !is.na(T2_GDMscreeningontime_28_28_manhighrbg)),
     T2_GDMscreeningontime_24_28_manhighrbg:=F]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_manhighrbg, addNA=T)
  
  
  T2[T2_GDMscreeningontime_24_28_manhighrbg==F & 
       (T2_GDMscreeningontime_24_24_manhighrbg==T|
          T2_GDMscreeningontime_25_25_manhighrbg==T|
          T2_GDMscreeningontime_26_26_manhighrbg==T|
          T2_GDMscreeningontime_27_27_manhighrbg==T|
          T2_GDMscreeningontime_28_28_manhighrbg==T),
     T2_GDMscreeningontime_24_28_manhighrbg:=T]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_manhighrbg, addNA=T)
  
 if(IS_GAZA){
  
  # intermediate values

  # intermediate values,  but dont want them for WB because management is in free text
  T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       is.na(T2_GDMscreeningontime_24_28_manhighrbg) &
       is.na(T2_GDMscreeningontime_24_28_manhighfbs) &
       (T2_labbloodglu_likelyGDM_24_28==T|
          T2_labfastbloodglu_likelyGDM_24_28==T),T2_GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA=T)
   # managment is repeat FBS with in 3 weeks
   
  # do this by one week intervals
  
  # 24 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       T2_labfastbloodglu_likelyGDM_24_24==T|
       T2_labbloodglu_likelyGDM_24_24==T,T2_GDMscreeningontime_24_24_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_24_24_manintmbg==F &
       (T2_labfastbloodglu_likelyGDM_24_24==T|
       T2_labbloodglu_likelyGDM_24_24==T) &
       T2_repeatFBS_27_27==T,T2_GDMscreeningontime_24_24_manintmbg:=T]
  
   
   # 25 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       T2_labfastbloodglu_likelyGDM_25_25==T,T2_GDMscreeningontime_25_25_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_25_25_manintmbg==F &
       (T2_labfastbloodglu_likelyGDM_25_25==T|
       T2_labbloodglu_likelyGDM_25_25==T) &
       T2_repeatFBS_28_28==T,T2_GDMscreeningontime_25_25_manintmbg:=T]
  
  
  # 26 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       T2_labfastbloodglu_likelyGDM_26_26==T|
       T2_labbloodglu_likelyGDM_26_26==T,T2_GDMscreeningontime_26_26_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_26_26_manintmbg==F &
       (T2_labfastbloodglu_likelyGDM_26_26==T |
        T2_labbloodglu_likelyGDM_26_26==T) &
                                 T2_repeatFBS_29_29==T,T2_GDMscreeningontime_26_26_manintmbg:=T]
  
  # 27 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_27_27==T |
          T2_labbloodglu_likely_GDM_27_27==T),T2_GDMscreeningontime_27_27_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_27_27_manintmbg==F &
       (T2_labfastbloodglu_likelyGDM_27_27==T |
          T2_labbloodglu_likely_GDM_27_27==T) &
       T2_repeatFBS_30_30==T,T2_GDMscreeningontime_27_27_manintmbg:=T]
  
  
  # 28 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_28_28==T |
          T2_labbloodglu_likely_GDM_28_28==T),T2_GDMscreeningontime_28_28_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_28_28_manintmbg==F &
       (T2_labfastbloodglu_likelyGDM_28_28==T |
          T2_labbloodglu_likely_GDM_28_28==T) &
       T2_repeatFBS_31_31==T,T2_GDMscreeningontime_28_28_manintmbg:=T]

  
  
  # combined variable
  
  T2[,T2_GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (!is.na(T2_GDMscreeningontime_28_28_manintmbg)|
       !is.na(T2_GDMscreeningontime_27_27_manintmbg)|
       !is.na(T2_GDMscreeningontime_26_26_manintmbg)|
       !is.na(T2_GDMscreeningontime_25_25_manintmbg)|
       !is.na(T2_GDMscreeningontime_24_24_manintmbg)),T2_GDMscreeningontime_24_28_manintmbg:=FALSE ]
  
  T2[T2_GDMscreeningontime_24_28_manintmbg==F &
       (T2_GDMscreeningontime_24_24_manintmbg==T |
          T2_GDMscreeningontime_25_25_manintmbg==T|
          T2_GDMscreeningontime_26_26_manintmbg==T|
          T2_GDMscreeningontime_27_27_manintmbg==T|
          T2_GDMscreeningontime_28_28_manintmbg==T), T2_GDMscreeningontime_24_28_manintmbg:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_manintmbg, addNA=T)
  
 }else{
  
   # intermediate values,  but dont want them for WB because management is in free text
   T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
   T2[T2_GDMscreeningontime_24_28==T &
        T2_GDMscreeningontime_24_28_normal==FALSE &
        is.na(T2_GDMscreeningontime_24_28_manhighrbg) &
        (T2_labbloodglu_likelyGDM_24_28==T),T2_GDMscreeningontime_24_28_intmbg:=TRUE]
   
   xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA=T)
   
   
   
   T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_24_24_manintmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_25_25_manintmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_26_26_manintmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_27_27_manintmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_28_28_manintmbg:=as.logical(NA)]
   T2[,T2_GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
   
}

xtabs(~T2$T2_GDMscreeningontime_24_28, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_normal, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_highrbg, addNA = T)
xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA = T)


################
# > 28 weeks
################
T2[,T2_Opportunity_GDM_screening_after_28:=as.logical(NA)]


# checks
xtabs(~T2$T2_Opportunity_GDM_screening_24_28, addNA=T)


##############
# after 28
##############
## defining opportunities
# after 28
T2[bookgestagedays_cats %in% c("(202,216]",
                               "(216,237]",
                               "(237,244]",
                               "(244,265]"), T2_Opportunity_GDM_screening_after_28:=TRUE]
xtabs(~T2$T2_Opportunity_GDM_screening_after_28, addNA=T)

## defining successes
T2[,T2_GDMscreeningontime_after_28:=as.logical(NA)]
T2[,T2_GDMscreeningontime_after_28_normal:=as.logical(NA)]
T2[,T2_GDMscreeningontime_after_28_high:=as.logical(NA)]
T2[,T2_GDMscreeningontime_after_28_intmd:=as.logical(NA)]


# anyone who has a fasting or blood glu value and booked after 28 weeks
T2[T2_Opportunity_GDM_screening_after_28==TRUE, 
   T2_GDMscreeningontime_after_28:=FALSE]

T2[T2_GDMscreeningontime_after_28==F &
     (!is.na(booklabbloodglu)|
        !is.na(booklabfastbloodglu)), 
   T2_GDMscreeningontime_after_28:=TRUE]

#xtabs(~T2$screenafter28, addNA=T)
xtabs(~T2$T2_GDMscreeningontime_after_28, addNA=T)

#normal Values/ negative values
T2[,T2_GDMscreeningontime_after_28_normal:=as.logical(NA)]

T2[T2_Opportunity_GDM_screening_after_28==TRUE & 
     T2_GDMscreeningontime_after_28==T,
   T2_GDMscreeningontime_after_28_normal:=FALSE]

T2[T2_GDMscreeningontime_after_28_normal==FALSE &
     ((booklabbloodglu_high==FALSE &
         !is.na(booklabbloodglu))|
         (booklabfastbloodglu_high==F &
            !is.na(booklabfastbloodglu))),T2_GDMscreeningontime_after_28_normal:=T ]

xtabs(~T2$T2_GDMscreeningontime_after_28_normal, addNA=T)


# high values
T2[,T2_GDMscreeningontime_after_28_high:=as.logical(NA)]
T2[T2_GDMscreeningontime_after_28_normal==FALSE,T2_GDMscreeningontime_after_28_high:=FALSE]


# management
T2[T2_GDMscreeningontime_after_28_high==FALSE &
     bookhrhighsug==T,
   T2_GDMscreeningontime_after_28_high:=TRUE]
xtabs(~T2$T2_GDMscreeningontime_after_28_high, addNA=T)


xtabs(~T2$T2_GDMscreeningontime_after_28, addNA=T)
xtabs(~T2$T2_GDMscreeningontime_after_28_normal, addNA=T)
xtabs(~T2$T2_GDMscreeningontime_after_28_high, addNA=T)




varsgdm <- c("T2_Opportunity_GDM_screening_b4_24",
             "T2_GDMscreeningontime_b4_24_bookurglu_normal",
             "T2_GDMscreeningontime_b4_24_bookfastbloodglu_normal",
             "T2_GDMscreeningontime_b4_24_bookbloodglu_normal",
             "T2_GDMscreeningontime_b4_24_manposurglu",
             "T2_GDMscreeningontime_b4_24_manhighrbs",
             "T2_Opportunity_GDM_screening_24_28", 
             "T2_GDMscreeningontime_24_28",
             "T2_GDMscreeningontime_24_28_normal",
             "T2_GDMscreeningontime_24_24_manhighrbg",
             "T2_GDMscreeningontime_25_25_manhighrbg",
             "T2_GDMscreeningontime_26_26_manhighrbg",
             "T2_GDMscreeningontime_27_27_manhighrbg",
             "T2_GDMscreeningontime_28_28_manhighrbg",
             "T2_GDMscreeningontime_24_28_manhighrbg",
             "T2_GDMscreeningontime_24_28_intmbg",
             "T2_GDMscreeningontime_24_24_manintmbg",
             "T2_GDMscreeningontime_25_25_manintmbg",
             "T2_GDMscreeningontime_26_26_manintmbg",
             "T2_GDMscreeningontime_27_27_manintmbg",
             "T2_GDMscreeningontime_28_28_manintmbg",
             "T2_GDMscreeningontime_24_28_manintmbg",
             "T2_Opportunity_GDM_screening_after_28",
             "T2_GDMscreeningontime_after_28_normal",
             "T2_GDMscreeningontime_after_28",
             "T2_GDMscreeningontime_after_28_high")





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

           
