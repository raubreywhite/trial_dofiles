
########## GDM ########## 

########## GDM ########## 

###Redefining opportinites
smallD[,Opportunity_GDM_screening_1:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_2:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_3:=as.numeric(NA)]
smallD[,Opportunity_GDM_screening_4:=as.numeric(NA)]
#smallD[,Opportunity_GDM_Screening_5:=as.numeric(NA)]

# before 24
smallD[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_1:=1]
#24-28
smallD[bookgestagedays_cats %in% c("(167,202]")|
         TrialOne_anvisitnew_24_28==T,Opportunity_GDM_screening_2:=1]
# after 28
smallD[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]", 
                                   "(237,244]",
                                   "(244,265]"), Opportunity_GDM_screening_3:=1]

# high rbs anywhere outside of the 24-28
smallD[(TrialOne_labbloodglu_high_00_14==T|
          TrialOne_labbloodglu_high_15_17==T|
          TrialOne_labbloodglu_high_18_22==T|
          TrialOne_labbloodglu_high_23_23==T|
          TrialOne_labbloodglu_high_29_30==T|
          TrialOne_labbloodglu_high_31_33==T|
          TrialOne_labbloodglu_high_34_34==T|
          TrialOne_labbloodglu_high_35_37==T), Opportunity_GDM_screening_4:=1]

xtabs(~smallD$Opportunity_GDM_screening_1, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_2, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_3, addNA=T)
xtabs(~smallD$Opportunity_GDM_screening_4, addNA=T)




## Remove opportunities for people who were referred to HR or Hosp
#refHRHospmanRBG_1 rename to RefHr
smallD[,RefHr:=as.logical(NA)]
smallD[Opportunity_GDM_screening_1==1, RefHr:=FALSE]
smallD[(TrialOne_manRef_HR_00_00==T|
          TrialOne_manRef_HR_01_01==T|
          TrialOne_manRef_HR_02_02==T|
          TrialOne_manRef_HR_03_03==T|
          TrialOne_manRef_HR_04_04==T|
          TrialOne_manRef_HR_05_05==T|
          TrialOne_manRef_HR_06_06==T|
          TrialOne_manRef_HR_07_07==T|
          TrialOne_manRef_HR_08_08==T|
          TrialOne_manRef_HR_09_09==T|
          TrialOne_manRef_HR_10_10==T|
          TrialOne_manRef_HR_11_11==T|
          TrialOne_manRef_HR_12_12==T|
          TrialOne_manRef_HR_13_13==T|
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
       RefHr:=TRUE]
xtabs(~smallD$RefHr, addNA=T)

#refHrHosp_2 rename to refHr_2
smallD[,refHr_2:=(
  TrialOne_refHR_29_29==T|
    TrialOne_refHR_30_30==T|
    TrialOne_refHR_31_31==T|
    TrialOne_refHR_32_32==T|
    TrialOne_refHR_33_33==T|
    TrialOne_refHR_34_34==T|
    TrialOne_refHR_35_35==T|
    TrialOne_refHR_36_36==T|
    TrialOne_refHR_35_37==T)]


smallD[Opportunity_GDM_screening_2==1 &
         (TrialOne_anvisitnew_24_24==T & 
            (RefHr==T))|
         (TrialOne_anvisitnew_25_25==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T)), 
       Opportunity_GDM_screening_2:=Opportunity_GDM_screening_2-1]

# checks
xtabs(~smallD$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
smallD[,screenb424:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
smallD[screenb424==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenb424:=T]
xtabs(~smallD$screenb424, addNA=T)

scrb424 <- smallD[,.(A=sum(ident_dhis2_control==T),
                     B=sum(ident_dhis2_control==F)),
                  keyby=.(screenb424)]

##Defining Successes 
smallD[,GDMscreeningontime_1A:=as.logical(NA)]
smallD[,GDMscreeningontime_1B:=as.logical(NA)]
smallD[,GDMscreeningontime_1C:=as.logical(NA)]
smallD[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
smallD[screenb424==T, 
       GDMscreeningontime_1:=TRUE]

xtabs(~smallD$GDMscreeningontime_1, addNA=T)


smallD[,GDMscreeningontime_1A:=as.logical(NA)]
smallD[Opportunity_GDM_screening_1==1 & 
         booklaburglu=="NEG", 
       GDMscreeningontime_1A:=TRUE]

smallD[,GDMscreeningontime_1B:=as.logical(NA)]
smallD[Opportunity_GDM_screening_1==1 &
         booklaburglu=="POS" & 
         !is.na(booklabbloodglu), GDMscreeningontime_1B:=TRUE]

smallD[,GDMscreeningontime_1C:=as.logical(NA)]
smallD[booklabbloodglu_high==T &
         !is.na(booklabbloodglu) &
         RefHr==T, GDMscreeningontime_1C:=TRUE]



#24-28 weeks
smallD[,GDMscreeningontime_2:=as.logical(NA)]
smallD[Opportunity_GDM_screening_2==1 &
         (TrialOne_labbloodglu_exists_24_24==F &
            TrialOne_labbloodglu_exists_25_25==F &
            TrialOne_labbloodglu_exists_26_26==F &
            TrialOne_labbloodglu_exists_27_27==F &
            TrialOne_labbloodglu_exists_28_28==F) &
         (TrialOne_labfastbloodglu_exists_24_24==F &
            TrialOne_labfastbloodglu_exists_25_25==F &
            TrialOne_labfastbloodglu_exists_26_26==F &
            TrialOne_labfastbloodglu_exists_27_27==F &
            TrialOne_labfastbloodglu_exists_28_28==F), GDMscreeningontime_2:=F]
smallD[Opportunity_GDM_screening_2==1 & 
         (TrialOne_labbloodglu_exists_24_24==T|
            TrialOne_labbloodglu_exists_25_25==T|
            TrialOne_labbloodglu_exists_26_26==T|
            TrialOne_labbloodglu_exists_27_27==T|
            TrialOne_labbloodglu_exists_28_28==T) &
         (TrialOne_labbloodglu_high_24_24==F|
            TrialOne_labbloodglu_high_25_25==F|
            TrialOne_labbloodglu_high_26_26==F|
            TrialOne_labbloodglu_high_27_27==F|
            TrialOne_labbloodglu_high_28_28==F)|
         (TrialOne_labfastbloodglu_exists_24_24==T|
            TrialOne_labfastbloodglu_exists_25_25==T|
            TrialOne_labfastbloodglu_exists_26_26==T|
            TrialOne_labfastbloodglu_exists_27_27==T|
            TrialOne_labfastbloodglu_exists_28_28==T),GDMscreeningontime_2:=TRUE]
xtabs(~smallD$GDMscreeningontime_2, addNA=T)


#Screening after 28 weeks: Creating one var for 3 possibilities
smallD[,screenafter28:=as.logical(NA)]
smallD[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]"),
       screenafter28:=F]
smallD[screenafter28==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~smallD$screenafter28, addNA=T)

##Defining Success
smallD[,GDMscreeningontime_3:=as.logical(NA)]
smallD[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
smallD[screenafter28==T,GDMscreeningontime_3:=TRUE]
xtabs(~smallD$GDMscreeningontime_3, addNA=T)

#management fo high RBG outside of time windows
smallD[, GDMscreeningontime_4:=as.logical(NA)]
smallD[Opportunity_GDM_screening_4==1, GDMscreeningontime_4:= FALSE]
smallD[GDMscreeningontime_4==F & 
         (RefHr==T|refHr_2==T),GDMscreeningontime_4:=TRUE]


prelimGDM <- smallD[,.(N=.N,
                       Opportun_1=sum(Opportunity_GDM_screening_1==T, na.rm=T),
                       Success_1A=sum(GDMscreeningontime_1A==T, na.rm=T),
                       Success_1B=sum(GDMscreeningontime_1B==T, na.rm=T),
                       Success_1C=sum(GDMscreeningontime_1C==T, na.rm=T),
                       Screenb424=sum(screenb424==T, na.rm=T),
                       Screenb424False=sum(screenb424==F, na.rm=T),
                       Opportun_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2=sum(GDMscreeningontime_2==T, na.rm=T),
                       Opportun_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       screenafter28=sum(screenafter28==T, na.rm=T),
                       screenafter28False=sum(screenafter28==F, na.rm=T),
                       screenbtwn=sum(GDMscreeningontime_4==T, na.rm=T),
                       screenbtwnFalse=sum(GDMscreeningontime_4==F, na.rm=T),
                       Opportun_4=sum(Opportunity_GDM_screening_4==T, na.rm=T),
                       Succ_4=sum(GDMscreeningontime_4, na.rm=T)),
                    keyby=.(ident_dhis2_control)]



openxlsx::write.xlsx(prelimGDM,file.path(FOLDER_DATA_RESULTS,
                                         "T1",
                                         sprintf("%s_prelim_GDM.xlsx",
                                                 lubridate::today()))) 


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

