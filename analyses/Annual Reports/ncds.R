###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

# Data for Kjersti

# demographics table
# age at booking
# parity
#bookbmi before 12 weeks
# bookbmi before 12 weeks multipara vs primipara
#age by nullipara and multipara
# Diagnosed with HTN at different time periods
#diagnosed with gdm 24-28 weeks, and those with blood test before 24 weeks and diagnosed

# Demographics and risks at Booking

ar <- readRDS(file.path(FOLDER_DATA_CLEAN,"annual reports","annualreportdata.RDS"))

########## 
# demo
########## 

ar[bookgestage>40, bookgestage:=as.integer(NA)]

ar[is.infinite(bookbmi), bookbmi:=as.integer(NA)]

##making a table for data we want to analyze from the analysis data set
##for things like parity, make an ugly table because its not a box plot
##or histogram, so better to make a ugly table.
tab <- ar[ident_dhis2_booking==1,
          .(N=.N,
            "Mean Age"=mean(age, na.rm=T),
            "Mean Age First Pregnancy"=mean(agepregnancy, na.rm=TRUE),
            "Mean Age at Marriage"=mean(agemarriage, na.rm=T),
            "Mean Average Monthly Income"= mean(avgincome, na.rm=T),
            "Mean Education"= mean(education, na.rm=T),
            "Mean Book Weight"= mean(bookweight, na.rm=T),
            "Mean Book Height"=mean(bookheight, na.rm=T),
            "Mean BMI"=mean(bookbmi, na.rm=T),
            "Mean Systolic BP at booking"= mean(bookbpsyst, na.rm=T),
            "Mean Diastolic BP at Booking"=mean(bookbpdiast, na.rm=T),
            "Mean Hb at Booking"= mean(labhb_1, na.rm=T),
            "Mean Income"=mean(income, na.rm=T),
            "Proportion NOt missing Book HB"=sum(!is.na(labhb_1)),
            "Proprtion Weights 0"= mean(bookweight==0, na.rm=T),
            "Proportion of weights over 100 KG"= mean(bookweight>100, na.rm=T),
            "Proprtion of Parity"= mean(bookparity, na.rm=T),
            "Number book parity"=sum(bookparity==1, na.rm=T),
            "Mean Bookgestage"= mean(bookgestage, na.rm=T),
            "Proprtion Primi"=mean(bookprimi, na.rm=T),
            "NUmber of Primi at booking"=sum(bookprimi==1, na.rm=T),
            "Number of Perinatal Death"=sum(bookhistperi, na.rm=T),
            "Number Uterine Surgery at Booking"=sum(bookhistutesur==1, na.rm=T),
            "Number History of C-section"=sum(bookhistcs==1, na.rm=T),
            "Number History of CS complications"=sum(bookhistcscompl==1, na.rm=T),
            "Number History of Preterm Birth"=sum(bookhistpreterm, na.rm=T),
            "Number History of Ute"=sum(bookhistute==1, na.rm=T),
            "Number History of Abortion"=sum(bookhistabort==1, na.rm=T),
            "Number History of DM in Family" =sum(bookfamdm==1, na.rm=T),
            "Number History of HTN in Family"=sum(bookfamhtn==1, na.rm=T),
            "Number History of APH"=sum(bookhistaph==1, na.rm=T),
            "Number of Women with History of Clexane Use"=sum(bookhistclex==1, na.rm=T),
            "Number History of GDM"=sum(bookhistgdm==1, na.rm=T),
            "Number History of GHTN"=sum(bookhistghtn==1, na.rm=T),
            "Number History of Preterm Birth"=sum(bookhistpreterm==1, na.rm=T)), 
          keyby=.(bookyear)]

openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BackgroundAndHistory.xlsx"))


# agecat
tab <- ar[ident_dhis2_booking==T &
            bookgestage<40 & bookgestage>0,.(N=.N,
                                             "2019"=sum(bookyear==2019, na.rm=T),
                                             "2020"=sum(bookyear==2020, na.rm=T)),
          keyby=.(agecat)]


openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "agecat.xlsx"))


# parity
tab <- ar[ident_dhis2_booking==T,.(N=.N,
                                  "2019"=sum(bookyear==2019, na.rm=T),
                                  "2020"=sum(bookyear==2020, na.rm=T)),
          keyby=.(bookprimi)]

openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "parity.xlsx"))



# bookbmicat

tab <- ar[ident_dhis2_booking==T & bookgestage>0 & bookgestage<12,.(N=.N,
                                   "2019"=sum(bookyear==2019, na.rm=T),
                                   "2020"=sum(bookyear==2020, na.rm=T)),
          keyby=.(bookbmicat)]



openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookBmiCatsLessTHan12Wks.xlsx"))








demoage <- ar[ident_dhis2_booking==T & bookgestage>0 & bookgestage<12,.(N=.N,
  Bookprimi=sum(bookprimi==1, na.rm=T),
  BookPrimiNo=sum(bookprimi==0, na.rm=T),
  BookPrimiNA=sum(is.na(bookprimi))),
  keyby=.(bookyear,  bookbmicat)]


openxlsx::write.xlsx(demoage,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimivsbmi.xlsx"))




demoage <- ar[ident_dhis2_booking==T,.(N=.N,
  Bookprimi=sum(bookprimi=="1", na.rm=T),
  BookPrimiNo=sum(bookprimi=="0", na.rm=T),
  BookPrimiNA=sum(is.na(bookprimi))),
  keyby=.(bookyear,agecat)]


openxlsx::write.xlsx(demoage,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimivAgecat.xlsx"))



###########
# GDM
###########

# how many women have blood tests before 24 weeks


#Screening before 24 weeks: Creating one var for 3 possibilities
ar[,screenb424:=as.logical(NA)]
ar[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
   screenb424:=F]
ar[screenb424==F & (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
   screenb424:=T]
xtabs(~ar$screenb424, addNA=T)

# of those screened before 24 weeks, how many had a high blood gluc value

ar[,chronicDM:=as.logical(NA)]
ar[screenb424==T,chronicDM:=FALSE]
ar[screenb424==T &
     ((booklabbloodglu_high==T|
        booklabfastbloodglu_high==T)|
        TrialOne_labbloodglu_high_00_14==T|
        TrialOne_labbloodglu_high_15_17==T|
        TrialOne_labbloodglu_high_18_22==T|
        TrialOne_labbloodglu_high_23_23==T),chronicDM:=TRUE]
xtabs(~ar$chronicDM, addNA=T)



# screen at 24 weeks
ar[,screenat24:=as.logical(NA)]
ar[(bookgestagedays_cats %in% c("(167,202]")|
     TrialOne_anvisitnew_24_28==T) & chronicDM %in% c(FALSE,NA),screenat24:=FALSE]
ar[screenat24==FALSE &
     (TrialOne_labbloodglu_exists_24_28==T |
     TrialOne_labfastbloodglu_exists_24_28==T), screenat24:=TRUE]
xtabs(~ar$screenat24, addNA=T)

ar[,gdm:=as.logical(NA)]
ar[screenat24==TRUE, gdm:=FALSE]
ar[TrialOne_labfastbloodglu_high_24_28==T|
     TrialOne_labbloodglu_high_24_28==T, gdm:=TRUE]
xtabs(~ar$gdm, addNA=T)

gdm <- ar[,.(Screenedb424andNotHigh=sum(chronicDM==FALSE, na.rm=T),
             HighGlucValsb424=sum(chronicDM==TRUE, na.rm=T),
             ScreenedAt24andNotHigh=sum(gdm==FALSE, na.rm=T),
             HighGlucValat24=sum(gdm==TRUE, na.rm=T)),
          keyby=.(bookyear)]

openxlsx::write.xlsx(gdm,
                     file.path(
                      FOLDER_DATA_RESULTS,
                     "annual reports",
                     "2020",
                     sprintf("%s_gdm.xlsx", lubridate::today())))


####################
# Hypertension
####################
# chronic HTN
# anyone who has a visit and a bp before 18 weeks and a high systolic and diastolic bp

# gHtn at 22 weeks: anyone who has a high bp value  at this time and doesnt have one before

ar[,chronicHTN:=as.logical(NA)]
ar[(TrialOne_anbpsyst_present_00_14==T & TrialOne_anbpdiast_present_00_14==T)|
      (TrialOne_anbpsyst_present_15_17==T & TrialOne_anbpdiast_present_15_17==T), chronicHTN:=FALSE]

ar[(TrialOne_anbpsyst_high_00_14==T & TrialOne_anbpdiast_high_00_14==T)|
     (TrialOne_anbpsyst_high_15_17==T & TrialOne_anbpdiast_high_15_17==T), chronicHTN:=TRUE]

xtabs(~ar$chronicHTN, addNA=T)


# GHTN

ar[,ghtn:=as.logical(NA)]
ar[chronicHTN %in% c(FALSE,NA) & 
     (TrialOne_anbpsyst_present_18_22==T & TrialOne_anbpdiast_present_18_22==T), ghtn:=FALSE]

ar[ghtn==FALSE & TrialOne_anbpdiast_high_18_22==T & TrialOne_anbpdiast_high_18_22==T, ghtn:=TRUE]

xtabs(~ar$ghtn, addNA=T)



htn <- ar[,.(NotChronicHTN=sum(chronicHTN==FALSE, na.rm=T),
             chronicHTN=sum(chronicHTN==TRUE, na.rm=T),
             NotGHTN=sum(ghtn==FALSE, na.rm=T),
             GHTN=sum(ghtn==TRUE, na.rm=T)),
          keyby=.(bookyear)]

openxlsx::write.xlsx(htn,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       sprintf("%s_ghtn.xlsx", lubridate::today())))

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
                            T2_anbpdiast_modSevHTN_00_14==T),T2_manchronichtn_00_14:=T]

xtabs(~T2$T2_manchronichtn_00_14, addNA=T)



################
#15-17 weeks
################
# T2_anvisitnew: take opportunity and success from the attendance script


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
                                  T2_refHosp_16_16==T|
                                  T2_refSpec_17_17==T),T2_manchronichtn_15_17:=T]

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

#############
# adjust tab
#############

prelimHTN <- ar[ident_dhis2_booking==T,
                .(N=.N,
                  "16 Week Visit"=sum(Succ_1==T, na.rm=T),
                  "BP_16"=sum(Succ_1==T &
                                TrialOne_anbpsyst_present_15_17==T &
                                TrialOne_anbpdiast_present_15_17==T, na.rm=T),
                  "20 Week Visit"=sum(Succ_2==T, na.rm=T),
                  "BP_20"=sum(Succ_2==T &
                                TrialOne_anbpsyst_present_18_22==T &
                                TrialOne_anbpdiast_present_18_22==T, na.rm=T),
                  "24_28 Week Visit"=sum(Succ_3==T, na.rm=T),
                  "BP_24_28"=sum(Succ_3==T &
                                   TrialOne_anbpsyst_present_24_28==T &
                                   TrialOne_anbpdiast_present_24_28==T,na.rm=T),
                  "32 Week Visit"=sum(Succ_4==T, na.rm=T),
                  "BP_32"=sum(Succ_4==T &
                                TrialOne_anbpsyst_present_31_33==T &
                                TrialOne_anbpdiast_present_31_33==T, na.rm=T),
                  
                  "36 Week Visit"=sum(Succ_5==T, na.rm=T),
                  "Bp_36 weeks"=sum(Succ_5 &
                                      TrialOne_anbpsyst_present_35_37==T &
                                      TrialOne_anbpdiast_present_35_37==T, na.rm=T)
                ),
                
                keyby=.(bookyear)]

openxlsx::write.xlsx(prelimHTN,file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         "2020",
                                         sprintf("%s_BpOntime.xlsx",
                                                 lubridate::today()))) 




