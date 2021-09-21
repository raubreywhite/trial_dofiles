### Need to import smallD
#check this code and how to read it in despite the date
#anemia <- fread("C:/data processing/data_clean/Trial_1_Outcomes/"Anemia.xlsx",encoding="UTF#-8"")

## T2_screeniningontimeber of rows, 
# check management dates and risk data and other dates for other dates



####################  Trial 2 Outcomes #################### 


###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)
#Setup(IS_GAZA=TRUE)



###### SETUP ENDS ######
#IS_GAZA=T

if(IS_GAZA==F){


  T2 <- readRDS(file.path(FOLDER_DATA_CLEAN,
                            "T2_clean",
                            "WB",
                            sprintf("T2_dataset_%s_WB.rds",CLINIC_INTERVENTION_DATE)))
  nrow(T2)

  T2 <- T2[!is.na(firstvisitinT2) & !is.na(TrialArm)]
  
  nrow(T2)
  
 } else {
  
  T2 <- readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                          "T2_clean",
                           sprintf("T2_dataset_%s_Gaza.rds", CLINIC_INTERVENTION_DATE)))
  
  T2 <- T2[!is.na(firstvisitinT2) & !is.na(TrialArm)]
  
  nrow(T2)
  
  
  
}


################
# 15-17 weeks

# anyone booked before 23 weeks
################




#T2 <- smallD


# only those booked at this category, so add an and statements that does this

#T2_Oppt
T2[,T2_Oppt_anemia_00_23:=as.logical(NA)]

T2[bookgestagedays_cats %in% c("(0,104]",
                               "(104,125]",
                               "(125,160]",
                               "(160,167]"), T2_Oppt_anemia_00_23:=T]



xtabs(~TrialArm+T2_Oppt_anemia_00_23, data=T2,addNA=T)

# 00_23
T2[,T2_screeniningontime_anemia_00_23:=as.logical(NA)]
T2[T2_Oppt_anemia_00_23==T,T2_screeniningontime_anemia_00_23:=FALSE]
T2[T2_screeniningontime_anemia_00_23==F & !is.na(booklabhb),T2_screeniningontime_anemia_00_23:=TRUE]

xtabs(~TrialArm+T2_screeniningontime_anemia_00_23, data=T2, addNA=T)

# no anemia
T2[,T2_screeniningontime_no_anemia_00_23:=as.logical(NA)]
T2[T2_screeniningontime_anemia_00_23==T, T2_screeniningontime_no_anemia_00_23:=FALSE]
T2[T2_screeniningontime_no_anemia_00_23==F & booklabhb>10.9, T2_screeniningontime_no_anemia_00_23:=TRUE]
xtabs(~T2$T2_screeniningontime_no_anemia_00_23, addNA=T)

# 00-23 weeks severe anemia
T2[,T2_mansevanemia_00_23:=as.logical(NA)]
T2[T2_Oppt_anemia_00_23==T & booklabhb<7 & booklabhb>0,T2_mansevanemia_00_23:=F]
xtabs(~T2$T2_mansevanemia_00_23, addNA=T)

# should probably use manhb variable here
T2[T2_mansevanemia_00_23==F &
     (T2_manhb_01_01==T |
        T2_manhb_02_02==T |
        T2_manhb_03_03==T |
        T2_manhb_04_04==T |
        T2_manhb_05_05==T |
        T2_manhb_06_06==T |
        T2_manhb_07_07==T |
        T2_manhb_08_08==T |
        T2_manhb_09_09==T |
        T2_manhb_10_10==T |
        T2_manhb_11_11==T |
        T2_manhb_12_12==T |
        T2_manhb_13_13==T |
        T2_manhb_14_14==T |
        T2_manhb_15_15==T |
        T2_manhb_16_16==T |
        T2_manhb_17_17==T |
        T2_manhb_18_18==T |
        T2_manhb_19_19==T |
        T2_manhb_20_20==T |
        T2_manhb_21_21==T |
        T2_manhb_22_22==T |
        T2_manhb_23_23==T),T2_mansevanemia_00_23:=T]

xtabs(~TrialArm+T2_mansevanemia_00_23, data=T2, addNA=T)

##########
#management
##########


# 00-23 weeks moderate anemia
T2[,T2_manmilmodane_00_23:=as.logical(NA)]
T2[T2_Oppt_anemia_00_23==T & 
     booklabhb>=7 & booklabhb<=10.9,T2_manmilmodane_00_23:=F]
xtabs(~T2$T2_manmilmodane_00_23, addNA=T)
T2[T2_manmilmodane_00_23==F &
     (T2_manhb_mildmodhbret_03_03==T|
        T2_manhb_mildmodhbret_04_04==T|
        T2_manhb_mildmodhbret_05_05==T|
        T2_manhb_mildmodhbret_06_06==T|
        T2_manhb_mildmodhbret_07_07==T|
        T2_manhb_mildmodhbret_08_08==T|
        T2_manhb_mildmodhbret_09_09==T|
        T2_manhb_mildmodhbret_10_10==T|
        T2_manhb_mildmodhbret_11_11==T|
        T2_manhb_mildmodhbret_12_12==T|
        T2_manhb_mildmodhbret_13_13==T|
        T2_manhb_mildmodhbret_14_14==T|
        T2_manhb_mildmodhbret_15_15==T|
        T2_manhb_mildmodhbret_16_16==T|
        T2_manhb_mildmodhbret_17_17==T|
        T2_manhb_mildmodhbret_18_18==T|
        T2_manhb_mildmodhbret_19_19==T|
        T2_manhb_mildmodhbret_20_20==T|
        T2_manhb_mildmodhbret_21_21==T|
        T2_manhb_mildmodhbret_22_22==T|
        T2_manhb_mildmodhbret_23_23==T|
        T2_manhb_mildmodhbret_24_24==T|
        T2_manhb_mildmodhbret_25_25==T|
        T2_manhb_mildmodhbret_26_26==T),T2_manmilmodane_00_23:=T]

xtabs(~T2$T2_manmilmodane_00_23, addNA=T)

# T2_screeniningontime_no_anemia_29_34 create this variable and at 24_28


################
#24-28 weeks
################

# 24-28 weeks
T2[,T2_Oppt_anemia_24_28:=as.logical(NA)]
T2[T2_anvisitnew_24_28==T, T2_Oppt_anemia_24_28:=F]
T2[T2_anvisitnew_24_28==T & T2_screeniningontime_no_anemia_00_23==T, T2_Oppt_anemia_24_28:=TRUE ]

xtabs(~T2$T2_Oppt_anemia_24_28)

# 24-28
T2[,T2_screeniningontime_anemia_24_28:=as.logical(NA)]
T2[T2_Oppt_anemia_24_28==T,T2_screeniningontime_anemia_24_28:=FALSE]
T2[T2_screeniningontime_anemia_24_28==F &
     T2_labhb_exists_24_28==T,T2_screeniningontime_anemia_24_28:=TRUE]

xtabs(~T2$T2_screeniningontime_anemia_24_28, addNA=T)


# no anemia 24- 28 weeks

T2[,T2_screeniningontime_no_anemia_24_28:=as.logical(NA)]
T2[T2_screeniningontime_anemia_24_28==TRUE, T2_screeniningontime_no_anemia_24_28:=FALSE]
T2[T2_screeniningontime_no_anemia_24_28==F & T2_labhb_normal_24_28==T, 
   T2_screeniningontime_no_anemia_24_28:=TRUE]

xtabs(~T2$T2_screeniningontime_no_anemia_24_28, addNA=T)


##########
#management
##########

T2[,T2_manmildmodanemia_24_28:=as.logical(NA)]
T2[T2_Oppt_anemia_24_28==T & T2_riskMildModAne_24_28==T,T2_manmildmodanemia_24_28:=F]
xtabs(~T2$T2_manmildmodanemia_24_28, addNA=T)

T2[T2_manmildmodanemia_24_28==F &
    (T2_manhb_mildmodhbret_27_27==T|
        T2_manhb_mildmodhbret_28_28==T|
        T2_manhb_mildmodhbret_29_29==T|
        T2_manhb_mildmodhbret_30_30==T|
        T2_manhb_mildmodhbret_31_31==T),T2_manmildmodanemia_24_28:=T]

xtabs(~T2$T2_manmildmodanemia_24_28, addNA=T)


# 24-28 weeks severe anemia
T2[,T2_mansevanemia_24_28:=as.logical(NA)]
T2[T2_Oppt_anemia_24_28==T & 
     T2_riskSevAne_24_28==T,T2_mansevanemia_24_28:=F]
xtabs(~T2$T2_mansevanemia_24_28, addNA=T)

# should probably use manhb variable here
T2[T2_mansevanemia_24_28==F &
     (T2_manhb_24_24==T |
        T2_manhb_25_25==T |
        T2_manhb_26_26==T |
        T2_manhb_27_27==T |
        T2_manhb_28_28==T),T2_mansevanemia_24_28:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$T2_mansevanemia_24_28)


################
#29-34 weeks
################

# only those booked at this category, so add an and statements that does this

#T2_Oppt
T2[,T2_Oppt_anemia_29_34:=as.logical(NA)]

T2[bookgestagedays_cats %in% c("(202,216]",
                               "(216,237]",
                               "(237,244]"), T2_Oppt_anemia_29_34:=TRUE ]

xtabs(~T2$T2_Oppt_anemia_29_34, addNA=T)

# 29-34 screening
T2[,T2_screeniningontime_anemia_29_34:=as.logical(NA)]
T2[T2_Oppt_anemia_29_34==T,T2_screeniningontime_anemia_29_34:=FALSE]
T2[T2_screeniningontime_anemia_29_34==F & !is.na(booklabhb),
      T2_screeniningontime_anemia_29_34:=TRUE]

xtabs(~T2$T2_screeniningontime_anemia_29_34, addNA=T)

# no anemia
T2[,T2_screeniningontime_no_anemia_29_34:=as.logical(NA)]
T2[T2_screeniningontime_anemia_29_34==T, T2_screeniningontime_no_anemia_29_34:=FALSE]
T2[T2_screeniningontime_no_anemia_29_34==F & booklabhb>10.9, T2_screeniningontime_no_anemia_29_34:=TRUE]


# 29_34 weeks severe anemia
T2[,T2_mansevanemia_29_34:=as.logical(NA)]
T2[T2_Oppt_anemia_29_34==T & booklabhb<7 & booklabhb>0,T2_mansevanemia_29_34:=F]
xtabs(~T2$T2_mansevanemia_29_34, addNA=T)

# should probably use manhb variable here
T2[T2_mansevanemia_29_34==F &
     (T2_manhb_29_29==T |
        T2_manhb_30_30==T |
        T2_manhb_31_31==T |
        T2_manhb_32_32==T |
        T2_manhb_33_33==T |
        T2_manhb_34_34==T ),T2_mansevanemia_29_34:=T]

xtabs(~T2$T2_mansevanemia_29_34, addNA=T)
xtabs(~T2$T2_Oppt_anemia_29_34)
# T2_manhb_sev==T or T2_riskMildModAne




##########
#management
##########


# 29-34 weeks severe anemia
T2[,T2_manmilmodane_29_34:=as.logical(NA)]
T2[T2_Oppt_anemia_29_34==T & 
     booklabhb>=7 & booklabhb<=10.9,T2_manmilmodane_29_34:=F]
xtabs(~T2$T2_manmilmodane_29_34, addNA=T)
T2[T2_manmilmodane_29_34==F &
     (T2_manhb_mildmodhbret_32_32==T|
        T2_manhb_mildmodhbret_33_33==T|
        T2_manhb_mildmodhbret_34_34==T|
        T2_manhb_mildmodhbret_35_35==T|
        T2_manhb_mildmodhbret_36_36==T),T2_manmilmodane_29_34:=T]
xtabs(~T2$T2_manmilmodane_29_34, addNA=T)


################
#35-37 weeks
################

#35-37 weeks
T2[,T2_Oppt_anemia_35_37:=as.logical(NA)]
T2[T2_anvisitnew_35_37==T, T2_Oppt_anemia_35_37:=F]
T2[T2_anvisitnew_35_37==T & (T2_screeniningontime_no_anemia_24_28==T | T2_screeniningontime_no_anemia_29_34==T), T2_Oppt_anemia_35_37:=TRUE ]

xtabs(~T2$T2_Oppt_anemia_35_37)

# 35_37 screening on time
T2[,T2_screeniningontime_anemia_35_37:=as.logical(NA)]
T2[T2_Oppt_anemia_35_37==T,T2_screeniningontime_anemia_35_37:=FALSE]
T2[T2_screeniningontime_anemia_35_37==F &
     T2_labhb_exists_35_37==T,T2_screeniningontime_anemia_35_37:=TRUE]

xtabs(~T2$T2_screeniningontime_anemia_35_37, addNA=T)


##########
#management
##########

# 35_37 weeks severe anemia
T2[,T2_mansevanemia_35_37:=as.logical(NA)]
T2[T2_Oppt_anemia_35_37==T & 
     T2_riskSevAne_35_37==T,T2_mansevanemia_35_37:=F]
xtabs(~T2$T2_mansevanemia_35_37, addNA=T)

T2[T2_mansevanemia_35_37==F &
     (T2_manhb_35_35==T |
        T2_manhb_36_36==T |
        T2_manhb_37_37==T),T2_mansevanemia_35_37:=T]
# T2_manhb_sev==T or T2_riskMildModAne

xtabs(~T2$T2_mansevanemia_35_37, addNA=T)


# change variable option sets
vars <- c("T2_Oppt_anemia_00_23",
          "T2_screeniningontime_anemia_00_23",
          "T2_screeniningontime_no_anemia_00_23",
          "T2_mansevanemia_00_23",
          "T2_manmilmodane_00_23",
          "T2_Oppt_anemia_24_28",
          "T2_screeniningontime_anemia_24_28",
          "T2_screeniningontime_no_anemia_24_28",
          "T2_manmildmodanemia_24_28",
          "T2_mansevanemia_24_28",
          "T2_Oppt_anemia_29_34",
          "T2_screeniningontime_anemia_29_34",
          "T2_screeniningontime_no_anemia_29_34",
          "T2_mansevanemia_29_34",
          "T2_manmilmodane_29_34",
          "T2_Oppt_anemia_35_37",
          "T2_screeniningontime_anemia_35_37",
          "T2_mansevanemia_35_37")


for (i in vars){
  

T2[,(i):=as.character(get(i))]

T2[stringr::str_detect(get(i),"TRUE"), (i):="Successful"]
T2[stringr::str_detect(get(i),"FALSE"), (i):="Not Successful"]
T2[is.na(get(i)), (i):="Not Applicable"]

}

xtabs(~T2$T2_screeniningontime_anemia_35_37)





