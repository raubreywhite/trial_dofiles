###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

#CheckFilesAndVariables(folder="e.reg-intervention")
#CheckFilesAndVariables(folder="e.reg-control")
CheckFilesAndVariables(folder="e.reg-intervention", 
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)
CheckFilesAndVariables(folder="e.reg-control", 
                       REF_DATE = REF_CLINIC_CONTROL_DATE, 
                       CHECK_DATE = CLINIC_CONTROL_DATE)


###### SETUP ENDS ######

# how trial 2 data works 

# T2_process_outcomes.R
# T2_Anemia
# T2_Hypertension
# T2_GDM
# Save file after run Anemia, htn,GDM

# Merge with T2_attendance

# Run T2_QIDSMSOutcomes

# append T2 WB and T2 gaza

# calculate outcomes in long format for all four arms

# save copies of WB and Gaza data
# Append the two data sets and send out

# background variable stuff

#Background characteristics  
#•	Women – demographics e.g age, and the following SEP variables
#o	average monthly household incomes (less than 200; 200 – 900; 901 – 1824; 1825 – 3054; and > 3055 Israeli new Shekel#), 
#o	mother’s years of education (<10; 10- 13 years; >13 years), 
#o	age at marriage (less than 20; 21-25; 26- 30; 31- 35; 36- 40; greater than 40 years) 
#o	age at first pregnancy (less than 20; 20-25; 26- 30; 31- 35; 36- 40; greater than 40 years). 
#•	Clinic characteristics   – e.g. lab, UL, level,   
#•	Variables used in the randomization 
#o	phase (time point) of the eRegistry implementation (stratified)
#o	laboratory availability, ultrasound availability and the size of the PHC (constrained)



###################
# APPEND
###################

FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")

FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")
 
WB <- readRDS(file.path(FOLDER_DATA_CLEAN,
                                      "T2_clean",
                                      "T2_FINAL_dataset_2021-08-12_WB.rds"))
nrow(WB)



FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")


# make sure correct data for this file before running
Gaza <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                         "T2_clean",
                         "T2_FINAL_dataset_2020-10-01_Gaza.rds"))
nrow(Gaza)

 
# combine the data sets

fullT2data <-rbind(WB,
                   Gaza,
                   fill=T)

nrow(fullT2data)==nrow(Gaza)+nrow(WB)



# check the trial codes
# GP019999-MCH	335
# GP543127-MCH	177
# GP010520-MCH	216
# GP532815-MCH	193
# GP100645-MCH	366
# GP523245-MCH	281
# GP150922-MCH	227
# GP502655-MCH	193


# remove variables from event data that we dont want

# bookdate
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_bookdate")]
fullT2data[,(vars):=NULL]

# ident_WB
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_ident_WB")]
fullT2data[,(vars):=NULL]

# ident_WB
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_ident_schedev_")]
fullT2data[,(vars):=NULL]


# orgname
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_orgname_")]
fullT2data[,(vars):=NULL]


# orgunitcode
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_orgunitcode_")]
fullT2data[,(vars):=NULL]


nrow(WB)+nrow(Gaza)==nrow(fullT2data)

xtabs(~fullT2data$TrialArm, addNA=T)


###############################
# lab stuff
###############################
# copied original sheet and changed names to match the ones in our structural data
# will prevent duplicates from coming up
# merge data for US and lab availability

labandus <- data.table(readxl::read_excel(file.path(
                                FOLDER_DATA_RAW,
                                "structural_data",
                                "clinics_for_randomization_100718.xlsx")))

length(unique(labandus$clustername))

#make clinic names lower case
#labandus[,clustername:=ExtractOnlyEnglishLetters(clustername)]
#xtabs(~labandus$clustername,addNA=T)


labandus[,lab:=as.numeric(NA)]
labandus[labavailability=="Yes",lab:=1]
labandus[labavailability=="No",lab:=0]

labandus[,us:=as.numeric(NA)]
labandus[usavailability=="Yes",us:=1]
labandus[usavailability=="No",us:=0]


merged <- merge(fullT2data,
                labandus,
                by="str_TRIAL_2_Cluster",
                all.x=T)


nrow(fullT2data)
nrow(merged)
nrow(labandus)

nrow(merged[is.na(str_TRIAL_2_Cluster)])
nrow(merged[is.na(clustername)])
nrow(merged[is.na(phase)])

xtabs(~merged$phase, addNA=T)


#unique(merged[is.na(clustername)]$bookorgname)


#################################################
# save data set
#################################################
# these are the variables we have in the data set


## here we delete the sensitive variables
merged[,trackedentity:=NULL]
merged[,dummy:=NULL]
merged[,idtype:=NULL]
merged[,motheridno:=NULL]
merged[,firstname:=NULL]
merged[,datecreated:=NULL]
merged[,fathersname:=NULL]
merged[,middlename:=NULL]
merged[,familyname1:=NULL]
merged[,familyname2:=NULL]
merged[,husbandsname:=NULL]
merged[,street:=NULL]
merged[,village:=NULL]
merged[,city:=NULL]
merged[,camp:=NULL]
merged[,mobile:=NULL]
merged[,phone:=NULL]
merged[,email:=NULL]
merged[,cosang:=NULL]
merged[,dob:=NULL]
merged[,income:=NULL]
merged[,education:=NULL]
merged[,agemarriage:=NULL]
merged[,agepregnancy:=NULL]
merged[,members:=NULL]
merged[,age:=NULL]

# this is what i do if i know the entire name of the variable
merged[,bookdate:=NULL]
merged[,booklong:=NULL]
merged[,booklat:=NULL]
merged[,bookorgname:=NULL]
#merged[,bookorgcode:=NULL]
#merged[,bookorgunit:=NULL]
merged[,bookidnumber:=NULL]
merged[,demoorgname:=NULL]
merged[,demoorgunit:=NULL]
merged[,bookorgdistrict:=NULL]

# this is what i do if i know part of the variable name
merged[,names(merged)[stringr::str_detect(names(merged),"latitude")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"organisationunit")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"womanfirst")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"womanfamily")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"husbandname")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"address")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"dataextractor")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"alternateidentificationnum")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"firstname")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"fathersname")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"husbandsfamilyname")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"husbandsname")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"middlename")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"village")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"city")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"dateofbirth")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"mobile")]:=NULL]
#merged[,names(merged)[stringr::str_detect(names(merged),"education")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"ageatmarriage")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"ageatfirstpreg")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"monthlyhouseholdincome")]:=NULL]
merged[,names(merged)[stringr::str_detect(names(merged),"numberofmembers")]:=NULL]

badNames <- names(merged)[stringr::str_detect(names(merged),"name")]
goodNames <- badNames[stringr::str_detect(badNames,"labtestname")]
badNames <- badNames[!badNames %in% goodNames]
merged[,(badNames):=NULL]

merged[,booklmp:=NULL]
merged[,bookdatelastbirth:=NULL]
merged[,dateupdated:=NULL]
merged[,expecteddateofdelivery:=NULL]
merged[,calc_expected_due_delivery:=NULL]
merged[,avgincome:=NULL]


merged[,districts:=NULL]
merged[,clustername:=NULL]



saveRDS(merged,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset.rds"))
write.csv(merged,file.path(
                   FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset.csv"))

merged[,str_TRIAL_2_ClusSize:=NULL]

merged[,str_TRIAL_2_ClusSize:=clussize]

merged[,phase.x:=NULL]
setnames(merged, "phase.y","phase")

#################################################
# save data set with vars we want
#################################################
# these are the variables we have in the data set
# add vars we merged in from sheet above
outcomes <- merged[, c(  "uniqueid",
                         "bookevent",
                         "booknum",
                         "bookorgdistricthashed",
                         "bookorgunit",
                         "bookgestage",
                         "bookgestagedays_cats",
                         "agecat",
                         "agemarriagecat",
                         "agepregnancycat",
                         "avgincomecat",
                         "educationcat",
                         "bookbmicat",
                         "para",
                         "gravida",
                         "bookhistdm", 
                         "bookhistcs", 
                         "bookhistgdm",
                         "bookhistpreecl", 
                         "bookhistghtn", 
                         "bookhistperi", 
                         "bookhistpph", 
                         "bookhistaph",
                         "bookhistabort",
                         "bookhistpreterm",
                         "wantSMS",
                         "firstvisitinT2",
                         "booklabhb",
                         "bookbpsyst",
                         "bookbpdiast",
                         "bookhighrisk",
                         "bookhrhighsug",
                         "booklaburglu",
                         "booklabbloodglu",
                         "booklabbloodglu_high",
                         "booklabbloodglu_intmd",
                         
                         "ident_dhis2_booking",
                         "ident_WB",
                         "TrialArm",
                         "str_TRIAL_2_Cluster",
                         "str_TRIAL_2_ClusSize",
                         "ident_hr_clinic",
                         
                         
                         "T2_Oppt_anemia_00_23",
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
                         "T2_manmildmodanemia_29_34",
                         "T2_mansevanemia_29_34",
                         "T2_Oppt_anemia_35_37",
                         "T2_screeniningontime_anemia_35_37",
                         "T2_manmildmodanemia_35_37",
                         "T2_mansevanemia_35_37",
                         
                         
                         "T2_Oppt_bp_00_14",
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
                         "T2_manmodsevhtn_35_37",
                         
                         
                        "T2_Opportunity_GDM_screening_b4_24",
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
                        "T2_GDMscreeningontime_after_28",
                        "T2_GDMscreeningontime_after_28_normal",
                        "T2_GDMscreeningontime_after_28_high", 
                        
                         "denom_15_17",
                         "denom_18_22",
                         "denom_24_28",
                         "denom_31_33",
                         "denom_35_37",
                         "num_15_17",
                         "num_18_22",
                         "num_24_28",
                         "num_31_33",
                         "num_35_37",
                        varsT2qidsms,
                         "phase",
                         "clussize",
                         "labavailability",
                         "usavailability",
                        "lab",
                        "us"), with=F]


# need to merge with two things so we dont get the duplicate rows
mergedoutcomes <- merge(outcomes,
                labandus,
                by="str_TRIAL_2_Cluster",
                all.x=T)

nrow(mergedoutcomes)
nrow(outcomes)

saveRDS(outcomes,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  sprintf("T2_FINAL_dataset_outcomes_%s.rds", CLINIC_INTERVENTION_DATE)))

write.csv(outcomes,file.path(
                            FOLDER_DATA_CLEAN,
                            "T2_clean",
                            sprintf("T2_FINAL_dataset_outcomes_%s.csv", CLINIC_INTERVENTION_DATE)))

#########################################################################################################



attcheck <- readRDS(file.path(FOLDER_DATA_CLEAN,
                              "T2_clean",
                              "T2_FINAL_dataset_outcomes_2021-08-12.rds"))


outcomes <- attcheck[,.(
  "15-17 week numeratorT"=sum(num_15_17==T, na.rm=T),
  "15-17 week numeratorF"=sum(num_15_17==F, na.rm=T),
  "15-17 Denom"=sum(denom_15_17==T, na.rm=T),
  "18-22 week numeratorT"=sum(num_18_22==T, na.rm=T),
  "18-22 week numeratorF"=sum(num_18_22==F, na.rm=T),
  "18-22 Denom"=sum(denom_18_22==T, na.rm=T),
  
  "24-28 week numeratorT"=sum(num_24_28==T, na.rm=T),
  "24-28 week numeratorF"=sum(num_24_28==F, na.rm=T),
  "24-28 Denom"=sum(denom_24_28==T, na.rm=T),
  "31-33 week numeratorT"=sum(num_31_33==T, na.rm=T),
  "31-33 week numeratorF"=sum(num_31_33==F, na.rm=T),
  "31-33 Denom"=sum(denom_31_33==T, na.rm=T),
  "35-37 week numeratorT"=sum(num_35_37==T, na.rm=T),
  "35-37 week numeratorF"=sum(num_35_37==F, na.rm=T),
  "35-37 Denom"=sum(num_35_37==T |
                      num_35_37==F, na.rm=T)),
  keyby=.(TrialArm,phase)]

outcomes[,prop_15_17_attend:=round(`15-17 week numeratorT`/`15-17 Denom`, digits=3)]

outcomes[,prop_18_22_attend:=round(`18-22 week numeratorT`/`18-22 Denom`, digits=3)]

outcomes[,prop_24_28_attend:=round(`24-28 week numeratorT`/`24-28 Denom`, digits=3)]

outcomes[,prop_31_33_attend:=round(`31-33 week numeratorT`/`31-33 Denom`, digits=3)]

outcomes[,prop_35_37_attend:=round(`35-37 week numeratorT`/`35-37 Denom`, digits=3)]


openxlsx::write.xlsx(outcomes,
                     file.path(FOLDER_DATA_CLEAN,
                               "T2_clean",
                               sprintf("%s_attendance_check_wide_data.xlsx",lubridate::today())))

