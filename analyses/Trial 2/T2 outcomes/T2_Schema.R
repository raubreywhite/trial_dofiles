

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

FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")
 
WB <- readRDS(file.path(FOLDER_DATA_CLEAN,
                                      "T2_clean",
                                      sprintf("T2_FINAL_dataset_%s_WB.rds",CLINIC_INTERVENTION_DATE)))
nrow(WB)


FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")



Gaza <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                         "T2_clean",
                         "T2_FINAL_dataset_2020-10-01_Gaza.rds"))
nrow(Gaza)

 
# combine the data sets

fullT2data <-rbind(WB,
                   Gaza,
                   fill=T)

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


#################################################
# save data set
#################################################
# these are the variables we have in the data set


## here we delete the sensitive variables
fullT2data[,trackedentity:=NULL]
fullT2data[,dummy:=NULL]
fullT2data[,idtype:=NULL]
fullT2data[,motheridno:=NULL]
fullT2data[,firstname:=NULL]
fullT2data[,datecreated:=NULL]
fullT2data[,fathersname:=NULL]
fullT2data[,middlename:=NULL]
fullT2data[,familyname1:=NULL]
fullT2data[,familyname2:=NULL]
fullT2data[,husbandsname:=NULL]
fullT2data[,street:=NULL]
fullT2data[,village:=NULL]
fullT2data[,city:=NULL]
fullT2data[,camp:=NULL]
fullT2data[,mobile:=NULL]
fullT2data[,phone:=NULL]
fullT2data[,email:=NULL]
fullT2data[,cosang:=NULL]
fullT2data[,dob:=NULL]
fullT2data[,income:=NULL]
fullT2data[,education:=NULL]
fullT2data[,agemarriage:=NULL]
fullT2data[,agepregnancy:=NULL]
fullT2data[,members:=NULL]
fullT2data[,age:=NULL]

# this is what i do if i know the entire name of the variable
fullT2data[,bookdate:=NULL]
fullT2data[,booklong:=NULL]
fullT2data[,booklat:=NULL]
fullT2data[,bookorgname:=NULL]
fullT2data[,bookorgcode:=NULL]
#fullT2data[,bookorgunit:=NULL]
fullT2data[,bookidnumber:=NULL]
fullT2data[,demoorgname:=NULL]
fullT2data[,demoorgunit:=NULL]
fullT2data[,bookorgdistrict:=NULL]

# this is what i do if i know part of the variable name
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"latitude")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"organisationunit")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"womanfirst")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"womanfamily")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"husbandname")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"address")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"dataextractor")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"alternateidentificationnum")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"firstname")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"fathersname")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"husbandsfamilyname")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"husbandsname")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"middlename")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"village")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"city")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"dateofbirth")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"mobile")]:=NULL]
#fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"education")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"ageatmarriage")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"ageatfirstpreg")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"monthlyhouseholdincome")]:=NULL]
fullT2data[,names(fullT2data)[stringr::str_detect(names(fullT2data),"numberofmembers")]:=NULL]

badNames <- names(fullT2data)[stringr::str_detect(names(fullT2data),"name")]
goodNames <- badNames[stringr::str_detect(badNames,"labtestname")]
badNames <- badNames[!badNames %in% goodNames]
fullT2data[,(badNames):=NULL]

fullT2data[,booklmp:=NULL]
fullT2data[,bookdatelastbirth:=NULL]
fullT2data[,dateupdated:=NULL]
fullT2data[,expecteddateofdelivery:=NULL]
fullT2data[,calc_expected_due_delivery:=NULL]
fullT2data[,avgincome:=NULL]

saveRDS(fullT2data,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset.rds"))
write.csv(fullT2data,file.path(
                   FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset.csv"))



#################################################
# save data set with vars we want
#################################################
# these are the variables we have in the data set

outcomes <- fullT2data[,
                       c(
                         "uniqueid",
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
                         "bookgestage", 
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
                         "booklaburglu",
                         "booklabbloodglu",
                         "booklabbloodglu_high",
                         "booklabbloodglu_intmd",
                         
                         "ident_dhis2_booking",
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
                         "T2_mansevanemia_29_34",
                         "T2_manmilmodane_29_34",
                         "T2_Oppt_anemia_35_37",
                         "T2_screeniningontime_anemia_35_37",
                         "T2_screeniningontime_anemia_35_37",
                         "T2_mansevanemia_35_37",
                         "T2_Oppt_bp_00_14",
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
                         "T2_manmodsevhtn_35_37",
                         "T2_Opportunity_GDM_screening_b4_24",
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
                         "T2_GDMscreeningontime_4",
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
                         "T2_qidsms_Oppt_anemia_24_28",                 
                         "T2_qidsms_screeniningontime_anemia_24_28",   
                         "T2_qidsms_screeniningontime_no_anemia_24_28", 
                         "T2_qidsms_manmildmodanemia_24_28",           
                         "T2_qidsms_mansevanemia_24_28",
                         "T2_qidsms_Oppt_anemia_35_37",                
                         "T2_qidsms_screeniningontime_anemia_35_37",
                         "T2_qidsms_mansevanemia_35_37",               
                         "T2_qidsms_Oppt_bp_15_17",
                         "T2_qidsms_bpontime_15_17",                   
                         "T2_qidsms_manchronichtn_15_17",
                         "T2_qidsms_Oppt_bp_18_22",                    
                         "T2_qidsms_bpontime_18_22",                   
                         "T2_qidsms_manchronichtn_18_22",              
                         "T2_qidsms_Oppt_bp_24_28",                     
                         "T2_qidsms_bpontime_24_28",                   
                         "T2_qidsms_manmildhtn_24_28",                  
                         "T2_qidsms_manmodsevhtn_24_28",               
                         "T2_qidsms_Oppt_bp_31_33",                     
                         "T2_qidsms_bpontime_31_33",                   
                         "T2_qidsms_manmildhtn_31_33",                  
                         "T2_qidsms_manmodsevhtn_31_33",               
                         "T2_qidsms_Oppt_bp_35_37",                     
                         "T2_qidsms_bpontime_35_37",                   
                         "T2_qidsms_manmildhtn_35_37",                 
                         "T2_qidsms_manmodsevhtn_35_37",               
                         "T2_qidsms_RefHr",                             
                         "T2_qidsms_Opportunity_GDM_screening_24_28",      
                         "T2_qidsms_GDMscreeningontime_24_28",             
                         "T2_qidsms_GDMscreeningontime_24_28_normal",            
                         "T2_qidsms_GDMscreeningontime_24_28_high",             
                         "T2_qidsms_GDMscreeningontime_24_28_intmd")]



saveRDS(outcomes,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset_outcomes.rds"))

write.csv(outcomes,file.path(
                            FOLDER_DATA_CLEAN,
                            "T2_clean",
                            "T2_FINAL_dataset_outcomes.csv"))

