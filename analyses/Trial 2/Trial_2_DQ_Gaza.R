###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_pniphabstracts2018", list.files("r_pniphabstracts2018", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_trial2", list.files("r_trial2", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = TRUE)

###### SETUP ENDS ######
d <- LoadDataFileFromNetworkGaza()



#quality checks for sms
d[,SMSyes:= areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits]

smalld <- d[ident_dhis2_booking==TRUE & 
              ident_hr_clinic==FALSE &
              bookyear>=2019,
            c("SMSyes",
              "bookgestage",
              "ident_dhis2_booking",
              "ident_dhis2_an",
              "ident_dhis2_ppc",
              "ident_hr_clinic",
              "ident_TRIAL_2_3_Control",
              "ident_TRIAL_2_and_3",
              "ident_TRIAL_2",
              "ident_TRIAL_3",
              "str_TRIAL_2_Cluster",
              "bookevent",
              "bookorgname",
              "bookmonth",
              "bookyearmonth")
            
            ]

smsqcheck <- smalld[,.(
  Booked=sum(ident_dhis2_booking),
  WantSMS=sum(SMSyes, na.rm=TRUE),
  SMSNO=sum(SMSyes==0, na.rm=TRUE),
  MissingSMS=sum(is.na(SMSyes))
),
keyby=.(str_TRIAL_2_Cluster,
        bookorgname,
        #has_mobilephone,
        ident_TRIAL_2_3_Control,
        ident_TRIAL_2,
        ident_TRIAL_3,
        bookyearmonth)
]

openxlsx::write.xlsx(smsqcheck, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "sms_monitoring",
                       sprintf("%s_SMS_quality_check.xlsx",lubridate::today())))

###booking by gestational age distribution###
smsbookingA <- smalld[,.(
  Booked=sum(ident_dhis2_booking),
  WantSMS=sum(SMSyes, na.rm=TRUE),
  SMSNO=sum(SMSyes==0, na.rm=TRUE),
  MissingSMS=sum(is.na(SMSyes))
),
keyby=.(str_TRIAL_2_Cluster,
        bookorgname,
        bookgestage,
        ident_TRIAL_2_3_Control,
        ident_TRIAL_2,
        bookyearmonth 
        
)
]

openxlsx::write.xlsx(smsbookingA, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "sms_monitoring",
                       sprintf("%s_BookbyGA.xlsx",lubridate::today())))

###all clinics
smalld <- d[ident_dhis2_booking==TRUE & 
              bookyear>=2019,
            c("SMSyes",
              "ident_dhis2_booking",
              "ident_dhis2_an",
              "ident_dhis2_ppc",
              "ident_hr_clinic",
              "ident_TRIAL_2_3_Control",
              "ident_TRIAL_2_and_3",
              "ident_TRIAL_2",
              "ident_TRIAL_3",
              "str_TRIAL_2_Cluster",
              "bookevent",
              "bookorgname",
              "bookmonth",
              "bookyearmonth")
            
            ]

smsqcheck <- smalld[,.(
  Booked=sum(ident_dhis2_booking),
  WantSMS=sum(SMSyes, na.rm=TRUE)
),
keyby=.(ident_TRIAL_2_3_Control,
        ident_TRIAL_2,
        str_TRIAL_2_Cluster,
        bookorgname, 
        bookyearmonth, 
        ident_hr_clinic)
]

openxlsx::write.xlsx(smsqcheck, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "sms_monitoring",
                       sprintf("%s_SMS_quality_check_ALL_clinics.xlsx",
                               lubridate::today())))


#xtabs(~bookorgname + areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits, data=smalld)

########ANC Who have PPC proportions######

PPCprop <- d[isExpectedToHaveDelivered==TRUE &
               bookyear=="2018",
             c("ident_dhis2_ppc",
               "ident_dhis2_an",
               "ident_dhis2_booking",
               "bookorgname")
             ]
uglytable <- PPCprop[,
                     .(denominator=sum(ident_dhis2_booking, na.rm=TRUE),
                       numeratorPPC=sum(ident_dhis2_ppc==TRUE, na.rm=TRUE),
                       numANC=sum(ident_dhis2_an, na.rm=T) 
                      ),
                     keyby=.(bookorgname)]

#uglytable <- uglytable[, perc:=round(100*numerator/denominator,2)]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "booking_proportions",
                       sprintf("%s_ANC_PPC_Proportions.xlsx", lubridate::today())))


