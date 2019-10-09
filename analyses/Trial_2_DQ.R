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

Setup(IS_GAZA = FALSE)

###### SETUP ENDS ######

#d <- LoadDataFileFromNetworkGaza()
d <- LoadDataFileFromNetworkWB()


#quality checks for sms
d[,SMSyes:= areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits]
d[,has_mobilephone:=FALSE]
d[mobile!="",has_mobilephone:=TRUE]


smalld <- d[ident_dhis2_booking==TRUE & 
              ident_hr_clinic==FALSE &
              bookyearmonth%in%c("2019-06","2019-07") &
              ident_TRIAL_2_and_3==TRUE,
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
                    "bookyearmonth",
                    "has_mobilephone")
                 
                  ]

smsqcheck <- smalld[,.(
                        Booked=sum(ident_dhis2_booking),
                        WantSMS=sum(SMSyes, na.rm=TRUE),
                        SMSNO=sum(SMSyes==0, na.rm=TRUE),
                        MissingSMS=sum(is.na(SMSyes))
                         ),
                        keyby=.(str_TRIAL_2_Cluster,
                                bookorgname,
                                has_mobilephone,
                                #ident_hr_clinic,
                                ident_TRIAL_2_3_Control,
                                ident_TRIAL_2,
                                bookyearmonth 
                                
                                )
                      ]

openxlsx::write.xlsx(smsqcheck, 
                     file.path(
                       FOLDER_DATA_RESULTS,
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
                       FOLDER_DATA_RESULTS,
                       "sms_monitoring",
                       sprintf("%s_BookbyGA.xlsx",lubridate::today())))

###all clinics
smalld <- d[ident_dhis2_booking==TRUE & 
              bookyearmonth%in%c("2019-06","2019-07") &
              ident_TRIAL_2_and_3==TRUE,
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
        str_TRIAL_2_Cluster,
        bookorgname, 
        bookyearmonth, 
        ident_hr_clinic)
]

openxlsx::write.xlsx(smsqcheck, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       sprintf("%s_SMS_quality_check_ALL_clinics.xlsx",lubridate::today())))


#xtabs(~bookorgname + areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits, data=smalld)

########ANC Who have PPC proportions######

PPCprop <- d[ident_dhis2_an==TRUE & 
               isExpectedToHaveDelivered==TRUE &
               bookyear=="2018",
                    c("ident_dhis2_ppc",
                      "ident_dhis2_booking",
                      "bookorgname")
               ]
uglytable <- PPCprop[,
                     .(denominator=sum(ident_dhis2_booking, na.rm=TRUE),
                       numerator=sum(ident_dhis2_ppc==TRUE, na.rm=TRUE)

                     ),
                        keyby=.(bookorgname)]

uglytable <- uglytable[, perc:=round(100*numerator/denominator,2)]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "ANC_PPC_Proportions_2018.xlsx"))


