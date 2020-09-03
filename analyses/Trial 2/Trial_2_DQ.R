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

IS_GAZA <- TRUE

Setup(IS_GAZA = TRUE)

###### SETUP ENDS ######

if(IS_GAZA==F){


  d <- LoadDataFileFromNetworkWB()
  fileTag <- "WB"

  }else {
  
  d <- LoadDataFileFromNetworkGaza()
  fileTag <- "GAZA"
  
  
  
  
  }



# export bookorgunitcodes

# Trial 2 Variables (move to creating further variables when run new data)
d[,ident_T2:=as.logical(NA)]
d[bookyearmonth>="2019-12" &
    (ident_TRIAL_2_and_3),ident_T2:=T]

# make a variable for trial 2 and 3 (qid and sms)
d[ident_T2==T,ident_T2_T3:=FALSE]
d[ident_T2==T & ident_TRIAL_2==T & ident_TRIAL_3==T, ident_T2_T3:=TRUE]


# renaming arms
d[ident_T2==T & ident_TRIAL_2_3_Control==T, prettyExposure:="Trial Arm A"]
d[ident_T2==T & ident_TRIAL_2==T & 
    ident_T2_T3==F, prettyExposure:="Trial Arm C"]
d[ident_T2==T & 
    ident_TRIAL_3==T & ident_T2_T3==F, prettyExposure:="Trial Arm B"]
d[ident_T2==T & ident_T2_T3==T, prettyExposure:="Trial Arm D"]



# bookorgcodes for each of the clinics
bookorgcodes <-d[ident_TRIAL_2_and_3==T & bookyearmonth>="2019-12",
                 c("bookorgname","bookorgcode","prettyExposure")]

setorder(bookorgcodes,bookorgname)

bookorgcodes[,Numtimes:=1:.N,by=.(bookorgname)]
bookorgcodes <- bookorgcodes[Numtimes==1]



# adjust this code as a a function for is_gaza
openxlsx::write.xlsx(bookorgcodes,file.path(FOLDER_DATA_RESULTS_GAZA,
                                            "T2",
                                            sprintf("bookorgcodes_GAZA.xlsx",
                                            lubridate::today())))



#quality checks for sms
d[,SMSyes:= areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits]
d[,has_mobilephone:=FALSE]
d[mobile!="",has_mobilephone:=TRUE]


smalld <- d[ident_dhis2_booking==TRUE & 
              ident_hr_clinic==FALSE &
              bookyear>=2019 &
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
                        WantSMSandEligible=sum(has_mobilephone==T &
                                                 SMSyes==T, na.rm=T),
                        SMSNO=sum(SMSyes==0, na.rm=TRUE),
                        MissingSMS=sum(is.na(SMSyes)),
                        has_mobilephone=sum(has_mobilephone,na.rm=T),
                        meanbookgestage=mean(bookgestage,na.rm=T)
                         ),
                        keyby=.(str_TRIAL_2_Cluster,
                                bookorgname,
                                #ident_hr_clinic,
                                ident_TRIAL_2_3_Control,
                                ident_TRIAL_2,
                                bookyearmonth 
                                
                                )
                      ]

openxlsx::write.xlsx(smsqcheck, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "sms_monitoring",
                       sprintf("%s_%s_SMS_quality_check.xlsx",lubridate::today(),fileTag)))

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
                       sprintf("%s_%s_BookbyGA.xlsx",lubridate::today(),fileTag)))

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
                       FOLDER_DATA_RESULTS,
                       "sms_monitoring",
                       sprintf("%s_%s_SMS_quality_check_ALL_clinics.xlsx",
                               lubridate::today(),fileTag)))


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
                       sprintf("%s_ANC_PPC_Proportions_2018.xlsx",fileTag
                               )))


##### QID

smallD <- d[bookyear>=2019, c("ident_TRIAL_2_and_3",
                             "ident_TRIAL_3",
                             "ident_TRIAL_2",
                             "ident_TRIAL_2_3_Control",
                             "ident_dhis2_booking",
                             "bookyearmonth",
                             "str_TRIAL_2_Cluster")]

qidqc <- smallD[!is.na(str_TRIAL_2_Cluster),.(Booked=sum(ident_dhis2_booking==T)),
                keyby=.(str_TRIAL_2_Cluster,
                        ident_TRIAL_2,
                        ident_TRIAL_2_3_Control,
                        ident_TRIAL_2_and_3,
                        ident_TRIAL_3,
                       bookyearmonth)]

openxlsx::write.xlsx(qidqc, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "sms_monitoring",
                       sprintf("%s_%s_qidbooking.xlsx",lubridate::today(),fileTag)))




