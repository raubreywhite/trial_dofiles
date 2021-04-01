###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)


# run set up from runwb or run gaza

###### SETUP ENDS ######

##### Notes ##### 
# global options -->  unticked cleanup output after successful... 
# and ticked View Rcheck
# eventsData events scheduled
# msglogs= messages sent

if(IS_GAZA==F){
  
  # read in logs #
  
  # all scheduled events
  # setting header=FALSE bc doesnt have column names, so specifying them ourselves and dont want to read in first row of data as
  eventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-10-26.csv", header=FALSE)
  
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-09-22.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/data_raw/smslogs/dhis2allmsgssent/2020-09-22.csv")
  
  ###### Load in Data Set ###### 
  d <- LoadDataFileFromNetwork()
  
  t2 <- readRDS(file.path(FOLDER_DATA_CLEAN,
                          "T2_clean",
                          "WB",
                          "T2_dataset_2020-12-19_WB.rds"))
  
  fileTag <- "WB"
  
  
  
} else {
  
  
  # put gazas stuff here
  fileTag <- "GAZA"
  
  
  # all scheduled events
  eventsraw <- fread("C:/data processing/gaza_data_raw/smslogs/schedevents/2020-12-29.csv", header=TRUE)
  
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/gaza_data_raw/smslogs/dhis2allschedmsgs/2020-08-25.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/gaza_data_raw/smslogs/dhis2allmsgssent/2020-08-25.csv")
  
 
 
  # Load in data from network
  d <- LoadDataFileFromNetworkGaza()
  
  
}

############ Trial 2 SMS Monitoring ############  ###############



varsT2 <- names(t2)[stringr::str_detect(names(t2),"^T2")]

setnames(t2,
         "areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits",
         "wantSMS")


t2[,andate_0:=bookdate]
t2[,angestage_0:=bookgestage]
t2[,anevent_0:=bookevent]

varsancevent <- names(t2)[stringr::str_detect(names(t2),"^anevent")]
varsancdate <- names(t2)[stringr::str_detect(names(t2),"^andate")]
varsancgestage <- names(t2)[stringr::str_detect(names(t2),"^angestage")]



#52705
t2 <- t2[!is.na(firstvisitinT2) & !is.na(TrialArm)]
nrow(t2)


t2 <- t2[,c("uniqueid",
            "bookevent",
            "bookorgname",
            "bookdate",
            "bookgestage",
            "firstvisitinT2",
            "USorLMPdate",
            "ident_dhis2_booking",
            "str_TRIAL_2_Cluster",
            "TrialArm",
            "wantSMS",
            varsancevent,
            varsancdate,
            varsancgestage,
            varsT2), with=F]



nrow(t2)


#test <- t2[1:2]
# reshape dataset to long so we can merge with events data

longA <- melt(t2,
            id.vars=c("uniqueid",
                                 "bookevent",
                                 "bookorgname",
                                 "bookdate",
                                 "bookgestage",
                                 "USorLMPdate",
                                 "ident_dhis2_booking",
                                 "str_TRIAL_2_Cluster",
                                 "TrialArm",
                                 "wantSMS"),
            measure.vars=patterns("firstvisitinT2",
                                  "^anevent_",
                                  "^andate_",
                                  "^angestage_"), 
            value.name=c("firstvisitinT2",
                         "anevent",
                         "andate",
                         "angestage"))

nrow(longA)==length(unique(longA$anevent))

# manually looked into two rows and saw that bc this is dt, all missing values from measure vars were being included. 
#need to remove them from here

longA <- longA[!is.na(anevent)]
nrow(longA)==length(unique(longA$anevent))

longA[,variable:=NULL]

setorder(longA,uniqueid,andate)

nrow(longA)

# anything not in measure vars or values vars will be set to the id vars
# newlong <- melt(t2, measure = patterns("^anevent_", "^andate_", "^angestage_"),
# value.name = c("anevent", "andate","angestage"))[, variable := NULL][]



# tag our data 
longA[,ident_dhis2:=T]
longA[bookevent!=anevent, ident_visit:="ANC event"]
longA[bookevent==anevent, ident_visit:="Booking event"]

long <- longA[andate>="2019-12-01" & andate<="2020-03-22"]

setnames(long, "anevent","event")
xtabs(~long$ident_visit)

setorder(long,uniqueid,andate)

long[,numDhis2visit:=1:.N, by=uniqueid]
xtabs(~long$numDhis2visit, addNA=T)

nrow(long)


length(unique(long$uniqueid))
length(unique(t2$uniqueid))

###################################
############ Events DATA ######### 
###################################

#with=F looks inside the vars and get the values
eventsraw[1:5]

# based on sql query, these are the names
setnames(eventsraw,1,"event")
setnames(eventsraw,2,"status")
setnames(eventsraw,3,"deleted")
setnames(eventsraw,4,"dueDate")
setnames(eventsraw,5,"createddate")
setnames(eventsraw,6,"enrollmentid")
setnames(eventsraw,7,"tei")
setnames(eventsraw,8,"programStagename")
setnames(eventsraw,9,"programstageid")
setnames(eventsraw,10,"eventdate")
setnames(eventsraw,11,"orgunitcode")
setnames(eventsraw,12,"orgname")
setnames(eventsraw,13,"completeddate")

setorder(eventsraw,tei,eventdate)

eventsData <- eventsraw

nrow(eventsData)
### cleaning var types
#deleted
eventsData[deleted=="t",deleted:="TRUE"]
eventsData[deleted=="f",deleted:="FALSE"]
eventsData[,deleted:=as.logical(deleted)]
xtabs(~eventsData$deleted)

if(IS_GAZA==F){
  
#duedate
eventsData[,dueDate:=stringr::str_sub(dueDate,1,10)]
eventsData[,dueDate:=as.Date(dueDate)]
xtabs(~eventsData$dueDate)


#createddate
eventsData[,createddate:=stringr::str_sub(createddate,1,10)]
eventsData[,createddate:=as.Date(createddate)]
xtabs(~eventsData$createddate)

#eventdate
eventsData[,eventdate:=stringr::str_sub(eventdate,1,10)]
eventsData[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
xtabs(~eventsData$eventdate)

#completeddate
eventsData[,completeddate:=stringr::str_sub(completeddate,1,10)]
eventsData[completeddate=="", completeddate:=NA]
eventsData[,completeddate:=as.Date(completeddate, formmat="%Y-%m-%d")]

xtabs(~eventsData$completeddate, addNA=T)

} else {
  #duedate
   # remove time 
  eventsData[,dueDate:=stringr::str_remove_all(dueDate," [0-9]*:[0-9][0-9]")]
  eventsData[,dueDate:=lubridate::mdy(dueDate)]
  xtabs(~eventsData$dueDate)
  
  
  #createddate
  eventsData[,createddate:=stringr::str_remove_all(createddate," [0-9]*:[0-9][0-9]")]
  eventsData[,createddate:=lubridate::mdy(createddate)]
  xtabs(~eventsData$createddate)
  
  #eventdate
  eventsData[,eventdate:=stringr::str_remove_all(eventdate," [0-9]*:[0-9][0-9]")]
  eventsData[,eventdate:=lubridate::mdy(eventdate)]
  xtabs(~eventsData$eventdate)
  
  #completeddate
  eventsData[,completeddate:=stringr::str_remove_all(completeddate," [0-9]*:[0-9][0-9]")]
  eventsData[,completeddate:=lubridate::mdy(completeddate)]
  xtabs(~eventsData$completeddate, addNA=T)
  
}


# cleaning orgname
eventsData[,orgname:=ExtractOnlyEnglishLetters(orgname)]
xtabs(~eventsData$orgname)




# order data set
setorderv(eventsData,c("tei",
                        "eventdate"),
          c(1,1), na.last=T)

nrow(eventsData)


# create bookdate for each pregnancy
eventsData[,bookdate:= as.Date(NA)]
eventsData[programstageid=="WZbXY0S00lP", bookdate:=eventdate, by=tei]

# fill in bookdates for the rest of the values
eventsData[,bookdate:=zoo::na.locf(bookdate),by=tei]
xtabs(~eventsData$bookdate, addNA=T)

setorder(eventsData,tei,eventdate)


#### event defintions #### 
#1.	event status of either complete or active AND have a dueDate is AFTER event date: the woman did not have a visit per #schedule, but came earlier than scheduled. 
#2.	event status of either complete or active AND  have a dueDate AND event date on the same day: the woman had a visit #as per what was scheduled. 

eventsData[,ident_schedev:=T]
setnames(eventsData,"tei", "uniqueid")


########################################################### 
########## start processing long and events data ########## 
########################################################### 

#longcut <- long[andate>="2019-12-01" & andate<="2020-03-22" & !is.na(TrialArm)]

longcut <- long

#setnames(longcut,"anevent","event")

z <- merge(eventsData,
           longcut,
           by=c("event","uniqueid"),
           all=T)

setorder(z,uniqueid,eventdate)

nrow(z)
nrow(z[ident_schedev==T & ident_dhis2==T])

length(unique(z$uniqueid))

length(unique(z[ident_dhis2==T]$uniqueid))
length(unique(z[ident_dhis2==T & ident_schedev==T]$uniqueid))

z[is.na(ident_dhis2) & ident_schedev==T]

# look at the scheduled events for the uniqueids from the eReg data and check them here-- they should have merged... 

xtabs(~ident_dhis2+status, data=z)

# list of uniqueid's merged with dhis2

length(unique(t2$uniqueid))

xtabs(~z$bookorgname)

xtabs(~z$TrialArm)




###########################
# fill in vars by uniqueid
###########################

z[,USorLMPdate:=zoo::na.locf(USorLMPdate),by=uniqueid]
z[,TrialArm:=zoo::na.locf(TrialArm),by=uniqueid]
z[,str_TRIAL_2_Cluster:=zoo::na.locf(str_TRIAL_2_Cluster),by=uniqueid]

# due date only exists for scheduled events that didnt happen, when events have status==schedule
z[,gAatdueDate:=as.numeric(difftime(dueDate,
                         USorLMPdate,
                         units="days"))]
xtabs(~z$gAatdueDate, addNA=T)


z[,gAateventdate:=as.numeric(difftime(eventdate,
                         USorLMPdate,
                         units="days"))]
xtabs(~z$gAateventdate, addNA=T)


z[,gAatcreateddate:=as.numeric(difftime(createddate,
                         USorLMPdate,
                         units="days"))]

xtabs(~z$gAatcreateddate, addNA=T)



# creating bookgestagecats

z[,bookgestagecat:=cut(bookgestage,
                       breaks=c(0,14,17,22,23,28,30,33,34,37,40),
                       include.lowest=T)]



setorder(z,uniqueid,andate,na.last=T)



########################################## 
# save data sets to send out an email
##########################################

t2 <- readRDS(file.path(FOLDER_DATA_CLEAN,
                        "T2_clean",
                        "WB",
                        "T2_dataset_2020-12-19_WB.rds"))


smallD <- t2[1:1000]
saveRDS(smallD,file.path(FOLDER_DATA_CLEAN,
                         "T2_clean",
                         "WB",
                         sprintf("T2_dataset_sample_%s.rds",
                                 CLINIC_INTERVENTION_DATE)))




tz <- z[!is.na(TrialArm) &
         ((dueDate>="2019-12-01" &
             dueDate<="2020-03-22")|
            (eventdate>="2019-12-01" &
               eventdate<="2020-03-22"))]
saveRDS(tz,
        file.path(
          FOLDER_DATA_CLEAN,
          "T2_clean",
          "WB",
          "T2_attendance_sample.rds"
        ))

###############################
# idenfify sched & attended evs
###############################

#########
# 15-17
#########

# denom
z[(status %in% c("SCHEDULE","SKIPPED") &
     (gAatdueDate>=105 & gAatdueDate<=125) & 
     is.na(eventdate))|
    (status %in% c("COMPLETED","ACTIVE","VISITED") &
                          eventdate>createddate & 
                          (gAateventdate>=105 & gAateventdate<=125 &
                             gAatcreateddate>0 & gAatcreateddate<105)) &
    programStagename=="Antenatal care visit", denom_15_17:= T]

xtabs(~z$denom_15_17, addNA=T)

# numerator
# scheduled only
z[denom_15_17==T &
    (status %in% c("SCHEDULE","SKIPPED") & gAatdueDate>=105 & gAatdueDate<=125) & 
    is.na(eventdate) &
    programStagename=="Antenatal care visit",num_15_17:=F]


# those who attended
z[denom_15_17==T &
    (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
     eventdate>createddate) & (gAateventdate>=105 & gAateventdate<=125) &
    (gAatcreateddate>0 & gAatcreateddate<105) &
  programStagename=="Antenatal care visit",num_15_17:=T]

xtabs(~z$num_15_17, addNA=T)


#########
# 18-22
#########

# denom
z[(status %in% c("SCHEDULE","SKIPPED") &
     (gAatdueDate>=126 & gAatdueDate<=160) & 
     is.na(eventdate)) |
    (status %in% c("COMPLETED","ACTIVE","VISITED") &
       eventdate>createddate & 
       (gAateventdate>=126 & gAateventdate<=160 &
          gAatcreateddate>0 & gAatcreateddate<126)) &
    programStagename=="Antenatal care visit", denom_18_22:= T]

xtabs(~z$denom_18_22, addNA=T)

# numerator
# scheduled only
z[denom_18_22==T & 
    (status %in% c("SCHEDULE","SKIPPED") & is.na(eventdate) &
     gAatdueDate>=126 & gAatdueDate<=160) &
    programStagename=="Antenatal care visit",num_18_22:=F]


# those who attended
z[denom_18_22==T &
    (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
     eventdate>createddate) & (gAateventdate>=126 & gAateventdate<=160) & 
    programStagename=="Antenatal care visit",num_18_22:=T]


xtabs(~z$num_18_22, addNA=T)

#########
# 24-28
#########

# denom
z[(status %in% c("SCHEDULE","SKIPPED") & 
     (gAatdueDate>=168 & gAatdueDate<=202) & 
     is.na(eventdate))|
     ((status %in% c("ACTIVE","COMPLETED","VISITED") & 
         eventdate>createddate & 
        (gAateventdate>=168 & gAateventdate<=202) &
           gAatcreateddate>0 & gAatcreateddate<168)) & 
    programStagename=="Antenatal care visit",denom_24_28:=T]

xtabs(~z$denom_24_28, addNA=T)


# numerator
# scheduled only
z[denom_24_28==T &
    (status %in% c("SCHEDULE","SKIPPED") & 
       gAatdueDate>=168 & gAatdueDate<=202) &
    programStagename=="Antenatal care visit", num_24_28:=F]


# those who attended
z[denom_24_28==T &
    (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
     eventdate>createddate) & (gAateventdate>=168 & gAateventdate<=202) & 
    programStagename=="Antenatal care visit",num_24_28:=T]


xtabs(~z$num_24_28, addNA=T)


#########
# 31-33
#########
# denom
z[(status %in% c("SCHEDULE","SKIPPED") &
      is.na(eventdate) &
      (gAatdueDate>=217 & gAatdueDate<=237))|
     (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
         eventdate>createddate &
        (gAateventdate>=217 & gAateventdate<=237) &
        (gAatcreateddate>0 & gAatcreateddate<217)) & 
    programStagename=="Antenatal care visit",denom_31_33:=T]


xtabs(~z$denom_31_33, addNA=T)



# numerator
# scheduled only
z[denom_31_33==T &
    (status %in% c("SCHEDULE","SKIPPED") & 
       is.na(eventdate) &
       gAatdueDate>=217 & gAatdueDate<=237) &
    programStagename=="Antenatal care visit",num_31_33:=F]


# those who attended
z[denom_31_33==T &
    (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
     eventdate>createddate) &
      (gAateventdate>=217 & gAateventdate<=237) & 
    programStagename=="Antenatal care visit",num_31_33:=T]

xtabs(~z$num_31_33, addNA=T)



#########
# 35-37
#########
# denom
z[(status%in% c("SCHEDULE","SKIPPED") & 
      is.na(eventdate) &
      (gAatdueDate>=245 & gAatdueDate<=265)|
     (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
         eventdate>createddate) & (gAateventdate>=245 & gAateventdate<=265 &
                                     gAatcreateddate>0 & gAatcreateddate<245)) & 
    programStagename=="Antenatal care visit",denom_35_37:=T]


xtabs(~z$denom_35_37, addNA=T)

# numerator
# scheduled only
z[denom_35_37==T &
    (status %in% c("SCHEDULE","SKIPPED") &
       is.na(eventdate) &
       gAatdueDate>=245 & gAatdueDate<=265) & 
    programStagename=="Antenatal care visit",num_35_37:=F]


# those who attended
z[denom_35_37==T &
    (status %in% c("ACTIVE", "COMPLETED","VISITED") & 
     eventdate>createddate) & (gAateventdate>=245 & gAateventdate<=265 &
                                 gAatcreateddate>0 & gAatcreateddate<245) & 
    programStagename=="Antenatal care visit",num_35_37:=T]

xtabs(~z$num_35_37, addNA=T)


#####################
# Restrict to T2 DATA
#####################


# restrict data set to trial 2 data #
nrow(z)

y <- z[!is.na(TrialArm) &
         ((dueDate>="2019-12-01" &
             dueDate<="2020-03-22")|
            (eventdate>="2019-12-01" &
               eventdate<="2020-03-22"))]

length(unique(y$uniqueid))





attendance <- z[,.(
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
                keyby=.(TrialArm)]



if(IS_GAZA==F){
  
openxlsx::write.xlsx(attendance,
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "Outcomes",
                               "Attendance_eventsData.xlsx"))
  
 } else{
  
   openxlsx::write.xlsx(attendance,
                        file.path(FOLDER_DATA_RESULTS_GAZA,
                                  "T2",
                                  "Outcomes",
                                  "Attendance_eventsData.xlsx"))
  
}






### get trial arm for each of the rows

########## Attendance Tables ########## 

####### check merges
####### may have to use bookdate as second factor

# merge this w (wide) by uniqueid and bookdate into the entire dataset 
T2small <- d[bookdate>="2019-01-01"]
nrow(T2small)

wyy[,ident_schedev:=T]
nrow(wyy)

t2merged <- merge(T2small,
                   wyy,
                   by=c("uniqueid"),
                   all.x=T)
nrow(t2merged)
xtabs(~t2merged$ident_schedev, addNA=T)

t2merged[,bookT2Arm:=as.character(NA)]
t2merged[ident_TRIAL_2_3_Control==T,bookT2Arm:="control" ]
t2merged[ident_TRIAL_3==T & ident_TRIAL_2==F,bookT2Arm:="qidonly" ]
t2merged[ident_TRIAL_2==T & ident_TRIAL_3==F,bookT2Arm:="smsonly" ]
t2merged[ident_TRIAL_2==T & ident_TRIAL_3==T,bookT2Arm:="smsandqid" ]
xtabs(~t2merged$bookT2Arm, addNA = T)

t2merged <- t2merged[bookorgname!="ppconly"]

att <- t2merged[,.(
  "15-17 week numeratorT"=sum(num_15_17==T, na.rm=T),
  "15-17 week numeratorF"=sum(num_15_17==F, na.rm=T),
  "15-17 Denom"=sum(num_15_17==T |
                      num_15_17==F, na.rm=T),
  "18-22 week numeratorT"=sum(num_18_22==T, na.rm=T),
  "18-22 week numeratorF"=sum(num_18_22==F, na.rm=T),
  "18-22 Denom"=sum(num_18_22==T |
                      num_18_22==F, na.rm=T),
  
  "24-28 week numeratorT"=sum(num_24_28==T, na.rm=T),
  "24-28 week numeratorF"=sum(num_24_28==F, na.rm=T),
  "24-28 Denom"=sum(num_24_28==T |
                      num_24_28==F, na.rm=T),
  "31-33 week numeratorT"=sum(num_31_33==T, na.rm=T),
  "31-33 week numeratorF"=sum(num_31_33==F, na.rm=T),
  "31-33 Denom"=sum(num_31_33==T |
                      num_31_33==F, na.rm=T),
  "35-37 week numeratorT"=sum(num_35_37==T, na.rm=T),
  "35-37 week numeratorF"=sum(num_35_37==F, na.rm=T),
  "35-37 Denom"=sum(num_35_37==T |
                      num_35_37==F, na.rm=T)),
  keyby=.(bookT2Arm)]


openxlsx::write.xlsx(att,
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "Outcomes",
                               "Attendance_eventsData.xlsx"))


####
xtabs(~t2merged$num_15_17)
xtabs(~t2merged$num_18_22)
xtabs(~t2merged$num_24_28)
xtabs(~t2merged$num_31_33)
xtabs(~t2merged$num_35_37)



saveRDS(t2merged,file.path(FOLDER_DATA_CLEAN,"t2merged_eReg.rds"))
saveRDS(longcut,file.path(FOLDER_DATA_CLEAN,"t2_long.rds"))
saveRDS(eventsData,file.path(FOLDER_DATA_CLEAN,"eventsdata.rds"))
saveRDS(z,file.path(FOLDER_DATA_CLEAN,"mergedfile.rds"))
saveRDS(wyy,file.path(FOLDER_DATA_CLEAN,"eventswide.rds"))


############################################################
# maybe shoud merge in structural sheet to get clusternum, etc
############################################################

T2 <- readRDS(file.path(FOLDER_DATA_CLEAN,
                        "T2_clean",
                        "WB",
                        "T2_dataset_2020-12-19_WB.rds"))


# compare uniqueids
length(unique(T2$uniqueid) %in% unique(z$uniqueid))
nrow(T2)
nrow(z)



