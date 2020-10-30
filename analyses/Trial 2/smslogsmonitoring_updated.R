###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

##### 
# global options -->  unticked cleanup output after successful... 
# and ticked View Rcheck
# schedevents events scheduled
# msglogs= messages sent

if(IS_GAZA==F){
  
  # read in logs #
  
  # all scheduled events
  # setting header=FALSE bc doesnt have column names, so specifying them ourselves and dont want to read in first row of data as
  schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-09-29.csv", header=FALSE)
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-09-22.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/data_raw/smslogs/dhis2allmsgssent/2020-09-22.csv")
  
  ###### Load in Data Set ###### 
  d <- LoadDataFileFromNetwork()
  
  fileTag <- "WB"
  
  
  
} else {
  
  
  # put gazas stuff here
  fileTag <- "GAZA"
  
 
  # all scheduled events
  schedeventsraw <- fread("C:/data processing/gaza_data_raw/smslogs/schedevents/2020-08-25.csv", header=FALSE)
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/gaza_data_raw/smslogs/dhis2allschedmsgs/2020-08-25.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/gaza_data_raw/smslogs/dhis2allmsgssent/2020-08-25.csv")
  
  # Load in data from network
  d <- LoadDataFileFromNetworkGaza()
  
  
}

############ Trial 2 SMS Monitoring ###############

varsancevent <- names(d)[stringr::str_detect(names(d),"^anevent")]
varsancdate <- names(d)[stringr::str_detect(names(d),"^andate")]
varsancgestage <- names(d)[stringr::str_detect(names(d),"^angestage")]


# only adding bookdate here to decrease size of the data


# check these definitions
t2 <- d[bookdate>="2019-12-03",
        c("uniqueid",
          "bookevent",
          "bookorgname",
          "bookdate",
          "bookgestage",
          "ident_dhis2_booking",
          "ident_TRIAL_2_and_3",
          "ident_TRIAL_2_3_Control",
          "ident_TRIAL_2",
          "ident_TRIAL_3",
          "str_TRIAL_2_Cluster",
          "lmpT1",
          "areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits",
          varsancevent,
          varsancdate,
          varsancgestage), with=F]


setnames(t2,
      "areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits",
      "wantSMS")

t2 <-t2[,anevent_0:=bookevent]
t2 <-t2[,andate_0:=bookdate]

# trial arms
t2[ident_TRIAL_2_3_Control==T,TrialArm:="Control"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==F,TrialArm:="SMS only"]

t2[ident_TRIAL_2==F & 
     ident_TRIAL_3==T,TrialArm:="QID only"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==T,TrialArm:="SMS and QID"]



t2[,precovid:=as.logical(NA)]
t2[bookdate>="2019-12-01" &
     bookdate<="2020-03-30",bookprecovid:=TRUE]
t2[bookdate>="2020-06-22", bookprecovid:=FALSE]
nrow(t2)

t2 <- t2[,c("uniqueid",
            "bookevent",
            "bookorgname",
            "bookdate",
            "bookgestage",
            "bookprecovid",
            "lmpT1",
            "anevent_0",
            "andate_0",
            "ident_dhis2_booking",
            "str_TRIAL_2_Cluster",
            "TrialArm",
            "wantSMS",
            varsancevent,
            varsancdate,
            varsancgestage), with=F]



nrow(t2)


######################### ATTENDANCE ######################### 

t2[,andate_0:=bookdate]
t2[,angestage_0:=bookgestage]

# id maximum amount of things
t2[,firstvisitinT2:=as.numeric(NA)]


temp <- stringr::str_subset(names(t2),"^andate_")

# -1 because have 22 dates and have a 0 in there
for(i in 0:(length(temp)-1)){
  
  datevar <- paste0("andate_",i)
  
  gestagevar <- paste0("angestage_",i)
  
  t2[is.na(firstvisitinT2) & 
           get(datevar)>="2019-12-01",firstvisitinT2:=i]
  
  # add other limits like in the precovid stuff
  
  
}

for(i in 0:(length(temp)-1)){
  
  datevar <- paste0("andate_",i)
  
  gestagevar <- paste0("angestage_",i)
  
  outcomevar <- paste0("anT2gestagedays_",i)
  
  t2[i>=firstvisitinT2,(outcomevar):=as.numeric(floor(difftime(get(datevar),
                                                               lmpT1, 
                                                               units="days")))]
  
}

xtabs(~TrialArm + firstvisitinT2, data=t2, addNA=T)



################ t2 ends here ################ 

#with=F looks inside the vars and get the values

##### import scheduled events #####
schedeventsraw[1:3]
setnames(schedeventsraw,1,"event_id")
setnames(schedeventsraw,2,"enrollmentid")
setnames(schedeventsraw,3,"programStage")
setnames(schedeventsraw,4,"enrollmentdate")
setnames(schedeventsraw,5,"incidentdate")
setnames(schedeventsraw,6,"eventdate")
setnames(schedeventsraw,7,"dueDate")
setnames(schedeventsraw,8,"completeddate")
setnames(schedeventsraw,9,"enrollmentstatus")
setnames(schedeventsraw,10,"eventstatus")
setnames(schedeventsraw,11,"orgUnit")
setnames(schedeventsraw,12,"orgname")
setnames(schedeventsraw,13,"orgCode")
setnames(schedeventsraw,14,"uniqueid")


schedevents <- schedeventsraw[,c(1:14)]

names(schedevents)

### cleaning var types

#eventdate
schedevents[,enrollmentdate:=stringr::str_sub(enrollmentdate,1,10)]
schedevents[,enrollmentdate:=as.Date(enrollmentdate)]
xtabs(~schedevents$enrollmentdate)

#incidentdate
schedevents[,incidentdate:=stringr::str_sub(incidentdate,1,10)]
schedevents[,incidentdate:=as.Date(incidentdate)]
xtabs(~schedevents$incidentdate)

#eventdate
schedevents[,eventdate:=stringr::str_sub(eventdate,1,10)]
schedevents[,eventdate:=as.Date(eventdate)]
xtabs(~schedevents$eventdate)

#completeddate
schedevents[,completeddate:=stringr::str_sub(completeddate,1,10)]
schedevents[,completeddate:=as.Date(completeddate)]
xtabs(~schedevents$completeddate)


#dueDate
schedevents[,dueDate:=stringr::str_sub(dueDate,1,10)]
schedevents[,dueDate:=as.Date(dueDate)]
xtabs(~schedevents$dueDate)

# cleaning orgname
schedevents[,orgname:=ExtractOnlyEnglishLetters(orgname)]
xtabs(~schedevents$orgname)

# eventnum
schedevents[,eventnum:=1:.N, by=uniqueid]
xtabs(~schedevents$eventnum)

setorder(schedevents,enrollmentid,uniqueid,event_id,eventdate)

# identifying that these are all scheduled events
schedevents[,ident_schedevent:=T]

nrow(schedevents)


#### scheduled message logs ####

msglogsraw[1]

#need to know the difference between the date variables
#name the first one something so they arent missing

msglogs <- setnames(msglogsraw, c("msg_name",
                    "msg_generated",
                    "msg_scheduledFor",
                    "msg_id",
                    "creater_name",
                    "creater_userid",
                    "event_createdat",
                    "event_id"),
         c("msgname",
           "datesmsgenerated",
           "datesmsschedfor",
           "msgid",
           "creatername",
           "createruserid",
           "datesmssched",
           "event_id"))

msglogs[1]

nrow(msglogs)

unique(msglogs$msgname)
unique(msglogs$event_id)

#cleaning var types
msglogs[,datesmssched:=stringr::str_sub(datesmssched,1,10)]
msglogs[,datesmssched:=as.Date(datesmssched)]
xtabs(~msglogs$datesmssched)

msglogs[,datesmsschedfor:=stringr::str_sub(datesmsschedfor,1,10)]
msglogs[,datesmsschedfor:=as.Date(datesmsschedfor)]
xtabs(~msglogs$datesmsschedfor)

msglogs[,datesmsgenerated:=stringr::str_sub(datesmsgenerated,1,10)]
msglogs[,datesmsgenerated:=as.Date(datesmsgenerated)]
xtabs(~msglogs$datesmsgenerated)

msglogs[,datesmsgeneratedmmyy:=format(as.Date(datesmsgenerated,
                                              "%Y-%m-%d"), "%Y-%m")] 
xtabs(~msglogs$datesmsgeneratedmmyy)

nrow(msglogs)

####### remove duplicate messages #######

setorder(msglogs,event_id,datesmssched)
msglogs[,firstdatesent:=min(datesmssched),by="event_id"]

#date of previous one sent
# changed datesmssched to datesmsschedfor 
msglogs[,prevdatesent:=shift(datesmssched), by="event_id"]

#do same for message id as above so we can compare them later
msglogs[,prevmsgname:=shift(msgname), by="event_id"]

#m
msglogs[,timesincelastmsg:=as.numeric(difftime(datesmssched, 
                                               prevdatesent,units="days"))]
xtabs(~msglogs$timesincelastmsg, addNA=T)

#get rid of duplicates

msglogs<- msglogs[is.na(timesincelastmsg)|
                     timesincelastmsg!=0 |
                     (timesincelastmsg==0 & prevmsgname!=msgname)]
#when we get rid of duplicates, we end up losing some important messages
#so, maybe we should include the name of the template instead?

nrow(msglogs[msgname!=prevmsgname |is.na(prevmsgname)])
sort(unique(msglogs$msgname))
sort(unique(msglogsraw$msg_name))

xtabs(~msglogs$msgname, addNA=T)
xtabs(~msglogsraw$msgname, addNA=T)

sort(unique(msglogsraw$msg_name))

#msglogsclean <- msglogs[msgname!=prevmsgname |is.na(prevmsgname)]


names(msglogs)
xtabs(~msglogs$timesincelastmsg,addNA=T)

#want to decast to wide to say if its first message, second message
msglogs[,msgnumber:=1:.N, by="event_id"]
xtabs(~msglogs$msgnumber, addNA=T)



nrow(msglogs)
times <-msglogs[,c("msgname",
                   "datesmsgenerated",
                   "datesmsschedfor")
                ]
# creating new variable for msgtype
msglogs[,msgtype:=as.character(NA)]

# one week reminder
msglogs[stringr::str_detect(msgname,"^SMS: [0-9][0-9]_1_"), msgtype:= "one_week_reminder"]

msglogs[stringr::str_detect(msgname,"^SMS: [0-9][0-9]-[0-9][0-9]_1_"), 
        msgtype:= "one_week_reminder"]


# 3 day reminder
msglogs[stringr::str_detect(msgname,"^SMS: [0-9][0-9]_3_"),
        msgtype:= "3_day_reminder"]

msglogs[stringr::str_detect(msgname,"^SMS: [0-9][0-9]-[0-9][0-9]_3_"), 
        msgtype:= "3_day_reminder"]



# missed appt
msglogs[stringr::str_detect(msgname,"MISSEDAppt_[0-9]$"), msgtype:= "Missed_Appt"]

# recap
msglogs[stringr::str_detect(msgname,"Recapture$"), msgtype:= "Recap"]

# not sure about this one
msglogs[stringr::str_detect(msgname,"^SMS: 24$"), msgtype:= "24-hour reminder"]


xtabs(~msglogs$msgtype, addNA=T)

msglogs[,ident_schedsms:=T]



### variable for timing of scheduling
msglogs[,msgschedb4visit:=as.logical(NA)]
msglogs[!is.na(datesmssched) & !is.na(datesmsschedfor), msgschedb4visit:=FALSE]

msglogs[,timeschedb4visit:= as.numeric(NA)]
msglogs[,timeschedb4visit:=difftime(datesmsschedfor,datesmssched,units = "days")]
xtabs(~msglogs$timeschedb4visit)

msglogs[msgschedb4visit==F & timeschedb4visit>0 , msgschedb4visit:=TRUE]

xtabs(~msglogs$msgschedb4visit, addNA=T)



########### dhis2 messages sent ###########
setnames(allsmssentraw,4,"phone")


allsmssent <- allsmssentraw[,processed:=stringr::str_sub(processed,1,10)]
allsmssent[,processed:=as.Date(processed)]

# sms sent variable
allsmssent[,ident_sms_sent:=T]

############ enrollment data ############

# check so we dont lose anyone
length(unique(t2$uniqueid))

# first reshape wide data and want each variable to be a heading because want to do analysis by event

long <- melt(t2, id.vars = c("uniqueid", 
                             "bookevent",
                             "TrialArm",
                             "bookorgname",
                             "bookdate",
                             "bookprecovid",
                             "ident_dhis2_booking",
                             "str_TRIAL_2_Cluster"),
             measure = patterns("^andate_", 
                                "^anevent_",
                                "^angestage",
                                "wantSMS"
                                ), 
             value.name = c("andate", 
                            "anevent",
                            "angestage",
                            "wantSMS"))


length(unique(long$uniqueid))


######## END enrollment data ######## 

nrow(schedevents)
t2schedev <- merge(schedevents,
                   t2,
                   by="uniqueid")

nrow(t2schedev)


##### merge these with msglogs to get template

t2schedevsms <- merge(t2schedev,
                      msglogs,
                      by="event_id", all=T)

nrow(t2schedevsms)


allsmssentcut <- allsmssent[processed>="2019-11-01"]
allsmssentcut[,numevents:=1:.N, by="event_id"]
xtabs(~allsmssentcut$numevents)



setorder(allsmssentcut, event_id,processed)

#date of previous one sent
# changed datesmssched to datesmsschedfor 
allsmssentcut[,prevdateproc:=shift(processed), by="event_id"]

#do same for message id as above so we can compare them later
allsmssentcut[,prevmsgname:=shift(msg_text), by="event_id"]

nrow(allsmssentcut[prevmsgname==msg_text & (prevdateproc==processed)])





t2schedevsmssent <- merge(t2schedevsms,
                          allsmssentcut,
                          by="event_id", all=T)


sentsched <- merge(msglogs,
                   allsmssent,
                   by="event_id", all=Y)






################ schedevents data only ################ 
###### use this when want to find sched events only

sData <- as.data.table(readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx"))

# trial arms
sData[,TrialArm:=as.character(NA)]
sData[ident_TRIAL_2_3_Control=="Y" & 
        ident_TRIAL_2_and_3=="Y" &
        is.na(ident_hr_clinic), TrialArm:="Control"]

sData[ident_TRIAL_2=="Y" & 
        ident_TRIAL_2_and_3=="Y" &
        is.na(ident_TRIAL_3),TrialArm:="SMS only"]

sData[is.na(ident_TRIAL_2) &
        ident_TRIAL_2_and_3=="Y" &
        ident_TRIAL_3=="Y",TrialArm:="QID only"]

sData[ident_TRIAL_2=="Y" & 
        ident_TRIAL_3=="Y" &
        ident_TRIAL_2_and_3=="Y",TrialArm:="SMS and QID"]

sData <- sData[, c("bookorgname",
                   "NEW_bookorgname",
                   "bookorgdistrict",
                   "str_TRIAL_2_Cluster",
                   "str_TRIAL_2_ClusSize",
                   "TrialArm")]


schedevents[,bookorgname:=orgname]

# nrow before merge
nrow(schedevents)

sData_schedevents <- merge(schedevents,
                     sData,
                     by="bookorgname", all.x=T)

# nrow after merge
nrow(sData_schedevents)

sData_schedevents[is.na(schedev_precovid),schedev_precovid:="NA"]

sData_schedevents[is.na(orgname),orgname:="NA"]

xtabs(~sData_schedevents$TrialArm, addNA=T)

xtabs(~sData_schedevents$orgname, addNA=T)



sData_schedevents[,schedev_precovid:=as.logical(NA)]
sData_schedevents[eventdate>="2019-12-01" &
                    eventdate<="2020-03-22",schedev_precovid:=TRUE]
sData_schedevents[eventdate>="2020-06-01",schedev_precovid:=FALSE]

sData_schedevents[is.na(TrialArm), TrialArm:="NA"]
xtabs(~sData_schedevents$schedev_precovid, addNA=T)

xtabs(~sData_schedevents$schedev_precovid, addNA=T)



taball <- sData_schedevents[eventdate>="2019-12-01" & 
                               ((eventstatus=="SCHEDULE" & 
                              dueDate>eventdate) |
                            (eventstatus=="SCHEDULE" &
                               is.na(eventdate) &  
                               dueDate>="2019-12-01")),.(
                              NumSchedEvents=sum(!is.na(event_id))),
                            keyby=.(TrialArm,orgname,schedev_precovid)]


taball2 <-sData_schedevents[eventdate>="2019-12-02" &
                              ((eventstatus=="ACTIVE" | 
                               eventstatus=="COMPLETED") &
                               dueDate >eventdate),.(
                                 NumSchedEvents=sum(!is.na(event_id))),
                            keyby=.(TrialArm,orgname,schedev_precovid)]

taball3 <-sData_schedevents[eventdate>="2019-12-02" &
                              ((eventstatus=="ACTIVE" | 
                                  eventstatus=="COMPLETED") &
                                 dueDate==eventdate),.(
                                   NumSchedEvents=sum(!is.na(event_id))),
                            keyby=.(TrialArm,orgname,schedev_precovid)]

IF(IS_GAZA==F){
  
  openxlsx::write.xlsx(taball, file.path(FOLDER_DATA_RESULTS,
                                      "T2",
                                      "monitoring",
                                      sprintf("%s_SchedEventsONLY_by_Arm.xlsx",
                                              lubridate::today())))
  
  openxlsx::write.xlsx(taball2, file.path(FOLDER_DATA_RESULTS,
                                      "T2",
                                      "monitoring",
                                      sprintf("%s_Complete&Active_by_Arm.xlsx",
                                              lubridate::today())))
  
  openxlsx::write.xlsx(taball3, file.path(FOLDER_DATA_RESULTS,
                                          "T2",
                                          "monitoring",
                       sprintf("%s_Complete&ActiveEvDate=dueDate_by_Arm.xlsx",
                                                  lubridate::today())))
} else {

  openxlsx::write.xlsx(taball, file.path(FOLDER_DATA_RESULTS_GAZA,
                                               "T2",
                                               "monitoring",
                                  sprintf("%s_SchedEventsONLY_by_Arm.xlsx",
                                                       lubridate::today())))
  
  openxlsx::write.xlsx(taball2, file.path(FOLDER_DATA_RESULTS_GAZA,
                                         "T2",
                                         "monitoring",
                                  sprintf("%s_Complete&Active_by_Arm.xlsx",
                                                 lubridate::today())))
  
  openxlsx::write.xlsx(taball3, file.path(FOLDER_DATA_RESULTS_GAZA,
                                          "T2",
                                          "monitoring",
                        sprintf("%s_Complete&ActiveEvdate=duedate_by_Arm.xlsx",
                                                  lubridate::today())))
  
  
}







# no high risk clinics
tab <- sData_schedevents[(eventstatus=="SCHEDULE" & 
                      (dueDate>=eventdate))|
                     ((eventstatus=="ACTIVE" | eventstatus=="COMPLETED") &
                        dueDate == eventdate),.(
  NumSchedEvents=sum(!is.na(event_id))),
  keyby=.(TrialArm,orgname,schedev_precovid)]


IF(IS_GAZA==F){
  openxlsx::write.xlsx(tab, file.path(FOLDER_DATA_RESULTS,
                                      "T2",
                                      "monitoring",
                                      sprintf("%s_SchedEvents_by_Arm.xlsx",
                                              lubridate::today())))
} else {
  
  
  openxlsx::write.xlsx(tab, file.path(FOLDER_DATA_RESULTS_GAZA,
                                      "T2",
                                      "monitoring",
                                      sprintf("%s_SchedEvents_by_Arm.xlsx",
                                              lubridate::today())))
  
}


tabbyorgunit <- sData_schedevents[eventstatus=="SCHEDULE",.(
  NumSchedEvents=sum(!is.na(event_id))),
  keyby=.(TrialArm,orgname,schedev_precovid)]


IF(IS_GAZA==F){
  openxlsx::write.xlsx(tabbyorgunit, file.path(FOLDER_DATA_RESULTS,
                                               "T2",
                                               "monitoring",
                                               sprintf("%s_SchedEvents_by_orgunit.xlsx",
                                                       lubridate::today())))
} else {
  
  
  openxlsx::write.xlsx(tabbyorgunit, file.path(FOLDER_DATA_RESULTS_GAZA,
                                               "T2",
                                               "monitoring",
                                               sprintf("%s_SchedEvents_by_orgunit.xlsx",
                                                       lubridate::today())))
  
}

tabbyCluster1 <- sData_schedevents[eventstatus=="SCHEDULE",.(
  NumSchedEvents=sum(!is.na(event_id))),
  keyby=.(TrialArm,str_TRIAL_2_Cluster,orgname,schedev_precovid)]

tabbyCluster <- sData_schedevents[evenstatus=="SCHEDULE",.(
  NumSchedEvents=sum(!is.na(event_id))),
  keyby=.(TrialArm,str_TRIAL_2_Cluster,schedev_precovid)]

IF(IS_GAZA==F){
  openxlsx::write.xlsx(tabbyCluster, file.path(FOLDER_DATA_RESULTS,
                                               "T2",
                                               "monitoring",
                                               sprintf("%s_SchedEvents_by_Cluster.xlsx",
                                                       lubridate::today())))
} else {
  
  
  openxlsx::write.xlsx(tabbyCluster, file.path(FOLDER_DATA_RESULTS_GAZA,
                                               "T2",
                                               "monitoring",
                                               sprintf("%s_SchedEvents_by_Cluster.xlsx",
                                                       lubridate::today())))
  
}

library(tidyverse)
library(dplyr)

sched_sent <- msglogs %>% 
  dplyr::left_join(allsmssent, by=c("event_id"="event_id")) %>%
  filter(((processed>="2019-12-01" & 
          processed<="2020-03-22") |
            processed>="2020-06-22"))
setDT(sched_sent)

sched_sent <- msglogs %>% 
  dplyr::left_join(allsmssent, by="event_id")






######### schedevents with msglogs

sData_schedevsms <- merge(sData_schedevents,
                          msglogs,
                          by="event_id", all=T)

sData_schedevsms[,number.x:=NULL]
sData_schedevents[,number.y:=NULL]

schevsmssent <- merge(sData_schedevsms,
                      allsmssent,
                      by="event_id",allow.cartesian = T, all=T)
















########## schedevents with sms sent ########## 

t2_schedev <- merge(schedevents,
                    t2,
                    by="uniqueid", all.x=T)

t2_schedev[,bookorgname.y:=NULL]
setnames(t2_schedev,bookorgname.x,bookorgname]





########## COMBINING SMALLER DATA SETS TO GET ONES WE WANT ########## 

# merge schedevents with sched sms to get templates that we want
schedev_schedsms <- merge(schedevents,
                          msglogs,
                          by="event_id", all=T)
length(unique(schedev_schedsms$uniqueid))


# getting an idea of the numbers
xtabs(~ident_schedsms + ident_schedevent, data=schedev_schedsms, addNA=T)

xtabs(~msgtype +
        datesmsgeneratedmmyy, 
      data=schedev_schedsms[datesmsschedfor>="2019-12-01"], addNA=T)

# num schedev by msgtype
schedevmsgtype <- schedev_schedsms[datesmsschedfor>="2019-12-01",.(
                                    N=.N),
                                   keyby=.(schedev_orgname,
                                           msgtype,
                                           datesmsgeneratedmmyy)]
openxlsx::write.xlsx(schedevmsgtype, 
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "monitoring",
                               sprintf(
                                 "scheduled_sms_events_smsgeneratedandmsglogs_%s_%s.xlsx",
                                 fileTag,lubridate::today())))


xtabs(~uniqueid+anstatus, data=schedev_schedsms[is.na(ident_schedevent) & 
                                                 ident_schedsms==T],addNA=T)


xtabs(~schedev_schedsms$schedev_dateapptcreatedyyyymm, addNA=T)

xtabs(~schedev_schedsms$datesmsgenerated, addNA=T)

xtabs(~datesmsgenerated+schedev_orgname, data=schedev_schedsms)

xtabs(~datesmsschedfor+schedev_orgname, data=schedev_schedsms[datesmsschedfor>="2019-12-01"])


##########################################################################
########### Merge booking data with events and scheduled smses ########### 

# merging scheduled events and scheduled messages with bookings
schedev_schedsms_bookings <- merge(schedev_schedsms,
                                   long,
                                   by="uniqueid", all=T)

xtabs(~ident_dhis2_booking + ident_schedevent, 
                    data=schedev_schedsms_bookings, addNA=T)

xtabs(~ident_dhis2_booking + ident_schedsms, 
      data=schedev_schedsms_bookings, addNA=T)

length(unique(schedev_schedsms_bookings[ident_dhis2_booking==T]$uniqueid))
length(schedev_schedsms_bookings[wantSMS==1 & ident_dhis2_booking==1]$uniqueid)
nrow(t2[ident_dhis2_booking==T & wantSMS==0])

#############################################################################


##########################################################################
########### Merge all with SENT ########### 

# merging scheduled events and scheduled messages with bookings
# only merge with stuff from the trial

schedev_schedsms_bookings_sent <- merge(schedev_schedsms_bookings,
                                   allsmssent,
                                   by="event_id")

length(unique(schedev_schedsms_bookings$uniqueid))


schedbooksent <-merge(schedev_schedsms_bookings,
                      allsmssent,
                      by="event_id", all=T)
length(unique(schedbooksent$uniqueid))



schedev_schedsms_sent <- merge(allsmssent,
                               schedev_schedsms,
                               by="event_id",all=T)

msglogs_sent <- merge(allsmssent,
                      msglogs, 
                      by="event_id", all=T, allow.cartesian = T)

schedev_sent <- merge(allsmssent,
                      schedevents, 
                      by="event_id", all=T)



####### Recruitment ####### 
#= scheduled messages with enrollment date
#Among the women booked December to March and July-August, how many 1) 1-week messages were scheduled 2) how many recapture messages were scheduled?"


setDT(schedev_schedsms_bookings_sent)

xtabs(~schedev_schedsms_bookings_sent$ident_sms_sent, addNA=TRUE)

sentbyorg <- schedev_schedsms_bookings_sent[status=="SENT",.(
                                                N=.N),
                                            keyby=.(schedev_orgname,
                                                    bookorgname)]




#################### Data with restrictions ############################

schedev_schedsms_bookings_sent <- schedev_schedsms_bookings_sent[bookdate>="2019-12-01" &ident_dhis2_booking==T,]


schedev_schedsms_bookings_sent[,smsbyuniqueid:=1:.N, by="uniqueid"]
schedev_schedsms_bookings[,smsbyuniqueid:=1:.N, by="uniqueid"]


# use the one without the sent because the one with out the sent only includes stuff that has been matched
smsrecruitment <- schedev_schedsms_bookings[,.(
                numBooked=length(unique(uniqueid)),
                wantsms=sum(wantSMS==1 & smsbyuniqueid==1, na.rm=T),
                noSMS=sum(wantSMS==0 & smsbyuniqueid==1, na.rm=T)),
            keyby=.(TrialArm,bookorgname, precovid)]

openxlsx::write.xlsx(smsrecruitment, 
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "monitoring",
                               sprintf(
                                "SMS_recruitment_from_Data_Dec_2019_%s_%s.xlsx",
                                 fileTag,lubridate::today())))





####### Message scheduled by type and arm from sent messages ####### 

# data set already defined to people who have booked at 

basic <- schedbooksent[,.(
  oneweekreminder=sum(msgtype=="one_week_reminder", na.rm=T),
  oneweekremindersent=sum(msgtype=="one_week_reminder" &
                            ident_sms_sent==T, na.rm=T),
  onedayreminder=sum(msgtype=="24-hour reminder", na.rm=T),
  onedayremindersent=sum(msgtype=="24-hour reminder" &
                           ident_sms_sent==T, na.rm=T),
  recap=sum(msgtype=="Recap", na.rm=T),
  recapsent=sum(msgtype=="Recap" &
                  ident_sms_sent==T, na.rm=T),
  threedayreminder=sum(msgtype=="3_day_reminder",na.rm=T),
  threedayremindersent=sum(msgtype=="3_day_reminder" &
                             ident_sms_sent==T,na.rm=T),
  missedAppt=sum(msgtype=="Missed_Appt", na.rm=T),
  missedApptsent=sum(msgtype=="Missed_Appt" &
                       ident_sms_sent==T, na.rm=T)
  ),
  keyby=.(TrialArm,
          str_TRIAL_2_Cluster,
          schedev_orgname,
          bookorgname,
          datesmsgeneratedmmyy,
          precovid)]


if(IS_GAZA==F){
  openxlsx::write.xlsx(basic, 
                       file.path(FOLDER_DATA_RESULTS,
                                 "T2",
                                 "monitoring",
                                 sprintf(
                                   "scheduled_sms_byarm__%s_%s.xlsx",
                                   fileTag,lubridate::today())))
} else {
  openxlsx::write.xlsx(basic, 
                       file.path(FOLDER_DATA_RESULTS_GAZA,
                                 "T2",
                                 "monitoring",
                                 sprintf(
                                   "scheduled_sms_byarm__%s_%s.xlsx",
                                   fileTag,lubridate::today())))
  
  
}


############ general numbers############ 
nrow(schedev_sent)
nrow(allsmssent)
length(unique(allsmssent$event_id))
length(allsmssent$event_id)
length(unique(msglogs$event_id))
length(msglogs$event_id)
length(unique(schedevents$event_id))
length(schedevents$event_id)
length(schedevents$uniqueid)
length(unique(schedevents$uniqueid))


########## after merge ########## 
# merge sched events with msg logs
nrow(schedev_schedsms)
length(unique(schedev_schedsms$event_id))
length(unique(schedev_schedsms$uniqueid))
nrow(schedev_schedsms[ident_schedevent==T])
nrow(schedev_schedsms[ident_schedsms==T])
nrow(schedev_schedsms[ident_schedevent==T & ident_schedsms==T])



