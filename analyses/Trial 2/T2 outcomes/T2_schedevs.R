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
  schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-10-07.csv", header=FALSE)
  
  
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

d[,andate_0:=bookdate]
d[,angestage_0:=bookgestage]
d[,anevent_0:=bookevent]

varsancevent <- names(d)[stringr::str_detect(names(d),"^anevent")]
varsancdate <- names(d)[stringr::str_detect(names(d),"^andate")]
varsancgestage <- names(d)[stringr::str_detect(names(d),"^angestage")]


# only adding bookdate here to decrease size of the data


# check these definitions
t2 <- d[,
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
            "ident_dhis2_booking",
            "str_TRIAL_2_Cluster",
            "TrialArm",
            "wantSMS",
            varsancevent,
            varsancdate,
            varsancgestage), with=F]



nrow(t2)


#test <- t2[1:2]
# reshape dataset to long so we can merge with events data

long <- melt(t2,
            id.vars=c("uniqueid",
                                 "bookevent",
                                 "bookorgname",
                                 "bookdate",
                                 "bookgestage",
                                 "bookprecovid",
                                 "lmpT1",
                                 "ident_dhis2_booking",
                                 "str_TRIAL_2_Cluster",
                                 "TrialArm",
                                 "wantSMS"),
            measure.vars=patterns("^anevent_",
                                   "^andate_",
                                  "^angestage_"), 
            value.name=c("anevent",
                         "andate",
                         "angestage"))

nrow(long)==length(unique(long$anevent))

# manually looked into two rows and saw that bc this is dt, all missing values from measure vars were being included. 
#need to remove them from here

long <- long[!is.na(anevent)]
nrow(long)==length(unique(long$anevent))

long[,variable:=NULL]


# anything not in measure vars or values vars will be set to the id vars
# newlong <- melt(t2, measure = patterns("^anevent_", "^andate_", "^angestage_"),
# value.name = c("anevent", "andate","angestage"))[, variable := NULL][]


setnames(long, "anevent","event")


# tag our data 
long[,ident_dhis2:=T]
long[bookevent!=event, ident_visit:="ANC event"]
long[bookevent==event, ident_visit:="Booking event"]

# identifying if event happened in time frame we want
long[,T2event:=as.logical(NA)]
long[bookdate>="2019-12-01" | andate>="2019-12-01",T2event:=TRUE]
long[bookdate<="2019-12-01" | andate<="2019-12-01", T2event:=FALSE]


######### sched events ######### 
#with=F looks inside the vars and get the values
schedeventsraw[1:3]

# based on sql query, these are the names
setnames(schedeventsraw,1,"event")
setnames(schedeventsraw,2,"status")
setnames(schedeventsraw,3,"dueDate")
setnames(schedeventsraw,4,"createddate")
setnames(schedeventsraw,5,"enrollmentid")
setnames(schedeventsraw,6,"programStagename")
setnames(schedeventsraw,7,"programstageid")
setnames(schedeventsraw,8,"eventdate")
setnames(schedeventsraw,9,"orgunitcode")
setnames(schedeventsraw,10,"orgname")
setnames(schedeventsraw,11,"completeddate")

schedevents <- schedeventsraw

### cleaning var types

#duedate
schedevents[,dueDate:=stringr::str_sub(dueDate,1,10)]
schedevents[,dueDate:=as.Date(dueDate)]
xtabs(~schedevents$dueDate)


#createddate
schedevents[,createddate:=stringr::str_sub(createddate,1,10)]
schedevents[,createddate:=as.Date(createddate)]
xtabs(~schedevents$createddate)

#eventdate
schedevents[,eventdate:=stringr::str_sub(eventdate,1,10)]
schedevents[,eventdate:=as.Date(eventdate)]
xtabs(~schedevents$eventdate)

#completeddate
schedevents[,completeddate:=stringr::str_sub(completeddate,1,10)]
schedevents[completeddate=="", completeddate:=NA]
schedevents[,completeddate:=as.Date(completeddate, formmat="%Y-%m-%d")]

xtabs(~schedevents$completeddate, addNA=T)



# cleaning orgname
schedevents[,orgname:=ExtractOnlyEnglishLetters(orgname)]
xtabs(~schedevents$orgname)

# eventnum
schedevents[,eventnum:=1:.N, by=tei]
xtabs(~schedevents$eventnum)

setorder(schedevents,tei,eventdate)

# identifying that these are all scheduled events
schedevents[,ident_schedevent:=T]

nrow(schedevents)


############ merge sData with the schedevents

sData <- readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx") 
setDT(sData)

sData <- sData[,c("bookorgname",
                  "sched_NEW_bookorgname",
                  "sched_bookorgdistrict",
                  "sched_ident_TRIAL_2",
                  "Sched_ident_TRIAL_2_and_3",
                  "sched_ident_TRIAL_3",
                  "sched_ident_TRIAL_2_3_Control",
                  "sched_str_TRIAL_2_Cluster")]

setnames(sData,"bookorgname","orgname")

nrow(schedevents)

test <- merge(schedevents,
              sData,
              by="orgname",
              all.x=T)

nrow(test)


xtabs(~test$ident_TRIAL_2)

test[ident_TRIAL_2_3_Control=="Y", sched_TrialArm:="Control"]

test[ident_TRIAL_2=="Y" & 
     is.na(ident_TRIAL_3),sched_TrialArm:="SMS only"]

test[is.na(ident_TRIAL_2) & 
     ident_TRIAL_3=="Y",sched_TrialArm:="QID only"]

test[ident_TRIAL_2=="Y" & 
     ident_TRIAL_3=="Y",sched_TrialArm:="SMS and QID"]

xtabs(~test$sched_TrialArm, addNA=T)

#################################################################################
# merge the two data sets
xtabs(~test$ident_schedevent, addNA=T)
nrow(schedevents)

merged <- merge(test,
                long,
                by="event",
                all=T)
nrow(merged)

write.csv(merged,file=sprintf("%s_T2_events_and_dhis2_data.csv",
                              lubridate::today()), fileEncoding = "UTF-8")


# identify duplicate bookevents
merged[,eventnumbyevent:=0]
merged[,eventnumbyevent:=.N,by=event]

length(unique(merged$event))==nrow(merged)


# put all this in an excel sheet
nrow(merged)
nrow(schedevents)
nrow(merged[ident_dhis2==T & ident_schedevent==T])
nrow(merged[ident_schedevent==T])

gennums <- merged[,.(SQLonly=sum(ident_schedevent==T,na.rm=T),
                     Merged=sum(ident_schedevent==T &
                                  ident_dhis2==T,na.rm=T),
                     Notmerged=sum(is.na(ident_dhis2)),
                     AllSQLsched_trialarmnotmissing=sum(ident_schedevent==T &
                                                        !is.na(sched_TrialArm)),
                     Allmerged_sched_Trialarm=sum(ident_dhis2==T &
                                                    ident_schedevent==T &
                                                    !is.na(sched_TrialArm),
                                                  na.rm=T),
                     AllSQLdhis_trialarmnotmissing=sum(ident_schedevent==T &
                                                          !is.na(TrialArm),
                                                       na.rm=T),
                     Allmerged_dhis_Trialarm=sum(ident_dhis2==T &
                                                   ident_schedevent==T &
                                                    !is.na(TrialArm),na.rm=T),
                     SQLdaterestrictiononly=sum(ident_schedevent==T &
                                              eventdate>="2019-12-01" &
                                              eventdate<="2020-03-22",na.rm=T),
                     Mergeddaterestriction=sum(ident_schedevent==T &
                                  ident_dhis2==T &
                                    eventdate>="2019-12-01" &
                                    eventdate<="2020-03-22",na.rm=T),
                     SQLdaterestrictionandT2schedarm=sum(ident_schedevent==T &
                                              !is.na(sched_TrialArm) &
                                              eventdate>="2019-12-01" &
                                              eventdate<="2020-03-22",na.rm=T),
                     MergeddaterestrictionandT2tarm=sum(ident_schedevent==T &
                                                 ident_dhis2==T &
                                                  !is.na(TrialArm) &
                                                 eventdate>="2019-12-01" &
                                                 eventdate<="2020-03-22",na.rm=T) )]

openxlsx::write.xlsx(gennums,
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "data",
                               sprintf("%s_Baseline_data_per_source.xlsx",
                                       lubridate::today())))

###### scheduled events data only

schedevonly <- merged[ident_schedevent==T,.(
                    Booked=sum(programStagename=="Booking visit",
                                           na.rm=T),
                    ANCVisits=sum(programStagename=="Antenatal care visit",
                                  na.rm=T),
                  
                    BookedschedTarm=sum(programStagename=="Booking visit" &
                                          !is.na(sched_TrialArm),
                                          na.rm=T),
                    ANCVisitsTarm=sum(programStagename=="Antenatal care visit" &
                                        !is.na(sched_TrialArm),
                                  na.rm=T),
                    
                    BookeddhisTarm=sum(programStagename=="Booking visit" &
                                          !is.na(TrialArm),
                                        na.rm=T),
                  ANCVisitdhisarm=sum(programStagename=="Antenatal care visit" &
                                        !is.na(TrialArm),
                                      na.rm=T),
                    
                    
                    BookedDaterest=sum(programStagename=="Booking visit" &
                                         eventdate>="2019-12-01" &
                                         eventdate<="2020-03-22",na.rm=T),
                    ANCVisitsDaterest=sum(
                      programStagename=="Antenatal care visit" & 
                        eventdate>="2019-12-01" &
                        eventdate<="2020-03-22",na.rm=T),
                    BookedDaterestTarm=sum(programStagename=="Booking visit" &
                                         eventdate>="2019-12-01" &
                                         eventdate<="2020-03-22" & 
                                           !is.na(TrialArm),na.rm=T),
                    ANCVisitsDaterestT2arm=sum(
                      programStagename=="Antenatal care visit" & 
                        eventdate>="2019-12-01" &
                        eventdate<="2020-03-22" & 
                        !is.na(TrialArm),na.rm=T),
                  BookedDaterestschcTarm=sum(programStagename=="Booking visit" &
                                           eventdate>="2019-12-01" &
                                           eventdate<="2020-03-22" & 
                                           !is.na(sched_TrialArm),na.rm=T),
                  ANCVisitsDaterestschedT2arm=sum(
                    programStagename=="Antenatal care visit" & 
                      eventdate>="2019-12-01" &
                      eventdate<="2020-03-22" & 
                      !is.na(sched_TrialArm),na.rm=T),
                  Eventsnotmerged_booking=sum(is.na(ident_dhis2) &
                                  programStagename=="Booking visit"),
                  Eventsnotmerged_anc=sum(is.na(ident_dhis2) &
                                    programStagename=="Antenatal care visit")), keyby=.(status)]


openxlsx::write.xlsx(schedevonly,
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "data",
                    sprintf("%s_events_only_descriptives.xlsx",
                    lubridate::today())))



xtabs(~status + programStagename,data=merged[ident_schedevent==T], addNA=T)
xtabs(~status + programStagename,
      data=merged[ident_schedevent==T & 
                    is.na(ident_dhis2) &
                    !is.na(sched_TrialArm)], addNA=T)

xtabs(~status + programStagename,
      data=merged[ident_schedevent==T & 
                    is.na(ident_dhis2) &
                    !is.na(sched_TrialArm) &
                    eventdate>="2019-12-01" &
                    eventdate<="2020-03-22"], addNA=T)



xtabs(~status + programStagename,data=merged[ident_schedevent==T &
                                               eventdate>="2019-12-01" &
                                               eventdate<="2020-03-22"])



########### Merged data ########### 

mergedonly <- merged[ident_schedevent==T & 
                       ident_dhis2==T,.(
                         ALLmerged=.N,
  Booked=sum(programStagename=="Booking visit", na.rm=T),
  Bookeddhis2=sum(ident_dhis2_booking==T, na.rm=T),
  ANCVisits=sum(programStagename=="Antenatal care visit",na.rm=T),
  Booked=sum(programStagename=="Booking visit",na.rm=T),
  ANCVisits=sum(programStagename=="Antenatal care visit",na.rm=T),
  BookedschedTarm=sum(programStagename=="Booking visit" &
                        !is.na(sched_TrialArm),na.rm=T),
  ANCVisitsTarm=sum(programStagename=="Antenatal care visit" &
                      !is.na(sched_TrialArm),na.rm=T),
  BookedDaterest=sum(programStagename=="Booking visit" &
                       eventdate>="2019-12-01" &
                       eventdate<="2020-03-22",na.rm=T),
  ANCVisitsDaterest=sum(
    programStagename=="Antenatal care visit" & 
      eventdate>="2019-12-01" &
      eventdate<="2020-03-22",na.rm=T),
  BookedDaterestTarm=sum(programStagename=="Booking visit" &
                           eventdate>="2019-12-01" &
                           eventdate<="2020-03-22" & 
                           !is.na(TrialArm),na.rm=T),
  ANCVisitsDaterestT2arm=sum(
    programStagename=="Antenatal care visit" & 
      eventdate>="2019-12-01" &
      eventdate<="2020-03-22" & 
      !is.na(TrialArm),na.rm=T)), keyby=.(status)]


openxlsx::write.xlsx(mergedonly,
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "data",
                               sprintf("%s_mergedevents_only_descriptives.xlsx",
                                       lubridate::today())))




######################## Data that hasnt merged ########################

notmatched <- merged[is.na(ident_dhis2) &
                       ident_schedevent==T,.(
                     N=.N,
                     Booked=sum(programStagename=="Booking visit", na.rm=T),
                     ANC=sum(programStagename=="Antenatal care visit", na.rm=T),
                     BookedDaterest=sum(programStagename=="Booking visit" &
                                          eventdate>="2019-12-01" &
                                          eventdate<="2020-03-22", na.rm=T),
                     ANCDaterest=sum(programStagename=="Antenatal care visit" &
                                          eventdate>="2019-12-01" &
                                          eventdate<="2020-03-22", na.rm=T),
                     BookedDateresschedT2=sum(programStagename=="Booking visit" &
                                          eventdate>="2019-12-01" &
                                          eventdate<="2020-03-22" &
                                          !is.na(sched_TrialArm), na.rm=T),
                     ANCDaterestschedT2=sum(
                       programStagename=="Antenatal care visit" & 
                         !is.na(sched_TrialArm) &
                                       eventdate>="2019-12-01" &
                                       eventdate<="2020-03-22", na.rm=T)),
                                keyby=.(status)]

openxlsx::write.xlsx(notmatched,file.path(FOLDER_DATA_RESULTS,
                                          "T2",
                                          "data",
                                   sprintf("%s_notmatched_desc.xlsx",
                                           lubridate::today())))





t2[,orgname:=bookorgname]
t2[,orgname:=NULL]


# orgunits supposedly not in our data
missingorgnames <- unique(schedevents$orgname)[!unique(schedevents$orgname) %in% unique(long$bookorgname)]

openxlsx::write.xlsx(missingorgnames,file.path(FOLDER_DATA_RESULTS,
                                               "T2",
                                               "data",
                                               sprintf("%s_schedevorgnamesmissingfromeReg.xlsx",lubridate::today())))

# missing events from eReg data
notmerging <- merged[is.na(ident_dhis2) &
                       ident_schedevent==T & 
                       (status=="ACTIVE"| status=="COMPLETED"),
                     c("orgname",
                       "programStagename",
                       "event",
                       "eventdate",
                       "dueDate",
                       "completeddate",
                       "tei")]



openxlsx::write.xlsx(notmerging,file.path(FOLDER_DATA_RESULTS,
                                               "T2",
                                               "data",
                                               sprintf("%s_eventsmissingfromeReg.xlsx",lubridate::today())))


############# SMS logs ###################

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





######## merge sms stuff with logs ######## 

# change names in other data sets to match eventid
setnames(allsmssent,"event_id","event")
setnames(msglogs,"event_id","event")

nrow(merged)
nrow(merged[TrialArm=="SMS and QID"|TrialArm=="SMS only"])


### keeping everyone for now because we want to look at the control arm
### we can restrict the data set later to those not missing the trial arm so we ### dont lose any of the events
merged2 <- merge(merged,
                 msglogs,
                 by="event",
                 all=T)

nrow(merged2)

### didnt work so should probably remove duplicates
# merge sent sms with all of these
merged3 <- merge(merged2,
                 allsmssent,
                 by="event",
                 all.x=T)

Yes. Suppose an appointment was "Scheduled" and never actually occurs. If the event was created before dec 1, but scheduled for a date after dec 1, then this was still included in the export, because the "due date" is after dec 1.


