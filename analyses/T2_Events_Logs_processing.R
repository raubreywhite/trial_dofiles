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
  schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-10-26.csv", header=FALSE)
  
  
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






######### sched events ######### 
#with=F looks inside the vars and get the values
schedeventsraw[1:3]

# based on sql query, these are the names
setnames(schedeventsraw,1,"event")
setnames(schedeventsraw,2,"status")
setnames(schedeventsraw,3,"deleted")
setnames(schedeventsraw,4,"dueDate")
setnames(schedeventsraw,5,"createddate")
setnames(schedeventsraw,6,"enrollmentid")
setnames(schedeventsraw,7,"tei")
setnames(schedeventsraw,8,"programStagename")
setnames(schedeventsraw,9,"programstageid")
setnames(schedeventsraw,10,"eventdate")
setnames(schedeventsraw,11,"orgunitcode")
setnames(schedeventsraw,12,"orgname")
setnames(schedeventsraw,13,"completeddate")

schedevents <- schedeventsraw

### cleaning var types
#deleted
schedevents[deleted=="t",deleted:="TRUE"]
schedevents[deleted=="f",deleted:="FALSE"]
schedevents[,deleted:=as.logical(deleted)]
xtabs(~schedevents$deleted)


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

# creating eventnums

test[,eventnumtei:=1:.N, by=tei]
xtabs(~test$eventnumtei, addNA = T)




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

# Yes. Suppose an appointment was "Scheduled" and never actually occurs. If the event was created before dec 1, but scheduled for a date after dec 1, then this was still included in the export, because the "due date" is after dec 1.







############ Trial 2 SMS Monitoring ###############

d[,andate_0:=bookdate]
d[,angestage_0:=bookgestage]
d[,anevent_0:=bookevent]



t2 <-d[bookdate>="2019-01-01"]

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


