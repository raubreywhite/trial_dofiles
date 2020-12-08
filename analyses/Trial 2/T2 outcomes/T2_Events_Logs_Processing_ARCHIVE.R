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
schedeventsraw[1:5]


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

setorder(schedeventsraw,tei,eventdate)

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
schedevents[,eventdate:=as.Date(eventdate, format="%Y-%m-%d")]
xtabs(~schedevents$eventdate)

#completeddate
schedevents[,completeddate:=stringr::str_sub(completeddate,1,10)]
schedevents[completeddate=="", completeddate:=NA]
schedevents[,completeddate:=as.Date(completeddate, formmat="%Y-%m-%d")]

xtabs(~schedevents$completeddate, addNA=T)



# cleaning orgname
schedevents[,orgname:=ExtractOnlyEnglishLetters(orgname)]
xtabs(~schedevents$orgname)




# order data set
setorderv(schedevents,c("tei",
                       "eventdate"),
                       c(1,1), na.last=T)

nrow(schedevents)


# create bookdate for each pregnancy
schedevents[,bookdate:= as.Date(NA)]
schedevents[programstageid=="WZbXY0S00lP", bookdate:=eventdate, by=tei]

# fill in bookdates for the rest of the values
schedevents[,bookdate:=zoo::na.locf(bookdate),by=tei]

setorder(schedevents,tei,eventdate)
#### event defintions
#1.	event status of either complete or active AND have a dueDate is AFTER event date: the woman did not have a visit per #schedule, but came earlier than scheduled. 
#2.	event status of either complete or active AND  have a dueDate AND event date on the same day: the woman had a visit #as per what was scheduled. 

## variables to determine attendance

# attended on time
schedevents[,schedandatt:=as.logical(NA)]

#False if had a scheduled visit
schedevents[!is.na(dueDate),schedandatt:=FALSE]

#TRUE if scheduled AND attended on time
schedevents[schedandatt==FALSE & 
              status %in% c("ACTIVE","COMPLETED") &
              eventdate==dueDate,schedandatt:=TRUE]

xtabs(~schedevents$schedandatt, addNA=T)


# before scheduled visit
schedevents[,schedandattb4appt:=as.logical(NA)]

#False if had a scheduled visit
schedevents[!is.na(dueDate),schedandattb4appt:=FALSE]

#TRUE if scheduled AND attended before visit
schedevents[schedandatt==FALSE & 
              status %in% c("ACTIVE","COMPLETED") &
              eventdate<=dueDate,schedandattb4appt:=TRUE]

xtabs(~schedevents$schedandattb4appt, addNA=T)

setnames(schedevents,"tei", "uniqueid")

# order data set so can fill in appropriate bookdate for each
keycol <- c("enrollmentid", 
             "uniqueid", 
            "eventdate",
             "programStagename")

ordercol <- c(1,1,1,-1)

setorderv(schedevents,keycol,ordercol,na.last=T)


# eventnum
schedevents[,eventnum:=1:.N, by=uniqueid]
xtabs(~schedevents$eventnum)


schedevents[,enrollidnum:=1:.N, by=enrollmentid]
xtabs(~schedevents$enrollidnum)
schedevents[,enrollidnum:=NULL]



### find gestage at each event by merging in lmp and first 1-23 edd from data with tei to calculate the gA at scheduling and gA at visit here. then merge this small data set with our T2 data set

smallT2 <-d[,c("uniqueid",
               "bookdate",
               "USorLMPdate",
               "ident_TRIAL_2_and_3",
               "ident_dhis2_booking")]

schedevents[,ident_schedev:=T]

T2schedevsmall <- merge(smallT2,
                        schedevents,
                        by=c("uniqueid","bookdate"),
                        all=T)

nrow(schedevents)
nrow(T2schedev)

schedevents[,schedev_ident_schedev:=NULL]


#rename column names
colnames(schedevents) <- paste("schedev", colnames(schedevents),sep="_")

setnames(schedevents,c("schedev_enrollmentid",
                       "schedev_uniqueid",
                       "schedev_bookdate",
                       "schedev_eventnum"),
                  c("enrollmentid",
                    "uniqueid",
                    "bookdate",
                    "eventnum"))

# columns we have in this data set
colsIhave <- names(schedevents)

# reshape schedevents to wide
colsIneed <-c("schedev_event",
              "schedev_status",
              "schedev_deleted",
              "schedev_dueDate",
              "schedev_createddate",
              "schedev_enrollmentid",
              "schedev_programStagename",
              "schedev_programstageid",   
              "schedev_eventdate",
              "schedev_orgunitcode",
              "schedev_orgname",
              "schedev_completeddate",
              "schedev_schedandatt",
              "schedev_schedandattb4appt")

colsIdontWant <- c("enrollmentid",
                   "uniqueid",
                   "bookdate",
                   "eventnum") 



#identify columns not in this sheet
colsIwant<- colsIneed[colsIneed %in% colsIhave]


setorder(schedevents,uniqueid,bookdate,schedev_eventdate,na.last = T)

w = dcast(schedevents[eventnum<=30], enrollmentid+uniqueid + bookdate ~ eventnum, 
                          value.var =colsIwant)

w[,ident_schedev:=T]


###### merge this with d ###### 

wT2 <- merge(d,
             w,
             by=c("uniqueid","bookdate"),
             all=T)


xtabs(~ident_dhis2_booking + ident_schedev,data=wT2, addNA=T)


 ##########################
 #rename all variables last
 ##########################
 

 
 names(schedevents) 

 #rename vars we dont want to change
 setnames(schedevents,"schedev_uniqueid","uniqueid")
 setnames(schedevents,"schedev_eventnum","eventnum")
 setnames(schedevents,"schedev_bookdate","bookdate")

 
 # identifying that these are all scheduled events
 #schedevents[,ident_schedevent:=T]
 
 
 #############################
 #prepare smalld set to merge
 #############################
 
 
# Best to reshape to wide and merge with d instead
# remove anything more than 30


length(unique(schedevents$uniqueid))
 
T2schedev <- ReshapeToWideAndMergeALL(
  base=d,
  additional=schedevents,
  valueVarsRegex="^schedev_",
  dcastFormula="uniqueid+bookdate~eventnum",
  mergeVars=c("uniqueid","bookdate"),
  identName="ident_schedev"
)

xtabs(~ident_TRIAL_2_and_3+ident_schedev, data=T2schedev, addNA=T)


openxlsx::write.xlsx(T2schedev[is.na(ident_schedev)],
                     file.path(FOLDER_DATA_CLEAN,
                               "noevents.xlsx"))

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

test[,eventnumuid:=1:.N, by=uniqueid]
xtabs(~test$eventnumtei, addNA = T)

test[,eventnumenrolid:=1:.N, by=enrollmentid]
xtabs(~test$eventnumenrolid, addNA = T)


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


#### event defintions
#1.	event status of either complete or active AND have a dueDate is AFTER event date: the woman did not have a visit per #schedule, but came earlier than scheduled. 
#2.	event status of either complete or active AND  have a dueDate AND event date on the same day: the woman had a visit #as per what was scheduled. 




######## merge sms stuff with logs ######## 

# change names in other data sets to match eventid
setnames(allsmssent,"event_id","event")
setnames(msglogs,"event_id","event")

# merged data with scheduled messages

nrow(test)
nrow(msglogs)
merged <- merge(test,
                msglogs,
                by="event",
                all=T)


nrow(merged)



### keeping everyone for now because we want to look at the control arm
### we can restrict the data set later to those not missing the trial arm so we ### dont lose any of the events
# merged2 <- merge(merged,
#                  allsmssent,
#                  by="event",
#                  all.x=T)
# 
# nrow(merged2)
# 
# ### didnt work so should probably remove duplicates
# # merge sent sms with all of these
# merged3 <- merge(merged2,
#                  allsmssent,
#                  by="event",
#                  all.x=T)

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


##### merge events logs with our data

#setnames(test,"tei","uniqueid")

nrow(t2)
nrow(test)

t2merged <- merge(t2,
                  test,
                  by="uniqueid",
                  all=T)

nrow(t2merged)
nrow(schedevents)
xtabs(~t2merged$ident_schedevent, addNA=T)



###### keeep sms stuff in long format ######

# denominator: schedevents in timely manner based on gA
# chose events "schedule" and then later we will remove what we want to be removed
# create book gest age cats per tei

###########  Creating Denominator ###########  

#need to sort

# create gestational age variable
t2ds<-t2merged[!is.na(dueDate),gAbyevent:=difftime(dueDate,
                                            USorLMPdate, units="days")]


# create bookgestage
t2ds <- t2ds[,bookgestage:=as.numeric(NA)]
t2ds <- t2ds[programstageid=="WZbXY0S00lP" & !is.na(dueDate) & 
               !is.na(USorLMPdate),bookgestage:=]

# create TRUE/FALSE if scheduled appropriately per gestage cat



# 15 to 17 week opportunity
t2ds[,T2vdenom_1:=as.logical(NA)]
t2ds[bookgestage<105 & !is.na(bookgestage), T2vdenom_1:=FALSE]
t2ds[status=="SCHEDULE" & programstageid=="edqlbukwRfQ" & 
       T2vdenom_1==FALSE & gA>=105 & gA<=126,T2vdenom_1==TRUE]

# 18 to 22 week opportunity
t2ds[,T2vdenom_2:=as.logical(NA)]
t2ds[bookgestage<127 & !is.na(bookgestage), T2vdenom_2:=FALSE]
t2ds[programstageid=="edqlbukwRfQ" & 
       T2vdenom_2==FALSE & gA>=127 & gA<=161,T2vdenom_2==TRUE]

# 24 to 28 week opportunity
t2ds[,T2vdenom_3:=as.logical(NA)]
t2ds[bookgestage<168 & bookgestage>0, T2vdenom_3:=FALSE]
t2ds[programstageid=="edqlbukwRfQ" & 
       T2vdenom_3==FALSE & gA>=168 & gA<=203,T2vdenom_3==TRUE]

# 31 to 33 week opportunity
t2ds[,T2vdenom_4:=as.logical(NA)]
t2ds[bookgestage<217 & bookgestage>0, T2vdenom_4:=FALSE]
t2ds[programstageid=="edqlbukwRfQ" & 
       T2vdenom_4==FALSE & gA>=217 & gA<=238,T2vdenom_4==TRUE]

# 35 to 37 week opportunity
t2ds[,T2vdenom_5:=as.logical(NA)]
t2ds[bookgestage<245 & bookgestage>0, T2vdenom_5:=FALSE]
t2ds[programstageid=="edqlbukwRfQ" & 
       T2vdenom_5==FALSE & gA>=245 & gA<=266,T2vdenom_5==TRUE]






# numerator
# schedueled events are how many actually attended- based on algorithims in email
# could probably use gA at those visits to get T








# order data set so can fill in appropriate bookdate for each
keycol <- c("enrollmentid", 
            "uniqueid", 
            "eventdate",
            "programStagename")

ordercol <- c(1,1,1,-1)

setorderv(schedevents,keycol,ordercol,na.last=T)


# eventnum
schedevents[,eventnum:=1:.N, by=uniqueid]
xtabs(~schedevents$eventnum)


schedevents[,enrollidnum:=1:.N, by=enrollmentid]
xtabs(~schedevents$enrollidnum)
schedevents[,enrollidnum:=NULL]





































################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################

############# ARCHIVE ############# 


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

#################################################################################
# merge the two data sets
xtabs(~test$ident_schedevent, addNA=T)
nrow(schedevents)

merged <- merge(test,
                long,
                by="event",
                all.x=T)
nrow(merged)

write.csv(merged,file=sprintf("%s_T2_events_and_dhis2_data.csv",
                              lubridate::today()), fileEncoding = "UTF-8")


# identify duplicate bookevents
merged[,eventnum:=0]
merged[,eventnumbyevent:=.N,by=enrollmentid]

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


