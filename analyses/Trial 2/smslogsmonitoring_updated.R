###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

##### 
# global options -->  unticked cleanup output after successful... 
# and ticked View Rcheck

if(IS_GAZA==F){
  
  # read in logs #
  
  # all scheduled events
  schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-08-05_schedevents.csv")
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-08-05.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/data_raw/smslogs/dhis2allmsgssent/2020-08-05.csv")
  
  ###### Load in Data Set ###### 
  d <- LoadDataFileFromNetwork()
  
  
  
  
  
} else {
  
  
  # put gazas stuff here
  
  
}

############ Trial 2 SMS Monitoring ###############

varsancevent <- names(d)[stringr::str_detect(names(d),"^anevent")]
varsancdate <- names(d)[stringr::str_detect(names(d),"^andate")]
varsancgestage <- names(d)[stringr::str_detect(names(d),"^angestage")]


# check these definitions
t2 <- d[bookyear>=2019 & 
          ident_dhis2_booking==T & 
          ident_TRIAL_2_and_3==T,
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
          varsancevent,
          varsancdate,
          varsancgestage), with=F]

t2 <-t2[,anevent_0:=bookevent]
t2 <-t2[,andate_0:=bookdate]

# trial arms
t2[ident_TRIAL_2_3_Control==T,prettyExposure:="Control"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==F,TrialArm:="SMS only"]

t2[ident_TRIAL_2==F & 
     ident_TRIAL_3==T,TrialArm:="QID only"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==T,TrialArm:="SMS and QID"]

t2 <- t2[,c("uniqueid",
            "bookevent",
            "bookorgname",
            "bookdate",
            "bookgestage",
            "ident_dhis2_booking",
            varsancevent,
            varsancdate,
            varsancgestage), with=F]

#with=F looks inside the vars and get the values

##### import scheduled events #####
schedeventsraw[1:3]

schedevents<- setnames(schedeventsraw,c("number",
                                        "programStage",
                                        "orgUnit",
                                        "dueDate",
                                        "program",
                                        "event",
                                        "status",
                                        "enrollment",
                                        "trackedEntityInstance",
                                        "created",
                                        "name"),
                       c("num",
                         "schedev_programstage",
                         "schedev_orgunit",
                         "apptcreateddate",
                         "schedev_program",
                         "event_id",
                         "anstatus",
                         "enrollmentid",
                         "uniqueid",
                         "schedev_dateapptcreated",
                         "schedev_orgname"
                       ))





schedevents[1]

#cleaning var types
schedevents[,schedev_dateapptcreated:=stringr::str_sub(schedev_dateapptcreated,1,10)]
schedevents[,schedev_dateapptcreated:=as.Date(schedev_dateapptcreated)]
xtabs(~schedevents$schedev_dateapptcreated)



schedevents[1:3]

schedevents[,eventnum:=1:.N, by=uniqueid]

setorder(schedevents, uniqueid,event_id,schedev_dateapptcreated)

schedevents[,schedev_orgname:=ExtractOnlyEnglishLetters(schedev_orgname)]


#### scheduled message logs ####
msglogsraw[1]


#need to know the difference between the date variables
#name the first one something so they arent missing
msglogs <- msglogsraw[,c("msg_name",
                         "msg_generated",
                         "msg_scheduledFor",
                         "msg_id",
                         "creater_name",
                         "creater_userid",
                         "event_createdat",
                         "event_id")]
setnames(msglogs, c("msg_name",
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
           "anevent"))

msglogs[1]
unique(msglogs$msgname)
unique(msglogs$anevent)

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

xtabs(~msglogs$msgname)

nrow(msglogs)

####### remove duplicate messages #######

### this didnt work
setorder(msglogs,anevent,datesmssched)
msglogs[,firstdatesent:=min(datesmssched),by=anevent]

#date of previous one sent
msglogs[,prevdatesent:=shift(datesmssched), by=anevent]

#do same for message id as above so we can compare them later
msglogs[,prevmsgname:=shift(msgname), by=anevent]

#m
msglogs[,timesincelastmsg:=as.numeric(difftime(datesmssched, 
                                               prevdatesent,units="days"))]
xtabs(~msglogs$timesincelastmsg, addNA=T)

#get rid of duplicates
# we are losing messages this way which means the code we have used may not be correct

msglogs <- msglogs[is.na(timesincelastmsg)|timesincelastmsg>0 |(timesincelastmsg==0 & prevmsgname==msgname)]
#when we get rid of duplicates, we end up losing some important messages
#so, maybe we should include the name of the template instead?

nrow(msglogs[msgname!=prevmsgname |is.na(prevmsgname)])
sort(unique(msglogs$msgname))
sort(unique(msglogsraw$msg_name))

xtabs(~msglogs$msgname, addNA=T)
xtabs(~msglogsraw$msg_name, addNA=T)

sort(unique(msglogsraw$msg_name))

#msglogsclean <- msglogs[msgname!=prevmsgname |is.na(prevmsgname)]


names(msglogs)
xtabs(~msglogs$timesincelastmsg)

#want to decast to wide to say if its first message, second message
msglogs[,msgnumber:=1:.N, by=anevent]

#melt the small data set we have if we need to for the ReshapaheToWideAndMerge formula
#t2 already wide format
#t2long <- melt.data.table(t2, id.vars="uniqueid")

#valueVarsRegex and other variables after this


###### 
# schedevents events scheduled
# msglogs= messages sent

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

msglogs[stringr::str_detect(msgname,"^SMS: 24$"), msgtype:= "Remove Template"]


xtabs(~msglogs$msgtype, addNA=T)

msglogs[,ident_schedsms:=T]




### variable for timing of scheduling
msglogs[,msgschedb4visit:=as.logical(NA)]
msglogs[!is.na(datesmssched) & !is.na(datesmsschedfor), msgschedb4visit:=FALSE]

msglogs[,timeschedb4visit:= as.numeric(NA)]
msglogs[,timeschedb4visit:=difftime(datesmsschedfor,datesmssched,units = "days")]
xtabs(~msglogs$timeschedb4visit)

msglogs[msgschedb4visit==F & timeschedb4visit>0 , msgschedb4visit:=TRUE]




########### dhis2 messages sent ###########

setnames(allsmssentraw,"Phone","phone")

allsmssent <- allsmssentraw[,processed:=stringr::str_sub(processed,1,10)]
allsmssent[,processed:=as.Date(processed)]

# sms sent variable
allsmssent[,ident_sms_sent:=T]

############ enrollment data ############

enrollments <- t2[,c("bookorgname",
                     "bookdate",
                     "bookgestage",
                     "anevent_0",
                     "ident_dhis2_booking",
                     "uniqueid")]

#install.packages("tidyverse")
#library(tidyverse)

schedevents[,ident_schedevent:=T]

sentandsched <- merge(allsmssent[processed>="2019-12-01"], 
                      schedevents, 
                      by=c("event_id"), all=T)

xtabs(~sentandsched$ident_sms_sent, addNA=T)
xtabs(~sentandsched$ident_schedevent, addNA=T)

xtabs(~ident_schedevent + ident_sms_sent, data=sentandsched, addNA=T)
nrow(sentandsched)

setDT(sentandsched)


sentschedenroll <- merge(sentandsched, enrollments, by=c("uniqueid"))

#= scheduled messages with enrollment date
#Among the women booked December to March and July-August, how many 1) 1-week messages were scheduled 2) how many recapture messages were scheduled?"

opportunities <- sentschedenroll[(bookdate>="2019-12-01" &
                                    bookdate<="2020-03-30")|
                                   bookdate>="2020-06-22",.(
                                     N=.N),
                                 keyby=.(prettyExposure,msgtype)
                                 ]




######## END enrollment data ######## 


########## Scheduled events with anc data ########## 


# first reshape wide data and want each variable to be a heading because want to do analysis by event

long <- melt(t2, id.vars = c("uniqueid", 
                               "bookevent",
                               "TrialArm",
                               "bookorgname"),
               measure = patterns("^andate_", 
                                   "^anevent_",
                                   "^angestage"), 
               value.name = c("andate", 
                              "anevent",
                              "angestage"))


setnames(schedevents,"event_id","anevent")

ancdata <- merge(long, schedevents, by="anevent")






# if add all.y=T the number of rows goes from 21,069 to 42171
joined <- merge(schedevents,msglogs, by="anevent", all.x=T)

setnames(joined,"schedev_orgname","bookorgname")




######## clean bookorgname structural data sheet to merge here

# merge by tracked entity instance


joined <- merge(joined, sData, by="bookorgname")

joined <- joined[,TrialArm:=as.character(NA)]



uglytable <- joined[,.(N=.N,
                       numschedbeforevisit=sum(timeschedb4visit>0,na.rm=T),
                       numNotscheduled=sum(timeschedb4visit==0, na.rm=T),
                       numschedAftervisit=sum(timeschedb4visit<0, na.rm=T)),
                    keyby=.(TrialArm,bookorgname)]

if(IS_GAZA==FALSE){
  
  
  
  
} else{
  
  
  
  
  
  
}





