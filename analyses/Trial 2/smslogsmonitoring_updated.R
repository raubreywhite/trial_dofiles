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
  schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-08-25.csv")
  
  # all scheduled messages
  msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-08-25.csv")
  
  
  # all sent sms
  allsmssentraw <-fread("C:/data processing/data_raw/smslogs/dhis2allmsgssent/2020-08-25.csv")
  
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
t2[ident_TRIAL_2_3_Control==T,TrialArm:="Control"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==F,TrialArm:="SMS only"]

t2[ident_TRIAL_2==F & 
     ident_TRIAL_3==T,TrialArm:="QID only"]

t2[ident_TRIAL_2==T & 
     ident_TRIAL_3==T,TrialArm:="SMS and QID"]

t2 <- t2[bookdate>="2019-12-01" &
           bookdate<="2020-03-30",c("uniqueid",
            "bookevent",
            "bookorgname",
            "bookdate",
            "bookgestage",
            "anevent_0",
            "andate_0",
            "ident_dhis2_booking",
            "TrialArm",
            varsancevent,
            varsancdate,
            varsancgestage), with=F]

nrow(t2)



################ t2 ends here ################ 

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

schedevents[,ident_schedevent:=T]


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
           "event_id"))

msglogs[1]
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

xtabs(~msglogs$msgname)

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
# we are losing messages this way which means the code we have used may not be correct

msglogs <- msglogs[is.na(timesincelastmsg)|
                     timesincelastmsg!=0 |
                     (timesincelastmsg==0 & prevmsgname!=msgname)]
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




########### dhis2 messages sent ###########
setnames(allsmssentraw,4,"phone")


allsmssent <- allsmssentraw[,processed:=stringr::str_sub(processed,1,10)]
allsmssent[,processed:=as.Date(processed)]

# sms sent variable
allsmssent[,ident_sms_sent:=T]

############ enrollment data ############

# enrollments
enrollments <- t2[,c("bookorgname",
                     "bookdate",
                     "bookgestage",
                     "anevent_0",
                     "ident_dhis2_booking",
                     "uniqueid",
                     "TrialArm")]



# first reshape wide data and want each variable to be a heading because want to do analysis by event

long <- melt(t2, id.vars = c("uniqueid", 
                             "bookevent",
                             "TrialArm",
                             "bookorgname",
                             "bookdate",
                             "ident_dhis2_booking"),
             measure = patterns("^andate_", 
                                "^anevent_",
                                "^angestage"), 
             value.name = c("andate", 
                            "anevent",
                            "angestage"))



######## END enrollment data ######## 




########## COMBINING SMALLER DATA SETS TO GET ONES WE WANT ########## 

# merge schedevents with sched sms to get templates that we want
schedev_schedsms <- merge(schedevents,
                          msglogs,
                          by="event_id", all=T)


# getting an idea of the numbers
xtabs(~ident_schedsms + ident_schedevent, data=schedev_schedsms, addNA=T)


# merging scheduled events and scheduled messages with bookings
schedev_schedsms_bookings <- merge(schedev_schedsms,
                                   long,
                                   by="uniqueid", all=T)

xtabs(~ident_dhis2_booking + ident_schedevent, 
                    data=schedev_schedsms_bookings, addNA=T)

xtabs(~ident_dhis2_booking + ident_schedsms, 
      data=schedev_schedsms_bookings, addNA=T)

#= scheduled messages with enrollment date
#Among the women booked December to March and July-August, how many 1) 1-week messages were scheduled 2) how many recapture messages were scheduled?"


setnames(schedev_schedsms_bookings,"event_id.x","event_id")
sched_ev_sms_sent_booked <- merge(allsmssent,
                                  schedev_schedsms_bookings, 
                                  by="event_id",
                                  all=T)



# missing msg types havent been sent yet
opportunities <- sched_ev_sms_sent_booked[,.(
                                     N=.N),
                                 keyby=.(TrialArm,bookorgname,msgtype)
                                 ]

xtabs(~msgtype+anstatus, data=sched_ev_sms_sent_booked,addNA=T)



########## Scheduled events with anc data ########## 

ancdata <- sched_ev_sms_sent_booked[(bookdate>="2019-12-01" &
                                      bookdate<="2020-03-30") &
                                      ident_dhis2_booking==T &
                                      ident_sms_sched==T &
                                       ident_sms_sent==T,]

nrow(sched_ev_sms_sent_booked[ident_sms_sched==T &
                                ident_sms_sent==T,])



# could make the data set this way but its better to use what we have made before
# 
# ancdata <- merge(schedevents,
#                  long, 
#                  by="uniqueid", all=T)
# 
# xtabs(~ident_schedevent+bookorgname, data=ancdata)


# scheduling patterns by orgunit

##### Brians way ######










if(IS_GAZA==FALSE){
  
  
  
  
} else{
  
  
  
  
  
  
}





