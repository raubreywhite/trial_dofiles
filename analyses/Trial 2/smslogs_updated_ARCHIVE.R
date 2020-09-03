###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

##### 



if(IS_GAZA==F){
  
# read in logs #
  
# all scheduled events
schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-08-05_schedevents.csv")


# all scheduled messages
msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-08-05.csv")

###### Load in Data Set ###### 
d <- LoadDataFileFromNetwork()

sData <- readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx")

setDT(sData)

 

} else {
  
  
  sData <- readxl::read_excel("../gaza_data_raw/structural_data/bookorgname.xlsx")   
  setDT(sData)
  
  
}

############ Trial 2 SMS Monitoring ###############

varsancevent <- names(d)[stringr::str_detect(names(d),"^anevent")]
varsancdate <- names(d)[stringr::str_detect(names(d),"^anevent")]


# check these definitions
t2 <- d[bookyear>=2019 & ident_TRIAL_2_and_3==T,
        c("uniqueid",
          "bookevent",
          "bookorgname",
          "bookdate",
          "ident_TRIAL_2_and_3",
          "ident_TRIAL_2_3_Control",
          "ident_TRIAL_2",
          "ident_TRIAL_3",
          varsancevent,
          varsancdate), with=F]

t2 <-t2[,anevent_0:=bookevent]
t2 <-t2[,andate_0:=bookdate]

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
                         "programstage",
                         "orgunit",
                         "apptcreateddate",
                         "program",
                         "anevent",
                         "anstatus",
                         "enrollmentid",
                         "uniqueid",
                         "dateapptcreated",
                         "orgname"
                       ))

#remove duplicate vars and vars we dont want
schedevents <- schedevents[,c("programstage",
                              "orgunit",
                              "program",
                              "anevent",
                              "anstatus",
                              "enrollmentid",
                              "uniqueid",
                              "dateapptcreated",
                              "orgname")]

schedevents[1]

#cleaning var types
schedevents[,dateapptcreated:=stringr::str_sub(dateapptcreated,1,10)]
schedevents[,dateapptcreated:=as.Date(dateapptcreated)]
xtabs(~schedevents$dateapptcreated)

#renaming vars
setnames(schedevents,c("programstage",
                       "orgunit",
                       "anevent",
                       "anstatus",
                       "enrollmentid",
                       "uniqueid",
                       "dateapptcreated",
                       "orgname"),
         c("schedev_programstage",
           "schedev_orgunit",
           "anevent",
           "schedev_anstatus",
           "enrollmentid",
           "uniqueid",
           "schedev_dateapptcreated",
           "schedev_orgname"))

schedevents[1:3]

schedevents[,eventnum:=1:.N, by=uniqueid]

setorder(schedevents, uniqueid,anevent,schedev_dateapptcreated)

schedevents[,schedev_orgname:=ExtractOnlyEnglishLetters(schedev_orgname)]


#### scheduled message logs ####
msglogsraw[1]


#need to know the difference between the date variables
#name the first one something so they arent missing
msglogs <- msglogsraw[,c("number",
                         "msg_name",
                         "msg_generated",
                         "msg_scheduledFor",
                         "msg_id",
                         "creater_name",
                         "creater_userid",
                         "event_createdat",
                         "event_id")]
setnames(msglogs, c("number",
                    "msg_name",
                    "msg_generated",
                    "msg_scheduledFor",
                    "msg_id",
                    "creater_name",
                    "creater_userid",
                    "event_createdat",
                    "event_id"),
         c("number",
           "msgname",
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

#remove duplicate message

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

msglogsclean <- msglogs[msgname!=prevmsgname |is.na(prevmsgname)]


names(msglogs)
xtabs(~msglogs$timesincelastmsg)

#want to decast to wide to say if its first message, second message
msglogs[,msgnumber:=1:.N, by=anevent]

#melt the small data set we have if we need to for the ReshapaheToWideAndMerge formula
#t2 already wide format
#t2long <- melt.data.table(t2, id.vars="uniqueid")

#valueVarsRegex and other variables after this

setnames(msglogs,c("msgname",
                   "datesmsgenerated",
                   "datesmsschedfor"),
         c("sms_msgname",
           "sms_datecreated",
           "sms_datesent"))



###### 
# schedevents events scheduled
# msglogs= messages sent

nrow(msglogs)
times <-msglogs[,c("msgname",
                   "datesmsgenerated",
                   "datesmsschedfor",
                   "datesmssched")
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




### variable for timing of scheduling
msglogs[,msgschedb4visit:=as.logical(NA)]
msglogs[!is.na(datesmssched) & !is.na(datesmsschedfor), msgschedb4visit:=FALSE]

msglogs[,timeschedb4visit:= as.numeric(NA)]
msglogs[,timeschedb4visit:=difftime(datesmsschedfor,datesmssched,units = "days")]
xtabs(~msglogs$timeschedb4visit)

msglogs[msgschedb4visit==F & timeschedb4visit>0 , msgschedb4visit:=TRUE]



##### Merge scheduled events with data and then message logs
# first reshape wide data
long <-melt.data.table(t2, id.vars = c("uniqueid", "bookevent"))

ancdata <- merge(long, schedevents, by="anevent")


# if add all.y=T the number of rows goes from 21,069 to 42171
joined <- merge(schedevents,msglogs, by="anevent", all.x=T)

setnames(joined,"schedev_orgname","bookorgname")




######## clean bookorgname structural data sheet to merge here

  # merge by tracked entity instance
  
sData <- sData[,c("bookorgname",
                  "NEW_bookorgname",
                  "ident_hr_clinic",        
                  "ident_TRIAL_2_and_3",
                  "ident_TRIAL_2",        
                  "ident_TRIAL_3",
                  "ident_TRIAL_2_3_Control",
                  "str_TRIAL_2_Cluster",
                  "str_TRIAL_2_ClusSize" 
                  )]


joined <- merge(joined, sData, by="bookorgname")

joined <- joined[,TrialArm:=as.character(NA)]
joined <- joined[ident_TRIAL_2_3_Control==T,prettyExposure:="Control"]

joined <- joined[ident_TRIAL_2==T & 
                   ident_TRIAL_3==F,TrialArm:="SMS only"]

joined <- joined[ident_TRIAL_2==F & 
                   ident_TRIAL_3==T,TrialArm:="QID only"]

joined <- joined[ident_TRIAL_2==T & 
                   ident_TRIAL_3==T,TrialArm:="SMS and QID"]


uglytable <- joined[,.(N=.N,
                        numschedbeforevisit=sum(timeschedb4visit>0,na.rm=T),
                        numNotscheduled=sum(timeschedb4visit==0, na.rm=T),
                        numschedAftervisit=sum(timeschedb4visit<0, na.rm=T)),
                     keyby=.(TrialArm,bookorgname)]

if(IS_GAZA==FALSE){
  
  
  
  
  } else{
  
  
  
  
  
  
}






##################################################################################
###### cant merge cuz events created in sms logs ###### 
###### not same as anevent in data ###### 

#merge small data set with logs and then reshape to wide
print("RESHAPE TO WIDE AND MERGE LOGS")
smsanc <- ReshapeToWideAndMerge(
  base=t2,
  additional=schedevents,
  valueVarsRegex="^sms_",
  dcastFormula="anevent~msgnumber",
  mergeVars=c("anevent"),
  identName="ident_eventsched"
)


#merge small data set with logs and then reshape to wide
print("RESHAPE TO WIDE AND MERGE LOGS")
smsanc <- ReshapeToWideAndMerge(
  base=t2,
  additional=msglogs,
  valueVarsRegex="^sms_",
  dcastFormula="anevent~msgnumber",
  mergeVars=c("anevent"),
  identName="ident_smssched"
)


#merge small data set with logs and then reshape to wide
print("RESHAPE TO WIDE AND MERGE LOGS")
smsanc <- ReshapeToWideAndMerge(
  base=t2,
  additional=schedevents,
  valueVarsRegex="^sms_",
  dcastFormula="uniqueid~eventnum",
  mergeVars=c("anevent"),
  identName="ident_smssched"
)

#valueVarsRegex want the values from the long to be changed to wide
#dcastFormula: stuff left of tilda will id msg, stuff right to tilda is what makes it wide


length(t2$bookevent)
length(unique(t2$bookevent))
nrow(t2[ident_smssched==T])
nrow(d)
ncol(d)
