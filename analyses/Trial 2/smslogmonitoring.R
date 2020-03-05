###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

###### read in logs ###### 
schedeventsraw <- fread("C:/data processing/data_raw/smslogs/schedevents/2020-01-29_schedevents.csv")

msglogsraw <- fread("C:/data processing/data_raw/smslogs/dhis2allschedmsgs/2020-01-29.csv")

###### Load in Data Set ###### 
d <- LoadDataFileFromNetwork()

############ Trial 2 SMS Monitoring ###############
varsbooking <-names(d)[stringr::str_detect(names(d),"^book")]
varsanc <- names(d)[stringr::str_detect(names(d),"^an")]
varslab <-  names(d)[stringr::str_detect(names(d),"^lab")]
varsus <-  names(d)[stringr::str_detect(names(d),"^us")]


t2 <- d[bookyear>=2018 & (ident_TRIAL_2_and_3==T |
          ident_TRIAL_2==T),
                c("uniqueid",
                  "ident_dhis2_booking",
                  varsbooking,
                  varsanc,
                  varslab,
                  varsus), with=F]

#with=F looks inside the vars and get the values

##### import scheduled events #####
schedeventsraw[1]

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
           "schedev_enrollmentid",
           "uniqueid",
           "schedev_dateapptcreated",
           "schedev_orgname"))





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
                     "datesmscreated",
                     "datesmssent",
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

msglogs[,datesmssent:=stringr::str_sub(datesmssent,1,10)]
msglogs[,datesmssent:=as.Date(datesmssent)]
xtabs(~msglogs$datesmssent)

msglogs[,datesmscreated:=stringr::str_sub(datesmscreated,1,10)]
msglogs[,datesmscreated:=as.Date(datesmscreated)]
xtabs(~msglogs$datesmscreated)

xtabs(~msglogs$msgname)

#remove duplicate message

### this didnt work
setorder(msglogs,anevent,datesmssent)
#msglogs[,firstdatesent:=min(datesent),by=uniqueid]

#date of previous one sent
msglogs[,prevdatesent:=shift(datesmssent), by=anevent]

#do same for message id as above so we can compare them later
msglogs[,prevmsgname:=shift(msgname), by=anevent]

#m
msglogs[,timesincelastmsg:=as.numeric(difftime(datesmssent, prevdatesent,units="days"))]

#get rid of duplicates
# we are losing messages this way which means the code we have used may not be correct
#msglogs <- msglogs[is.na(timesincelastmsg)|timesincelastmsg>0]
#when we get rid of duplicates, we end up losing some important messages
#so, maybe we should include the name of the template instead?

nrow(msglogs[msgname!=prevmsgname |is.na(prevmsgname)])
sort(unique(msglogs$msgname))
sort(unique(msglogsraw$msg_name))

msglogs <- msglogs[msgname!=prevmsgname |is.na(prevmsgname)]

xtabs(~msglogs$uniqueid)
names(msglogs)
xtabs(~msglogs$timesincelastmsg)

#want to decast to wide to say if its first message, second message
msglogs[,msgnumber:=1:.N, by=uniqueid]

#melt the small data set we have if we need to for the ReshapaheToWideAndMerge formula
#t2 already wide format
#t2long <- melt.data.table(t2, id.vars="uniqueid")

#valueVarsRegex and other variables after this

setnames(msglogs,c("msgname",
                   "datesmscreated",
                   "datesmssent"),
                c("sms_msgname",
                  "sms_datecreated",
                  "sms_datesent"))





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

#valueVarsRegex want the values from the long to be changed to wide
#dcastFormula: stuff left of tilda will id msg, stuff right to tilda is what makes it wide


length(t2$bookevent)
length(unique(t2$bookevent))
nrow(t2[ident_smssched==T])
nrow(d)
ncol(d)





