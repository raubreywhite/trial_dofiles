###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

###### Load in Data Set ###### 
d <- LoadDataFileFromNetwork()

############ Trial 2 SMS Monitoring ###############
varsbooking <-names(d)[stringr::str_detect(names(d),"^book_")]
varsanc <- names(d)[stringr::str_detect(names(d),"^an_")]
varslab <-  names(d)[stringr::str_detect(names(d),"^lab_")]
varsus <-  names(d)[stringr::str_detect(names(d),"^us_")]


t2 <- d[ident_TRIAL_2_and_3==T |
          ident_TRIAL_2==T,
                c("uniqueid",
                  "ident_dhis2_booking",
                  varsbooking,
                  varsanc,
                  varslab,
                  varsus)]

msglogsraw <- fread("C:/data processing/data_raw/dhis2allschedmsgs/2020-01-29.csv")

msglogsraw[1]


#need to know the difference between the date variables
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
                     "datesent",
                     "dateapptscheduled",
                     "msgid",
                     "creatername",
                     "createruserid",
                     "datemsgcreated",
                     "uniqueid"))

msglogs[1]
unique(msglogs$msgname)
unique(msglogs$eventid)

#cleaning var types
msglogs[,datemsgcreated:=stringr::str_sub(datemsgcreated,1,10)]
msglogs[,datemsgcreated:=as.Date(datemsgcreated)]
xtabs(~msglogs$datemsgcreated)

msglogs[,datesent:=stringr::str_sub(datesent,1,10)]
msglogs[,datesent:=as.Date(datesent)]
xtabs(~msglogs$datesent)

msglogs[,dateapptscheduled:=stringr::str_sub(dateapptscheduled,1,10)]
msglogs[,dateapptscheduled:=as.Date(dateapptscheduled)]
xtabs(~msglogs$dateapptscheduled)

xtabs(~msglogs$msgname)

#remove duplicate message

### this didnt work
setorder(msglogs,uniqueid,datesent)
#msglogs[,firstdatesent:=min(datesent),by=uniqueid]

#date of previous one sent
msglogs[,prevdatesent:=shift(datesent), by=uniqueid]

#do same for message id as above so we can compare them later
msglogs[,prevmsgname:=shift(msgname), by=uniqueid]

#m
msglogs[,timesincelastmsg:=as.numeric(difftime(datesent, prevdatesent,units="days"))]

#get rid of duplicates
# we are losing messages this way which means the code we have used may not be correct
#msglogs <- msglogs[is.na(timesincelastmsg)|timesincelastmsg>0]
#when we get rid of duplicates, we end up losing some important messages
#so, maybe we should include the name of the template instead?

nrow(msglogs[msgname==prevmsgname])
sort(unique(msglogs$msgname))
sort(unique(msglogsraw$msg_name))

xtabs(~msglogs$uniqueid)
names(msglogs)
xtabs(~msglogs$timesincelastmsg)

#melt the small data set we have if we need to for the ReshapaheToWideAndMerge formula
#t2 already wide format
#t2long <- melt.data.table(t2, id.vars="uniqueid")

#valueVarsRegex and other variables after this

#merge small data set with logs and then reshape to wide
print("RESHAPE TO WIDE AND MERGE LOGS")
d <- ReshapeToWideAndMerge(
  base=t2,
  additional=msglogs,
  valueVarsRegex="^an",
  dcastFormula="uniqueid",
  mergeVars=c("uniqueid"),
  identName="uniqueid"
)
length(d$bookevent)
length(unique(d$bookevent))
nrow(data_DHIS2_Booking)
nrow(d)
ncol(d)

