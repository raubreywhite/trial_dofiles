############ Trial 2 SMS Monitoring ###############

t2 <- d[ident_TRIAL_2_and_3==T |
          ident_TRIAL_2==T,]

msglogsraw <- fread("C:/data processing/data_raw/smslogs/2020-01-20 log.csv")

msglogsraw[1]

msglogs <- msglogsraw[,c("event_id",
                      "msg_id",
                      "processed")]
setnames(msglogs, c("event_id",
                    "msg_id",
                    "processed"),
                  c("uniqueid",
                     "msgid",
                     "datesent"))

#cleaning var types
msglogs[,datesent:=stringr::str_sub(datesent,1,10)]
msglogs[,datesent:=as.Date(datesent)]

#remove duplicate message
setorder(msglogs,uniqueid,datesent)
#msglogs[,firstdatesent:=min(datesent),by=uniqueid]

#date of previous one sent
msglogs[,prevdatesent:=shift(datesent), by=uniqueid]

#do same for message id as above so we can compare them later
msglogs[,prevmsgid:=shift(msgid), by=uniqueid]

#m
msglogs[,timesincelastmsg:=as.numeric(difftime(datesent, prevdatesent,units="days"))]

#get rid of duplicates
msglogs <- msglogs[is.na(timesincelastmsg)| timesincelastmsg>0]

unique(msglogs$msgid)
