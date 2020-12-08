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

############ Trial 2 SMS Monitoring ###############

d[,andate_0:=bookdate]
d[,angestage_0:=bookgestage]
d[,anevent_0:=bookevent]

varsancevent <- names(d)[stringr::str_detect(names(d),"^anevent")]
varsancdate <- names(d)[stringr::str_detect(names(d),"^andate")]
varsancgestage <- names(d)[stringr::str_detect(names(d),"^angestage")]


# only adding bookdate here to decrease size of the data

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


# id maximum amount of things
t2[,firstvisitinT2:=as.numeric(NA)]

# need to change date back later to dec 01, 2019


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
  
  t2[i>=firstvisitinT2,(outcomevar):=as.numeric(floor(difftime(get(datevar),USorLMPdate, units="days")))]
  
}

xtabs(~TrialArm + firstvisitinT2, data=t2, addNA=T)




t2 <- t2[,c("uniqueid",
            "bookevent",
            "bookorgname",
            "bookdate",
            "bookgestage",
            "firstvisitinT2",
            "USorLMPdate",
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
                                 "USorLMPdate",
                                 "ident_dhis2_booking",
                                 "str_TRIAL_2_Cluster",
                                 "TrialArm",
                                 "wantSMS"),
            measure.vars=patterns("firstvisitinT2",
                                  "^anevent_",
                                  "^andate_",
                                  "^angestage_"), 
            value.name=c("firstvisitinT2",
                         "anevent",
                         "andate",
                         "angestage"))

nrow(long)==length(unique(long$anevent))

# manually looked into two rows and saw that bc this is dt, all missing values from measure vars were being included. 
#need to remove them from here

long <- long[!is.na(anevent)]
nrow(long)==length(unique(long$anevent))

long[,variable:=NULL]

setorder(long,uniqueid,andate)

# anything not in measure vars or values vars will be set to the id vars
# newlong <- melt(t2, measure = patterns("^anevent_", "^andate_", "^angestage_"),
# value.name = c("anevent", "andate","angestage"))[, variable := NULL][]



# tag our data 
long[,ident_dhis2:=T]
long[bookevent!=event, ident_visit:="ANC event"]
long[bookevent==event, ident_visit:="Booking event"]

long <- long[andate>="2019-12-01"]

setnames(long, "anevent","event")


setorder(long,uniqueid,andate)

long[,numDhis2visit:=1:.N, by=uniqueid]
xtabs(~long$numDhis2visit, addNA=T)

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

schedevents[,ident_schedev:=T]
setnames(schedevents,"tei", "uniqueid")

longcut <- long[andate>="2019-12-01"]

z <- merge(schedevents,
           longcut,
           by=c("event","uniqueid"),
           all=T)

setorder(z,uniqueid,eventdate)

# look at ident_dhis2_booking==0 and ident_schedev==NA
# these are ppc only
# bookevents start with INT, INT12666

# unify bookdates between the two sources
z[,bookdate:=bookdate.y]
z[is.na(bookdate.y) & !is.na(bookdate.x),bookdate:=bookdate.x]

#z[,bookdate.x:=NULL]
#z[,bookdate.y:=NULL]


z[,gAatdueDate:=as.numeric(difftime(dueDate,
                         USorLMPdate,
                         units="days"))]
xtabs(~z$gAatdueDate, addNA=T)


z[,gAateventdate:=as.numeric(difftime(eventdate,
                         USorLMPdate,
                         units="days"))]
xtabs(~z$gAateventdate, addNA=T)


z[,gAatcreateddate:=as.numeric(difftime(createddate,
                         USorLMPdate,
                         units="days"))]

xtabs(~z$gAatcreateddate, addNA=T)



# creating bookgestagecats

z[,bookgestagecat:=cut(bookgestage,
                       breaks=c(0,14,17,22,23,28,30,33,34,37,40),
                       include.lowest=T)]






###############################
# idenfify sched & attended evs
###############################

#########
# 15-17
#########

# denom
z[(status %in% c("SCHEDULE","SKIPPED") &
     (gAatdueDate>=105 & gAatdueDate<=125) & 
    is.na(eventdate)) |
    (status %in% c("COMPLETED","ACTIVE","VISITED") &
    eventdate>createddate & 
    (gAateventdate>=105 & gAateventdate<=125 &
    gAatcreateddate>0 & gAatcreateddate<105)) &
    programStagename=="Antenatal care visit", denom_15_17:= T]

xtabs(~z$denom_15_17, addNA=T)

# numerator
# scheduled only
z[(status %in% c("SCHEDULE","SKIPPED") & gAatdueDate>=105 & gAatdueDate<=125) & 
    is.na(eventdate) &
    programStagename=="Antenatal care visit",num_15_17:=F]


# those who attended
z[(status %in% c("ACTIVE", "COMPLETED") & 
     eventdate>createddate) & (gAateventdate>=105 & gAateventdate<=125) &
    (gAatcreateddate>0 & gAatcreateddate<105) &
  programStagename=="Antenatal care visit",num_15_17:=T]

xtabs(~z$num_15_17, addNA=T)


#########
# 18-22
#########

# denom
z[(status %in% c("SCHEDULE","SKIPPED") &
     (gAatdueDate>=126 & gAatdueDate<=160) & 
     is.na(eventdate)) |
    (status %in% c("COMPLETED","ACTIVE","VISITED") &
       eventdate>createddate & 
       (gAateventdate>=126 & gAateventdate<=160 &
          gAatcreateddate>0 & gAatcreateddate<126)) &
    programStagename=="Antenatal care visit", denom_18_22:= T]

xtabs(~z$denom_18_22, addNA=T)

# numerator
# scheduled only
z[(status %in% c("SCHEDULE","SKIPPED") & gAatdueDate>=126 & gAatdueDate<=160) &
    programStagename=="Antenatal care visit",num_18_22:=F]


# those who attended
z[(status %in% c("ACTIVE", "COMPLETED") & 
     eventdate>createddate) & (gAateventdate>=126 & gAateventdate<=160) & 
    programStagename=="Antenatal care visit",num_18_22:=T]


xtabs(~z$num_18_22, addNA=T)

#########
# 24-28
#########

# denom
z[((status=="SCHEDULE" & gAatdueDate>=168 & gAatdueDate<=202) |
     ((status %in% c("ACTIVE", "COMPLETED") & 
         eventdate>createddate)) & (gAateventdate>=168 & gAateventdate<=202)) & 
    programStagename=="Antenatal care visit",denom_24_28:=T]

xtabs(~z$denom_24_28, addNA=T)


# numerator
# scheduled only
z[(status=="SCHEDULE" & gAatdueDate>=168 & gAatdueDate<=202),num_24_28:=F]


# those who attended
z[(status %in% c("ACTIVE", "COMPLETED") & 
     eventdate>createddate) & (gAateventdate>=168 & gAateventdate<=202) & 
    programStagename=="Antenatal care visit",num_24_28:=T]


xtabs(~z$num_24_28, addNA=T)


#########
# 31-33
#########
# denom
z[((status=="SCHEDULE" & gAatdueDate>=217 & gAatdueDate<=237) |
     ((status %in% c("ACTIVE", "COMPLETED") & 
         eventdate>createddate)) & (gAateventdate>=217 & gAateventdate<=237)) & 
    programStagename=="Antenatal care visit",denom_31_33:=T]


xtabs(~z$denom_31_33, addNA=T)



# numerator
# scheduled only
z[(status=="SCHEDULE" & gAatdueDate>=217 & gAatdueDate<=237),num_31_33:=F]


# those who attended
z[(status %in% c("ACTIVE", "COMPLETED") & 
     eventdate>createddate) & (gAateventdate>=217 & gAateventdate<=237) & 
    programStagename=="Antenatal care visit",num_31_33:=T]

xtabs(~z$num_31_33, addNA=T)



#########
# 35-37
#########
# denom
z[((status=="SCHEDULE" & gAatdueDate>=245 & gAatdueDate<=265) |
     ((status %in% c("ACTIVE", "COMPLETED") & 
         eventdate>createddate)) & (gAateventdate>=245 & gAateventdate<=265)) & 
    programStagename=="Antenatal care visit",denom_35_37:=T]


xtabs(~z$denom_35_37, addNA=T)

# numerator
# scheduled only
z[(status=="SCHEDULE" & gAatdueDate>=245 & gAatdueDate<=265),num_35_37:=F]


# those who attended
z[(status %in% c("ACTIVE", "COMPLETED") & 
     eventdate>createddate) & (gAateventdate>=245 & gAateventdate<=265) & 
    programStagename=="Antenatal care visit",num_35_37:=T]

xtabs(~z$num_35_37, addNA=T)




###############################
# selecting data set
###############################





t2events <- z[(is.na(eventdate)|
                 createddate!= eventdate) & 
                (dueDate >= "2019-12-01" & dueDate <="2020-03-22") & 
                !is.na(TrialArm)]


## add high risk clinics to structural sheet ##
#proportions per pregnancy



###############################
# proportions per woman
###############################
# all scheduled visits
t2events[,denomperpreg:=as.numeric(NA)]

# all attended visits
t2events[,numperpreg:=as.numeric(NA)]




saveRDS(t2,file.path(FOLDER_DATA_CLEAN,"t2_eReg.rds"))
saveRDS(longcut,file.path(FOLDER_DATA_CLEAN,"t2_long.rds"))
saveRDS(schedevents,file.path(FOLDER_DATA_CLEAN,"eventsdata.rds"))
saveRDS(z,file.path(FOLDER_DATA_CLEAN,"mergedfile.rds"))








