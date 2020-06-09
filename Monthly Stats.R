##Monthly Stats##

###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

#CheckFilesAndVariables(folder="e.reg-intervention")
#CheckFilesAndVariables(folder="e.reg-control")
CheckFilesAndVariables(folder="e.reg-intervention", 
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)
CheckFilesAndVariables(folder="e.reg-control", 
                       REF_DATE = REF_CLINIC_CONTROL_DATE, 
                       CHECK_DATE = CLINIC_CONTROL_DATE)

d <- LoadDataFileFromNetwork()
###### SETUP ENDS ######

nrow(d[bookyearmonth>=2019])
nrow(d[bookyearmonth>=2020])
nrow(d[bookyearmonth>=2021])
nrow(d[bookyearmonth>="2020-01"])
nrow(d[bookyearmonth>="2020-02"])
nrow(d[bookyearmonth>="2020-03"])
nrow(d[bookyearmonth>="2020-04"])

d[,labhbxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labhb_")]
for(i in vars){d[is.na(get(i)),labhbxxx:=0]}
for(i in vars){d[(get(i)<17),labhbxxx:=get(i)]}
for(i in vars){d[!is.na(get(i)),labhbxxx:=1]}

d[labhbxxx==0, labhbTF1:="Miss"]
d[labhbxxx==1, labhbTF1:="Avail"]

Accu_HB<-(d[!is.na(bookorgname),
            c("bookorgname",
              "labhbTF1")])

xtabs(~bookorgname+labhbTF1,data=Accu_HB,addNA = TRUE)
XTBSHB<-as.data.frame.matrix(xtabs(~bookorgname+labhbTF1,data=Accu_HB,addNA = TRUE))

library(scales)

XTBSHB$Tot<-(XTBSHB$Avail+XTBSHB$Miss)
XTBSHB$XPer<-(percent(XTBSHB$Avail/XTBSHB$Tot))
XTBSHB$XPern<-(XTBSHB$Avail/XTBSHB$Tot)
XTBSHB$Miss=NULL
XTBSHB <- XTBSHB[order(XTBSHB$XPern),]
XTBSHB <- XTBSHB[c("XPer", "Tot", "Avail")]
XTBSHB
percent((sum(XTBSHB$Avail))/(sum(XTBSHB$Tot)))

d$labhbxxx

nrow(d[bookyearmonth>="2020-03"])
nrow(d[labhbxxx])
nrow(d[labhbxxx==1])
nrow(d[labhbxxx==1 & bookyearmonth>="2020-03"])
M<-(d[labhbxxx & bookyearmonth>="2020-03"])