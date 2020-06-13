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
###Date Test###
nrow(d[bookyearmonth>=2019])
nrow(d[bookyearmonth>=2020])
nrow(d[bookyearmonth>=2021])
nrow(d[bookyearmonth>="2020-01"])
nrow(d[bookyearmonth>="2020-02"])
nrow(d[bookyearmonth>="2020-03"])
nrow(d[bookyearmonth>="2020-05"])
##

d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="JERU","Jerusalem")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="TULK","Tulkarem")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="Tulk","Tulkarem")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="H","Hebron")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="Q","Qalqilya")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="R","Ramallah")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="N","Nablus")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="NH","North Hebron")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="SH","South Hebron")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="J","Jenin")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="JERICHO","Jericho")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="B","Bethlehem")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="TUBAS","Tubas")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="S","Salfit")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="Y","Yatta")
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="N balata",NA)
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="BookedasPalestine",NA)
d$bookorgdistrict<-replace(d$bookorgdistrict, d$bookorgdistrict=="Test",NA)
unique(d$bookorgdistrict)

####
d[,labhbxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labhb_")]
for(i in vars){d[is.na(get(i)),labhbxxx:=0]}
for(i in vars){d[(get(i)<17),labhbxxx:=get(i)]}
for(i in vars){d[!is.na(get(i)),labhbxxx:=1]}

d[labhbxxx==0, labhbTF1:="Miss"]
d[labhbxxx==1, labhbTF1:="Avail"]

Accu_HB<-(d[!is.na(bookorgname),
            c("bookorgname", "bookorgdistrict", "bookyearmonth", "labhbTF1")])
ACCU_HB2 <- (Accu_HB[bookyearmonth >= "2020-05",
            c("bookorgname", "bookorgdistrict", "bookyearmonth", "labhbTF1")])

xtabs(~bookorgdistrict+labhbTF1,data=ACCU_HB2,addNA = FALSE)
XTBSHB<-as.data.frame.matrix(xtabs(~bookorgdistrict+labhbTF1,data=ACCU_HB2,addNA = FALSE))

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