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

library(readxl)
library(tidyverse)
library(data.table)
library(openxlsx)
library(dplyr)
library(compareDF)
library(janitor)
library(scales)

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
unique(d$bookorgdistrict)

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

A1<-(d[!is.na(bookorgname),c("bookorgname", "bookorgdistrict", "bookyearmonth", "dob", "uniqueid" )])
A2<-(d %>% select(starts_with("angestage")))
A3<-(d %>% select(starts_with("labhb_")))
A4<-(d %>% select(starts_with("labgestage")))
A5<-(d %>% select("bookgestage"))
A6<-(d %>% select(starts_with("labanpp")))
A7<-(c(A1, A2, A3, A4, A5, A6))
A8<-(as.data.table(A7))

ACCU_HB<-(A8[bookyearmonth>="2019"])

ACCU_HB[labgestage_1>=24 & labgestage_1<=28 & labhb_1<=17 & labhb_1>=1 ,Result_1:="OK"]
ACCU_HB[labgestage_1>=24 & labgestage_1<=28 & (labhb_1>17 | labhb_1<1) ,Result_1:="Bad"]
ACCU_HB[labgestage_1<24 | labgestage_1>28 ,Result_1:="No Need"]
ACCU_HB[is.na(labgestage_1) ,Result_1:="No Need"]

ACCU_HB[labgestage_2>=24 & labgestage_2<=28 & labhb_2<=17 & labhb_2>=1 ,Result_2:="OK"]
ACCU_HB[labgestage_2>=24 & labgestage_2<=28 & (labhb_2>17 | labhb_2<1) ,Result_2:="Bad"]
ACCU_HB[labgestage_2<24 | labgestage_2>28 ,Result_2:="No Need"]
ACCU_HB[is.na(labgestage_2) ,Result_2:="No Need"]

ACCU_HB[labgestage_3>=24 & labgestage_3<=28 & labhb_3<=17 & labhb_3>=1 ,Result_3:="OK"]
ACCU_HB[labgestage_3>=24 & labgestage_3<=28 & (labhb_3>17 | labhb_3<1) ,Result_3:="Bad"]
ACCU_HB[labgestage_3<24 | labgestage_3>28 ,Result_3:="No Need"]
ACCU_HB[is.na(labgestage_3) ,Result_3:="No Need"]

ACCU_HB[labgestage_4>=24 & labgestage_4<=28 & labhb_4<=17 & labhb_4>=1 ,Result_4:="OK"]
ACCU_HB[labgestage_4>=24 & labgestage_4<=28 & (labhb_4>17 | labhb_4<1) ,Result_4:="Bad"]
ACCU_HB[labgestage_4<24 | labgestage_4>28 ,Result_4:="No Need"]
ACCU_HB[is.na(labgestage_4) ,Result_4:="No Need"]

ACCU_HB[labgestage_5>=24 & labgestage_5<=28 & labhb_5<=17 & labhb_5>=1 ,Result_5:="OK"]
ACCU_HB[labgestage_5>=24 & labgestage_5<=28 & (labhb_5>17 | labhb_5<1) ,Result_5:="Bad"]
ACCU_HB[labgestage_5<24 | labgestage_5>28 ,Result_5:="No Need"]
ACCU_HB[is.na(labgestage_5) ,Result_5:="No Need"]

ACCU_HB[labgestage_6>=24 & labgestage_6<=28 & labhb_6<=17 & labhb_6>=1 ,Result_6:="OK"]
ACCU_HB[labgestage_6>=24 & labgestage_6<=28 & (labhb_6>17 | labhb_6<1) ,Result_6:="Bad"]
ACCU_HB[labgestage_6<24 | labgestage_6>28 ,Result_6:="No Need"]
ACCU_HB[is.na(labgestage_6) ,Result_6:="No Need"]

ACCU_HB[labgestage_7>=24 & labgestage_7<=28 & labhb_7<=17 & labhb_7>=1 ,Result_7:="OK"]
ACCU_HB[labgestage_7>=24 & labgestage_7<=28 & (labhb_7>17 | labhb_7<1) ,Result_7:="Bad"]
ACCU_HB[labgestage_7<24 | labgestage_7>28 ,Result_7:="No Need"]
ACCU_HB[is.na(labgestage_7) ,Result_7:="No Need"]

ACCU_HB[labgestage_8>=24 & labgestage_8<=28 & labhb_8<=17 & labhb_8>=1 ,Result_8:="OK"]
ACCU_HB[labgestage_8>=24 & labgestage_8<=28 & (labhb_8>17 | labhb_8<1) ,Result_8:="Bad"]
ACCU_HB[labgestage_8<24 | labgestage_8>28 ,Result_8:="No Need"]
ACCU_HB[is.na(labgestage_8) ,Result_8:="No Need"]

ACCU_HB[labgestage_9>=24 & labgestage_9<=28 & labhb_9<=17 & labhb_9>=1 ,Result_9:="OK"]
ACCU_HB[labgestage_9>=24 & labgestage_9<=28 & (labhb_9>17 | labhb_9<1) ,Result_9:="Bad"]
ACCU_HB[labgestage_9<24 | labgestage_9>28 ,Result_9:="No Need"]
ACCU_HB[is.na(labgestage_9) ,Result_9:="No Need"]

ACCU_HB[labgestage_10>=24 & labgestage_10<=28 & labhb_10<=17 & labhb_10>=1 ,Result_10:="OK"]
ACCU_HB[labgestage_10>=24 & labgestage_10<=28 & (labhb_10>17 | labhb_10<1) ,Result_10:="Bad"]
ACCU_HB[labgestage_10<24 | labgestage_10>28 ,Result_10:="No Need"]
ACCU_HB[is.na(labgestage_10) ,Result_10:="No Need"]

ACCU_HB[,Resultz:=as.character()]
vars <- names(ACCU_HB)[stringr::str_detect(names(ACCU_HB),"^Result_")]
for(i in vars){ACCU_HB[get(i)=="OK",Resultz:=get(i)]}
for(i in vars){ACCU_HB[is.na(Resultz)& get(i)=="Bad",Resultz:=get(i)]}
for(i in vars){ACCU_HB[is.na(Resultz)& get(i)=="No Need",Resultz:=get(i)]}

ACCU_HB[bookgestage>=24 & bookgestage<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_1>=24 & angestage_1<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_2>=24 & angestage_2<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_3>=24 & angestage_3<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_4>=24 & angestage_4<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_5>=24 & angestage_5<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_6>=24 & angestage_6<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_7>=24 & angestage_7<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_8>=24 & angestage_8<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_9>=24 & angestage_9<=28 & Resultz=="No Need" ,Resultz:="Bad"]
ACCU_HB[angestage_10>=24 & angestage_10<=28 & Resultz=="No Need" ,Resultz:="Bad"]

Resultx<-(ACCU_HB[,c("Result_1", "Result_2", "Result_3", "Result_4", "Result_5", 
                     "Result_6", "Result_7", "Result_8", "Result_9", "Result_10")])

xtabs(~ACCU_HB$Resultz)

xtabs(~Resultz+bookorgdistrict,data=ACCU_HB,addNA = TRUE)

XXX<-as.data.frame.matrix(xtabs(~bookorgdistrict+Resultz,data=ACCU_HB,addNA = FALSE))
setDF(XXX)
XXX$`No Need`=NULL
XXX$Tot<-(XXX$OK+XXX$Bad)
XXX$Per<-(percent(XXX$OK/XXX$Tot))
XXX
openxlsx::write.xlsx(ACCU_HB,file.path(FOLDER_DATA_RESULTS,"WTF.xlsx"))
