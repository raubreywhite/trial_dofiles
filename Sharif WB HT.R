######### SHARIF #########
######### WB HT ##########
###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

library(readxl)
library(tidyverse)
library(data.table)
library(openxlsx)
library(dplyr)
library(compareDF)
library(janitor)
library(scales)

###### SETUP ENDS ######
#loading in raw data
e <- fread("C:/data processing/data_raw/e.reg-intervention/2020-02-06/Clinical Booking Visit.csv", encoding="UTF-8")

###################

setDT(e)
e[,bookorgname:=`Organisation unit name`]
e[,`Organisation unit name`:=NULL]
e[,hypertension:=`ANC Systolic blood pressure (mmHg)`]
e[,`ANC Systolic blood pressure (mmHg)`:=NULL]
e[,bookdate:=`Event date`]
e[,`Event date`:=NULL]
BD<-e$bookdate
gsub("00:00:00.0","",BD)
BD<-as.Date(BD)
e$bookdate<-BD
e[,January:=as.Date(bookdate, "%m/%d/%Y")]
a<-e[January>="2020-01-01" & January<="2020-01-31",]

#############

a[,HT:=as.numeric()]
vars <- names(a)[stringr::str_detect(names(a),"hypertension")]
for(i in vars){a[is.na(get(i)),HT:=0]}
for(i in vars){a[get(i)<0,HT:=0]}
for(i in vars){a[get(i)>49,HT:=1]}


a[HT==0, HTT:="Miss"]
a[HT==1, HTT:="Avail"]

Accu_HT<-(a[!is.na(bookorgname),
               c("bookorgname",
                 "HTT")])


XTBSU<-as.data.frame.matrix(xtabs(~bookorgname+HTT,data=Accu_HT,addNA = TRUE))

XTBSU$Total<-(XTBSU$Avail+XTBSU$Miss)
XTBSU$XPer<-(percent(XTBSU$Avail/XTBSU$Tot))
XTBSU$Miss=NULL
XTBSU
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))
sum(XTBSU$Tot)

openxlsx::write.xlsx(XTBSU,file.path(FOLDER_DATA_RESULTS,"XTBUWBHT.xlsx"))
openxlsx::write.xlsx(XTBSU,file.path(FOLDER_DATA_RESULTS,"XTBUWBHT.xlsx"))


############END###############