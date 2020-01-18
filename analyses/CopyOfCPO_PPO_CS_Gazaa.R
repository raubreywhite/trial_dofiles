###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

library(readxl)
library(tidyverse)
library(data.table)
library(openxlsx)
library(dplyr)
library(compareDF)

###### SETUP ENDS ######
#loading in raw data
d <- fread("C:/data processing/gaza_data_raw/e.reg-intervention/2020-01-15/Clinical Lab results.csv", encoding="UTF-8")
e <- fread("C:/data processing/gaza_data_raw/e.reg-intervention/2020-01-15/Clinical Booking Visit.csv", encoding="UTF-8")
c<-(left_join(d, e))

###################

setDT(c)
c[,bookorgname:=`Organisation unit name`]
c[,`Organisation unit name`:=NULL]
c[,laburglub:=`LAB Urine stick; sugar`]
c[,`LAB Urine stick; sugar`:=NULL]
c[,bookdate:=`Event date`]
c[,`Event date`:=NULL]
c[,December:=as.Date(bookdate, "%m/%d/%Y")]
a<-c[December>="2019-12-01" & December<="2019-12-31",]



#############



a[,laburglu4:=as.numeric()]
vars <- names(a)[stringr::str_detect(names(a),"laburglub")]
for(i in vars){a[is.na(get(i)),laburglu4:=0]}
for(i in vars){a[get(i) %in% c("POS","NEG"),laburglu4:=1]}

a[laburglu4==0, laburgluTF1:="Miss"]
a[laburglu4==1, laburgluTF1:="Avail"]

Accu_URGLU<-(a[!is.na(bookorgname),
               c("bookorgname",
                 "laburgluTF1")])


XTBSU<-as.data.frame.matrix(xtabs(~bookorgname+laburgluTF1,data=Accu_URGLU,addNA = TRUE))

library(scales)

XTBSU$Total<-(XTBSU$Avail+XTBSU$Miss)
XTBSU$XPer<-(percent(XTBSU$Avail/XTBSU$Tot))
XTBSU$Miss=NULL
XTBSU
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))
sum(XTBSU$Tot)

openxlsx::write.xlsx(XTBSU,file.path(FOLDER_DATA_RESULTS,"XTBUGZ.xlsx"))


############END###############