######### SHARIF #########
######### Gaza HT ########
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
g <- fread("C:/data processing/gaza_data_raw/e.reg-intervention/2020-02-02/Clinical Booking Visit.csv", encoding="UTF-8")
###################

###################
setDT(g)
g[,bookorgname:=`Organisation unit name`]
g[,`Organisation unit name`:=NULL]
g[,hypertension:=`ANC Systolic blood pressure (mmHg)`]
g[,`ANC Systolic blood pressure (mmHg)`:=NULL]
g[,bookdate:=`Event date`]
g[,`Event date`:=NULL]
g[,January:=as.Date(bookdate, "%m/%d/%Y")]
b<-g[January>="2020-01-01" & January<="2020-01-31",]

#############

b[,HT:=as.numeric()]
vars <- names(b)[stringr::str_detect(names(b),"hypertension")]
for(i in vars){b[is.na(get(i)),HT:=0]}
for(i in vars){b[get(i)<0,HT:=0]}
for(i in vars){b[get(i)>49,HT:=1]}


b[HT==0, HTT:="Miss"]
b[HT==1, HTT:="Avail"]

Accu_HT_GZ<-(b[!is.na(bookorgname),
            c("bookorgname",
              "HTT")])


XTBSU_GZ<-as.data.frame.matrix(xtabs(~bookorgname+HTT,data=Accu_HT_GZ,addNA = TRUE))

XTBSU_GZ$Total<-(XTBSU_GZ$Avail+XTBSU_GZ$Miss)
XTBSU_GZ$XPer<-(percent(XTBSU_GZ$Avail/XTBSU_GZ$Tot))
XTBSU_GZ$Miss=NULL
XTBSU_GZ
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))
sum(XTBSU$Tot)

openxlsx::write.xlsx(XTBSU_GZ,file.path(FOLDER_DATA_RESULTS,"XTBUGZHT.xlsx"))


############END###############