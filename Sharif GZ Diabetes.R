######### SHARIF #########
######### WB HT ##########
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
library(janitor)
library(scales)

###### SETUP ENDS ######
#loading in raw data
e <- fread("C:/data processing/gaza_data_raw/e.reg-intervention/2020-03-10/Clinical Lab results.csv", encoding="UTF-8")
###################

setDT(e)

e[,bookorgname:=`Organisation unit name`]
e[,`Organisation unit name`:=NULL]
e[,laburglu:=`LAB Urine stick; sugar`]
e[,`LAB Urine stick; sugar`:=NULL]
e[,bookdate:=`Event date`]
e[,`Event date`:=NULL]

e[,February:=as.Date(bookdate, "%m/%d/%Y")]
a<-e[February>="2020-02-01" & February<="2020-02-29",]

#############

a[,DI:=as.numeric()]
vars <- names(a)[stringr::str_detect(names(a),"laburglu")]
for(i in vars){a[is.na(get(i)),DI:=0]}
for(i in vars){a[get(i) %in% c("POS","NEG"),DI:=1]}

a[is.na(DI), DII:="Miss"]
a[DI==1, DII:="Avail"]

Accu_DI<-(a[!is.na(bookorgname),
            c("bookorgname",
              "DII")])


XTBSU<-as.data.frame.matrix(xtabs(~bookorgname+DII,data=Accu_DI,addNA = TRUE))

XTBSU$Total<-(XTBSU$Avail+XTBSU$Miss)
XTBSU$XPer<-(percent(XTBSU$Avail/XTBSU$Tot))
XTBSU$Miss=NULL
XTBSU
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))
sum(XTBSU$Tot)

openxlsx::write.xlsx(XTBSU,file.path(FOLDER_DATA_RESULTS,"XTBUGZDI.xlsx"))

GZOrg<-(sort(unique(a$bookorgname)))

openxlsx::write.xlsx(GZOrg,file.path(FOLDER_DATA_RESULTS,"GZOrg.xlsx"))

############END###############


