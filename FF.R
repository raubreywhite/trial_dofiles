### Clinical Results
###### SETUP STARTS ######

library(readxl)
library(tidyverse)
library(data.table)
library(openxlsx)
library(dplyr)
library(compareDF)
library(janitor)
library(scales)

setwd("C:/data processing/data_raw/e.reg-intervention/2020-06-01")

Q <- read.csv ("Clinical Lab results.csv")

setwd("C:/data processing/results")

######

Q1<-(Q[!is.na(ANC_Gestational.age.at.visit..weeks) & 
         ANC_Gestational.age.at.visit..weeks>=1 & 
         ANC_Gestational.age.at.visit..weeks <=40
              ,c("Organisation.unit.name", 
                   "ANC.or.PPC.visit", 
                   "ANC_Gestational.age.at.visit..weeks", 
                   "Event.date", 
                   "LAB.CBC..Hemoglobin" )])

Q1$ANC_Gestational.age.at.visit..weeks

write.csv(Q1, file = "Q1.csv", fileEncoding = "UTF-8")


