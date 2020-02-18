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
e <- fread("C:/data processing/gaza_data_raw/e.reg-intervention/2020-02-02/Clinical Booking Visit.csv", encoding="UTF-8")
###################
