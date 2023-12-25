


###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

fileSources = file.path("cs_code", list.files("cs_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)


FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")

FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")

FOLDER_DATA_RESULTS <<- file.path(getwd(),"../results")

# say which packages we want, and install if neccessary
# (note, this should really only happen once)
desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64",
                     "haven",
                     "lubridate",
                     "ggplot2",
                     "irr",
                     "rel",                      
                     "gridExtra",
                     "openssl",
                     "fmsb",
                     "ICC",
                     "arabicStemR",
                     "lme4",
                     "fs",
                     "fancycut"
)


for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

# load libraries
library(data.table)
library(ggplot2)


#since we are finished with control data for trial 1, we can use the export below.
CLINIC_INTERVENTION_DATE <- "2023-03-07"
#"2021-08-12"
CLINIC_CONTROL_DATE <- "2019-07-07"
DATE <<- lubridate::today()
DATA_DATE <<- min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)

weekyear <<- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
yearmonth <<- sprintf("%s-%s",
                      lubridate::year(lubridate::today()),
                      lubridate::month(lubridate::today()))


########################


# load in and create our data set #
avi <- CS_Master(keepMotherID=NULL, includeObs=TRUE)
xtabs(~avi$avicennanum, addNA=T)

avi[avicennanum>5, c(1:5)]
avi[motheridno %in% c(401403936,
                      854843901), c("motheridno","amddate_1","abbbabybirthresult_1","abbbabybirthdate_1")]


nrow(avi[is.na(abbbabybirthresult_1)])

# problems
# lab (b) is missing an id number
# set up is incorrect, problem when read files from the cs folder
# 












