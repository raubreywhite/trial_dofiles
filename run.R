#RColorBrewer::display.brewer.all() 
###### SETUP STARTS
###################
###################
###################

# define our dates
CLINIC_INTERVENTION_DATE <- "2018-07-26"
CLINIC_CONTROL_DATE <- "2018-07-26"

# define the folders
tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})
FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
FOLDER_DATA_RESULTS <- file.path(getwd(),"../results/")
FOLDER_DATA_MBO <- file.path(getwd(),"../results/mbo_r/")
FOLDER_DROPBOX_RESULTS <- file.path(
  "~",
  "..",
  "eRegistry CRCT Dropbox",
  "Data management eRegQual",
  "Results_From_PNIPH",
  "Results",
  lubridate::today())


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
                     "openssl"
                     )
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)
library(ggplot2)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
# make sure that all of the file sources go DIRECTLY
# into the **global** environment
sapply(fileSources, source, .GlobalEnv)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()
DATA_DATE <- min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)

weekyear <- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
yearmonth <- sprintf("%s-%s",
        lubridate::year(lubridate::today()),
        lubridate::month(lubridate::today()))

# CREATE FOLDERS

dir.create(FOLDER_DROPBOX_RESULTS)
dir.create(file.path(FOLDER_DROPBOX_RESULTS,"hbo_completeness"))
dir.create(file.path(FOLDER_DROPBOX_RESULTS,"booking_descriptives"))
dir.create(file.path(FOLDER_DROPBOX_RESULTS,"data_quality"))
dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima"))

dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","random"))
dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","trial_1"))


#dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1"))
#dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1","demographics"))
#dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1","random_indicators"))
#dir.create(file.path(FOLDER_DROPBOX_RESULTS,"indicators_for_mahima"))


###################
###################
###################
###### SETUP ENDS
###################
###################
###################

####################
####################
# CODE STARTS HERE #
####################
####################

# make this TRUE if you want to include the PPC
d <- CleanAllData(includePPC=FALSE)

####################
#NOW run HBO stuff##

SaveAllDataFiles(d)

Analyses(d=LoadDataFileFromNetwork())

##################
##################
# CODE ENDS HERE #
##################
##################

MissingHBO()


####
# PLACE OF DELIVERY INFORMATION CHECKING
# LATER ON, PUT THIS AUTOMATICALLY IN AN EXCEL REPORT
d <- LoadDataFileFromNetwork()
#xtabs(~d$cpoplaceofbirth_1+d$ppcplaceofdelivery_1,addNA=T)
####
