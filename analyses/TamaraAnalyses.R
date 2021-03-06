#RColorBrewer::display.brewer.all() 
###### SETUP STARTS
###################
###################
###################

# define our dates
CLINIC_INTERVENTION_DATE <- "2018-09-27"
CLINIC_CONTROL_DATE <- "2018-09-27"

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
                     "openssl",
                     "fmsb"
)
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)


# from net but the above already in R

library(data.table)
library(ggplot2)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)...WE CAN deal it as library but it doesnot

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
### SETUP ENDS

# Load in datafile

d <- LoadDataFileFromNetwork()
sink()
sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "TRIAL_1_Numbers.txt"))
cat("\nNumber of Women in TRIAL\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T])

cat("\nNumber of Women in ARM A\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T &
         ident_dhis2_control==T])

cat("\nNumber of Women in ARM B\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T &
         ident_dhis2_control==F])


#Mahima's stuff
#Numbers by age categories
cat("\nAge Cat_ALL women in Trial_1\n")
Mabstract <- d[ident_TRIAL_1==TRUE,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)

cat("\nAge Cat_ALL women in Trial_1_ARM A\n")
Mabstract <- d[ident_TRIAL_1==TRUE&
              ident_dhis2_control==T,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)


cat("\nAge Cat_ALL women in Trial_1_ARM B\n")
Mabstract <- d[ident_TRIAL_1==TRUE&
                 ident_dhis2_control==F,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)








