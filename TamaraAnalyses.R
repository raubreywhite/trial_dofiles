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

Mahimastaff
Mabstract <- d[ident_TRIAL_1==TRUE,
               .(numWomen=.N),
               
               keyby= agecat]




nrow(d[ident_dhis2_control==F &ident_dhis2_booking==T])
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T])


vars <- names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")]

d[,labhb_x:=0]

for(i in vars){
  d[get(i)==1, labhb_x:=labhb_x+1]
}

sum(d[ident_dhis2_control==F]$labhb_x,na.rm=T)





vars <- names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")]

d[,usevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

sum(d[ident_dhis2_control==F]$usevent_x,na.rm=T)

#HYPERTENTION
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")]

d[,anbpsyst_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, anbpsyst_x:=anbpsyst_x + 1]
}

sum(d[ident_dhis2_control==F]$anbpsyst_x,na.rm=T)

# 

#xtabs(~d$ident_dhis2_control + d$ident_dhis2_booking)   


#diabetes






# urinary tract infections






#PPC
d <- CleanAllData(includePPC=T,
                  minBookDate="2017-01-01",
                  maxBookDate="2018-09-27",
                  delete=c("^lab",
                           "^us",
                           "^man",
                           "^risk",
                           "^an",
                           "^book"
                            ))

d <- d[
  ident_dhis2_ppc==1 &
  ident_dhis2_control==F]


#As of 2017, the MCH e Registry contains data on 24,832 registered antenatal care visits, 
#18,374 postpartum care visits and 16,409 newborn care visits. From antenatal care, 
#data on core process indicators is available on screening of anemia        hypertension,
#diabetes                and urinary tract infections.


