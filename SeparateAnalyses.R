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
                     "openssl"
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

smalldataset <- d[ident_dhis2_control==F &
                  ident_dhis2_an==T &
                  ident_dhis2_ppc==T &
                  ident_dhis2_cpo==T &
                  bookdate>="2017-09-01" & bookdate<="2018-09-01",
              c("cpocomplicationsnone_1",
                "cpopregoutcome_1",
                "cpopuerpalsepsis_2"
               )
                  
          ]

vars_cpodvt <- names(d)[stringr::str_detect(names(d),"^cpodvt_")]
vars_cpoeclampsia <-names(d)[stringr::str_detect(names(d),"^cpoeclampsia_")]
vars_cpopreeclampsia <- names(d)[stringr::str_detect(names(d),"^cpopreeclampsia_")]
vars_cpocomplicationsnone <- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
vars_cpoantepartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]

#vars_cpopregoutcome <-names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]

smallD <- d[ident_dhis2_control==F &
              bookdate>="2017-09-01" & bookdate<="2018-09-01" &
              ident_dhis2_booking==TRUE & 
              ident_dhis2_an==TRUE & 
              ident_dhis2_cpo==TRUE & 
              ident_dhis2_ppc==TRUE,
            c(
              "bookevent",
              vars_cpodvt,
              vars_cpoeclampsia,
              vars_cpopreeclampsia,
              vars_cpocomplicationsnone,
              vars_cpoantepartumhemorrhage,
              vars_cpopostpartumhemorrhage,
              vars_cpopuerpalsepsis
            ),with=F]



smallD[,id:=1:.N]
long <- melt.data.table(smallD, id.vars=c("id"),variable.factor = F, value.factor = F)

uglytable <- long[,
                  .(
                    denominator=.N,
                    is_NA=sum(is.na(value)),
                    not_NA=sum(!is.na(value)),
                    value0=sum(value==0,na.rm=T),
                    value1=sum(value==1,na.rm=T),
                    value2=sum(value==2,na.rm=T),
                    value3=sum(value==3,na.rm=T)
                  ),
                  keyby=.(variable)
                  ]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "pniph",
                       "abstracts_2018",
                       "cpo.xlsx"))

# 
#   

nrow(smalldataset)
