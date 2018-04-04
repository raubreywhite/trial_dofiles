
CLINIC_INTERVENTION_DATE <- "2018-03-04"
CLINIC_CONTROL_DATE <- "2018-03-18"

tryCatch({
  setwd("X:/data processing/")
}, error=function(err){
  setwd("Z:/data processing/")
})

desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64")
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()



isControl <- TRUE

data_DHIS2_Demographics <- DHIS2_Demographics()





