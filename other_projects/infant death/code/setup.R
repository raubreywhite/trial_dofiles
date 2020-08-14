Setup <- function(){
  assign("IS_GAZA",IS_GAZA,envir = .GlobalEnv)
  #
 
  FOLDER_DATA_CLEAN_WB <<- file.path(getwd(),"../data_clean")

  FOLDER_DATA_RAW_WB <<- file.path(getwd(),"../data_raw")
  
  FOLDER_DATA_RESULTS_WB <<- file.path(getwd(),"../results")
  
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
 
    
    FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")
    FOLDER_DATA_CLEAN <<- FOLDER_DATA_CLEAN_WB
    FOLDER_DATA_RESULTS <<- FOLDER_DATA_RESULTS_WB
    FOLDER_DATA_MBO <<- file.path(getwd(),"../results/mbo_r/")
    FOLDER_DROPBOX_RESULTS <<- FOLDER_DROPBOX_RESULTS_WB
    
 
  
  # load libraries
  library(data.table)
  library(ggplot2)
  
  # date stuff
  MAX_YEAR <<- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
  MAX_MONTH <<- substr(CLINIC_CONTROL_DATE,6,7)
  
  DATE <<- lubridate::today()
  DATA_DATE <<- min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)
  
  weekyear <<- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
  yearmonth <<- sprintf("%s-%s",
                        lubridate::year(lubridate::today()),
                        lubridate::month(lubridate::today()))
  
  # CREATE FOLDERS
  
  
}