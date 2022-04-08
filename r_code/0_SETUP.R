Setup <- function(IS_GAZA=FALSE){
  assign("IS_GAZA",IS_GAZA,envir = .GlobalEnv)
  # REFERENCE DATES FOR VARIABLE NAMES
  REF_GAZA_CLINIC_INTERVENTION_DATE <- "2019-09-25"
  REF_GAZA_CLINIC_CONTROL_DATE<- "2019-09-25"
  
  REF_WB_CLINIC_INTERVENTION_DATE <- "2019-03-04"
  REF_WB_CLINIC_CONTROL_DATE <- "2019-03-04"
  
  # define our date
  GAZA_CLINIC_INTERVENTION_DATE <- "2020-10-01"
  GAZA_CLINIC_CONTROL_DATE<- "2020-10-01"

  
  #since we are finished with control data for trial 1, we can use the export below.
  WB_CLINIC_INTERVENTION_DATE <- "2022-03-08"
    #"2021-08-12"
  WB_CLINIC_CONTROL_DATE <- "2019-07-07"
  
  FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")
  FOLDER_DATA_CLEAN_WB <<- file.path(getwd(),"../data_clean")
  
  FOLDER_DATA_RAW_GAZA <<- file.path(getwd(),"../gaza_data_raw")
  FOLDER_DATA_RAW_WB <<- file.path(getwd(),"../data_raw")
  
  FOLDER_DATA_RESULTS_GAZA <<- file.path(getwd(),"../gaza_results")
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
  
  # MERETT/TAMARA CHECK OUT IF THIS IS ACTUALLY CORRECT
  #FOLDER_DROPBOX <- file.path("~","..","eRegistry CRCT Dropbox")
  # MERVETT/TAMARA DELETE THIS ONCE DROPBOX IS INSTALLED
  FOLDER_DROPBOX <<- "C:/data processing/FAKE_DROPBOX/eRegistry CRCT Dropbox"
  
  FOLDER_DROPBOX_RESULTS_GAZA <<- file.path(
    FOLDER_DROPBOX,
    "Data management eRegQual",
    "Results_From_PNIPH",
    "Gaza_Results",
    lubridate::today())
  
  FOLDER_DROPBOX_RESULTS_WB <<- file.path(
    FOLDER_DROPBOX,
    "Data management eRegQual",
    "Results_From_PNIPH",
    "WB_Results",
    lubridate::today())
  
  if(IS_GAZA){
    REF_CLINIC_INTERVENTION_DATE <<- REF_GAZA_CLINIC_INTERVENTION_DATE
    REF_CLINIC_CONTROL_DATE <<- REF_GAZA_CLINIC_CONTROL_DATE
    
    CLINIC_INTERVENTION_DATE <<- GAZA_CLINIC_INTERVENTION_DATE
    CLINIC_CONTROL_DATE <<- GAZA_CLINIC_CONTROL_DATE
    
    FOLDER_DATA_RAW <<- file.path(getwd(),"../gaza_data_raw")
    FOLDER_DATA_CLEAN <<- FOLDER_DATA_CLEAN_GAZA
    FOLDER_DATA_RESULTS <<- FOLDER_DATA_RESULTS_GAZA
    FOLDER_DATA_MBO <<- file.path(getwd(),"../gaza_results/mbo_r/")
    FOLDER_DROPBOX_RESULTS <<- FOLDER_DROPBOX_RESULTS_GAZA
    
  } else {
    REF_CLINIC_INTERVENTION_DATE <<- REF_WB_CLINIC_INTERVENTION_DATE
    REF_CLINIC_CONTROL_DATE <<- REF_WB_CLINIC_CONTROL_DATE
    
    CLINIC_INTERVENTION_DATE <<- WB_CLINIC_INTERVENTION_DATE
    CLINIC_CONTROL_DATE <<- WB_CLINIC_CONTROL_DATE
    
    FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")
    FOLDER_DATA_CLEAN <<- FOLDER_DATA_CLEAN_WB
    FOLDER_DATA_RESULTS <<- FOLDER_DATA_RESULTS_WB
    FOLDER_DATA_MBO <<- file.path(getwd(),"../results/mbo_r/")
    FOLDER_DROPBOX_RESULTS <<- FOLDER_DROPBOX_RESULTS_WB
    
  }
  
  FOLDER_DATA_RESULTS_PAL <<- file.path(getwd(),"../pal_results/")
  FOLDER_DROPBOX_RESULTS_PAL <<- file.path(
    FOLDER_DROPBOX,
    "Data management eRegQual",
    "Results_From_PNIPH",
    "Pal_Results",
    lubridate::today())
  
 
  # load libraries
  library(data.table)
  library(ggplot2)
  #library(tidyverse)
  

  
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
  suppressWarnings({
    dir.create(FOLDER_DROPBOX_RESULTS_PAL)
    dir.create(FOLDER_DROPBOX_RESULTS_WB)
    dir.create(FOLDER_DROPBOX_RESULTS_GAZA)
    
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_PAL,"data_quality"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_WB,"data_quality"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_GAZA,"data_quality"))
    
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_PAL,"trial_2"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_WB,"trial_2"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS_GAZA,"trial_2"))
    
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"booking_descriptives"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"hbo_completeness"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"pniph"))
    
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","random"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","trial_1"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"pniph","abstracts_2018"))
  })
  
}