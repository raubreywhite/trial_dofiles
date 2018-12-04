Setup <- function(IS_GAZA=FALSE){
  assign("IS_GAZA",IS_GAZA,envir = .GlobalEnv)
  # define our dates
  GAZA_CLINIC_INTERVENTION_DATE <- "2018-04-02"
  #GAZA_CLINIC_CONTROL_DATE <- "2018-09-27"
  
  WB_CLINIC_INTERVENTION_DATE <- "2018-11-29"
  WB_CLINIC_CONTROL_DATE <- "2018-11-29"
  
  FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")
  FOLDER_DATA_CLEAN_WB <<- file.path(getwd(),"../data_clean")
  
  if(IS_GAZA){
    CLINIC_INTERVENTION_DATE <<- GAZA_CLINIC_INTERVENTION_DATE
    CLINIC_CONTROL_DATE <<- GAZA_CLINIC_CONTROL_DATE
    
    FOLDER_DATA_RAW <<- file.path(getwd(),"../gaza_data_raw")
    FOLDER_DATA_CLEAN <<- file.path(getwd(),"../gaza_data_clean")
    FOLDER_DATA_RESULTS <<- file.path(getwd(),"../gaza_results/")
    FOLDER_DATA_MBO <<- file.path(getwd(),"../gaza_results/mbo_r/")
    FOLDER_DROPBOX_RESULTS <<- file.path(
      "~",
      "..",
      "eRegistry CRCT Dropbox",
      "Data management eRegQual",
      "Results_From_PNIPH",
      "Gaza_Results",
      lubridate::today())
  } else {
    CLINIC_INTERVENTION_DATE <<- WB_CLINIC_INTERVENTION_DATE
    CLINIC_CONTROL_DATE <<- WB_CLINIC_CONTROL_DATE
    
    FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")
    FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")
    FOLDER_DATA_RESULTS <<- file.path(getwd(),"../results/")
    FOLDER_DATA_MBO <<- file.path(getwd(),"../results/mbo_r/")
    FOLDER_DROPBOX_RESULTS <<- file.path(
      "~",
      "..",
      "eRegistry CRCT Dropbox",
      "Data management eRegQual",
      "Results_From_PNIPH",
      "WB_Results",
      lubridate::today())
  }
  
  
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
                       "arabicStemR"
  )
  for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)
  
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
  suppressWarnings({
    dir.create(FOLDER_DROPBOX_RESULTS)
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"booking_descriptives"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"hbo_completeness"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"data_quality"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"pniph"))
    
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","random"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","trial_1"))
    dir.create(file.path(FOLDER_DROPBOX_RESULTS,"pniph","abstracts_2018"))
  })
  
}