###### SETUP STARTS ######

tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup()

FOLDER_DROPBOX_RESULTS <<- file.path(
  "~",
  "..",
  "eRegistry CRCT Dropbox",
  "Data management eRegQual",
  "Results_From_PNIPH",
  "Gaza_Results",
  lubridate::today())

###### SETUP ENDS ######

d <- CleanAllData(includePPC=T,
                  minBookDate="2017-01-01",
                  maxBookDate="2018-09-01",
                  delete=c("^lab",
                           "^man"
                  ),
                  IS_GAZA=FALSE)

#d <- LoadDataFileFromNetworkGaza()
#d <- LoadDataFileFromNetworkWB()

# DO ANALYSES HERE