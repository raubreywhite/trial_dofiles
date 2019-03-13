###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_dataquality", list.files("r_dataquality", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

d <- LoadDataFileFromNetworkPal()
###### SETUP ENDS ######

Analyse_DataQualityVars(d=d,location="wb")
#Analyse_DataQualityVars(d=d,location="gaza")

Analyse_AutoDuplications(d=d,location="wb")
#Analyse_AutoDuplications(d=d,location="gaza")

Analyse_NumberOfBookingsPerMonth(d=d, location="pal")

Bookdate_vs_booklmp(d=d, location="wb")

# what you care about
DataCompletion()