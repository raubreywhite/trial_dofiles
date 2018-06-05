# setttnig up folders
tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})
FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
FOLDER_RESULTS_DROPBOX <- file.path("~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results",lubridate::today())
dir.create(FOLDER_RESULTS_DROPBOX)

library(data.table)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# THIS IS HOW TO LOAD IN THE DATASET!!!!!!!!!!!!
# you could also just write in the .rds address in full
# but this way is nicer
d <- readRDS(file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
# d <- readRDS("Z:/file/file/file3/full_data_from_r.rds")

# this will print out the variables that start with "ident"
names(d)[stringr::str_detect(names(d),"^ident")]

table(d$ident_dhis2_b4_2017_01_15, useNA="always")

sink()
# sink is basically the same as log in stata
sink(file.path(FOLDER_RESULTS_DROPBOX,"numbers.txt"))

# in sprintf, %s is replaced with whatever comes afterwards
# \n means "new line"
sprintf("REPORT for %s",lubridate::today())

sprintf("Number of people: %s",nrow(d))

sprintf("Number of people booked after 2017-01-15: %s",
  nrow(d[ident_dhis2_b4_2017_01_15==FALSE]))

sprintf(
  "Number of people in phase 1 clinics after 2017-01-15: %s",
  nrow(d[ident_dhis2_b4_2017_01_15==FALSE & ident_phase1clinic==TRUE]))

sprintf(
  "Number of people in phase 1 clinics after 2017-01-15 and have expected to deliver: %s",
  nrow(d[ident_dhis2_b4_2017_01_15==FALSE & 
           ident_phase1clinic==TRUE &
         isExpectedToHaveDelivered==TRUE]))

sprintf(
  "Number of people in phase 1 clinics after 2017-01-15 and have expected to deliver and in Aviccena: %s",
  nrow(d[ident_dhis2_b4_2017_01_15==FALSE & 
           ident_phase1clinic==TRUE &
           isExpectedToHaveDelivered==TRUE &
           ident_avic_any==TRUE]))

## loop through all identifying variables and give a number
for(i in names(d)[stringr::str_detect(names(d),"^ident")]){
  num <- sum(d[[i]],na.rm=T) # by using [[]] i have completely "pulled out" the variable
  print(sprintf("Number %s: %s",i,num))
}


sink()


