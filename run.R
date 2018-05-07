CLINIC_INTERVENTION_DATE <- "2018-03-04"
CLINIC_CONTROL_DATE <- "2018-03-18"

if(.Platform$OS.type=="unix"){
  RAWmisc::InitialiseOpinionatedUnix(project="code_major/2018/pniph_ereg")
  setwd("/git/trial_dofiles")
  FOLDER_DATA_RAW <- RAWmisc::PROJ$RAW
  FOLDER_DATA_CLEAN <- RAWmisc::PROJ$SHARED_TODAY
} else {
  tryCatch({
    setwd("X:/data processing/trial_dofiles")
  }, error=function(err){
    setwd("Z:/data processing/trial_dofiles")
  })
  FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
  FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
}

desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64",
                     "haven")
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()

dhis=suppressWarnings(DHIS2_Master())
dhis <- dhis[ident_dhis2_booking==1]
keepMotherID <- unique(dhis$motheridno)
avicenna=AVICENNA_Master(keepMotherID=keepMotherID, includeObs = FALSE)

nrow(dhis)
d <- MergeDHISToAVICENNA(
  dhis=dhis,
  avicenna=avicenna)
nrow(d)

d[,avicennanum:=NULL]
d[,minDate:=NULL]

nrow(d)
d <- MergeDHISToAVICENNA(
  dhis=d,
  avicenna=HBO_Master())
nrow(d)

lists <- c()
for(i in 1:ncol(d)) if(is.list(d[[i]])) lists <- c(lists,i)
for(i in lists){
  n <- names(d)[i]
  d[,(n):=as.character(get(n))]
}

saveRDS(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
haven::write_dta(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.dta"))





tokeep <- d[
  isExpectedToHaveDelivered==TRUE &
  is.na(d$ident_avic_any) &
  is.na(d$ident_dhis2_dhis2hbo) &
  is.na(d$ident_hbo),
  c(
    "motheridno",
    "bookorgname",
    "bookdate",
    "firstname",
    "fathername",
    "middlename",
    "familyname1",
    "familyname2",
    "husbandname",
    "dob",
    "mobile",
    "booklmp",
    "village",
    "bookintendbirth",
    "bookrecobirth",
    "calc_expected_due_delivery"
    )]

openxlsx::write.xlsx(tokeep,file.path(FOLDER_DATA_CLEAN,"missing_birth_outcomes.xlsx"))

