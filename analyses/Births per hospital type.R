###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_dataquality", list.files("r_dataquality", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

###### SETUP ENDS ######

d <- LoadDataFileFromNetwork()

xtabs(~d$matching+d$matching_private)
#d[matching=="Avicenna" & matching_private==T]

####want to know how many births per hospital
uglytable <- d[
  ident_TRIAL_1==T & 
    !is.na(merged_namehospbirth) ,
  .(
    N=.N
  ),
  keyby=.(
    matching,
    matching_private,
    bookorgdistrict,
    merged_namehospbirth
    )]

openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DROPBOX_RESULTS,"births_per_hospital.xlsx")
)

