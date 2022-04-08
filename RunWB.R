###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

#CheckFilesAndVariables(folder="e.reg-intervention")
#CheckFilesAndVariables(folder="e.reg-control")
CheckFilesAndVariables(folder="e.reg-intervention", 
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)
CheckFilesAndVariables(folder="e.reg-control", 
                       REF_DATE = REF_CLINIC_CONTROL_DATE, 
                       CHECK_DATE = CLINIC_CONTROL_DATE)


###### SETUP ENDS ######

####################
####################
# CODE STARTS HERE #
####################
####################

# data base cleaning make this TRUE if you want to include the PPC
d <- CleanAllData(includePPC=T, IS_GAZA=FALSE)
xtabs(~d$matching, addNA=T)

nrow(d)
xtabs(~d$ident_bad_all)

#####################
#NOW run HBO stuff##

SaveAllDataFiles(d)

WBAnalyses(d=LoadDataFileFromNetwork())
nrow(d)

##################
##################
# CODE ENDS HERE #
##################
##################

# did those two functions  for Trial 1, BUT dont need it anymore
#if(!IS_GAZA) SaveCISMACDataBase()

#MissingHBO()


####LOAD d from Network####
d <- LoadDataFileFromNetwork()
nrow(d)    

# in theory, this should give an avicenna dataset:
d[ident_abb==TRUE,stringr::str_subset(names(d),"^a"),with=F]

# raw AVICENNA database:
AVICENNA_Master()

openxlsx::write.xlsx(d[ident_TRIAL_1==T & ident_dhis2_control==F,c("uniqueid")], 
                     FOLDER_DATA_RESULTS,
                     "trial1intervention_missinguid.xlsx" 
                     
)


