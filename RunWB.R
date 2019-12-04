###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

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

##################
##################
# CODE ENDS HERE #
##################
##################

if(!IS_GAZA) SaveCISMACDataBase()

#MissingHBO()


####LOAD d from Network####
d <- LoadDataFileFromNetwork()
#### bookvisitspec shouldnt be known  pcnidnumber_1  amdmotherbirthdate_1
###previdnumber_1   manidnumber  riskidnumber   d$hbodaltidnum_1   hbodaltidnum_1
###anidnumber_1     labid     usid     

# in theory, this should give an avicenna dataset:
d[ident_abb==TRUE,stringr::str_subset(names(d),"^a"),with=F]

# raw AVICENNA database:
AVICENNA_Master()


