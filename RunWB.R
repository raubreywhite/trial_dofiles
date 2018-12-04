###### SETUP STARTS ######

tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

####################
####################
# CODE STARTS HERE #
####################
####################

# make this TRUE if you want to include the PPC
d <- CleanAllData(includePPC=FALSE, IS_GAZA=FALSE)
xtabs(~d$matching, addNA=T)

####################
#NOW run HBO stuff##

SaveAllDataFiles(d)

WBAnalyses(d=LoadDataFileFromNetwork())

##################
##################
# CODE ENDS HERE #
##################
##################

#if(!IS_GAZA) SaveCISMACDataBase()

MissingHBO()


####
# PLACE OF DELIVERY INFORMATION CHECKING
# LATER ON, PUT THIS AUTOMATICALLY IN AN EXCEL REPORT
d <- LoadDataFileFromNetwork()
#### bookvisitspec shouldnt be known  pcnidnumber_1  amdmotherbirthdate_1
###previdnumber_1   manidnumber  riskidnumber   d$hbodaltidnum_1   hbodaltidnum_1
###anidnumber_1     labid     usid     


