###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

CheckFilesAndVariables(folder="e.reg-intervention")

###### SETUP ENDS ######

####################
####################
# CODE STARTS HERE #
####################
####################

# make this TRUE if you want to include the PPC
d <- CleanAllData(includePPC=FALSE, IS_GAZA=TRUE)
nrow(d)
####################
#NOW run HBO stuff##

SaveAllDataFiles(d)

GazaAnalyses(d=LoadDataFileFromNetwork())

##################
##################
# CODE ENDS HERE #
##################
##################

####
# PLACE OF DELIVERY INFORMATION CHECKING
# LATER ON, PUT THIS AUTOMATICALLY IN AN EXCEL REPORT
d <- LoadDataFileFromNetwork()
CreatingFurtherVariablesPNIPH(d)
CreatingFurtherVariablesNormal(d)

IndicatorsOsloANCVisits(d)
IndicatorsOsloGenerate(d)
IndicatorsOsloRandom(d)

#### bookvisitspec shouldnt be known  pcnidnumber_1  amdmotherbirthdate_1
###previdnumber_1   manidnumber  riskidnumber   d$hbodaltidnum_1   hbodaltidnum_1
###anidnumber_1     labid     usid     


