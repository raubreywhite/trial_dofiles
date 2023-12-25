###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

CheckFilesAndVariables(folder="e.reg-intervention",
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)



###### SETUP ENDS ######

####################
####################
# CODE STARTS HERE #
####################
####################

# make this TRUE if you want to include the PPC
d <- CleanAllData(includePPC=TRUE, IS_GAZA=TRUE)
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

#############
# load data #
#############

d <- LoadDataFileFromNetwork()

# run extra variables #
CreatingFurtherVariablesNormal(d)
CreatingFurtherVariablesMahima(d)

CreatingFurtherVariablesPNIPH(d)

IndicatorsOsloANCVisits(d)
IndicatorsOsloGenerate(d)
IndicatorsOsloRandom(d)


