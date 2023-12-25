
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



####LOAD d from Network####
d <- LoadDataFileFromNetwork()


# number bookorgs

length(unique(d$bookorgname))

bon <- unique(d$bookorgname)




lrbn <- unique(d$bookorgname)[!stringr::str_detect(unique(d$bookorgname),"^hr")]
length(lrbn)

# number anorgs

ancorgs <- stringr::str_detect(names)

# number ppcorgs