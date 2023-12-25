###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=TRUE)

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

nrow(d)

vars <- names(d)[stringr::str_detect(names(d),"^labrh_")]


xtabs(~d$labrh_1, addNA=T)

#d[ident_dhis2_booking==1, rhneg:=FALSE]

for(i in vars){
 
  d[get(i)=="POS", rhneg:=FALSE]
  d[get(i)=="NEG", rhneg:=TRUE]
  
}

xtabs(~d$rhneg)

tab <- d[ident_dhis2_booking==T,.(N=.N,
                                  rhneg=sum(rhneg==TRUE, na.rm=TRUE),
                                  rhpos=sum(rhneg==FALSE, na.rm=TRUE)),
                                  keyby=.(bookyear)]

tab[,denom:=sum(rhneg,
                rhpos),
    by=bookyear]
tab[,percRhNeg:=100*(rhneg/denom)]
tab[,percRhpos:=100*(rhpos/denom)]

t <- tab[bookyear>2016, c("bookyear",
                          "rhneg",
                          "rhpos",
                          "denom",
                          "percRhNeg",
                          "percRhpos")]

if(IS_GAZA==F){
openxlsx::write.xlsx(t,
                     file.path(FOLDER_DATA_RESULTS,
                               "guidelines",
                               "rh_negative_proportions.xlsx"))


} else {
  
  openxlsx::write.xlsx(t,
                       file.path(FOLDER_DATA_RESULTS_GAZA,
                                  "rh_negative_proportions.xlsx"))
  
}