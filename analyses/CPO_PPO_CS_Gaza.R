###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

###### SETUP ENDS ######
#loading in raw data
currentpregout <- fread("C:/data processing/data_raw/e.reg-interventionGaza/2019-01-08/Clinical Current pregnancy outcome.csv", encoding="UTF-8")

for (i in names(currentpregout)){
  setnames(currentpregout, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])
  
}


#renaming variables
setnames(currentpregout,"previousplaceofbirth", "PODCURRENT")
setnames(currentpregout,"ancmodeofpreviousdelivery", "MODCURRENT")


sink()
sink("C:/data processing/a research hRHR/presenting data/CPO_Crosstabs_Gaza.txt")

# extracting data

currentpregout[,.(N=.N), keyby=.(PODCURRENT)]

xtabs(~currentpregout$PODCURRENT, addNA = T)

currentpregout[,.(N=.N), keyby=.(MODCURRENT)]

xtabs(~currentpregout$MODCURRENT, addNA = T)

currentpregout[,.(N=.N), keyby=.(MODCURRENT,PODCURRENT)]

setorder(currentpregout)

length(unique(currentpregout$event))
length(currentpregout$event)

xtabs(~currentpregout$PODCURRENT+currentpregout$MODCURRENT, addNA = T)

sink()


#loading in prev preg sheet
prevpregout <- fread("C:/data processing/data_raw/e.reg-interventionGaza/2019-01-08/Clinical Previous pregnancies.csv", encoding="UTF-8")

setnames(prevpregout,"Previous place of birth", "PODprev")
setnames(prevpregout,"ANC Mode of previous delivery", "MODprev")

xtabs(~prevpregout$PODprev, addNA = T)
xtabs(~prevpregout$MODprev, addNA = T)
xtabs(~prevpregout$PODprev+prevpregout$MODprev, addNA = T)


sink()
sink("C:/data processing/a research hRHR/presenting data/CPO_Crosstabs_fulldataset_Gaza.txt")

# extracting data

#######GAZA########
cpo_analysis <- d[ident_dhis2_control==F,]
cpo_analysis[,.(N=.N), keyby=.(cpoplaceofbirth_1)]

xtabs(~cpo_analysis$cpoplaceofbirth_1, addNA = T)

cpo_analysis[,.(N=.N), keyby=.(cpomodedelivery_1)]

xtabs(~cpo_analysis$cpomodedelivery_1, addNA = T)

cpo_analysis[,.(N=.N), keyby=.(cpomodedelivery_1,cpoplaceofbirth_1)]

xtabs(~cpo_analysis$cpoplaceofbirth_1+cpo_analysis$cpomodedelivery_1, addNA = T)


####NBC####
NBC<- fread("C:/data processing/data_raw/e.reg-interventionGaza/2019-01-08/Clinical Newborn care.csv", encoding="UTF-8")

length(unique(NBC$Event))
length(NBC$Event)



#####Gaza_PPC####

PPC <- fread("C:/data processing/data_raw/e.reg-interventionGaza/2019-01-08/Clinical Postpartum care.csv", encoding="UTF-8")

length(unique(PPC$Event))
length(PPC$Event)


sink()

