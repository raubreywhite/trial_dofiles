###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######
#loading in raw data
currentpregout <- fread("C:/data processing/data_raw/e.reg-intervention/2019-04-03/Clinical Current pregnancy outcome.csv", encoding="UTF-8")

for (i in names(currentpregout)){
  setnames(currentpregout, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])
  
}


#renaming variables
setnames(currentpregout,"previousplaceofbirth", "PODCURRENT")
setnames(currentpregout,"ancmodeofpreviousdelivery", "MODCURRENT")


sink()
sink("C:/data processing/a research hRHR/presenting data/CPO_Crosstabs.txt")

# extracting data

currentpregout[,.(N=.N), keyby=.(PODCURRENT)]

xtabs(~currentpregout$PODCURRENT, addNA = T)

currentpregout[,.(N=.N), keyby=.(MODCURRENT)]

xtabs(~currentpregout$MODCURRENT, addNA = T)

currentpregout[,.(N=.N), keyby=.(MODCURRENT,PODCURRENT)]

setorder(currentpregout)

xtabs(~currentpregout$PODCURRENT+currentpregout$MODCURRENT, addNA = T)

sink()


#loading in prev preg sheet
prevpregout <- fread("C:/data processing/data_raw/e.reg-intervention/2019-03-04/Clinical Previous pregnancies.csv", encoding="UTF-8")

setnames(prevpregout,"Previous place of birth", "PODprev")
setnames(prevpregout,"ANC Mode of previous delivery", "MODprev")

xtabs(~prevpregout$PODprev, addNA = T)
xtabs(~prevpregout$MODprev, addNA = T)
xtabs(~prevpregout$PODprev+prevpregout$MODprev, addNA = T)




#matching 
d <- LoadDataFileFromNetwork()
nrow(d)
nrow(d[ident_avic_acs==T])

unique(d$abbbabybirthtype_1)



# ###Number of Casearean sections
# nrow(d[abbbabybirthtype_1=="Cesarean Section (with General Anesthesia)"])
# nrow(d[abbbabybirthtype_1=="Cesarean Section (with Local Anesthesia)"])
# 
# 
# ###Number of normal Births
# nrow(d[abbbabybirthtype_1=="Normal Birth (without Epidural)"])
# nrow(d[abbbabybirthtype_1=="Normal Birth (with Epidural)"])
# nrow(d[abbbabybirthtype_1=="Normal Birth"])
# 

matchedwithAvic <- d[,.(N=.N,
                        HasCPO=sum(ident_dhis2_cpo==T)),
                       keyby=.(abbbabybirthtype_1,
                               ident_avic_any,
                                cpoplaceofbirth_1,
                                cpomodedelivery_1)]


openxlsx::write.xlsx(matchedwithAvic,"C:/data processing/a research hRHR/presenting data/CS_Avic_CPO.xlsx")



sink()
sink("C:/data processing/a research hRHR/presenting data/CPO_Crosstabs_fulldataset.txt")

# extracting data
cpo_analysis <- d[ident_dhis2_control==F,]
cpo_analysis[,.(N=.N), keyby=.(cpoplaceofbirth_1)]

xtabs(~cpo_analysis$cpoplaceofbirth_1, addNA = T)

cpo_analysis[,.(N=.N), keyby=.(cpomodedelivery_1)]

xtabs(~cpo_analysis$cpomodedelivery_1, addNA = T)

cpo_analysis[,.(N=.N), keyby=.(cpomodedelivery_1,cpoplaceofbirth_1)]



xtabs(~cpo_analysis$cpoplaceofbirth_1+cpo_analysis$cpomodedelivery_1, addNA = T)

sink()

