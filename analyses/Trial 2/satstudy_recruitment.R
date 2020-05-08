### not to be run on server ###

# set working directory

setwd("C:/Users/Mervett_Isbeih/sat_study")
getwd()

#setting up folders 
FOLDER_SAT_RESULTS <<-file.path("C:/Users/Mervett_Isbeih/sat_study/sat_results")
FOLDER_SAT_DATA_CLEAN <<-file.path("C:/Users/Mervett_Isbeih/sat_study/sat_data_clean")



#idenfifying packages we want  
desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64",
                     "haven",
                     "lubridate",
                     "ggplot2",
                     "irr",
                     "rel",                      
                     "gridExtra",
                     "openssl",
                     "fmsb",
                     "ICC",
                     "arabicStemR",
                     "lme4",
                     "fs",
                     "fancycut"
                     
)


for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)
library(readxl)

#load in key data set
skey<-fread("C:/Users/Mervett_Isbeih/sat_study/sat_data/T2_key.csv", encoding = "UTF-8")

# Load in data for this week
satresults<-fread("C:/Users/Mervett_Isbeih/sat_study/sat_data/raw.csv", encoding="UTF-8")
nrow(satresults)

sat <- merge(skey,satresults, by="cliniccode", all.y = TRUE)
nrow(sat)

sat <- setDT(sat)
nrow(sat)


########### cleaning variales ########### 

## adjusting variable structures and some basic cleaning
setnames(sat,"cliniccode", "clustercode")

# yes or no questions
sat[, q9a:=as.logical(NA)]
sat[q9=="no", q9a:=FALSE]
sat[q9=="yes", q9a:=TRUE]
setnames(sat,"q9a","leavehome")
#sat[,leavehome:=q9a]

sat[,q10a:=as.logical(NA)]
sat[q10=="no", q10a:=FALSE]
sat[q10=="yes", q10a:=TRUE]
setnames(sat,"q10a","primipreg")
#sat[,q10a:=primipreg]

setnames(sat,"q11","bookgAmonth")
#sat[bookgAmonth:=q11]

sat[, q12a:=as.logical(NA)]
sat[q12=="no", q12a:=FALSE]
sat[q12=="yes", q12a:=TRUE]
setnames(sat,"q12a","usother")
#sat[,usother:=q12a]

sat[, q13a:=as.logical(NA)]
sat[q13=="no", q13a:=FALSE]
sat[q13=="yes", q13a:=TRUE]
setnames(sat,"q13a","ancother")
#sat[,ancother:=q13a]

sat[, q14a:=as.logical(NA)]
sat[q14=="no", q14a:=FALSE]
sat[q14=="yes", q14a:=TRUE]
setnames(sat,"q14a","refHR")
#sat[,refHR:=q14a]

vars <- c("q15",
          "q16",
          "q17",
          "q18",
          "q19",
          "q20",
          "q21",
          "q22",
          "q23",
          "q24",
          "q25",
          "q26",
          "q27",
          "q28",
          "q29",
          "q30",
          "q31",
          "q32",
          "q33",
          "q34",
          "q35",
          "q36",
          "q37",
          "q38",
          "q39",
          "q40")

# ident variables
sat[,T2:=as.logical(NA)]
sat[ident_TRIAL_2=="Y", T2:=TRUE]

sat[,T3:=as.logical(NA)]
sat[ident_TRIAL_3=="Y", T3:=TRUE]

sat[,T2T3:=as.logical(NA)]
sat[ident_TRIAL_2_and_3=="Y", T2T3:=TRUE]

sat[,T2T3control:=as.logical(NA)]
sat[ident_TRIAL_2_3_Control=="Y", T2T3control:=TRUE]


### outliers ###
xtabs(~sat$educyears, addNA=TRUE)
xtabs(~sat$district, addNA=T)

### cleaning day end via time end ### 
sat[,dayend:=timeend]
sat[,dayend:=stringr::str_remove_all(as.character(dayend)," [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$")]
sat[,dayend:=stringr::str_remove_all(as.character(dayend)," [0-9][0-9]:[0-9][0-9]$")]
sat[,dayend:=stringr::str_remove_all(as.character(dayend)," [0-9][0-9]$")]
sat[,dayend:=stringr::str_remove_all(as.character(dayend)," [0-9]:[0-9][0-9]$")]

library(lubridate)
sat[,dayend:=mdy(dayend)]

### cleaning end time ### 
sat[,endtime:=timeend]
unique(sat$endtime)

sat[,endtime:=stringr::str_remove_all(endtime,"^[0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] ")]
sat[,endtime:=stringr::str_remove_all(endtime,"^[0-9]/[0-9]/[0-9][0-9][0-9][0-9] ")]
unique(sat$endtime)



### age categories ### 
# need to calculate age first
# change birthyear to birthdate and subtract from todays date to get years
unique(sat$birthyear)

sat[,birthyearDate:=lubridate::ymd(birthyear, truncated = 2L)]
unique(sat$birthyearDate)

sat[,age:=floor(as.numeric(difftime(lubridate::today(),birthyearDate, units="days")/365.25))]
unique(sat$age)

sat[,agecat:=cut(age,
                 breaks=c(0,20,24,29,34,39,100),
                 include.lowest=T)]
xtabs(~sat$agecat, addNA = T)

### educ categories ###
sat[,educat:=cut(educyears,
                 breaks=c(-1,0,6,12,16,25),
                 include.lowest=T)]
xtabs(~sat$educat)

### educ level ###
sat[,edulevel:=as.character(NA)]
sat[educat=="[-1,0]", edulevel:="None"]
sat[educat=="(0,6]", edulevel:="Primary"]
sat[educat=="(6,12]", edulevel:="Secondary"]
sat[educat=="(12,16]", edulevel:="College or University"]
sat[educat=="(16,25]", edulevel:="After college or university"]

### gestational age at booking ### (q11)
# these are months in pregnancy, change to weeks
sat[,bookgestage:=as.numeric(NA)]
sat[!is.na(bookgAmonth),bookgestage:=4*bookgAmonth]

# bookgAmonthcat
sat[,bookgAmonthcat:=cut(bookgAmonth,
                         breaks=c(0,3,6,10),
                         include.lowest=T)]
xtabs(~sat$bookgAmonthcat,addNA=T)

# attendance
setnames(sat,"q15","attend_allanc")
setnames(sat,"q16","attend_testdiab")
setnames(sat,"q17","attend_testanemia")
setnames(sat,"q18","attend_testhtn")
setnames(sat,"q19","attend_fg")

# visit
setnames(sat,"q20","visit_schedvisitconfid")
setnames(sat,"q21","visit_waittime")
setnames(sat,"q22","visit_healthstaff")
setnames(sat,"q23","visit_testpurpose")
setnames(sat,"q24","visit_testgA")
setnames(sat,"q25","visit_recommend")
setnames(sat,"q26","visit_returnnextpreg")
setnames(sat,"q27","visit_satisfaction")


# worry 
setnames(sat,"q28","worry_housing")
setnames(sat,"q29","worry_money")
setnames(sat,"q30","worry_partner")
setnames(sat,"q31","worry_family")
setnames(sat,"q32","worry_ownhealth")
setnames(sat,"q33","worry_otherhealth")
setnames(sat,"q34","worry_employment")
setnames(sat,"q35","worry_baby")
setnames(sat,"q36","worry_stillbirth")
setnames(sat,"q37","worry_hospital")
setnames(sat,"q38","worry_internalexam")
setnames(sat,"q39","worry_givingbirth")
setnames(sat,"q40","worry_coping")


################ Anonymized Data Set ################ 

# Choose only first 4 if more than four
sat <- sat[order(clustercode,dayend)]
sat[eligibility=="agree" & withdraw!="yes",SampNum:=1:.N, by=.(clustercode)]

# Make a second variable to include only first four women
sat[,instudy:=as.logical(NA)]
sat[eligibility=="agree" & withdraw!="yes" & SampNum<=4,instudy:=TRUE]
sat[eligibility=="agree" & withdraw!="yes" & SampNum>4, instudy:= FALSE]

## need to anonymize, code different arms (control and not control), and rename variables
sat[,exposure:=as.character(NA)]
sat[T2T3control==T, exposure:="A"]
sat[T2==T|T3==T, exposure:="B"]
xtabs(~sat[eligibility=="agree" & withdraw!="yes"]$exposure=="B", addNA=T)

nocalls<-sat[withdraw!="yes",.(N=.N,
                               Control=sum(exposure=="A"),
                               Intervention=sum(exposure=="B")),
             keyby=.(eligibility)]

# collector code
sat[,collectorcode:=as.character(NA)]
sat[collector=="naila", collectorcode:="A"]
sat[collector=="entisar", collectorcode:="B"]
sat[collector=="khadija", collectorcode:="C"]
sat[collector=="najah", collectorcode:="D"]

### vars for anonymization data set
varskeep <- c("SampNum",
              "instudy",
              "clustercode",
              "exposure",
              "collectorcode",
              "clinicsize",
              "timestarted",
              "timeend",
              "dayend",
              "endtime",
              "samplnum",
              "eligibility",
              "herphone",
              "district",
              "agecat",
              "educat",
              "edulevel",
              "leavehome",
              "primipreg",
              "bookgAmonth",
              "bookgAmonthcat",
              "usother",
              "ancother",
              "refHR",
              "attend_allanc",
              "attend_testdiab",
              "attend_testanemia",
              "attend_testhtn",
              "attend_fg",
              "visit_schedvisitconfid",
              "visit_waittime",
              "visit_healthstaff",
              "visit_testpurpose",
              "visit_testgA",
              "visit_recommend",
              "visit_returnnextpreg",
              "visit_satisfaction",
              "worry_housing",
              "worry_money",
              "worry_partner",
              "worry_family",
              "worry_ownhealth",
              "worry_otherhealth",
              "worry_employment",
              "worry_baby",
              "worry_stillbirth",
              "worry_hospital",
              "worry_internalexam",
              "worry_givingbirth",
              "worry_coping",
              "callbackanothertime",
              "callended_1",
              "callended_2",
              "callended_3",
              "withdraw")

satKeep <- sat[,varskeep, with=F]
satKeep <- setDT(satKeep)
nrow(satKeep)


openxlsx::write.xlsx(satKeep, 
                     file.path(FOLDER_SAT_DATA_CLEAN,
                               sprintf("Sat_data_clean_%s.xlsx",lubridate::today())))


background <- c("agecat",
                "edulevel",
                "bookgAmonthcat",
                "leavehome",
                "primipreg",
                "refHR",
                "usother",
                "ancother")                 



smallD<-satKeep[instudy==T,c("exposure",background), with=F]

long <- melt.data.table(smallD, 
                        id.vars=c("exposure"),variable.factor = F)

uglytable <- long[,
                  .(
                    N=.N,
                    control=sum(exposure=="A"),
                    intervention=sum(exposure=="B")),
                  keyby=.(
                    variable,value)]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_SAT_RESULTS,
                       "freqtabs",
                       sprintf("Background_%s.xlsx", lubridate::today())))


################ Primary outcome ################

primary <- names(satKeep)[stringr::str_detect(names(satKeep),"^worry_")]

smallD<-satKeep[instudy==T,c("exposure",primary), with=F]

long <- melt.data.table(smallD, 
                        id.vars=c("exposure"),variable.factor = F)

uglytable <- long[,
                  .(
                    N=.N,
                    mean=round(mean(value, na.rm=T),digits=2),
                    sd=round(sd(value, na.rm=T),digits=2)),
                  keyby=.(exposure,variable)]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_SAT_RESULTS,
                       "freqtabs",
                       sprintf("%s_Primary_outcomes(rounded).xlsx", lubridate::today())))

# confidence intervals
#t.test(var1~exposure, data=long, conf.level=0.95)
#t.test(var2~exposure, data=long, conf.level=0.95)
#t.test(var3~exposure, data=long, conf.level=0.95)

#variables_to_test <- c("worry_baby", "worry_coping", "worry_mondy")
retval <- vector("list", length=length(primary))
for(i in seq_along(retval)){
  var_of_interest <- primary[i]
  formula <- glue::glue("{var_of_interest} ~ exposure")
  fit <- t.test(as.formula(formula), data = satKeep, conf.level=0.95)
  ### extract results here
  #temp <- data.frame(conf_level=fit$conf.int, var = var_of_interest)
  temp <- data.frame(conf_level_l95=fit$conf.int[1], 
                     conf_level_u95=fit$conf.int[2],
                     statistic=fit$statistic,var = var_of_interest)
  retval[[i]] <- temp
}
retval <- rbindlist(retval)


openxlsx::write.xlsx(retval, 
                     file.path(
                       FOLDER_SAT_RESULTS,
                       "freqtabs",
                       sprintf("%s_Primary_outcomes_Confidence_Intervals.xlsx", lubridate::today())))





################ Frequency Tables ################ 

#### use satkeep for analysis
# vars for frequency tables 

freqvars <- c("attend_allanc",
              "attend_testdiab",
              "attend_testanemia",
              "attend_testhtn",
              "attend_fg",
              "visit_schedvisitconfid",
              "visit_waittime",
              "visit_healthstaff",
              "visit_testpurpose",
              "visit_testgA",
              "visit_recommend",
              "visit_returnnextpreg",
              "visit_satisfaction",
              "worry_housing",
              "worry_money",
              "worry_partner",
              "worry_family",
              "worry_ownhealth",
              "worry_otherhealth",
              "worry_employment",
              "worry_baby",
              "worry_stillbirth",
              "worry_hospital",
              "worry_internalexam",
              "worry_givingbirth",
              "worry_coping")


smallD<-sat[instudy==T,c("exposure",
                         freqvars),
            with=F
            ]

long <- melt.data.table(smallD, id.vars=c(
  "exposure"
),variable.factor = F)



uglytable <- long[,
                  .(
                    not_NA=sum(!is.na(value)),
                    value0=sum(value==0,na.rm=T),
                    value1=sum(value==1, na.rm=TRUE),
                    value2=sum(value==2, na.rm=T),
                    value3=sum(value==3, na.rm=T),
                    value4=sum(value==4, na.rm=T),
                    value5=sum(value==5, na.rm=T),
                    Missing=sum(is.na(value))
                    
                  ),
                  keyby=.(
                    variable,
                    exposure)
                  ]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_SAT_RESULTS,
                       "freqtabs",
                       sprintf("frequencies_%s.xlsx", lubridate::today())))



############ Completeness Report and Numbers For data extraction ############ 

#completeness report
DQ <- sat[eligibility=="agree" & withdraw!="yes",.(
  N=.N,
  herphone=sum(herphone=="yes"|herphone=="no", na.rm=T),
  district=sum(!is.na(district)),
  birthyear=sum(!is.na(birthyear)),
  educyears=sum(!is.na(educyears)),
  #meanEdu=mean(q11, na.rm = T),
  notmissing_q9=sum(!is.na(q9a)),
  q9T=sum(q9a==T, na.rm=T),
  q9F=sum(q9a==F, na.rm=T),
  notmissing_q10=sum(!is.na(q10a)),
  q10T=sum(q10a==T, na.rm=T),
  q10F=sum(q10a==F, na.rm=T),
  notmissing_q11=sum(!is.na(q11)),
  notmissing_q12=sum(!is.na(q12a)),
  q12T=sum(q12a==T, na.rm=T),
  q13T=sum(q13a==T, na.rm=T),
  q13F=sum(q13a==F, na.rm=T),
  notmissing_q14=sum(!is.na(q14a)),
  q14T=sum(q14a==T, na.rm=T),
  notmissing_q15=sum(!is.na(q15)),
  notmissing_q16=sum(!is.na(q16)),
  notmissing_q17=sum(!is.na(q17)),
  notmissing_q18=sum(!is.na(q18)),
  notmissing_q19=sum(!is.na(q19)),
  notmissing_q20=sum(!is.na(q20)),
  notmissing_q21=sum(!is.na(q21)),
  notmissing_q22=sum(!is.na(q22)),
  notmissing_q23=sum(!is.na(q23)),
  notmissing_q24=sum(!is.na(q24)),
  notmissing_q25=sum(!is.na(q25)),
  notmissing_q26=sum(!is.na(q26)),
  notmissing_q27=sum(!is.na(q27)),
  notmissing_q28=sum(!is.na(q28)),
  notmissing_q29=sum(!is.na(q29)),
  notmissing_q30=sum(!is.na(q30)),
  notmissing_q31=sum(!is.na(q31)),
  notmissing_q32=sum(!is.na(q32)),
  notmissing_q33=sum(!is.na(q33)),
  notmissing_q34=sum(!is.na(q34)),
  notmissing_q35=sum(!is.na(q35)),
  notmissing_q36=sum(!is.na(q36)),
  notmissing_q37=sum(!is.na(q37)),
  notmissing_q38=sum(!is.na(q38)),
  notmissing_q39=sum(!is.na(q39)),
  notmissing_q40=sum(!is.na(q40)))]

openxlsx::write.xlsx(DQ,file.path(FOLDER_SAT_RESULTS,
                                  sprintf("%s_Completeness_Report.xlsx",
                                          lubridate::today())))


############ Data Extraction Reports ############            

# creating weekly report
satcounts <- sat[,.(N=.N,
                    agree=sum(eligibility=="agree" & withdraw!="yes", na.rm=T),
                    disagree=sum(eligibility=="disagree", na.rm=T),
                    cantcontact=sum(eligibility=="cantcontact", na.rm=T),
                    ineligible=sum(eligibility=="ineligiblegA", na.rm=T),
                    agreebutwithdraw=sum(eligibility=="agree" & 
                                           withdraw=="yes")),
                 keyby=.(weeknum,clustercode)]

openxlsx::write.xlsx(satcounts,file.path(FOLDER_SAT_RESULTS,
                                         sprintf("%s_satcounts_clinic.xlsx",
                                                 lubridate::today())))


satcountsTotal <- sat[,.(N=.N,
                         agree=sum(eligibility=="agree" & withdraw!="yes", na.rm=T),
                         disagree=sum(eligibility=="disagree", na.rm=T),
                         cantcontact=sum(eligibility=="cantcontact", na.rm=T),
                         ineligible=sum(eligibility=="ineligiblegA", na.rm=T),
                         agreebutwithdraw=sum(eligibility=="agree" & 
                                                withdraw=="yes")),
                      keyby=.(clustercode)]


openxlsx::write.xlsx(satcountsTotal,file.path(FOLDER_SAT_RESULTS,
                                              sprintf("%s_satcounts_clinicTotals.xlsx",
                                                      lubridate::today())))


removed <- sat[withdraw=="yes" & eligibility=="agree",.(N=.N), keyby=.(weeknum,clustercode)]


# ID clinics with less than 4 to send out
sendout <- sat[withdraw!="yes" & eligibility=="agree",]
sendout <-sendout[,.(N=.N), keyby=.(clustercode)]
sendout <- sendout[is.na(N) | N<4,]

openxlsx::write.xlsx(sendout,file.path(FOLDER_SAT_RESULTS,
                                       sprintf("%s_send out list.xlsx",
                                               lubridate::today())))


# results by data extractor
collectornums <- sat[,.(N=.N,
                        agree=sum(eligibility=="agree" &
                                    withdraw!="yes", na.rm=T),
                        disagree=sum(eligibility=="disagree", na.rm=T),
                        cantcontact=sum(eligibility=="cantcontact", na.rm=T),
                        ineligible=sum(eligibility=="ineligiblegA", na.rm=T),
                        withdraw=sum(withdraw=="yes", na.rm=T)),
                     keyby=.(collector)]



        