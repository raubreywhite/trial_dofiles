#RColorBrewer::display.brewer.all() 
###### SETUP STARTS
###################
###################
###################

# define our dates
CLINIC_INTERVENTION_DATE <- "2018-09-27"
CLINIC_CONTROL_DATE <- "2018-09-27"

# define the folders
tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})
FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
FOLDER_DATA_RESULTS <- file.path(getwd(),"../results/")
FOLDER_DATA_MBO <- file.path(getwd(),"../results/mbo_r/")
FOLDER_DROPBOX_RESULTS <- file.path(
  "~",
  "..",
  "eRegistry CRCT Dropbox",
  "Data management eRegQual",
  "Results_From_PNIPH",
  "Results",
  lubridate::today())


# say which packages we want, and install if neccessary
# (note, this should really only happen once)
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
                     "fmsb"
)
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)


# from net but the above already in R

library(data.table)
library(ggplot2)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)...WE CAN deal it as library but it doesnot

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
# make sure that all of the file sources go DIRECTLY
# into the **global** environment
sapply(fileSources, source, .GlobalEnv)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()
DATA_DATE <- min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)

weekyear <- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
yearmonth <- sprintf("%s-%s",
                     lubridate::year(lubridate::today()),
                     lubridate::month(lubridate::today()))
### SETUP ENDS

# Load in datafile

d <- LoadDataFileFromNetwork()
sink()
sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "TRIAL_1_Numbers.txt"))
cat("\nNumber of Women in TRIAL\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T])

cat("\nNumber of Women in ARM A\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T &
         ident_dhis2_control==T])

cat("\nNumber of Women in ARM B\n")
nrow(d[bookdate>="2017-01-15"&
         bookdate<="2017-09-15"&
         ident_TRIAL_1==T &
         ident_dhis2_control==F])


#Mahima's stuff
#Numbers by age categories
cat("\nAge Cat_ALL women in Trial_1\n")
Mabstract <- d[ident_TRIAL_1==TRUE,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)

cat("\nAge Cat_ALL women in Trial_1_ARM A\n")
Mabstract <- d[ident_TRIAL_1==TRUE&
              ident_dhis2_control==T,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)


cat("\nAge Cat_ALL women in Trial_1_ARM B\n")
Mabstract <- d[ident_TRIAL_1==TRUE&
                 ident_dhis2_control==F,
               .(numWomen=.N),
               
               keyby= agecat]
print(Mabstract)





sink()




#####

#Buthaina's staff
#you should run this from sink to sink
# to save things in notepad and dont forgett to put sink at the bottom
sink()
sink(file.path(FOLDER_DATA_RESULTS,
            "SystemUpdatedNumbers.txt"))

#cat("\nNumber of women in trial\n")
#nrow(d[ident_TRIAL_1==T &
#         ident_dhis2_control==T &
#         ident_dhis2_booking==T ])
#nrow(d[ident_dhis2_control==F &ident_dhis2_booking==T])


#ANC
cat("\nNumANC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_an==T])

cat("\nNumANCvisits\n")
vars <- c(
  "bookevent",
  names(d)[stringr::str_detect(names(d),"^anevent_[0-9]*")]
)
d[,anevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), anevent_x:=anevent_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$anevent_x,na.rm=T)

#NBC
cat("\nNumNBC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T& bookdate>="2017-01-01"&
         bookdate<="2017-12-31"])
cat("\nNumNBCvisits\n")
vars <- names(d)[stringr::str_detect(names(d),"^nbcevent_[0-9]*")]
d[,nbcevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), nbcevent_x:=nbcevent_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$nbcevent_x,na.rm=T)

###Anemia
cat("\nNumber of anemia tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")]
d[,labhb_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labhb_x:=labhb_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$labhb_x,na.rm=T)



###Ultrasound
cat("\nNumber of ultrasound tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")]
d[,usevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$usevent_x,na.rm=T)

#HYPERTENTION
cat("\nNumber of antenatal blood pressure tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")]
d[,anbpsyst_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, anbpsyst_x:=anbpsyst_x + 1]
}
sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$anbpsyst_x,na.rm=T)

# 

#xtabs(~d$ident_dhis2_control + d$ident_dhis2_booking)   


#diabetes seperately
cat("\nNumber of OGCT on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^labogct_[0-9]*")]

d[,labogct_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labogct_x:=labogct_x + 1]
}

sum(d[ident_dhis2_control==F]$labogct_x,na.rm=T)


###RBS
cat("\nNumber of RBS tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^labbloodglu_[0-9]*")]
d[,labbloodglu_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labbloodglu_x:=labbloodglu_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$labbloodglu_x,na.rm=T)

###FBS
cat("\nNumber of FBS tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^labfastbloodglu_[0-9]*")]

d[,labfastbloodglu_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labfastbloodglu_x:=labfastbloodglu_x + 1]
}

sum(d[ident_dhis2_control==F]$labfastbloodglu_x,na.rm=T)



#Diabetes(OGCT, FBS,RBS TOTAL)
cat("\nNumber of DIABETES ogct, fbs, and rbs tests on system\n")
vars <- c(names(d)[stringr::str_detect(names(d),"^labogct_[0-9]*")],
          names(d)[stringr::str_detect(names(d),"^labfastbloodglu_[0-9]*")],
          names(d)[stringr::str_detect(names(d),"^labbloodglu_[0-9]*")])

d[,diabetes_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, diabetes_x:=diabetes_x + 1]
}

sum(d[ident_dhis2_control==F]$diabetes_x,na.rm=T)



# urinary tract infections
cat("\nNumber of Urinary tract infections tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^laburuti_[0-9]*")]

d[,laburuti_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, laburuti_x:=laburuti_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"&
        bookdate<="2017-12-31"]$laburuti_x,na.rm=T)
#we can trust in sink
sink()



#PPC
#d <- CleanAllData(includePPC=T,
                #  minBookDate="2017-01-01",
                 # maxBookDate="2017-09-27",
                  #delete=c("^lab",
# "^us",
#  "^man",
#  "^risk",
#  "^an",
#  "^book"
#   ))

#d <- d[
# ident_dhis2_ppc==1 &
 # ident_dhis2_control==F]





#As of 2017, the MCH e Registry contains data on 24,832 registered antenatal care visits, 
#26640 postpartum care visits and 16,409 newborn care visits. From antenatal care, 
#data on core process indicators is available on screening of anemia        hypertension,
#diabetes                and urinary tract infections.


