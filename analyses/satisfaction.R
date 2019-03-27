
# change this to TRUE if you want to run for gaza
IS_GAZA <-F

### SETUP

###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=IS_GAZA)


### SETUP ENDS

## LOAD DATA
if(IS_GAZA){
  d <- LoadDataFileFromNetworkGaza()
  
  fileTag <- "gaza"
  columnsIWant <- c(
    "uniqueid",
    "gA_todaylmp",
    "gA_todayus_1",
    "bookgestage"
  )
  
  gestAgeEarly <- 252
  gestAgeLate <- 256
} else {
  d <- LoadDataFileFromNetworkWB()  
  
  fileTag <- "wb"
  columnsIWant <- c(
    "uniqueid",
    "firstname",
    "fathersname",
    "middlename",
    "familyname1",
    "familyname2",
    "husbandsname",
    "mobile",
    "phone",
    "bookgestage",
    "gA_todaylmp",
    "gA_todayus_1"
   
  ) 
  
  gestAgeEarly <- 252
  gestAgeLate <- 256
}

##


#xtabs(~d$mobile)
#d$mobile
#d$phone
#d$ident_TRIAL_1

#sum(d$mobile == d$phone, na.rm=T)
#sum(d$mobile != d$phone, na.rm=T)
#d[mobile!=phone,c("mobile","phone")]

#phoneNumbers <- d[is.na(mobile) | mobile==""]$phone
#sum(!is.na(phoneNumbers) & phoneNumbers=="")
#mean(100*(!is.na(phoneNumbers) & phoneNumbers==""))

# to know the format of variables and to know the completeness for chracter vars
#this formula for string variables ....mean(!is.na(d$mobile))

#class(d$mobile)

#d$mobile

#mean(!(is.na(d$mobile)| d$mobile==""))

# from where this variable?
#d$phone
#class(d$phone)


DD <- d[ident_dhis2_control==F & 
        ident_dhis2_booking==T,
        .(
          "% having mobile"=100*mean(!(is.na(mobile) | mobile=="")),
          "% having phone"=100*mean(!(is.na(phone)  | phone=="")),
          "% not having mobile and not having phone"=
            100*mean((is.na(mobile) | mobile=="") & (is.na(phone) | phone=="")),
          "% not 10 digits"=
        )]


#% of having call information about women is 100- the above final result


#creating new variable for clincs---the same for trial 2 clinics  
#xtabs(~d$ident_TRIAL_2) # 6347

# gestational age calculation:gest=35.4--36.2
###making gA variable

#d$usdate_1
#d$expecteddateofdelivery
#d$usegaweeks_1
#d$usegadays_1

# gA_today ....should be weeks and days

#from lmp

d[,gA_todaylmp:=round(as.numeric(
  difftime(lubridate::today(),booklmp, units="days")))]

#d[,gA_todayedd:=round(as.numeric(
  #difftime(lubridate::today(),usedd_1, units="days")))]

#d[gA_todayeddminus:=gA_todayedd-28]



#from us

d[,estimatedgest_1:=round(as.numeric(
  difftime(lubridate::today(),usdate_1, units="days")))]

d[,usgestage_1days:=usgestage_1*7]

d[,gA_todayus_1:=estimatedgest_1 + usgestage_1days]

#from edd
#d[,estimatedd:=round(as.numeric(
#  difftime(lubridate::today(),usedd_1, units="days")))]


#d[,gA_todayedd36:=estimatedd-28]


sum(is.na(d$booklmp))
sum(is.na(d$booklmp))

# here are the things that are different between gaza and WB
if(IS_GAZA==FALSE){
  # WEST BANK
  # select the booking/control/clinic people that we want
  SATISFYTABLE <- d[ident_dhis2_control==F & ident_dhis2_booking==T &
                      (ident_TRIAL_2==T | ident_TRIAL_3==T |ident_TRIAL_2_3_Control==T)]
  # delete the people who dont have either a phone or a mobile
  SATISFYTABLE <- SATISFYTABLE[mobile!="" | phone!=""]
} else {
  # GAZA
  # select the booking/control/clinic people that we want
  # (this should be everyone)
  SATISFYTABLE <- d[ident_dhis2_control==F & ident_dhis2_booking==T 
                     & (ident_TRIAL_2==T | ident_TRIAL_3==T
                    |ident_TRIAL_2_3_Control==T)]
}



# these are the things that are the same bewteen gaza and WB
# restrict on gestational age
SATISFYTABLE <- SATISFYTABLE[
  #(gA_todayedd36 >=gestAgeEarly & gA_todayedd36 <= gestAgeLate),
  (gA_todayus_1 >=gestAgeEarly & gA_todayus_1 <= gestAgeLate) |
  (gA_todaylmp >=gestAgeEarly & gA_todaylmp <= gestAgeLate),
  columnsIWant,
  with=F]

setorder(SATISFYTABLE,gA_todaylmp,gA_todayus_1)
nrow(SATISFYTABLE)

sum(is.na(d$booklmp))
sum(is.na(d$gA_todaylmp))
sum(is.na(SATISFYTABLE$gA_todaylmp))

if(IS_GAZA){
  # sort randomly
  set.seed(as.numeric(lubridate::today()))
  SATISFYTABLE[,randomNumber:=runif(.N)]
  setorder(SATISFYTABLE,randomNumber)
  SATISFYTABLE[,randomNumber:=NULL]
}
openxlsx::write.xlsx(SATISFYTABLE, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction script.xlsx",lubridate::today(),fileTag)))


# select cases  randu""
set.seed(as.numeric(lubridate::today()))
CASES <- SATISFYTABLE [sample(1:.N,size = min(20,.N), replace = F)]
if(!IS_GAZA) setorder(CASES,gA_todaylmp,gA_todayus_1)

openxlsx::write.xlsx(CASES, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction CASES.xlsx",lubridate::today(),fileTag)))

## END HERE


