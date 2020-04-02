
# change this to TRUE if you want to run for gaza
IS_GAZA <-F

###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=IS_GAZA)


###### SETUP ENDS ######

## LOAD DATA
if(IS_GAZA){
  d <- LoadDataFileFromNetworkGaza()
  
  fileTag <- "gaza"
  
  #add variable for clinic arm 
  #add clinic size
  columnsIWant <- c(
    "uniqueid",
    "bookorgname",
    "str_TRIAL_2_ClusSize",
    "str_TRIAL_2_Cluster",
    "gA_todaylmp",
    "gA_todayus_1",
    "bookgestage"
  )
  
  #need to define these 
  #depend on what day the data extraction begins in the week
  #38-39+6 weeks target gAs
  gestAgeEarly <- 263
  gestAgeLate <- 269
  
} else {
  d <- LoadDataFileFromNetworkWB() 
  
  fileTag <- "wb"
  
  #add variable for clinic arm 
  #add clinic size
  columnsIWant <- c(
    "uniqueid",
    "bookorgname",
    "str_TRIAL_2_Cluster",
    "str_TRIAL_2_ClusSize",
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
  
  # Data extraction will begin on Sundays (March 15, 2020)
  # Extract and send lists on Thursdays, so 266-3 days. Woman will be 38 weeks sunday
  # First week will be 38-39+3 days, second week will be 38 to 38+6, but we will   
  # extract 3 days earlier so everything goes back by 3 days
  
  # First week
  #gestAgeEarly <- 263
  #gestAgeLate <- 276
  
  # rest of the weeks
  gestAgeEarly <- 263
  gestAgeLate <- 269
  
  
}

xtabs(~d$str_TRIAL_2_ClusSize)
nrow(d[bookdate>="2019-07-01" & !is.na(booklmp)])
# Calculating gA

## LMP
d[,gA_todaylmp:=round(as.numeric(
  difftime(lubridate::today(),booklmp, units="days")))]

nrow(d[!is.na(booklmp)])
nrow(d[!is.na(gA_todaylmp)])

# US
d[,estimatedgest_1:=round(as.numeric(
  difftime(lubridate::today(),usdate_1, units="days")))]
xtabs(~d$estimatedgest_1)

d[,usgestage_1days:=usgestage_1*7]

d[,gA_todayus_1:=estimatedgest_1 + usgestage_1days]


# here are the things that are different between gaza and WB
if(IS_GAZA==FALSE){
  # WEST BANK
  # select the booking/control/clinic people that we want
  satstudy <- d[bookdate>="2019-07-01" & 
                ident_dhis2_control==F & 
                ident_dhis2_booking==T & 
                  is.na(cpopregoutcome_1) &
                (ident_TRIAL_2_and_3==T)]
  
  # delete the people who dont have either a phone or a mobile
  satstudy <- satstudy[mobile!="" | phone!=""]
 
} else {
  # GAZA
  # select the booking/control/clinic people that we want
  # (this should be everyone)
  satstudy <- d[bookdate>="2019-07-01" & 
                    is.na(cpopregoutcome_1) &
                    ident_dhis2_booking==T & 
                    (ident_TRIAL_2_and_3==T)]
}



# these are the things that are the same bewteen gaza and WB
# restrict on gestational age
satstudy <- satstudy[
  (gA_todayus_1 >=gestAgeEarly & gA_todayus_1 <= gestAgeLate) |
    (gA_todaylmp >=gestAgeEarly & gA_todaylmp <= gestAgeLate),
  columnsIWant,
  with=F]
#creating other columsn for data collection
satstudy[,sampleNum:=""]
satstudy[,status:=""]
satstudy[,callback_day_1:=""]
satstudy[,callback_time_1:=""]
satstudy[,callback_day_2:=""]
satstudy[,callback_time_2:=""]
satstudy[,callback_day_3:=""]
satstudy[,callback_time_3:=""]
satstudy[,reason_for_ending_call:=""]

setorder(satstudy,str_TRIAL_2_ClusSize,gA_todayus_1,gA_todaylmp)
nrow(satstudy)

#sum(is.na(d$booklmp))
#sum(is.na(d$gA_todaylmp))
#sum(is.na(satstudy$gA_todaylmp))


  # sort randomly
  #set.seed(as.numeric(lubridate::today()))
  #satstudy[,randomNumber:=runif(.N)]
  
  set.seed(4)
  #give everyone random number
  satstudy[,rand_num:=runif(.N)]
  #sort according to random number
  setorder(satstudy, str_TRIAL_2_ClusSize)
  #assign them a row number/order within bookorgname
  satstudy[,rand_order:=1:.N,by=str_TRIAL_2_Cluster]
  #select cases with 4 or less
  #satstudy[,rand_num:=NULL]
  satstudy <- satstudy[rand_order<=4]
  
  
  setorder(satstudy,str_TRIAL_2_ClusSize,gA_todayus_1, gA_todaylmp)
 

sattats <-satstudy[,.(N=.N),keyby=.(str_TRIAL_2_ClusSize,str_TRIAL_2_Cluster)]


openxlsx::write.xlsx(satstudy, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction WB.xlsx",
                               lubridate::today(),fileTag)))
openxlsx::write.xlsx(sattats, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satstats_WB.xlsx",
                               lubridate::today(),fileTag)))



openxlsx::write.xlsx(satstudy, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction_gaza.xlsx",
                               lubridate::today(),fileTag)))

openxlsx::write.xlsx(sattats, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satstats_gaza.xlsx",
                               lubridate::today(),fileTag)))

## END HERE


##
####################### Mobile number DQ START ####################### 

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



#add defintion for trial 2 -- dates and clinic arms
qcmobilenums <- d[ident_dhis2_control==F & 
                    ident_dhis2_booking==T,
                  .(N=.N,
                    "% mobileYes"=100*mean(!(is.na(mobile) | mobile=="")),
                    "% phoneYes"=100*mean(!(is.na(phone)  | phone=="")),
                    "% mobileNO & phoneNo"= 100*mean((is.na(mobile) | mobile=="") & 
                                                       (is.na(phone) | phone=="")))]

####################### Mobile number DQ END ####################### 
