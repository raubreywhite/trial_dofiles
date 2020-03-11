
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
    "gA_todaylmp",
    "gA_todayus_1",
    "bookgestage"
  )
  
  #need to define these 
  #depend on what day the data extraction begins in the week
  #38-39+6 weeks target gAs
  gestAgeEarly <- 266
  gestAgeLate <- 273
  
} else {
  d <- LoadDataFileFromNetworkWB() 
  
  #Creating our data set to use
  #T2WB<- d[Trial_2_and_3==T &
  #      bookdate >= "" & 
  #     bookdate<="",]
  #sat <-
  
  fileTag <- "wb"
  
  #add variable for clinic arm 
  #add clinic size
  columnsIWant <- c(
    "uniqueid",
    "bookorgname",
    #"str_Trial_2_ClusSize",
    "str_TRIAL_2_Cluster",
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
  
  #need to define these 
  #depend on what day the data extraction begins in the week
  gestAgeEarly <- 273
  gestAgeLate <- 279
  
}

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

# Calculating gA
## LMP
d[,gA_todaylmp:=round(as.numeric(
  difftime(lubridate::today(),booklmp, units="days")))]

#d[,gA_todayedd:=round(as.numeric(
#difftime(lubridate::today(),usedd_1, units="days")))]

#d[gA_todayeddminus:=gA_todayedd-28]



# US

d[,estimatedgest_1:=round(as.numeric(
  difftime(lubridate::today(),usdate_1, units="days")))]

d[usgestage_1<=22,usgestage_1days:=usgestage_1*7]

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
  satstudy <- d[bookdate>="2019-03-16" & 
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
  satstudy <- d[bookdate>="2019-03-16" & 
                    is.na(cpopregoutcome_1) &
                    ident_dhis2_booking==T & 
                    (ident_TRIAL_2_and_3==T)]
}



# these are the things that are the same bewteen gaza and WB
# restrict on gestational age
satstudy <- satstudy[
  #(gA_todayedd36 >=gestAgeEarly & gA_todayedd36 <= gestAgeLate),
  (gA_todayus_1 >=gestAgeEarly & gA_todayus_1 <= gestAgeLate) |
    (gA_todaylmp >=gestAgeEarly & gA_todaylmp <= gestAgeLate),
  columnsIWant,
  with=F]

setorder(satstudy,gA_todayus_1,gA_todaylmp)
nrow(satstudy)

sum(is.na(d$booklmp))
sum(is.na(d$gA_todaylmp))
sum(is.na(satstudy$gA_todaylmp))

if(IS_GAZA){
  # sort randomly
  set.seed(as.numeric(lubridate::today()))
  satstudy[,randomNumber:=runif(.N)]
  setorder(satstudy,randomNumber)
  satstudy[,randomNumber:=NULL]
}
openxlsx::write.xlsx(satstudy, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction WB.xlsx",
                               lubridate::today(),fileTag)))


#set.seed(4)
#give everyone random number
#d[,rand_num:=runif(.N)]
#sort according to random number
#setorder(d, bookorgname, rand_num)
#assign them a row number/order within bookorgname
#d[,rand_order:=1:.N,by=bookorgname]
#select cases with 4 or less
#small <- d[rand_order<=4]


openxlsx::write.xlsx(satstudy, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_%s_satisfaction CASES.xlsx",
                               lubridate::today(),fileTag)))

## END HERE


