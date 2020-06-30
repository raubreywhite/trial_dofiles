
# access to care study
# identify women who were pregnant and
# identify women who have given birth during this time 
# 530 women from ANC and 530 women from PPC
# 60% WB (318) and 40% gaza (212)

# to do this, want to find appropriate gAs during covid time frame and not greater 
# since gestational age is important, want anyone who booked after july 2019
 

# will be taking from women who were registrerd from anc and choose the gA from that sample instead

# calculate gest age early and gestage late from the two dates and see what


# change this to TRUE if you want to run for gaza
IS_GAZA <-T

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
  
  #add clinic size
  columnsIWant <- c(
    "uniqueid")
  
} else {
  d <- LoadDataFileFromNetworkWB() 
  
  fileTag <- "wb"
  
  columnsIWant <- c(
    "firstname",
    "fathersname",
    "middlename",
    "familyname1",
    "familyname2",
    "mobile",
    "phone"
    
  ) 
  
  
  
}

#### Creation of variables ####

# identify date restrictions

earlyDate <- as.Date("2020-03-22")
lateDate <- as.Date("2020-05-26")



### just looking at some numbers ###
nrow(d[is.na(booklmp)])
nrow(d[!is.na(booklmp)])

nrow(d[is.na(first_1_21_usedd)])
nrow(d[!is.na(first_1_21_usedd)])



# earlydate data 
d[,gestageEarly:=as.numeric(NA)]
d[!is.na(first_1_21_usedd), gestageEarly:=as.numeric((difftime(
  first_1_21_usedd,
  earlyDate,
  units="days"))+280,digits=1)]

d[is.na(first_1_21_usedd) &
    !is.na(booklmp_original),
  gestageEarly:=as.numeric((difftime(
    earlyDate,
    booklmp_original,
    units="days")),digits=1)]


# latedate data 
d[,gestageLate:=as.numeric(NA)]
d[!is.na(first_1_21_usedd), gestageLate:=as.numeric((difftime(
  first_1_21_usedd,
  lateDate,
  units="days"))+280,digits=1)]

d[is.na(first_1_21_usedd) &
    !is.na(booklmp_original),
  gestageLate:=as.numeric((difftime(
    lateDate,
    booklmp_original,
    units="days")),digits=1)]



# ppc date or anc date to between corona virus
d[,ancovid:=as.logical(NA)]
d[,ppccovid:=as.logical(NA)]

# ancovid variable
d[gestageEarly>0 & gestageLate<266, ancovid:=TRUE]

# ppccovid variable
d[gestageEarly>=266 & gestageLate<=322]



# here are the things that are different between gaza and WB
if(IS_GAZA==FALSE){
  # WEST BANK
  # ANC selecion
  anaccess <- d[ancovid==T]
  
  #remove people who dont have phone or mobile
  anaccess <- anaccess[mobile!="" | phone!=""]
  
  # PPC select the booking/control/clinic people that we want
  ppcaccess <- d[ppccovid==T]
  
  # delete the people who dont have either a phone or a mobile
  ppcaccess <- ppcaccess[mobile!="" | phone!=""]
  
} else {
  # GAZA
  # ANC select the booking/control/clinic people that we want
  anaccess <- d[bookcovid==T | ancovid==T]
  
  # PPC select the booking/control/clinic people that we want
  ppcaccess <- d[ppccovid==T]
  
}

nrow(anaccess)
nrow(ppcaccess)

### things that are the same between gaza and wb
# making smaller data sets to select from
anaccess[,columnsIWant, with=F]

ppcaccess[,columnsIWant, with=F]

# sort randomly
set.seed(7)

#give everyone random number
anaccess[,rand_num:=runif(.N)]
ppcaccess[,rand_num:=runif(.N)]


#sort according to random number
setorder(anaccess, rand_num)
setorder(ppcaccess, rand_num)

### can try this to sample-- because below one asks for df and not dt
# sample (c(1:10), size=3, replace =F)
# what happens to the columsn that i want after this step??
# ansample <- sample(ansample$rand_num, size=212, replace=F)


## or can try this way instead to take random sample
if(IS_GAZA){
  
  ansample <- anaccess[sample(nrow(anaccess),212), ]
  ppcsample <- ppcsample[sample(nrow(ppcsample),212),]
  
} else {
  
  ansample <- anaccess[sample(nrow(anaccess),318), ]
  ppcsample <- ppcsample[sample(nrow(ppcsample),318),]

}


###### saving files ###### 

# West Bank
openxlsx::write.xlsx(ansample, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "misc_requests",
                       sprintf("%s_accessibiliy_anc.xlsx",
                               lubridate::today(),fileTag)))
openxlsx::write.xlsx(ppcsample, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "misc_requests",
                       sprintf("%s_accessibiliy_ppc.xlsx",
                               lubridate::today(),fileTag)))


# Gaza
openxlsx::write.xlsx(ansample, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "misc_requests",
                       sprintf("%s_accessibiliy_anc.xlsx",
                               lubridate::today(),fileTag)))

openxlsx::write.xlsx(ppcsample, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "misc_requests",
                       sprintf("%s_accessibiliy_ppc.xlsx",
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
