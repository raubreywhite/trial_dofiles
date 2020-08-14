
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
  
  ###Making first usedd if its at <=21 gA variable
  # to mimick the  eRegistry, we have used the eReg rule
  # the rule takes an ultrasound less than 23 weeks and an lmp if no ultrasound is present
  nam <- names(d)[stringr::str_detect(names(d),"^usedd_[0-9]*$")]
  num <- stringr::str_replace(nam,"usedd_","")
  d[,first_1_21_usedd:=as.Date(NA)]
  for(i in num ){
    print(i)
    
    var_usedd <- sprintf("usedd_%s",i)
    var_usgestage <- sprintf("usgestage_%s",1)
    
    d[!is.na(get(var_usedd)) &
        
        !is.na(get(var_usgestage)) &
        get(var_usgestage) > 0 &
        get(var_usgestage) < 23 &
        is.na(first_1_21_usedd),
      first_1_21_usedd:=as.Date(get(var_usedd),format="%Y-%m-%d")]
  }
  
  #add columns we want here
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

### looking at numbers ###
nrow(d[is.na(booklmp)])
nrow(d[!is.na(booklmp)])

nrow(d[is.na(first_1_21_usedd)])
nrow(d[!is.na(first_1_21_usedd)])

# earlydate data 
d[,gestageEarly:=as.numeric(NA)]
d[!is.na(first_1_21_usedd), gestageEarly:=as.numeric((difftime(
  earlyDate,
  first_1_21_usedd,
  units="days"))+280,digits=1)]

d[is.na(first_1_21_usedd) &
    !is.na(booklmp),
  gestageEarly:=as.numeric((difftime(
    earlyDate,
    booklmp,
    units="days")),digits=1)]


# latedate data 
d[,gestageLate:=as.numeric(NA)]
d[!is.na(first_1_21_usedd), gestageLate:=as.numeric((difftime(
  lateDate,
  first_1_21_usedd,
  units="days"))+280,digits=1)]

d[is.na(first_1_21_usedd) &
    !is.na(booklmp),
  gestageLate:=as.numeric((difftime(
    lateDate,
    booklmp,
    units="days")),digits=1)]


# ppc date or anc date to between corona virus
d[,ancovid:=as.logical(NA)]
d[,ppccovid:=as.logical(NA)]

# ancovid variable
d[(gestageEarly>0 & gestageEarly<=265) & (gestageLate>0 & gestageLate<266), ancovid:=TRUE]
xtabs(~d$ancovid,addNA=T)

# ppccovid variable
d[(gestageEarly>=266 & gestageEarly <=350) & (gestageLate>=266 & gestageLate<350), ppccovid:=TRUE]
xtabs(~d$ppccovid,addNA=T)



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
  anaccess <- d[ancovid==T]
  
  # PPC select the booking/control/clinic people that we want
  ppcaccess <- d[ppccovid==T]
  
}

nrow(anaccess)
nrow(ppcaccess)

### things that are the same between gaza and wb
# making smaller data sets to select from
anc <- anaccess[,columnsIWant, with=F]

ppc <- ppcaccess[,columnsIWant, with=F]

# sort randomly
set.seed(7)

#give everyone random number
anc[,rand_num:=runif(.N)]
ppc[,rand_num:=runif(.N)]

#sort according to random number
setorder(anc, rand_num)
setorder(ppc, rand_num)

### can try this to sample-- because below one asks for df and not dt
# sample (c(1:10), size=3, replace =F)
# what happens to the columsn that i want after this step??
# ansample <- sample(ansample$rand_num, size=212, replace=F)
#or can be done this way:
#ppcaccess[,rand_num:=runif(.N)]
#ppcaccess[rand_num < 0.05] # select (approximately) 5% of the rows


## or can try this way instead to take random sample
if(IS_GAZA){
  
  # first sampling was done for 212, but 16% have no number
  # extracted another sample of 40 for anc bc 33 missing (16%)
  # extracted 30 for ppc because number of missing 24 (11%)
  # took 30, but 7 duplicates. tookanother sample of 40 to make sure
  ansample <- anc[sample(nrow(anc),253), ]
  ppcsample <- ppc[sample(nrow(ppc),253),]
  
} else {
  
  ansample <- anc[sample(nrow(anc),318), ]
  ppcsample <- ppc[sample(nrow(ppc),318),]

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
                       FOLDER_DATA_RESULTS_GAZA,
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
