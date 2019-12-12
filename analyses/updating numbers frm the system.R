
Mabstract <- d[ident_TRIAL_1==TRUE,
               .(numWomen=.N),
               
               keyby= agecat]

###want a data set with all of the rows incl ppc and nbc
###num of visits anc, ppc, nbc
###lab tests for hb(this is for anc and ppc)
###num us
###num ogct
###num syst and diast
###num lab uti

#making variable for ultrasound
vars <- names(d)[stringr::str_detect(names(d),"^usevent_[0-9]+")]
d[,usevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

sum(d[ident_dhis2_control==F]$usevent_x,na.rm=T)




#making variable for total ppc visits
vars <- names(d)[stringr::str_detect(names(d),"^ppcevent_[0-9]+")]
d[,ppcevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), ppcevent_x:=ppcevent_x + 1]
}

sum(d[ident_dhis2_control==F]$ppcevent_x,na.rm=T)


#making variable for total nbc visits
vars <- names(d)[stringr::str_detect(names(d),"^nbcevent_[0-9]+")]
d[,nbcevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), nbcevent_x:=nbcevent_x + 1]
}

sum(d[ident_dhis2_control==F]$nbcevent_x,na.rm=T)

#Making anevents into one variable
#the * means needs to be atleast 0 of the preceding letters
##so it was counting the usevent_x as well. we checked this with vars statements
# the "+" after the brackets means there has to be atleast one of the preceding letters
vars <- names(d)[stringr::str_detect(names(d),"^anevent_[0-9]+")]
d[,anevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), anevent_x:=anevent_x + 1]
}

sum(d[ident_dhis2_control==F]$anevent_x,na.rm=T)



#Booksyst
nrow(d[ident_dhis2_control==F & !is.na(bookbpsyst)])

#ANC BPSYST
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]+")]
d[,anbpsyst_x:=0]

print(vars)

#here, it includes values=<0, where as the below code doesnt
for(i in vars){
  d[!is.na(get(i)), anbpsyst_x:=anbpsyst_x + 1]
}

sum(d[ident_dhis2_control==F]$anbpsyst_x,na.rm=T)


#HYPERTENTION (ALL SYSTOLIC)
#This code isnt working to get ALL of the bps for booking and an
#str_subset and str_detect is the first one takes them all out
#to include bookbpsyst readings replace it with anbpsyst_0
d[,anbpsyst_0:=NULL]
d[,anbpsyst_0:=bookbpsyst]
vars <- stringr::str_subset(names(d),"^anbpsyst_[0-9]+")

d[,allbpsyst_x:=0]

for(i in vars){
  d[!is.na(get(i)) & get(i)>0, allbpsyst_x:=allbpsyst_x + 1]
}

sum(d[ident_dhis2_control==F]$allbpsyst_x,na.rm=T)

#Hypertension (ALL DIASTOLIC)
d[,anbpdiast_0:=0]
d[,anbpdiast_0:=bookbpdiast]
vars <- stringr::str_subset(names(d),"^anbpdiast_[0-9]+")

d[,allbpdiast_x:=0]

for(i in vars){
  d[!is.na(get(i)) & get(i)>0, allbpdiast_x:=allbpdiast_x + 1]
}

sum(d[ident_dhis2_control==F]$allbpdiast_x,na.rm=T)


# #HYERTENSION (PPC)
# vars <- names(d)[stringr::str_detect(names(d),"^ppcbpsyst_[0-9]*")]
# 
# d[,ppcbpsyst_x:=0]
# 
# for(i in vars){
#   d[!is.na(get(i))& get(i)>0, ppcbpsyst_x:=ppcbpsyst_x + 1]
# }
# 
# sum(d[ident_dhis2_control==F]$ppcbpsyst_x,na.rm=T)

##LAB FBS(Check variables)
vars <- names(d)[stringr::str_detect(names(d),"^labfastbloodglu_[0-9]+")]

d[,labfastbloodglu_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labfastbloodglu_x:=labfastbloodglu_x + 1]
}

sum(d[ident_dhis2_control==F]$labfastbloodglu_x,na.rm=T)

##LAB RBS
vars <- names(d)[stringr::str_detect(names(d),"^labbloodglu_[0-9]+")]

d[,labbloodglu_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labbloodglu_x:=labbloodglu_x + 1]
}

sum(d[ident_dhis2_control==F]$labbloodglu_x,na.rm=T)

##LAB OGCT
vars <- names(d)[stringr::str_detect(names(d),"^labogct_[0-9]+")]

d[,labogct_x:=0]

for(i in vars){
  d[!is.na(get(i)) & get(i)>0, labogct_x:=labogct_x + 1]
}

sum(d[ident_dhis2_control==F]$labogct_x,na.rm=T)

#Lab Hb 
vars <- names(d)[stringr::str_detect(names(d),"^labhb_[0-9]+")]
d[,labhb_x:=0]

for(i in vars){
  d[!is.na(get(i)) & get(i)>0,labhb_x:=labhb_x+1]
}

sum(d[ident_dhis2_control==F]$labhb_x,na.rm=T)

#Lab UTI
#laburuti_=="NEG"
#laburuti=="POS"

vars <- names(d)[stringr::str_detect(names(d),"^laburuti_[0-9]+")]
d[,laburuti_x:=0]


for(i in vars){
  d[get(i) %in% c("POS", "NEG"),laburuti_x:=laburuti_x+1 ]
  
}

sum(d[ident_dhis2_control==F]$laburuti_x,na.rm=T)

nrow(d[ident_dhis2_control==F &ident_dhis2_ppc==T])
nrow(d[ident_dhis2_control==F &ident_dhis2_booking==T])
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T])


#for now, get numbers for PPC and NBC from raw
##beaue the ones that dont have a booking are dropped from d during cleaning
##so we lose their visits 
UpdatedSystemNumbers <- d[ident_dhis2_control==F,
                          .(
  "Number of Bookings"= sum(ident_dhis2_booking==T, na.rm=T),
  "Number of ANC Visits"= sum(anevent_x, na.rm=T),
  "Number of PPC Visits"= sum(ppcevent_x, na.rm=T),
  "Number of Newborn Care Visits"= sum(nbcevent_x, na.rm=T),
  "Number of Systolic BP Screenings"= sum(allbpsyst_x, na.rm=T),
  "Number of Diastolic BP Screenings"= sum(allbpdiast_x, na.rm=T),
  "Number of Anemia Screenings"= sum(labhb_x, na.rm=T),
  "Number of FBS Tests"= sum(labfastbloodglu_x,na.rm=T),
  "Number of RBS Tests"= sum(labbloodglu_x,na.rm=T),
  "Number of OGCT Tests"= sum(labogct_x,na.rm=T),
  "Number of Ultrasounds Performed"= sum(usevent_x,na.rm=T),
  "Number of UTI Screenings"= sum(laburuti_x,na.rm=T)
  
  
)]

openxlsx::write.xlsx(UpdatedSystemNumbers,
                     file.path(FOLDER_DATA_RESULTS ,
                               sprintf("UpdatedSystemNumbers_%s.xlsx",lubridate::today())))
                             



#As of 2017, the MCH e Registry contains data on 24,832 registered antenatal care visits, 
#18,374 postpartum care visits and 16,409 newborn care visits. From antenatal care, 
#data on core process indicators is available on screening of anemia        hypertension,
#diabetes                and urinary tract infections.


##############################################################################
#####Feb 5, 2019 #####
##FBS 92-125, >=126
##RBS >140
##all for 23-30 weeks

###first find all gA's at 23-30 weeks at lab test, then see if has the other tests

###want number of fbs and rbs done during the 22-30 weeks of pregnancy
###make categories for these variables first, but could be labbloodglu_1-10??
#find out how many of each test there are first or alternatively:
#first find women who have the test and then make into categories?

####find labbloodglu and replace them with correc
#random bloodglu
#want lab gest age between 22-30 weeks
#created this variable before as numeric and so want to get rid of it so we wont run into problems
d[,labgestage_22_30:=NULL]

d[,labgestage_22_30:=as.character(NA)]
d[,min_dist_from_24:=999999]

vars <-names(d)[stringr::str_detect(names(d), "^labgestage_[0-9]+$")]
numbers <- stringr::str_replace(vars,"labgestage_","")
for(i in numbers){
  varGestAge <- sprintf("labgestage_%s",i)
  varRBS <- sprintf("labbloodglu_%s",i)
  
  d[,dist_from_24:=abs(get(varGestAge)-24)]
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varRBS)>=60 &
      get(varRBS)<=139,
    labgestage_22_30:="<139"]
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varRBS)>139,
    labgestage_22_30:="140+"]


 d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varRBS)>140,
    min_dist_from_24:=abs(get(varGestAge)-24)]
  
  
}

nrow(d[labgestage_22_30=="140+"])
#nrow=166
#sum(d$labgestage_22_30=="140+",na.rm=T)
#xtabs(~d$labgestage_22_30)
nrow(d[labgestage_22_30=="<139"])
#nrow=9765

nrow(d[ident_dhis2_booking==T & isExpectedToHaveDelivered==T])

####want number of fbs and rbs done during the 22-30 weeks of pregnancy
###make categories for these variables first, but could be labfastbloodglu_1-10??
#find out how many of each test there are first or alternatively:
#first find women who have the test and then make into categories?

#want lab gest age between 22-30 weeks
#created this variable before as numeric and so want to get rid of it so we wont run into problems
d[,labgestage_22_30:=NULL]
d[,labgestage_22_30:=as.character(NA)]
d[,min_dist_from_24:=999999]

vars <-names(d)[stringr::str_detect(names(d), "^labgestage_[0-9]+$")]
numbers <- stringr::str_replace(vars,"labgestage_","")
for(i in numbers){
  varGestAge <- sprintf("labgestage_%s",i)
  varFBS <- sprintf("labfastbloodglu_%s",i)
  
  d[,dist_from_24:=abs(get(varGestAge)-24)]
  
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varFBS)>=0 &
      get(varFBS)<=91,
    labgestage_22_30:="0-91"]
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varFBS)>=92 &
      get(varFBS)<=125,
    labgestage_22_30:="92-125"]
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varFBS)>125,
    labgestage_22_30:="126+"]
  
  
  d[get(varGestAge)>=22 & 
      get(varGestAge)<=30 &
      dist_from_24<min_dist_from_24 &
      get(varFBS)>125,
    min_dist_from_24:=abs(get(varGestAge)-24)]
  
  
  
}

nrow(d[labgestage_22_30=="126+"])
#nrow=48
nrow(d[labgestage_22_30=="92-125"])
#nrow=749

nrow(d[labgestage_22_30=="0-91"])
#nrow=3397


#####

#Buthaina's stuff
#you should run this from sink to sink
# to save things in notepad and dont forgett to put sink at the bottom
sink()
sink(file.path(FOLDER_DATA_RESULTS,
            "SystemUpdatedNumbers.txt"))
#Booking
sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$bookevent,na.rm=T)


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

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$anevent_x,na.rm=T)

#NBC
cat("\nNumNBC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T& bookdate>="2017-01-01"])
cat("\nNumNBCvisits\n")
vars <- names(d)[stringr::str_detect(names(d),"^nbcevent_[0-9]*")]
d[,nbcevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), nbcevent_x:=nbcevent_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$nbcevent_x,na.rm=T)

###Anemia
cat("\nNumber of anemia tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")]
d[,labhb_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, labhb_x:=labhb_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$labhb_x,na.rm=T)



###Ultrasound
cat("\nNumber of ultrasound tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")]
d[,usevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$usevent_x,na.rm=T)

#HYPERTENTION
cat("\nNumber of antenatal blood pressure tests on system\n")
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")]
d[,anbpsyst_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, anbpsyst_x:=anbpsyst_x + 1]
}
sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$anbpsyst_x,na.rm=T)

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

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$labbloodglu_x,na.rm=T)

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

sum(d[ident_dhis2_control==F& bookdate>="2017-01-01"]$laburuti_x,na.rm=T)
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





sink()


#As of 2017, the MCH e Registry contains data on 24,832 registered antenatal care visits, 
#26640 postpartum care visits and 16,409 newborn care visits. From antenatal care, 
#data on core process indicators is available on screening of anemia,hypertension,
#diabetes and urinary tract infections.



