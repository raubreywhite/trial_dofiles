
###### Indicators for accessibility ###### 

###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

# Load in data

If(IS_GAZA==F){
  
    d <- LoadDataFileFromNetwork()
  
} else {
    
    d <- LoadDataFileFromNetwork()
  
  
  
}


# indicators we want
 # num women and visits for anc, ppc, and immunizations
 # March to september 2017-2020

# total anc events per woman

d[,anevent_0:=bookevent]
vars <- names(d)[stringr::str_detect(names(d),"^anevent_[0-9]+")]
d[,anevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), anevent_x:=anevent_x + 1]
}

sum(d[ident_dhis2_control==F]$anevent_x,na.rm=T)


# total ppc events per woman
vars <- names(d)[stringr::str_detect(names(d),"^ppcevent_[0-9]+")]
d[,ppcevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), ppcevent_x:=ppcevent_x + 1]
}

sum(d[ident_dhis2_control==F]$ppcevent_x,na.rm=T)


# total nbc events per woman
#making variable for total nbc visits
vars <- names(d)[stringr::str_detect(names(d),"^nbcevent_[0-9]+")]
d[,nbcevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), nbcevent_x:=nbcevent_x + 1]
}

sum(d[ident_dhis2_control==F]$nbcevent_x,na.rm=T)




tab <- d[bookyear>=2017 & ident_dhis2_control==F,.(
            TotalRegisteredWomen=sum(ident_dhis2_booking==T, na.rm=T),
            TotalBookingandAncVisits=sum(anevent_x, na.rm=T),
            TotalPPCRegistrations=sum(ident_dhis2_ppc=T, na.rm=T),
            TotalNBCregistrations=sum(ident_dhis2_nbc==T, na.rm=T),
            TotalPPCvisits=sum(ppcevent_x, na.rm=T),
            TotalNBCvisits=sum(nbcevent_x, na.rm=T)),
         keyby=.(bookyear,bookyearmonth)]

length(unique(d[bookyear==2017 & ident_dhis2_control==F]$bookorgname))
length(unique(d[bookyear==2018 & ident_dhis2_control==F]$bookorgname))
length(unique(d[bookyear==2019 & ident_dhis2_control==F]$bookorgname))
length(unique(d[bookyear==2020 & ident_dhis2_control==F]$bookorgname))

openxlsx::write.xlsx(tab,file.path(FOLDER_DATA_RESULTS,
                                   "misc_requests",
                                   sprintf("ANC_PPC_NBC_%s.xlsx",
                                           lubridate::today())))
