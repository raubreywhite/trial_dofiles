
Mabstract <- d[ident_TRIAL_1==TRUE,
               .(numWomen=.N),

               keyby= agecat]




nrow(d[ident_dhis2_control==F &ident_dhis2_booking==T])
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T])


vars <- names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")]

d[,labhb_x:=0]

for(i in vars){
  d[get(i)==1, labhb_x:=labhb_x+1]
}

sum(d[ident_dhis2_control==F]$labhb_x,na.rm=T)





vars <- names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")]

d[,usevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

sum(d[ident_dhis2_control==F]$usevent_x,na.rm=T)

#HYPERTENTION
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")]

d[,anbpsyst_x:=0]

for(i in vars){
  d[!is.na(get(i))& get(i)>0, anbpsyst_x:=anbpsyst_x + 1]
}

sum(d[ident_dhis2_control==F]$anbpsyst_x,na.rm=T)

# 

 #xtabs(~d$ident_dhis2_control + d$ident_dhis2_booking)   


ident_dhis2_nbc==T
 #As of 2017, the MCH e Registry contains data on 24,832 registered antenatal care visits, 
#18,374 postpartum care visits and 16,409 newborn care visits. From antenatal care, 
#data on core process indicators is available on screening of anemia        hypertension,
#diabetes                and urinary tract infections.



