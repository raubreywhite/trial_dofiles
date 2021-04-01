
# number of bookings of 2020

nrow(d[ident_dhis2_booking==1 & bookyear==2020])

# 24398 women booked 2020

nrow(d[ident_dhis2_booking==1 & bookhisthtn==1 & bookyear==2020])
#200


nrow(d[ident_dhis2_booking==1 & bookhistgdm==1 & bookyear==2020])
#271


nrow(d[ident_dhis2_booking==1 & bookgestage>0 & bookgestage<=12 & bookyear==2020])
#12146

xtabs(~d[ident_dhis2_booking==1 & bookgestage>0 & bookgestage<=12 & bookyear==2020]$bookbmicat)
#[0,18.4]  (18.4,24.9]  (24.9,29.9] (29.9,1e+05] 
#549         5264         3691         2111



# any labfbs or rbs

vars <- names(d)[stringr::str_detect(names(d),"^labfastbloodglu_")]
d[,fbs:=as.logical(NA)]

for(i in vars){
  
  d[get(i)>0 & get(i)<126, fbs:=FALSE]
  d[get(i)>0 & get(i)>=126, fbs:=TRUE]
  
}

xtabs(~d[bookyear==2020 & ident_dhis2_booking==1]$fbs, addNA=T)






vars <- names(d)[stringr::str_detect(names(d),"^labbloodglu_")]
d[,rbg:=as.logical(NA)]

for(i in vars){
  
  d[get(i)>0 & get(i)<140, rbg:=FALSE]
  d[get(i)>=140, rbg:=TRUE]
  
}

xtabs(~d[bookyear==2020 & ident_dhis2_booking==T]$rbg, addNA=T)

nrow(d[ident_dhis2_booking==T & (!is.na(rbg)| !is.na(fbs)) & bookyear==2020])
#10308
nrow(d[ident_dhis2_booking==T & (rbg==T| fbs==T) & bookyear==2020])
#148
