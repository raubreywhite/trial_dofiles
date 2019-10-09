####HGBNotImproved from system numbers
####August 20, 2019

####HG not improved, Numbers from System
vars <- names(d)[stringr::str_detect(names(d),"^risktype_")]
d[,risktype_HGB_not_improved:=FALSE]
for(i in vars){
  d[stringr::str_trim(get(i))=="HGBNotImproved",risktype_HGB_not_improved:=TRUE]
}

# go through all the visits. if any visit=1
# then the variable = TRUE
for(i in vars){
  d[get(i)=="HGBNotImproved",risktype_HGB_not_improved:=TRUE]
}

unique(d[bookyear=="2019"&
           bookmonth=="05"]$risktype_HGB_not_improved)

nrow(d[bookyear=="2019" &
         bookmonth=="05"&
         risktype_HGB_not_improved==TRUE])

manxvars <- names(d)[stringr::str_detect(names(d),"^mantypex_")]

for(i in manxvars){
  d[,mantypex:=get(manxvars)]
}

manyvars <- names(d)[stringr::str_detect(names(d),"^mantypey_")]

for(i in manyvars){
  d[,mantypey:=get(manyvars)]
}


unique(d[bookyear=="2019" &
         #bookmonth=="05"&
         risktype_HGB_not_improved==TRUE]$mantypex)

#can put mantypex instead of mantypey

 smalltable <- d[
   bookyear=="2019" &
     risktype_HGB_not_improved==TRUE,
   .(N=.N),
   keyby=.(bookmonth, mantypey)
   ]





# smalltable <- d[
#   bookyear=="2019" &
#     bookmonth=="05",
#   .(N=.N),
#   keyby=.(risktype)
#   ]