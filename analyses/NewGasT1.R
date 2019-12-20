###making gestational age###
# at booking
#an ultrasound 18-20 weeks, edd
#lmp
#any ultrasound after 20 weeks

# + means repeat, $ means the end of the entire string
d[,lmpT1:=as.Date(NA)]
us_gestages <- stringr::str_subset(names(d), "^usgestage_[0-9]+")
us_date <- stringr::str_subset(names(d), "^usdate_[0-9]+")



#seq_along identical to 1:lenth()
for(i in seq_along(us_gestages)){
  var_us_gestage <- us_gestages[i]
  var_us_date<- us_date[i]
  
  d[is.na(lmpT1) & get(var_us_gestage)>=18 & 
      get(var_us_gestage)<= 20, 
      
      lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
  
  
}

d[is.na(lmpT1) & !is.na(booklmp), lmpT1:=booklmp]

for(i in seq_along(us_gestages)){
  var_us_gestage <- us_gestages[i]
  var_us_date<- us_date[i]
  
  d[is.na(lmpT1) & get(var_us_gestage)>=20 & get(vars_us_gestage)<40,
    
    lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
  
  
}

#qc new variables
d[is.na(lmpT1), c("booklmp",us_gestages, us_date), with=F]


#for anyone who may be missing the above restrictions but has an us earlier US before 18 weeks
#seq_along identical to 1:lenth()
for(i in seq_along(us_gestages)){
  var_us_gestage <- us_gestages[i]
  var_us_date<- us_date[i]
  
  d[is.na(lmpT1) & get(var_us_gestage)>=4 & get(vars_us_gestage)<18,
    lmpT1:=get(var_us_date)-get(var_us_gestage)*7]
  
  
}


##looping through to make gestages
#Us gAs
us_date <- stringr::str_subset(names(d), "^usdate_[0-9]+")
usT1_gA <- stringr::str_replace(us_date, "usdate","usT1gestagedays")

for(i in seq_along(us_date)){
  var_us_gestage <- usT1_gA[i]
  var_us_date<- us_date[i] 
  
  d[,(var_us_gestage):=as.numeric(difftime(get(var_us_date),lmpT1, units="days"))]
  
}
 

#ANC gAs
an_date <- stringr::str_subset(names(d), "^andate_[0-9]+")
anT1_gA <- stringr::str_replace(an_date, "andate","anT1gestagedays")

for(i in seq_along(an_date)){
  var_an_gestage <- anT1_gA[i]
  var_an_date<- an_date[i] 
  
  d[,(var_an_gestage):=as.numeric(difftime(get(var_an_date),lmpT1, units="days"))]
  
}

#Lab gAs
##NEED TO ROUND OR USE FLOOR
lab_date <- stringr::str_subset(names(d), "^labdate_[0-9]+")
labT1_gA <- stringr::str_replace(lab_date, "labdate","labT1gestagedays")

for(i in seq_along(lab_date)){
  var_lab_gestage <- labT1_gA[i]
  var_lab_date<- lab_date[i] 
  
  d[,(var_lab_gestage):=as.numeric(difftime(get(var_lab_date),lmpT1, units="days"))]
  
}

#Man gAs
man_date <- stringr::str_subset(names(d), "^mandate_[0-9]+")
manT1_gA <- stringr::str_replace(man_date, "mandate","manT1gestagedays")

for(i in seq_along(man_date)){
  var_man_gestage <- manT1_gA[i]
  var_man_date<- man_date[i] 
  
  d[,(var_man_gestage):=as.numeric(difftime(get(var_man_date),lmpT1, units="days"))]
  
}