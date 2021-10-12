###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

####### SETUP ENDS #######



#### LOAD d from Network ####

d <- LoadDataFileFromNetwork()

nrow(d)
xtabs(~d$bookyear, addNA=T)
xtabs(~ident_dhis2_booking+bookyear, data=d)

# id year of birth for pregnancies
d$expecteddateofdelivery

d[,yearofexpdeliv:=stringr::str_extract(expecteddateofdelivery,"^[0-9][0-9][0-9][0-9]")]
d[,yearofexpdeliv:=as.numeric(yearofexpdeliv)]
xtabs(~d$yearofexpdeliv, addNA=T)
# use this in table to see how many were expected to have given birth 


# id year of birth of deliveries
# year of cpo delivery
# use this variable instead because looking at the actual delivery date and not an estimate

d[,yearofdeliv:=stringr::str_extract(ppcdateofdelivery_1,"^[0-9][0-9][0-9][0-9]")]
d[,yearofdeliv:=as.numeric(yearofdeliv)]
xtabs(~d$yearofdeliv, addNA=T)

cpo <- d[yearofdeliv>=2016 & !is.na(cpoevent_1),.(N=.N,
            hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
            CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T)),
         keyby=.(yearofdeliv)]

cpo[,percCS:=round(CSdeliv/hasmodedeliv, digits=3)]

openxlsx::write.xlsx(cpo, file.path(FOLDER_DATA_RESULTS,
                                        "cs",
                                        "cs_delivs.xlsx"))

cpoplace <- d[yearofdeliv>=2016 &
                !is.na(cpoevent_1),.(N=.N,
                                hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
                                CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T)),
         keyby=.(yearofdeliv,cpoplaceofbirth_1)]

cpoplace[,numcsperyear:=sum(CSdeliv), by=yearofdeliv]
cpoplace[,perCS:=round(CSdeliv/numcsperyear, digits=2)]

openxlsx::write.xlsx(cpoplace, file.path(FOLDER_DATA_RESULTS,
                                        "cs",
                                        "cs_placeofdeliv.xlsx"))





# high risk at booking

cpoplace <- d[yearofdeliv>=2016 &
                !is.na(cpoevent_1),.(N=.N,
                                     hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
                                     CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T)),
              keyby=.(yearofdeliv,bookhighrisk,cpoplaceofbirth_1)]

cpoplace[,numcsperyear:=sum(CSdeliv), by=c("yearofdeliv","bookhighrisk")]
cpoplace[,perCS:=round(CSdeliv/numcsperyear, digits=2)]




# CPO
#ppcbw lga and sga

#gA from ppcdeliv date or cpo gA, check them out
d[,ppcdatedeliv_1:=as.Date(ppcdateofdelivery_1, format="%Y-%m-%d")]
xtabs(~d$ppcdatedeliv_1)
d[,gestageatbirth:=difftime(ppcdatedeliv_1,
                             USorLMPdate,
                             units="days"
)]

xtabs(~d$gestageatbirth, addNA=T)



############### 
# multifet preg
############### 

# uspres at 36 weeks 
d[,hasan36plusweeks:=FALSE]
#d[,hasanexampalp36:= FALSE]
vars <- stringr::str_subset(names(d),"^angestage_")
for (i in vars){
  print(i)
  d[get(i)>=36 & get(i)<=40, hasan36plusweeks:=TRUE]
  
}


xtabs(~d$hasan36plusweeks, addNA=T)

#fetal presentation at term
d[,presatterm:=as.character(NA)]

vars <- stringr::str_subset(names(d),"^uspres_")


for (var_pres in vars){
  
  vargestage <-stringr::str_replace(var_pres,"uspres", "usgestage")
  
  d[hasan36plusweeks==TRUE &
       get(vargestage)>=36 &
       get(vargestage)<=40 &
       !is.na(get(var_pres)) &
       get(var_pres)!="",
     presatterm:=get(var_pres)]
}

xtabs(~d$presatterm, addNA=T)

############
# multifetus
############

# multifetus: cpo num (up to 3, the rest are empty) or us
d[,multifet:=as.logical(NA)]
vars <- names(d)[stringr::str_detect(names(d),"^usnumberfetus_")]

for(i in vars){
  
  d[get(i)>1, multifet:=TRUE]
  d[get(i)<=1, multifet:=FALSE]
  
}

xtabs(~d[ident_dhis2_booking==1]$multifet, addNA=T)

# gA validate self reported with those that come from ANC
vars <- names(d)[stringr::str_detect(names(d),"^cpo")]

####### parity variable ####### 
d[,hasprevpreg:=as.logical(NA)]

# all we need is atleast the first one to identify if they atleast have a previous pregnancy

  d[is.na(prevevent_1),hasprevpreg:=FALSE]
  
# the first one will also tell us   
  d[!is.na(prevevent_1),hasprevpreg:=TRUE]
  
xtabs(~d$hasprevpreg)


# number of deliveries
d[,numdeliveries:=as.numeric(NA)]
d[!is.na(prevoutcome_1), numdeliveries:=0]

d[,allprevabo:=as.numeric(NA)]
d[!is.na(prevoutcome_1), allprevabo:=0]
vars <-names(d)[stringr::str_detect(names(d),"^prevoutcome_")]


for(i in vars){
  d[!is.na(get(i)) &
      hasprevpreg==T &
      get(i) %in% c("ABO",
                    "ECTOPIC_PREGNANCY",
                    "MOLAR_PREGNANCY"), allprevabo:=allprevabo+1]
  
  d[!is.na(get(i)) &
      hasprevpreg==T &
      !get(i) %in% c("ABO",
                    "ECTOPIC_PREGNANCY",
                    "MOLAR_PREGNANCY",
                    "",
                    NA), numdeliveries:=numdeliveries+1]
  
}

xtabs(~d$allprevabo)
xtabs(~d$numdeliveries)

# putting the two togeter to make sure we are making the right variable
d[,parity:=as.character(NA)]
d[hasprevpreg %in% c(FALSE,NA), parity:="nullipara"]
d[hasprevpreg==TRUE & numdeliveries %in% c(0,NA), parity:="nullipara"]

d[hasprevpreg==TRUE & numdeliveries>=1, parity:="multipara"]
xtabs(~d$parity, addNA=T)



# number of previous csections #
d[,numprevcs:=as.numeric(NA)]
d[!is.na(prevevent_1), numprevcs:=0]
vars <-names(d)[stringr::str_detect(names(d),"^prevmodedelivery_")]


for(i in vars){
  d[!is.na(get(i)) &
      hasprevpreg==T &
      get(i) %in% c("Caesarian section"), numprevcs:=numprevcs+1]
  
}

xtabs(~d$numprevcs, addNA=T)   




# making variables for robson group classifications #########
d[,robsongp:=as.character(NA)]

# robson classifications
## fix the variables
#nulliparous, single cephalic preg,at term (37 or more) & spont deliv
d[parity=="nullipara" &
     presatterm=="Cephalic" & 
     cpogestage_1>=37 &
     cpogestage_1<=44 &
     cpomodedelivery_1=="Spontaneous vaginal" &
     multifet==F,robsongp:="1"]
xtabs(~d$robsongp, addNA=T)

#primi, single cephalic preg,at term (37 or more) & deliv==csec or induced
d[parity=="nullipara" &
     presatterm=="Cephalic" & 
     cpogestage_1>=37 &
     cpogestage_1<=44 &
     cpomodedelivery_1 %in% c("Caesarian section") &
     multifet==F,robsongp:="2"]

xtabs(~d$robsongp, addNA=T)


#multiparous, single cephalic preg,at term (37 or more) & deliv= NVSD & no prev ut scar
d[parity=="multipara"&
     presatterm=="Cephalic" & 
     cpogestage_1>=37 &
     cpogestage_1<=44 &
     cpomodedelivery_1=="Spontaneous vaginal" &
     numprevcs %in% c(0,NA) &
     multifet==F,robsongp:="3"]
xtabs(~d$robsongp, addNA=T)

#multiparous, single cephalic preg,at term (37 or more) & deliv= induced or cs & no prev ut scar
d[parity=="multipara" &
     presatterm=="Cephalic" & 
     cpogestage_1>=37 &
     cpogestage_1<=44 &
     cpomodedelivery_1 %in% c("Caesarian section") &
     (numprevcs %in% c(0,NA)) &
     multifet==F,robsongp:="4"]
xtabs(~d$robsgp, addNA=T)


#multiparous,at term (37 or more) & deliv= induced or cs & no prev ut scar
d[parity=="multipara" &
     presatterm=="Cephalic" & 
     cpogestage_1>=37 &
     cpogestage_1<=44 &
     cpomodedelivery_1 %in% c("Caesarian section","induced") &
     multifet==F &
     (numprevcs>=1),robsongp:="5"]
xtabs(~d$robsongp, addNA=T)

#nulliparous,single preg, breech
d[parity=="nullipara" &
     presatterm %in% c("Breech","Trasverse") &
     multifet==F,robsongp:="6"]
xtabs(~d$robsongp, addNA=T)

#nulliparous,single preg, breech
d[parity=="multipara" &
    presatterm %in% c("Breech") &
    multifet==F,robsongp:="7"]
xtabs(~d$robsongp, addNA=T)


# all women with multiple preg and uterine scars
d[numprevcs>=1 &
     multifet==T,robsongp:="8"]
xtabs(~d$robsongp, addNA=T)


#
# all women with single preg and transv/oblique lie and prev cs
d[presatterm %in% c("Trasverse","Oblique") &
     multifet==F,robsongp:="9"]
xtabs(~d$robsongp, addNA=T)

# all women with single cephalic preg and less than 37 week 

d[(cpogestage_1<37 & cpogestage_1>=24) &
     presatterm=="Cephalic" & 
     multifet==F,robsongp:="10"]
xtabs(~d$robsongp, addNA=T)

# make variable numeric
d[,robsongp:=as.numeric(robsongp)]
xtabs(~d$robsongp, addNA=T)

tab <-d[!is.na(robsongp) & 
          !is.na(cpoevent_1) &
          !is.na(yearofdeliv) &
          yearofdeliv>=2017,.("Number of CS"=sum(cpomodedelivery_1=="Caesarian section", na.rm=T),
                               "Number of Women in group"=.N),
        keyby=.(yearofdeliv,robsongp)]

# number of women delivered in hospital (in this case, it would be in total)
tab[,totalwomen:=sum(`Number of Women in group`), by=.(yearofdeliv)]

tab[,"Group Size":=round(`Number of Women in group`/totalwomen, digits=3)]

tab[,"Group CS Rate (proportion)":=round(`Number of CS`/`Number of Women in group`, digits=3)]

tab[,"Absolute group contribution":=round(`Number of CS`/totalwomen, digits=3)]

tab[,TotalCS:=sum(`Number of CS`), by=.(yearofdeliv)]

tab[,"Relative contribution of group to overall cs rate":=round(`Number of CS`/TotalCS, digits = 3)]



setorder(tab, yearofdeliv,robsongp)

tab

openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                      "cs",
                       "Robsongps_byyearofdeliv_.xlsx"))




# categories of missing data for the noninclusive groups
dq <- tab <-d[!is.na(cpoevent_1) &
                !is.na(yearofdeliv) &
                yearofdeliv>=2017,.(
                  N=.N,
                  Missing_yearexpdeliv=sum(is.na(yearofexpdeliv)),
                  Missing_yearofdeliv=sum(is.na(yearofdeliv)),
                  Missing_modeofdeliv=sum(is.na(cpomodedelivery_1)),
                  Missing_cpoplaceofbirth=sum(is.na(cpoplaceofbirth_1)),
                  Missing_dateofdeliv=sum(is.na(ppcdatedeliv_1)),
                  Missing_USorLMPdate=sum(is.na(USorLMPdate)),
                  Missing_gestageatbirth=sum(is.na(gestageatbirth)),
                  Missing_presentation=sum(is.na(presatterm)),
                  Missing_multifet=sum(is.na(multifet)),
                  Missing_hasprevpreg=sum(is.na(hasprevpreg)),
                  Missing_numdeliv=sum(is.na(numdeliveries)),
                  Missing_parity=sum(is.na(parity)),
                  Missing_numprevcs=sum(is.na(numprevcs))),
              keyby=.(yearofdeliv, robsongp)]

openxlsx::write.xlsx(dq,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "cs",
                       "Missing_data_by_robsongp.xlsx"))

# can create a table like 5.2 in manual
# use that to get completeness or missing values

# parity, previous cs, number of fetus, fetal presentation gestational age, onset of labor
# gives us dq,types of population at delivery (epidemiological characteristics), cs rates (clinical practice, judge care)
# these tables and results not linked to improved outcomes, just a way to monitor situation
# cant be taken as recommendations


cpo <- d[yearofdeliv>=2016 & 
           !is.na(cpoevent_1),.(NumCPO=.N,
                               hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
                               CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T),
                               HasRobsongp=sum(!is.na(robsongp))),
         keyby=.(yearofdeliv)]

cpo[,percCS:=round(CSdeliv/hasmodedeliv, digits=3)]

openxlsx::write.xlsx(cpo, file.path(FOLDER_DATA_RESULTS,
                                    "cs",
                                    "cs_delivs.xlsx"))

#############################################################################################################
