


####LOAD d from Network####
d <- LoadDataFileFromNetwork()

nrow(d)
xtabs(~d$bookyear, addNA=T)
xtabs(~ident_dhis2_booking+bookyear, data=d)

# id year of birth for pregnancies
d$expecteddateofdelivery

d[,yearofexpdeliv:=stringr::str_extract(expecteddateofdelivery,"^[0-9][0-9][0-9][0-9]")]
d[,yearofexpdeliv:=as.numeric(yearofexpdeliv)]
xtabs(~d$yearofexpdeliv, addNA=T)

cpo <- d[yearofexpdeliv>=2016 & !is.na(cpoevent_1),.(N=.N,
            hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
            CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T)),
         keyby=.(yearofexpdeliv)]

cpo[,percCS:=round(CSdeliv/hasmodedeliv, digits=3)]

openxlsx::write.xlsx(cpo, file.path(FOLDER_DATA_RESULTS,
                                        "cs",
                                        "cs_delivs.xlsx"))

cpoplace <- d[yearofexpdeliv>=2016 &
                !is.na(cpoevent_1),.(N=.N,
                                hasmodedeliv=sum(!is.na(cpomodedelivery_1)),
                                CSdeliv=sum(cpomodedelivery_1=="Caesarian section",na.rm=T)),
         keyby=.(yearofexpdeliv,cpoplaceofbirth_1)]

cpoplace[,numcsperyear:=sum(CSdeliv), by=yearofexpdeliv]
cpoplace[,perCS:=round(CSdeliv/numcsperyear, digits=2)]

openxlsx::write.xlsx(cpoplace, file.path(FOLDER_DATA_RESULTS,
                                        "cs",
                                        "cs_placeofdeliv.xlsx"))




# CPO
#ppcbw lga and sga

#gA from ppcdeliv date or cpo gA, check them out
d[,gestageatbirth:=difftime(cpodate_1,
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

# robson classifications
## fix the variables
#primi, single cephalic preg,at term (37 or more) & spont deliv
d[bookprimi==1 &
     presatterm=="Cephalic" & 
     gestageatbirth>=259 &
     cpomodedelivery_1=="Spontaneous vaginal" &
     multifet==F,robsgp_1:=TRUE]
xtabs(~d$robsgp_1, addNA=T)

#primi, single cephalic preg,at term (37 or more) & deliv==csec or induced
d[bookprimi==1 &
     presatterm=="Cephalic" & 
     gestageatbirth>=259 &
     cpomodedelivery_1 %in% c("Caesarian section") &
     multifet==F,robsgp_2:=TRUE]

xtabs(~d$robsgp_2, addNA=T)


#multiparous, single cephalic preg,at term (37 or more) & deliv= NVSD & no prev ut scar
d[bookprimi==0 &
     presatterm=="Cephalic" & 
     gestageatbirth>=259 &
     cpomodedelivery_1=="Spontaneous vaginal" &
     (bookhistutesur==0 & bookhistcs==0) &
     multifet==F,robsgp_3:=TRUE]
xtabs(~d$robsgp_3, addNA=T)

#multiparous, single cephalic preg,at term (37 or more) & deliv= induced or cs & no prev ut scar
d[bookprimi==0 &
     presatterm=="Cephalic" & 
     gestageatbirth>=259 &
     cpomodedelivery_1 %in% c("Caesarian section") &
     (bookhistutesur==0 |bookhistcs==0) &
     multifet==F,robsgp_4:=TRUE]
xtabs(~d$robsgp_4, addNA=T)


#multiparous,at term (37 or more) & deliv= induced or cs & no prev ut scar
d[bookprimi==0 &
     presatterm=="Cephalic" & 
     gestageatbirth>=259 &
     cpomodedelivery_1 %in% c("Caesarian section","induced")&
     (bookhistutesur==1 | bookhistcs==1),robsgp_5:=TRUE]
xtabs(~d$robsgp_5, addNA=T)

#nulliparous,single preg, breech
d[bookprimi==1 &
     presatterm %in% c("Breech","Trasverse") &
     multifet==F,robsgp_6:=TRUE]
xtabs(~d$robsgp_6, addNA=T)

#nulliparous,single preg, breech
d[bookprimi==0 & 
     presatterm %in% c("Breech","Trasverse") &
     (bookhistutesur==1 | bookhistcs==1) &
     multifet==F,robsgp_7:=TRUE]
xtabs(~d$robsgp_7, addNA=T)


# all women with multiple preg and uterine scars
d[(bookhistutesur==1 | bookhistcs==1) &
     multifet==T,robsgp_8:=TRUE]
xtabs(~d$robsgp_8, addNA=T)

# all women with single preg and uterine scar
d[(bookhistutesur==1 | bookhistcs==1) &
     multifet==F,robsgp_9:=TRUE]
xtabs(~d$robsgp_9, addNA=T)

# all women with single cephalic preg and less than 37 week 

d[(gestageatbirth<259 & gestageatbirth>168) &
     presatterm=="Cephalic" & 
     multifet==F,robsgp_10:=TRUE]
xtabs(~d$robsgp_10, addNA=T)


robsongrps <- d[yearofexpdeliv>=2016 &
                  !is.na(cpoevent_1),.(
  N=.N,
  Group_1=sum(robsgp_1, na.rm=T),
  Group_1F=sum(robsgp_1==FALSE, na.rm=T),
  Group_2=sum(robsgp_2, na.rm=T),
  Group_2F=sum(robsgp_2==F, na.rm=T),
  Group_3=sum(robsgp_3, na.rm=T),
  Group_3F=sum(robsgp_3==F, na.rm=T),
  Group_4=sum(robsgp_4, na.rm=T),
  Group_4F=sum(robsgp_4==F, na.rm=T),
  Group_5=sum(robsgp_5, na.rm=T),
  Group_5F=sum(robsgp_5==F, na.rm=T),
  Group_6=sum(robsgp_6, na.rm=T),
  Group_6F=sum(robsgp_6==F, na.rm=T),
  Group_7=sum(robsgp_7, na.rm=T),
  Group_7F=sum(robsgp_7==F, na.rm=T),
  Group_8=sum(robsgp_8, na.rm=T),
  Group_8F=sum(robsgp_8==F, na.rm=T),
  Group_9=sum(robsgp_9, na.rm=T),
  Group_9F=sum(robsgp_9==F, na.rm=T),
  Group_10=sum(robsgp_10, na.rm=T),
  Group_10=sum(robsgp_10==F, na.rm=T)
),

keyby=.(yearofexpdeliv)]


robsongrps[,denom:=sum(N), by=yearofexpdeliv]


openxlsx::write.xlsx(robsongrps,
                     file.path(
                       FOLDER_DATA_RESULTS,
                      "cs",
                       "Robsongps_byyearexpedeliv.xlsx"))

# cpo_mode of deliv category
d[,cpomodcat:=as.character(NA)]
d[cpomodedelivery_1 %in% c("Assisted vaginal","Spontaneous vaginal","VACUUM"),cpomodcat:="Normal or assisted"]
d[cpomodedelivery_1=="Caesarian section",cpomodcat:="CS"]
d[cpomodedelivery_1=="",cpomodcat:="Unspecified"]
xtabs(~d$cpomodcat, addNA=T)


# podcats
d[,podcat:=as.character(NA)]
d[cpoplaceofbirth_1 %in% c("GOV"),podcat:="Governmental"]
d[cpoplaceofbirth_1=="PH",podcat:="Private"]
d[!podcat %in% c("Private","Governmental") & !is.na(cpoplaceofbirth_1),podcat:="Other"]
xtabs(~d$podcat, addNA=T)



robsongrps <- d[yearofexpdeliv>=2016 &
                  !is.na(cpoevent_1),.(
  N=.N,
  Group_1=sum(robsgp_1, na.rm=T),
  Group_1F=sum(robsgp_1==FALSE, na.rm=T),
  Group_2=sum(robsgp_2, na.rm=T),
  Group_2F=sum(robsgp_2==F, na.rm=T),
  Group_3=sum(robsgp_3, na.rm=T),
  Group_3F=sum(robsgp_3==F, na.rm=T),
  Group_4=sum(robsgp_4, na.rm=T),
  Group_4F=sum(robsgp_4==F, na.rm=T),
  Group_5=sum(robsgp_5, na.rm=T),
  Group_5F=sum(robsgp_5==F, na.rm=T),
  Group_6=sum(robsgp_6, na.rm=T),
  Group_6F=sum(robsgp_6==F, na.rm=T),
  Group_7=sum(robsgp_7, na.rm=T),
  Group_7F=sum(robsgp_7==F, na.rm=T),
  Group_8=sum(robsgp_8, na.rm=T),
  Group_8F=sum(robsgp_8==F, na.rm=T),
  Group_9=sum(robsgp_9, na.rm=T),
  Group_9F=sum(robsgp_9==F, na.rm=T),
  Group_10=sum(robsgp_10, na.rm=T),
  Group_10=sum(robsgp_10==F, na.rm=T)
),

keyby=.(yearofexpdeliv,podcat)]


robsongrps[,denom:=sum(N), by=yearofexpdeliv]
robsongrps[,perc_Group_1:=round(Group_1/N, digits=3)]
robsongrps[,perc_Group_2:=round(Group_2/N, digits=3)]
robsongrps[,perc_Group_3:=round(Group_3/N, digits=3)]
robsongrps[,perc_Group_4:=round(Group_4/N, digits=3)]
robsongrps[,perc_Group_5:=round(Group_5/N, digits=3)]
robsongrps[,perc_Group_6:=round(Group_6/N, digits=3)]
robsongrps[,perc_Group_7:=round(Group_7/N, digits=3)]
robsongrps[,perc_Group_8:=round(Group_8/N, digits=3)]
robsongrps[,perc_Group_9:=round(Group_9/N, digits=3)]
robsongrps[,perc_Group_10:=round(Group_10/N, digits=3)]


openxlsx::write.xlsx(robsongrps,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "cs",
                       "Robsongps_podcat.xlsx"))

# robson key by cpo_pod
