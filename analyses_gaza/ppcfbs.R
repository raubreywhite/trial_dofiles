
# run T2 process outcomes (smallD <-d)

# Run T2_GDM code and key by ident_dhis2_ppc to get women who were referred or not

# then do the below seperately


ppc <- smallD


vars <- names(ppc)[stringr::str_detect(names(ppc),"^ppcevent_")]

ppc[,numppcvisits:=0]

for( i in vars){
  
  ppc[!is.na(get(i)), numppcvisits:=numppcvisits+1 ]
  
}

xtabs(~ppc$numppcvisits, addNA=T)

ppc[,ppcyear_1:=stringr::str_extract(ppcdate_1,"^[0-9][0-9][0-9][0-9]")]
xtabs(~ppc$ppcyear_1, addNA=T)

visits <- ppc[,.(N=.N,
                 One_Visit=sum(numppcvisits>=1, na.rm=T),
                 Two_Visits=sum(numppcvisits>=2, na.rm=T),
                 Three_Visits=sum(numppcvisits>=3, na.rm=T),
                 Four_visits=sum(numppcvisits==4, na.rm=T)),
              keyby=.(ppcyear_1)]

 openxlsx::write.xlsx(visits,
                      file.path(FOLDER_DATA_RESULTS_GAZA,
                                "ppc_gdm",
                                "Coverage_ppc_Visits.xlsx"))
 
 
 # change lab date format
 varslabdate <- names(ppc)[stringr::str_detect(names(ppc), "labdate_")]
 
 for( i in varslabdate){
   
  ppc[, (i):=as.Date(get(i),
                     format="%Y-%m-%d")]
   
 }
 
 xtabs(~ppc$labdate_1, addNA=T)
 
 
 varslabanpp <- names(ppc)[stringr::str_detect(names(ppc),"^labanpp_")]
 varslabfastglu <- names(ppc)[stringr::str_detect(names(ppc),"^labfastbloodglu_")]

 
 
 # women who have lab at ppc
 
 ppc[,ppclab:=FALSE]
 
 
 
for(i in varslabanpp){
  
  
  ppc[get(i)=="PPC",ppclab:=TRUE ]
  

  
  
}
 xtabs(~ppc$ppclab)
 

 
 
 
 nam <- names(ppc)[stringr::str_detect(names(ppc),"^labanpp_[0-9]*$")]
 num <- stringr::str_replace(nam,"labanpp_","")
 for(i in num){
   print(i)
   
    
   ppc[ppclab==TRUE & 
         ppcdate_1==get(sprintf("labdate_%s",i)) &
         get(sprintf("labanpp_%s", i))=="PPC" &
         !is.na(get(sprintf("labfastbloodglu_%s",i))) ,
     firstppcfbs:=get(sprintf("labfastbloodglu_%s",i))]
   
   
   
   
   ppc[ppclab==TRUE & 
         ppcdate_1==get(sprintf("labdate_%s",i)) &
         get(sprintf("labanpp_%s", i))=="PPC" &
         !is.na(get(sprintf("labbloodglu_%s",i))) ,
       firstppcrbg:=get(sprintf("labbloodglu_%s",i))]
   
   
  
   
 }
 
 xtabs(~ppc$firstppcfbs, addNA=T)
 xtabs(~ppc$firstppcrbg, addNA=T)
 
 
 
 nam <- names(ppc)[stringr::str_detect(names(ppc),"^labanpp_[0-9]*$")]
 num <- stringr::str_replace(nam,"labanpp_","")
 for(i in num){
   print(i)
   
   
   ppc[ppclab==TRUE & get(sprintf("labanpp_%s", i))=="PPC" &
         ppcdate_1>=get(sprintf("labdate_%s",i)) &
         !is.na(get(sprintf("labfastbloodglu_%s",i))),
       (sprintf("ppcfbs_%s",i)):=get(sprintf("labfastbloodglu_%s",i))]
   
   
 }
 
 xtabs(~ppc$ppcfbs_1, addNA=T)
 
 
 ppc[T2_labfastbloodglu_high_00_14==T|
       T2_labfastbloodglu_high_15_17==T|
       T2_labfastbloodglu_high_18_22==T|
       T2_labfastbloodglu_high_23_23==T|
       T2_labfastbloodglu_high_24_28==T|
       T2_labfastbloodglu_high_29_30==T|
       T2_labfastbloodglu_high_31_33==T|
       T2_labfastbloodglu_high_34_34==T|
       T2_labfastbloodglu_high_35_37==T,highfbsall:=T]
 xtabs(~ppc$highfbsall, addNA=T)
 
 
 ppc[T2_labbloodglu_high_00_14==T|
       T2_labbloodglu_high_15_17==T|
       T2_labbloodglu_high_18_22==T|
       T2_labbloodglu_high_23_23==T|
       T2_labbloodglu_high_24_28==T|
       T2_labbloodglu_high_29_30==T|
       T2_labbloodglu_high_31_33==T|
       T2_labbloodglu_high_34_34==T|
       T2_labbloodglu_high_35_37==T,highrbgall:=T]
 
 xtabs(~ppc$highrbgall, addNA=T)
 
 
 
 ppc[,anyppcfbs:=as.logical(NA)]
 vars <- names(ppc)[stringr::str_detect(names(ppc),"^ppcfbs_")]
 
 for(i in vars){
   
   ppc[!is.na(get(i)), anyppcfbs:=FALSE]
   ppc[anyppcfbs==F & 
         (highfbsall==T |
            highrbgall==T), anyppcfbs:=T]
   
 }
 
xtabs(~ppc$anyppcfbs, addNA = T) 
 
varsGDM <- names(ppc)[stringr::str_detect(names(ppc), "^GDM")]

varsppcfbs <- names(ppc)[stringr::str_detect(names(ppc),"^ppcfbs_")]

check <-ppc[highfbsall==T, c("firstppcfbs",
                             "firstppcfbs",
                             "highfbsall",
                              varsGDM,
                              varsppcfbs), with=F]
 
 coverage <- ppc[bookyear>2016,.(N=.N,
                    "High RBG anytime at ANC"=sum(highrbgall==T, na.rm=T),
                    "High FBS anytime at ANC"=sum(highfbsall==T, na.rm=T),
                    "Either High FBS or RBG"=sum(highfbsall==T |
                                                   highfbsall==T, na.rm=T),
                    "High FBS and ANC and PPC"=sum(highfbsall==T &
                                                     ident_dhis2_booking==T &
                                             ident_dhis2_ppc==T, na.rm=T),
                    "Have GDM and attended PPC"=sum(highfbsall==T &
                                                      ident_dhis2_ppc==T, na.rm=T),
                    "Have GDM and  PPC >=1"=sum(highfbsall==T &
                                                      ident_dhis2_ppc==T & 
                                                      numppcvisits>=1, na.rm=T),
                    
                    "All who have fbs at first ppc"=sum(firstppcfbs>0, na.rm=T),
                    "GDM and Attended PPC Screened Day 1 PPC"=sum(highfbsall==T &
                                                                    ident_dhis2_ppc==T &
                                                                    firstppcfbs>0, na.rm=T),
                    "GDM and Attended PPC Screened for GDM at any PPC"=sum(highfbsall==T &
                                                                    ident_dhis2_ppc==T &
                                                                    firstppcfbs>0, na.rm=T),
                    "GDm via RBG and screened day 1 PPC"=sum(highrbgall==T &
                                                               ident_dhis2_ppc==T &
                                                               anyppcfbs==T, na.rm=T)),
                 
                 keyby=.(bookyear)]
 
 openxlsx::write.xlsx(coverage,
                      file.path(FOLDER_DATA_RESULTS_GAZA,
                                "ppc_gdm",
                                "Coverage_gdm.xlsx"))
 

##### from the system
 
ppc[,likelygdm:=as.logical(NA)] 
 

risktype <- names(ppc)[stringr::str_detect(names(ppc),"^risktype_")]

for(i in risktype){
  
  
  
  ppc[get(i)=="LikelyGDM", likelygdm:=TRUE]
  
}

xtabs(~smallD$likelygdm)





ppc[,gdmafter24:=as.logical(NA)] 


risktype <- names(ppc)[stringr::str_detect(names(ppc),"^risktype_")]

for(i in risktype){
  
  riskgestage <-names(ppc)[stringr::str_detect(names(ppc),"^riskgestage_")]
  
  ppc[get(i)=="LikelyGDM" &
        get(riskgestage)>=24 &
        get(riskgestage)<=28, gdmontime:=TRUE]
  
  ppc[get(i)=="LikelyGDM" &
        get(riskgestage)>=24, gdmafter24:=TRUE]
  
  
  
}

xtabs(~smallD$gdmafter24)




visits <- ppc[,.(N=.N,
                 One_Visit=sum(numppcvisits==1, na.rm=T),
                 NotMissingFBG=sum(!is.na(ppcfbs_1)),
                 Two_Visits=sum(numppcvisits==2, na.rm=T),
                 Three_Visits=sum(numppcvisits==3, na.rm=T),
                 Four_visits=sum(numppcvisits==4, na.rm=T)),
              keyby=.(ppcyear_1)]

openxlsx::write.xlsx(visits,
                     file.path(FOLDER_DATA_RESULTS_GAZA,
                               "ppc_gdm",
                               "Coverage_GDM_and_ppc_Visits.xlsx"))



     