
# run T2 process outcomes

# Run GDM code and key by ident_dhis2_ppc to get women who were referred or not

# then do the below seperately


ppc <- d[ident_dhis2_ppc==T]

vars <- names(ppc)[stringr::str_detect(names(ppc),"^ppcevent_")]

ppc[,numppcvisits:=0]

for( i in vars){
  
  ppc[!is.na(get(i)), numppcvisits:=numppcvisits+1 ]
  
}

xtabs(~ppc$numppcvisits, addNA=T)

ppc[,ppcyear_1:=stringr::str_extract(ppcdate_1,"^[0-9][0-9][0-9][0-9]")]
xtabs(~ppc$ppcyear_1, addNA=T)

visits <- ppc[,.(N=.N,
                 One_Visit=sum(numppcvisits==1, na.rm=T),
                 Two_Visits=sum(numppcvisits==2, na.rm=T),
                 Three_Visits=sum(numppcvisits==3, na.rm=T),
                 Four_visits=sum(numppcvisits==4, na.rm=T)),
              keyby=.(ppcyear_1)]

 openxlsx::write.xlsx(visits,
                      file.path(FOLDER_DATA_RESULTS_GAZA,
                                "ppc_gdm",
                                "Coverage_ppc_Visits.xlsx"))
 
 
 varslabdate <- names(ppc)[stringr::str_detect(names(ppc), "labdate_")]
 
 for( i in varslabdate){
   
  ppc[, (i):=as.Date(get(i),
                     format="%Y-%m-%d")]
   
 }
 
 xtabs(~ppc$labdate_1, addNA=T)
 
 
 varslabanpp <- names(ppc)[stringr::str_detect(names(ppc),"^labanpp_")]
 varslabfastglu <- names(ppc)[stringr::str_detect(names(ppc),"^labfastbloodglu_")]

 
 ppc[,ppclab:=FALSE]
 
 
 
for(i in varslabanpp){
  
  
  ppc[get(i)=="PPC",ppclab:=TRUE ]
  
  
}
 
 
 
 ppc[,ppcfirslab:=as.logical(NA)]
 
 for(i in varslabdate){
   
   ppc[ppclab==T & get(i)==ppcdate_1, ppcfirstlab:=TRUE]
   
 }
 
xtabs(~ppc$ppcfirstlab, addNA=T) 





ppc[,ppcfbs_1:=as.integer(NA)]


for(i in varslabdate){
  
  ppc[ppclab==T & get(i)==ppcdate_1, ppclabdate_1:=get(i)]
  
  
  
}

xtabs(~ppc$ppcfirstlab, addNA=T) 

 



for(i in varslabdate){
  
  
  varslabfastglu <- names(ppc)[stringr::str_detect(names(ppc),"^labfastbloodglu_")]
  
  ppc[ppclab==T & get(i)==ppcdate_1, ppcfbs_1:=get(varslabfastglu)]
  
  
  
}

xtabs(~ppc$ppcfbs_1, addNA=T) 


nrow(ppc[!is.na(ppcfbs_1)])


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



     