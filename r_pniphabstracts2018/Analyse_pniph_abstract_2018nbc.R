Analyse_pniph_abstract_2018_nbc_services<- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  
  # those lines are the same just the first one is one step(take it and replace) the other one is 2 steps(find make it yes then replace it)
  #vars_nbcmodeofdelivery	<- stringr::str_subset(names(d),"^nbcmodeofdelivery_")
  #vars_nbcplaceofdelivery	<- names(d)[stringr::str_detect(names(d),"^nbcplaceofdelivery_")]
  
  ###For making categories
  # to do loops with adding anew variables and renaming them instead of long way 
  vars <- c()
  arguments <- c("^nncriskevent_",
                 "^nncriskdate_",
                 "^nncriskorgname_",
                 "^nncriskidnumber_",
                 "^nncrisktype_",
                 "^nncriskdesx_",
                 "^nncriskdesy_",
                 "^ident_dhis2_nncrisk",
                 "^nbmanmanevent_",
                 "^nbmandate_",
                 "^nbmanorgname_",
                 "^nbmanidnumber_",
                 "^nbmandetail_",
                 "^nbmantypex_",
                 "^nbmanperf_",
                 "^nbmantypey_",
                 "^ident_dhis2_nbman",
                 "^nbcconditionofbaby_",
                 "^nbcsuspectedcongenitalmalformation_",
                 "^nbcsuspectedjaundice_",
                 "^nbcweightgrams_",
                 "^nbcbirthweightgrams_",
                 "^nbcgestationalageatdelivery_",
                 "^nbcevent_",
                 "^nbcheadcircumferencecm_",
                 "^nbcheightcm_",
                 "^nbcrespiratoryratebreathmin_",
                 "^nbcpulsebeatmin_",
                 "^nbcumbilicalstump_",
                 "^nbccyanosis_",
                 "^nbcdefecation_",
                 "^nbcurination_",
                 "^nbccounselingondangersignsfornewborn_",
                 "^nbccounselingaboutumbilicalcordcare_",
                 "^nbccounselingaboutvitaminsandsupplements_",
                 "^nbcvitaminaddropsgiven_",
                 "^nbclabscreeningperformedforpku_",
                 "^nbclabresultofpkuscreening_",
                 "^nbclabscreeningperformedforcongenitalhypothyreoidism_",
                 "^nbclabresultofcongenitalhypothyreoidismchscreening_",
                 "^nbcnewbornfeeding_",
                 "^nbcsex_",
                 "^nbcweightgrams_",
                 "^nbcbirthweightgrams_",
                 "^nbcgestationalageatdelivery_",
                 "^nbcevent_",
                 "^nbchepb_",
                 "^nbclatching_",
                 "^nbcnewborn_",
                 "^nbcchilddesignation_",
                 "^nbcbreastfeedingposition_",
                 "^nncriskdate_",
                 "^nbctemperaturec_"
  )
  for(i in arguments){
    print(i)
    result <- stringr::str_subset(names(d),i)
    vars <- c(vars,result)
  }
  
  #nbc risks need to add in our data set and make categories
  #NBCrisktype below
  #Umbilicalstumpinfected,Jaundice,Abnormaldefecation,
  #Congenitalanomalies,PKUpositive,Cyanosis,Abnormaldefection,
  #Abnormalurination
  
  #NBC Managments need to add this into the data set as well
  #nbcManagement detail(same as above but with spaces)
  #nbcRisk Type: RefALC or empty (referred to appropriate level of care)
  #management performed (yes or no)
 
  
  # 1 = risk+management
  # 2 = risk+appropriate management
  links <- list(
    "pniph_congenitalanomalies"="Congenital anomalies",
    "pniph_jaundice"="Jaundice",
    "pniph_cyanosis"="Cyanosis",
    "pniph_umbilicalstumpinfected"="Umbilicalstumpinfected",
    "pniph_abnormaldefecation"="Abnormaldefecation",
    "pniph_abnormalurination"="Abnormalurination"
  )
  
  for(j in 1:length(links)){
    newVar <- names(links)[[j]]
    risk <- links[[j]]
    
    d[,(newVar):=as.numeric(NA)]
    v	<- stringr::str_subset(names(d),"^nbmandetail_")
    v <- stringr::str_remove(v,"nbmandetail_")
    for(i in v){
      d[get(sprintf("nbmandetail_%s",i))==risk,
        (newVar):=1]
      d[get(newVar)==1 & get(sprintf("nbmantypex_%s",i))=="RefALC",
        (newVar):=2]
    }
  }
  var_risks <- names(links)
  
  smallD <-d[ident_dhis2_control==F &
               ident_dhis2_nbc==T
             ,
             unique(c(
               "bookorgdistrict",
               vars,
               var_risks
             )),with=F]
  
  # duplicate the dataset
  # make one of them have a district of palestine
  # then put them on top of each other
  smallDPalestine <- copy(smallD)
  smallDPalestine[,bookorgdistrict:="0PALESTINE"]
  
  smallD <- rbind(smallD,smallDPalestine)
  smallD[,id:=1:.N]
  
  long <- melt.data.table(smallD, id.vars=c(
    "id",
    "bookorgdistrict"
  ),variable.factor = F, value.factor = F)
  
  long[,visit:=stringr::str_extract(variable,"[0-9]*$")]
  long[,datasource:=stringr::str_sub(variable,1,3)]
  
  uglytable <- long[,
                    .(not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      value2=sum(value==2,na.rm=T),
                      value3=sum(value==3,na.rm=T),
                      valueMean=mean(as.numeric(value),na.rm=T),
                      valueMedian=median(as.numeric(value),na.rm=T),
                      value25thpercentile=quantile(as.numeric(value),na.rm=T,probs = 0.25),
                      value75thpercentile=quantile(as.numeric(value),na.rm=T,probs = 0.75),
                      valueMin=min(as.numeric(value),na.rm=T),
                      valueMax=max(as.numeric(value),na.rm=T),
                      valueSpontaneousVaginal=sum(value=="Spontaneous vaginal",na.rm=T),
                      valueCaesarianSection=sum(value=="Caesarian section",na.rm=T),
                      valueAssistedVaginal=sum(value=="Assisted vaginal",na.rm=T),
                      valueVacuum=sum(value=="VACCUUM",na.rm=T),
                      valueGOV=sum(value=="GOV",na.rm=T),
                      valuePH=sum(value=="PH",na.rm=T),
                      valuePC=sum(value=="PC",na.rm=T),
                      valueNGO=sum(value=="NGO",na.rm=T),
                      valueUNRWA=sum(value=="UNRWA",na.rm=T),
                      valueTRANS=sum(value=="TRANS",na.rm=T),
                      valueHOME=sum(value=="HOME",na.rm=T)
                    ),
                    keyby=.(
                      bookorgdistrict,
                      variable,
                      visit,
                      datasource)
                    ]
  

  denoms <- long[stringr::str_detect(variable,"^nncriskevent_") |
                stringr::str_detect(variable,"^nbmanmanevent_")|
                stringr::str_detect(variable,"^nbcevent_"),       
                 .(
                   denominator=sum(!is.na(value))
                 ),
                 keyby=.(
                   bookorgdistrict,
                   visit,
                   datasource)
                 ]

  prettytable <- merge(uglytable,
                       denoms,
                       by=c("visit",
                       "bookorgdistrict",
                       "datasource")
                       )
  
  prettytable[,na:=denominator-not_NA]
  prettytable[,percentage_of_completeness:=round(not_NA/denominator*100)]
  
  setcolorder(prettytable,c( "visit",
                            "bookorgdistrict",
                            "datasource",
                            "variable",
                            "denominator",
                            "percentage_of_completeness",
                            "na",
                            "not_NA"))
  

  openxlsx::write.xlsx(prettytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "nbccompleteness.xlsx"))
  
  
  
  #### number of clinics and visits, etc
  d <- d[
    ident_dhis2_control==F &
      ident_dhis2_nbc==T]
  unique(d$nbcorgname_1)
  tryCatch({
    # number of visits
    tab <- d[,.(
      NWomen=.N,
      numVisit1=sum(!is.na(nbcevent_1)),
      numVisit2=sum(!is.na(nbcevent_2)),
      numVisit3=sum(!is.na(nbcevent_3))
    ),keyby=.(
      nbcorgname_1
    )]
    
    openxlsx::write.xlsx(tab, 
                         file.path(
                           FOLDER_DROPBOX_RESULTS,
                           "pniph",
                           "abstracts_2018",
                           "nbc_list_of_clinics.xlsx"))
  })
  
  
}

# this is the long format version of the functions below
LONGPresentingNewBorn <- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  d <- d[ident_dhis2_nbc==T]
  
  long <- d[,stringr::str_subset(names(d),"^nbc"),with=F]
  long[,id:=1:.N]
  names(long)
  
  d[,nbcyear:=year(nbcdate_1)]
  
  
  ## TABLES
  uglytable <- d[,
                 .(
                   NumOfChild_1=sum(!is.na(nbcevent_1)),
                   NumOfChild_2=sum(!is.na(nbcevent_2)),
                   MeanWeight_1=mean(nbcweightgrams_1,na.rm=T),
                   MeanWeight_2=mean(nbcweightgrams_2,na.rm=T),
                   MeanHeight_1=mean(nbcheightcm_1,na.rm=T),
                   MeanHeight_2=mean(nbcheightcm_2,na.rm=T),
                   MeanHeadCircum_1=mean(nbcheadcircumferencecm_1,na.rm=T),
                   MeanHeadCircum_2=mean(nbcheadcircumferencecm_2,na.rm=T)
                 ),
                 keyby=.(nbcyear,bookorgdistrict)]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "birthoutcomesperdistrict.xlsx"))
}


PresentingNewBorn <- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  d <- d[ident_dhis2_nbc==T]
  
  d[,nbcyear:=year(nbcdate_1)]
  
  
  ## TABLES
  uglytable <- d[,
                  .(
                    NumOfChild_1=sum(!is.na(nbcevent_1)),
                    NumOfChild_2=sum(!is.na(nbcevent_2)),
                    MeanWeight_1=mean(nbcweightgrams_1,na.rm=T),
                    MeanWeight_2=mean(nbcweightgrams_2,na.rm=T),
                    MeanHeight_1=mean(nbcheightcm_1,na.rm=T),
                    MeanHeight_2=mean(nbcheightcm_2,na.rm=T),
                    MeanHeadCircum_1=mean(nbcheadcircumferencecm_1,na.rm=T),
                    MeanHeadCircum_2=mean(nbcheadcircumferencecm_2,na.rm=T)
                  ),
                  keyby=.(nbcyear,bookorgdistrict)]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "birthoutcomesperdistrict.xlsx"))
  
  
  uglytable <- d[,
                  .(
                    NumOfChild_1=sum(!is.na(nbcevent_1)),
                    NumOfChild_2=sum(!is.na(nbcevent_2)),
                    MeanWeight_1=mean(nbcweightgrams_1,na.rm=T),
                    MeanWeight_2=mean(nbcweightgrams_2,na.rm=T),
                    MeanHeight_1=mean(nbcheightcm_1,na.rm=T),
                    MeanHeight_2=mean(nbcheightcm_2,na.rm=T),
                    MeanHeadCircum_1=mean(nbcheadcircumferencecm_1,na.rm=T),
                    MeanHeadCircum_2=mean(nbcheadcircumferencecm_2,na.rm=T)
                  ),
                  keyby=.(nbcyear,nbcorgname_1)]
  
  
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "birthoutcomesperclinic.xlsx"))
  uglytable <- d[,
                  .(
                    NumOfresp_1=sum(!is.na(nbcrespiratoryratebreathmin_1)),
                    NumOfpulse_1=sum(!is.na(nbcpulsebeatmin_1)),
                    NumOftemp_1=sum(!is.na(nbctemperaturec_1)),
                    NumOfcconditionofbaby_1=sum(!is.na(nbcconditionofbaby_1)),
                    
                    Numofumpstump_1=sum(nbcumbilicalstump_1=1),
                    counsdangersign_1=sum(nbccounselingondangersignsfornewborn_1=1),
                    counsumpcare_1=sum(nbccounselingaboutumbilicalcordcare_1=1),
                    counsvitsupp_1=sum(nbccounselingaboutvitaminsandsupplements_1=1),
                    numofadol_1=sum(nbcvitaminaddropsgiven_1=1),
                
                    
                    screenpku_1=sum(nbclabscreeningperformedforpku_1=1),
                    screenpkupositive_1=sum(nbclabresultofpkuscreening_1=1),
                    screenpkunegative_1=sum(nbclabresultofpkuscreening_1=2),
                    
                    screenCMF_1=sum(nbclabscreeningperformedforcongenitalhypothyreoidism_1=1),
                    screenCMFpositive_1=sum(nbclabresultofcongenitalhypothyreoidismchscreening_1=1),
                    screenCMFnegative_1=sum(nbclabresultofcongenitalhypothyreoidismchscreening_1=2),
                    
                    
                    sumofnokitesforpku1=sum(nbclabscreeningperformedforpku_1=2),
                    sumofnokitesforCMF1=sum(nbclabscreeningperformedforcongenitalhypothyreoidism_1=1),
                    
                    sumofNOSERVICES_OTHERforpku1=sum(nbclabscreeningperformedforpku_1=3),
                    sumofNOSERVICES_OTHERforCMF1=sum(nbclabscreeningperformedforcongenitalhypothyreoidism_1=3),
                    
                    NumofCMF_1=sum(!is.na(nbcsuspectedcongenitalmalformation_1)),
                    NumofJUNDIECE_1=sum(!is.na(nbcsuspectedjaundice_1)),
                    Numofcyanosis_1=sum(!is.na(nbccyanosis_1)),
                    Numofdefecation_1=sum(!is.na(nbcdefecation_1)),
                    Numofurination__1=sum(!is.na(nbcurination_1)),
                    
                    
                    Numofweight__1=sum(!is.na(nbcweightgrams_1)),
                    Numofbirthweight__1=sum(!is.na(nbcbirthweightgrams_1)),
                    Numofgest__1=sum(!is.na(nbcgestationalageatdelivery_1)),
                    Numofhead__1=sum(!is.na(nbcheadcircumferencecm_1)),
                    Numofheight__1=sum(!is.na(nbcheightcm_1)),
                  
                    Numoffeeding__1=sum(!is.na(nbcnewbornfeeding_1)),
                    Numofsex__1=sum(!is.na(nbcsex_1)),
                    Numoflatching__1=sum(!is.na(nbclatching_1)),
                    Numoffeedingposition__1=sum(!is.na(nbcbreastfeedingposition_1)),
                    ),
                  keyby=.(nbcyear,bookorgdistrict)]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "otherservices.xlsx"))
}

Graphs <- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  
  unique(d$nbcbreastfeeding_1)
  unique(d$nncrisktype_1)
  
  d <- d[ident_dhis2_nbc==T]
  
  d[,nbcyear:=year(nbcdate_1)]
  
  ### MAKING REAL DATA HERE FOR DISEASES
  #cyansis 
  d[,cyanosis_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbccyanosis_")
  for(i in vars){
    d[get(i)==1, cyanosis_disease:=TRUE]
  }
  
  d[cyanosis_disease==TRUE,cyanosis_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[cyanosis_disease==TRUE & get(i)=="Cyanosis", cyanosis_ref:=TRUE]
  }
  
  d[cyanosis_ref==TRUE,cyanosis_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[cyanosis_ref==TRUE & get(i)=="Cyanosis", cyanosis_man:=TRUE]
  }
  xtabs(~d$cyanosis_disease)
  xtabs(~d$cyanosis_ref)
  xtabs(~d$cyanosis_man)
  ## CYANOSIS ENDS
  
  #temp
  d[,cyanosis_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbccyanosis_")
  for(i in vars){
    d[get(i)==1, cyanosis_disease:=TRUE]
  }
  
  d[cyanosis_disease==TRUE,cyanosis_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[cyanosis_disease==TRUE & get(i)=="Cyanosis", cyanosis_ref:=TRUE]
  }
  
  d[cyanosis_ref==TRUE,cyanosis_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[cyanosis_ref==TRUE & get(i)=="Cyanosis", cyanosis_man:=TRUE]
  }
  xtabs(~d$cyanosis_disease)
  xtabs(~d$cyanosis_ref)
  xtabs(~d$cyanosis_man)
  ## temp ENDS
 ## figures for feeding
  uglytable <- d[,
                  .(
                    N=.N
                  ),
                  keyby=.(nbcnewbornfeeding_1)]
  
  uglytable[,prettyX:="Missing"]
  uglytable[nbcnewbornfeeding_1==1, prettyX:="Normal feeding"]
  uglytable[nbcnewbornfeeding_1==2, prettyX:="Artificial feeding"]
  uglytable[nbcnewbornfeeding_1==3, prettyX:="Both"]
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N))
  p <- p + geom_col()
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "feeding.png"
    ), plot = p, width = 297, height = 210, units = "mm")
  
  
    ###figures for referral services
  uglytable <- d[,
                 .(
                   N=.N
                 ),
                 keyby=.(nncrisktype_1)]
  
  uglytable[,prettyX:="Missing"]
  uglytable[nncrisktype_1=="Jaundice"]
  uglytable[nncrisktype_1=="Umbilicalstumpinfected"]
  uglytable[nncrisktype_1== "Abnormaldefecation"]
  uglytable[nncrisktype_1== "Abnormalurination"] 
  uglytable[nncrisktype_1== "Cyanosis"]
  uglytable[nncrisktype_1== "Congenitalanomalies"]
 
  
  
  p <- ggplot(uglytable, aes(x=nncrisktype_1, y=N))
  p <- p + geom_col()
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "risktypes.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  
  
  
  ### CYANOSIS

  uglytable <- d[,
                 .(
                   cyanosis_disease=sum(cyanosis_disease,na.rm=T),
                   cyanosis_ref=sum(cyanosis_ref,na.rm=T),
                   cyanosis_man=sum(cyanosis_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  
  p <- ggplot(uglytable, aes(x=variable, y=value))
  p <- p + geom_col()
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "graph2.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
    
}

