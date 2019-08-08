###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

CheckFilesAndVariables(folder="e.reg-intervention")
CheckFilesAndVariables(folder="e.reg-control")

###### SETUP ENDS ######
LoadNBCLongFromNetworkWB <- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  
  
  d[,uniquepregid_1:=1:.N]
  d[,uniquepregid_2:=1:.N]
  d[,uniquepregid_3:=1:.N]
  d[,uniquepregid_4:=1:.N]
  
  # those lines are the same just the first one is one step(take it and replace) the other one is 2 steps(find make it yes then replace it)
  #vars_nbcmodeofdelivery	<- stringr::str_subset(names(d),"^nbcmodeofdelivery_")
  #vars_nbcplaceofdelivery	<- names(d)[stringr::str_detect(names(d),"^nbcplaceofdelivery_")]
  
  ###For making categories
  # to do loops with adding anew variables and renaming them instead of long way 
  variablenames <- c(
    "^uniquepregid_",
    "^nbcorgdistrict_",
    "^nbcorgname_",
    "^nncriskevent_",
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
    "^nbcweightgrams_",
    "^nbcbirthweightgrams_",
    "^nbcgestationalageatdelivery_",
    "^nbcevent_",
    "^nbcheadcircumferencecm_",
    "^nbcheightcm_",
    "^nbcrespiratoryratebreathmin_",
    "^nbcpulsebeatmin_",
    "^nbcsuspectedcongenitalmalformation_",
    "^nbcsuspectedjaundice_",
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
    "^nbcdate_",
    "^nbctemperaturec_"
  )
  # strip out the duplicates
  variablenames <- unique(variablenames)
  
  # longnames is the names of variable in long format
  longnames = c()
  # widenames is the names of variable in wide format
  widenames = list()
  for(i in seq_along(variablenames)){
    print(i)
    # use ExtractOnlyEnglishLetters to get rid of ^ and _
    longnames <- c(longnames, ExtractOnlyEnglishLetters(variablenames[i])[[1]])
    widenames[[i]] <- stringr::str_subset(names(d),variablenames[i])
  }
  
  nbclong = melt(d[ident_dhis2_nbc==T,unlist(widenames),with=F],
                 measure = widenames,
                 value.name = longnames)
  # get rid of people who are missing nbcevent
  sum(is.na(nbclong$uniquepregid))
  nbclong <- nbclong[!is.na(nbcevent)]
  sum(is.na(nbclong$uniquepregid))
  
  return(nbclong)
}

Analyse_pniph_abstract_2018_nbc_services<- function(){
  nbclong <- LoadNBCLongFromNetworkWB()
  
  
  #nbc risks need to add in our data set and make categories
  #NBCrisktype below
  
  #Umbilicalstumpinfected,Jaundice,Abnormaldefecation,
  #Congenitalanomalies,PKUpositive,Cyanosis,Abnormaldefection,
  #Abnormalurination
  
  #NBC Managments need to add this into the data set as well
  #nbcManagement detail(same as above but with spaces)
  #nbcRisk Type: RefALC or empty (referred to appropriate level of care)
  #management performed (yes or no)
  
  # 
  # # 1 = risk+management
  # # 2 = risk+appropriate management
  # links <- list(
  #   "pniph_congenitalanomalies"="Congenital anomalies",
  #   "pniph_jaundice"="Jaundice",
  #   "pniph_cyanosis"="Cyanosis",
  #   "pniph_umbilicalstumpinfected"="Umbilicalstumpinfected",
  #   "pniph_abnormaldefecation"="Abnormaldefecation",
  #   "pniph_abnormalurination"="Abnormalurination"
  # )
  # 
  # for(j in 1:length(links)){
  #   newVar <- names(links)[[j]]
  #   risk <- links[[j]]
  #   
  #   d[,(newVar):=as.numeric(NA)]
  #   v	<- stringr::str_subset(names(d),"^nbmandetail_")
  #   v <- stringr::str_remove(v,"nbmandetail_")
  #   for(i in v){
  #     d[get(sprintf("nbmandetail_%s",i))==risk,
  #       (newVar):=1]
  #     d[get(newVar)==1 & get(sprintf("nbmantypex_%s",i))=="RefALC",
  #       (newVar):=2]
  #   }
  # }
  # var_risks <- names(links)
  
  smallD <- copy(nbclong)
  
  # duplicate the dataset
  # make one of them have a district of palestine
  # then put them on top of each other
  smallDPalestine <- copy(smallD)
  smallDPalestine[,nbcorgdistrict:="0PALESTINE"]
  
  smallD <- rbind(smallD,smallDPalestine)
  smallD[,id:=1:.N]
  
  long <- melt.data.table(smallD, id.vars=c(
    "id",
    "nbcorgdistrict"
  ),variable.factor = F, value.factor = F)
  
  long[,visit:=stringr::str_extract(variable,"[0-9]*$")]
  long[,datasource:=stringr::str_sub(variable,1,3)]
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      na=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
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
                      nbcorgdistrict,
                      variable,
                      visit,
                      datasource)
                    ]
  
  prettytable <- uglytable
  prettytable[,percentage_of_completeness:=round(not_NA/denominator*100)]
  
  setcolorder(prettytable,c( "visit",
                             "nbcorgdistrict",
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
  tab <- nbclong[,
                 .(
                   numVisit=.N
                 ),
                 keyby=.(nbcorgdistrict,nbcorgname)
                 ]
  
  openxlsx::write.xlsx(tab, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "nbc_list_of_clinics.xlsx"))
  
  
}

# this is the long format version of the functions below
LONGPresentingNewBorn <- function(){
  nbclong <- LoadNBCLongFromNetworkWB()
  
  smallD <- copy(nbclong)
  smallD[,nbcorgdistrict:="0PALESTINE"]
  nbclong <- rbind(nbclong,smallD)
  
  nbclong[,nbcyear:=year(nbcdate)]
  # 
  # xtabs(~nbclong$nbcchilddesignation)
  # nbclong[nbcchilddesignation==4]$uniquepregid
  # nbclong[uniquepregid== 57353]$nbcchilddesignation
  # 
  # nbclong[uniquepregid== 38551]
  # 
  # # make every visit in the preg have the maximum number of children designation
  # nbclong[,numberofchildreninpreg:=length(unique(nbcchilddesignation)),
  #         by=uniquepregid]
  # # only the first visit is allowed to contain number of children
  # nbclong[variable!=1,numberofchildreninpreg:=NA]
  # nbclong[numberofchildreninpreg==2]$uniquepregid
  # 
  ## TABLES
  uglytable <-nbclong [,
                       .(
                         NumPregnancies=length(unique(uniquepregid)),
                         NumVisit=.N,
                         #Numchild=sum(numberofchildreninpreg,na.rm=T),
                         
                         MeanWeight=mean(nbcweightgrams,na.rm=T),
                         MedianWeight=median(nbcweightgrams,na.rm=T),
                         
                         MeanHeight=mean(nbcheightcm,na.rm=T),
                         MedianHeight=median(nbcheightcm,na.rm=T),
                         
                         MeanHeadCircum=mean(nbcheadcircumferencecm,na.rm=T),
                         MedianHeadCircum=mean(nbcheadcircumferencecm,na.rm=T)
                       ),
                       keyby=.(nbcyear,nbcorgdistrict)]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "birthoutcomesperdistrict.xlsx"))
}


PresentingNewBorn <- function(){
  nbclong <- LoadNBCLongFromNetworkWB()
  
  smallD <- copy(nbclong)
  smallD[,nbcorgdistrict:="0PALESTINE"]
  nbclong <- rbind(nbclong,smallD)
  
  nbclong[,nbcyear:=year(nbcdate)]
  
  uglytable <- nbclong[,
                       .(
                         NumOfresp=sum(!is.na(nbcrespiratoryratebreathmin)),
                         NumOfpulse=sum(!is.na(nbcpulsebeatmin)),
                         NumOftemp=sum(!is.na(nbctemperaturec)),
                         NumOfcconditionofbaby=sum(!is.na(nbcconditionofbaby)),
                         
                         Numofumpstump=sum(nbcumbilicalstump==1),
                         counsdangersign=sum(nbccounselingondangersignsfornewborn==1),
                         counsumpcare=sum(nbccounselingaboutumbilicalcordcare==1),
                         counsvitsupp=sum(nbccounselingaboutvitaminsandsupplements==1),
                         numofadol=sum(nbcvitaminaddropsgiven==1),
                         
                         screenpku=sum(nbclabscreeningperformedforpku==1),
                         screenpkupositive=sum(nbclabresultofpkuscreening==1),
                         screenpkunegative=sum(nbclabresultofpkuscreening==2),
                         
                         screenCMF=sum(nbclabscreeningperformedforcongenitalhypothyreoidism==1),
                         screenCMFpositive=sum(nbclabresultofcongenitalhypothyreoidismchscreening==1),
                         screenCMFnegative=sum(nbclabresultofcongenitalhypothyreoidismchscreening==2),
                         
                         
                         sumofnokitesforpku=sum(nbclabscreeningperformedforpku==2),
                         sumofnokitesforCMF=sum(nbclabscreeningperformedforcongenitalhypothyreoidism==1),
                         
                         sumofNOSERVICES_OTHERforpku=sum(nbclabscreeningperformedforpku==3),
                         sumofNOSERVICES_OTHERforCMF=sum(nbclabscreeningperformedforcongenitalhypothyreoidism==3),
                         
                         NumofCMF=sum(!is.na(nbcsuspectedcongenitalmalformation)),
                         NumofJUNDIECE=sum(!is.na(nbcsuspectedjaundice)),
                         Numofcyanosis=sum(!is.na(nbccyanosis)),
                         Numofdefecation=sum(!is.na(nbcdefecation)),
                         Numofurination=sum(!is.na(nbcurination)),
                         
                         Numofweight=sum(!is.na(nbcweightgrams)),
                         Numofbirthweight=sum(!is.na(nbcbirthweightgrams)),
                         Numofgest=sum(!is.na(nbcgestationalageatdelivery)),
                         Numofhead=sum(!is.na(nbcheadcircumferencecm)),
                         Numofheight=sum(!is.na(nbcheightcm)),
                         
                         Numoffeeding=sum(!is.na(nbcnewbornfeeding)),
                         Numofsex=sum(!is.na(nbcsex)),
                         Numoflatching=sum(!is.na(nbclatching)),
                         Numoffeedingposition=sum(!is.na(nbcbreastfeedingposition))
                       ),
                       keyby=.(nbcyear,nbcorgdistrict)]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "otherservices.xlsx"))
}


Graphsnbc <- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  
  nbclong <- LoadNBCLongFromNetworkWB()
  
  
  
  ## figures for feeding
  uglytable <-  nbclong[,
                        .(
                          N=.N
                        ),
                        keyby=.(nbcnewbornfeeding)]
  
  uglytable[,prettyX:="Missing"]
  uglytable[nbcnewbornfeeding==1, prettyX:="Normal feeding"]
  uglytable[nbcnewbornfeeding==2, prettyX:="Artificial feeding"]
  uglytable[nbcnewbornfeeding==3, prettyX:="Both"]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Feeding",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of Newborns(28 days or younger)")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "feeding.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  ###figures for referral services
  
  nbclong[nncrisktype=="Abnormaldefection",nncrisktype:="Abnormaldefecation"]
  uglytable <-  na.omit(nbclong[,
                                .(
                                  N=.N
                                ),
                                keyby=.(nncrisktype)])
  
  uglytable[,prettyX:="Missing"]
  uglytable[nncrisktype=="Jaundice", prettyX:="Jaundice"]
  uglytable[nncrisktype=="Umbilicalstumpinfected", prettyX:="Umbilical\nstump"]
  uglytable[nncrisktype== "Abnormaldefecation", prettyX:="Abnormal\ndefecation"]
  uglytable[nncrisktype== "Abnormalurination", prettyX:="Abnormal\nurination"] 
  uglytable[nncrisktype== "Cyanosis", prettyX:="Cyanosis"]
  uglytable[nncrisktype== "Congenitalanomalies", prettyX:="Congenital\nanomalies"]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of Newborns(up to 28 days old) with Risk")
  p <- p + theme_gray(20)
  p <- p + theme(legend.key = element_rect(size = 7),
                 legend.key.size = unit(2, 'lines'))
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "risktypes.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  
  
  
  ### MAKING REAL DATA HERE FOR DISEASES
  
  #cyanosis 
  unique(d$nbccyanosis_1)
  unique(d$nncrisktype_1)
  unique(d$nbmandetail_1)
  
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
  
  
  uglytable <- d[,
                 .(
                   cyanosis_disease=sum(cyanosis_disease,na.rm=T),
                   cyanosis_ref=sum(cyanosis_ref,na.rm=T),
                   cyanosis_man=sum(cyanosis_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="cyanosis_disease", prettyX:="Cyanosis disease"]
  uglytable[variable=="cyanosis_ref", prettyX:="Cyanosis referral"]
  uglytable[variable== "cyanosis_man", prettyX:="Cyanosis management"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Cyanosis disease",
    "Cyanosis referral",
    "Cyanosis management"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Newborns Diagnosed, Referred and Managed (Cyanosis, up to 28 days old)")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "cyanosis.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  ###jaundice
  
  xtabs(~d$nbcsuspectedjaundice_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,jaundice_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbcsuspectedjaundice_")
  for(i in vars){
    d[get(i)==1, jaundice_disease:=TRUE]
  }
  
  d[jaundice_disease==TRUE,jaundice_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[jaundice_disease==TRUE & get(i)=="Jaundice", jaundice_ref:=TRUE]
  }
  
  d[jaundice_ref==TRUE,jaundice_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[jaundice_ref==TRUE & get(i) %in% c("Jaundice","Suspected jaundice"),jaundice_man:=TRUE]
  }
  xtabs(~d$jaundice_disease)
  xtabs(~d$jaundice_ref)
  xtabs(~d$jaundice_man)
  
  
  
  uglytable <- d[,
                 .(
                   jaundice_disease=sum(jaundice_disease,na.rm=T),
                   jaundice_ref=sum(jaundice_ref,na.rm=T),
                   jaundice_man=sum(jaundice_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="jaundice_disease", prettyX:="Jaundice disease"]
  uglytable[variable=="jaundice_ref", prettyX:="Jaundice referral"]
  uglytable[variable== "jaundice_man", prettyX:="Jaundice management"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Jaundice disease",
    "Jaundice referral",
    "Jaundice management"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Newborns Diagnosed, Referred and Managed (Jaundice, up to 28 days old)")
  p <- p + theme_gray(20)
  p
  
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "jaundice.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  
  
  ###abnormal defecation
  
  
  xtabs(~d$nbcdefecation_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,defecation_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbcdefecation_")
  for(i in vars){
    d[get(i)=="ABNORMAL", defecation_disease:=TRUE]
  }
  
  d[defecation_disease==TRUE,defecation_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[defecation_disease==TRUE & get(i)=="Abnormaldefecation", defecation_ref:=TRUE]
  }
  
  d[defecation_ref==TRUE,defecation_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[defecation_ref==TRUE & get(i)=="Abnormal defecation",defecation_man:=TRUE]
  }
  xtabs(~d$defecation_disease)
  xtabs(~d$defecation_ref)
  xtabs(~d$defecation_man)
  
  
  
  uglytable <- d[,
                 .(
                   defecation_disease=sum(defecation_disease,na.rm=T),
                   defecation_ref=sum(defecation_ref,na.rm=T),
                   defecation_man=sum(defecation_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="defecation_disease", prettyX:="Defecation disease"]
  uglytable[variable=="defecation_ref", prettyX:="Defecation referral"]
  uglytable[variable== "defecation_man", prettyX:="Defecation management"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Defecation disease",
    "Defecation referral",
    "Defecation management"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=uglytable, aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of people")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "defecation.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  ###abnormal urination
  xtabs(~d$nbcurination_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,urination_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbcurination_")
  for(i in vars){
    d[get(i)=="ABNORMAL", urination_disease:=TRUE]
  }
  
  d[urination_disease==TRUE,urination_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[urination_disease==TRUE & get(i)=="Abnormalurination", urination_ref:=TRUE]
  }
  
  d[urination_ref==TRUE,urination_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[urination_ref==TRUE & get(i)=="Abnormal urination",urination_man:=TRUE]
  }
  xtabs(~d$urination_disease)
  xtabs(~d$urination_ref)
  xtabs(~d$urination_man)
  
  
  
  uglytable <- d[,
                 .(
                   urination_disease=sum(urination_disease,na.rm=T),
                   urination_ref=sum(urination_ref,na.rm=T),
                   urination_man=sum(urination_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="urination_disease", prettyX:="Urination disease"]
  uglytable[variable=="urination_ref", prettyX:="Urination referral"]
  uglytable[variable== "urination_man", prettyX:="Urination management"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Urination disease",
    "Urination referral",
    "Urination management"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of people")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "urination.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  ###umbilical stamp 
  xtabs(~d$nbcumbilicalstump_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,umbilicalstump_disease:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbcumbilicalstump_")
  for(i in vars){
    d[get(i)==2, umbilicalstump_disease:=TRUE]
  }
  
  d[umbilicalstump_disease==TRUE,umbilicalstump_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[umbilicalstump_disease==TRUE & get(i)=="Umbilicalstumpinfected", umbilicalstump_ref:=TRUE]
  }
  
  d[umbilicalstump_ref==TRUE,umbilicalstump_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[umbilicalstump_ref==TRUE & get(i)=="Umbilical stump infected",umbilicalstump_man:=TRUE]
  }
  xtabs(~d$umbilicalstump_disease)
  xtabs(~d$umbilicalstump_ref)
  xtabs(~d$umbilicalstump_man)
  
  
  
  uglytable <- d[,
                 .(
                   umbilicalstump_disease=sum(umbilicalstump_disease,na.rm=T),
                   umbilicalstump_ref=sum(umbilicalstump_ref,na.rm=T),
                   umbilicalstump_man=sum(umbilicalstump_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="umbilicalstump_disease", prettyX:="Umbilical stump\ndisease"]
  uglytable[variable=="umbilicalstump_ref", prettyX:="Umbilical stump\nreferral"]
  uglytable[variable== "umbilicalstump_man", prettyX:="Umbilical stump\nmanagement"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Umbilical stump\ndisease",
    "Umbilical stump\nreferral",
    "Umbilical stump\nmanagement"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of people")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "umbilicalstump.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  ###conginital malformation 
  xtabs(~d$nbcsuspectedcongenitalmalformation_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,conginital_malformation:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbcsuspectedcongenitalmalformation_")
  for(i in vars){
    d[get(i)==1, conginital_malformation:=TRUE]
  }
  
  d[conginital_malformation==TRUE,conginital_malformation_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[conginital_malformation==TRUE & get(i)=="Congenitalanomalies", conginital_malformation_ref:=TRUE]
  }
  
  d[conginital_malformation_ref==TRUE,conginital_malformation_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[conginital_malformation_ref==TRUE & get(i)=="Congenital anomalies",conginital_malformation_man:=TRUE]
  }
  xtabs(~d$conginital_malformation)
  xtabs(~d$conginital_malformation_ref)
  xtabs(~d$conginital_malformation_man)
  
  
  
  uglytable <- d[,
                 .(
                   conginital_malformation=sum(conginital_malformation,na.rm=T),
                   conginital_malformation_ref=sum(conginital_malformation_ref,na.rm=T),
                   conginital_malformation_man=sum(conginital_malformation_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="conginital_malformation", prettyX:="Congenital malformation\ndisease"]
  uglytable[variable=="conginital_malformation_ref", prettyX:="Congenital malformation\nreferral"]
  uglytable[variable== "conginital_malformation_man", prettyX:="Congenital malformation\nmanagement"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "Congenital malformation\ndisease",
    "Congenital malformation\nreferral",
    "Congenital malformation\nmanagement"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of people")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "conginital_malformation.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  ###PKUpositive
  xtabs(~d$nbclabresultofpkuscreening_1)
  xtabs(~d$nncrisktype_1)
  xtabs(~d$nbmandetail_1)
  
  d[,PKUpositive:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbclabresultofpkuscreening_")
  for(i in vars){
    d[get(i)==1, PKUpositive:=TRUE]
  }
  
  d[PKUpositive==TRUE,PKUpositive_ref:=FALSE]
  vars <- stringr::str_subset(names(d),"^nncrisktype_")
  for(i in vars){
    d[PKUpositive==TRUE & get(i)=="PKUpositive", PKUpositive_ref:=TRUE]
  }
  
  d[PKUpositive_ref==TRUE,PKUpositive_man:=FALSE]
  vars <- stringr::str_subset(names(d),"^nbmandetail_")
  for(i in vars){
    d[PKUpositive_ref==TRUE & get(i)=="PKU Positive",PKUpositive_man:=TRUE]
  }
  xtabs(~d$PKUpositive)
  xtabs(~d$PKUpositive_ref)
  xtabs(~d$PKUpositive_man)
  
  
  
  uglytable <- d[,
                 .(
                   PKUpositive=sum(PKUpositive,na.rm=T),
                   PKUpositive_ref=sum(PKUpositive_ref,na.rm=T),
                   PKUpositive_man=sum(PKUpositive_man,na.rm=T)
                 )]
  uglytable[,id:=1:.N]
  uglytable <- melt.data.table(uglytable, id.vars="id")
  uglytable[,N:=value]
  
  uglytable[,prettyX:="Missing"]
  uglytable[variable=="PKUpositive", prettyX:="PKU positive\ndisease"]
  uglytable[variable=="PKUpositive_ref", prettyX:="PKU positive\nreferral"]
  uglytable[variable== "PKUpositive_man", prettyX:="PKU positive\nmanagement"]
  ### NEED TO MAKE IT A FACTOR SO THAT IT IS I NTHE RIGHT ORDER
  # OTHERWISE IT IS IN ALPHABETICAL ORDER
  uglytable[,prettyX:=factor(prettyX,levels=c(
    "PKU positive\ndisease",
    "PKU positive\nreferral",
    "PKU positive\nmanagement"
  ))]
  
  maxYVAL <- max(uglytable$N)
  labelAdjust <- maxYVAL*0.01
  
  p <- ggplot(uglytable, aes(x=prettyX, y=N, fill=prettyX, label=N))
  p <- p + geom_col(alpha=0.75)
  p <- p + geom_text(mapping=aes(y=N+labelAdjust),vjust=0)
  p <- p + scale_fill_brewer("Referal services",palette = "Set1")
  p <- p + scale_x_discrete("")
  p <- p + scale_y_continuous("Number of Newborns Refered")
  p <- p + theme_gray(20)
  p
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    "pniph",
    "abstracts_2018",
    "PKUpositive.png"
  ), plot = p, width = 297, height = 210, units = "mm")
  
  
  
  
  # to analuze nbc measurement, we can add them to the above table:
  
  
  xtabs(~d$nbctemperaturec_1)
  xtabs(~d$nbcrespiratoryratebreathmin_1)
  xtabs(~d$nbcpulsebeatmin_1)
  
  
  
  #uglytable <- d[,
  #               .(
  #                 NumOfresp_2=mean(nbcrespiratoryratebreathmin_1,na.rm=T),
  #                 NumOfresp_3=sum(nbcrespiratoryratebreathmin_1 <40 & nbcrespiratoryratebreathmin_1 >60,na.rm=T),
  
}                 

