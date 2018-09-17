

# this one makes nice excel tables containing
# the summary indicators (e.g. proportions)
eRegSysIndicators <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
 
  ###Want numbers for all in system: 
  #Bp tests done in total
  #Hb tests done in total
  #UTI tests done in total
  #Gluose tests done in total
  #FGR/US done in total
  
  #ANC visits in total
  d$anvisitweeks_1
  listofANCvisits <- names(d)[stringr::str_detect(names(d), "anvisitweeks_")]
  listofANCvisits
  d[,temp_totalANCvisits:=0]
  for(i in listofANCvisits){
    d[get(i)>0,temp_totalANCvisits:=temp_totalANCvisits+1]
  }
  xtabs(~d$temp_totalANCvisits)
  sum(d$temp_totalANCvisits)
  
  #find all of the bp variables
  listOfBPvars <- names(d)[stringr::str_detect(names(d),"^bp_")]
  listOfBPvars
  #create number of observations per pregnancy since each pregnancy
  ##is a record in the system
  d[,temp_totalBPobs:=0]
  #start loop through all bp variables
  for(i in listOfBPvars){
    d[get(i)>0,temp_totalBPobs:=temp_totalBPobs+1]
  }
  xtabs(~d$temp_totalBPobs)
  
  ###here we will sum the live births per pregnancy 
  resLiveBirths <- d[,.(
    numberoflivebirths=sum(temp_cpolivebirths,na.rm=T)
  ),by=.(
    cpoorgname_1, cpoyearmonth_1
    
  )]
   
  
  
  #ALL BPsystolic variables
  d$bookbpsyst
  d$anbpsyst_1
  d$ppcdiastolicbloodpressuremmhg_1
  d$ppcsystolicbloodpressuremmhg_1
  listOfBPsystvars <- names(d)[stringr::str_detect(names(d),"^bpsyst_")]
  listOfBPsystvars
  #create number of observations per pregnancy since each pregnancy
  ##is a record in the system
  d[,temp_totalBPsystobs:=0]
  #start loop through all bp variables
  for(i in listOfBPvars){
    d[get(i)>0,listOfBPsystvars:=listOfBPsystvars+1]
  }
  xtabs(~d$temp_totalBPsystobs)
  
  # ALL BPdiastolic
  d$bookbpdiast
  d$anbpdiast_1
  listOfBPdiastvars <- names(d)[stringr::str_detect(names(d),"^bpdiast_")]
  listOfBPdiastvars
  #create number of observations per pregnancy since each pregnancy
  ##is a record in the system
  d[,temp_totalBPdiastobs:=0]
  #start loop through all bp variables
  for(i in listOfBPvars){
    d[get(i)>0,listOfBPdiastvars:=listOfBPdiastvars+1]
  }
  xtabs(~d$temp_totalBPdiastobs)


  #Diabetes testing
  xtabs(~d$labbloodglu_1)
  xtabs(~d$labfastbloodglu_1)
  xtabs(~d$laburglu_1)
  xtabs(~d$labogct_1)
  #counting glood glucose observations
  listOfGluvars <- names(d)[stringr::str_detect(names(d),"^glu_")]
  d[,temp_totalGluobs:=0]
  #start loop through all bp variables
  for(i in listOfGluvars){
    d[get(i)>0,temp_totalGluobs:=temp_totalGluobs+1]
  }
  xtabs(~d$temp_totalGluobs)

    
  #Diabetes testing
  #counting ogct glucose observations
  xtabs(~d$labogct_1)
  
  
 
  
 #of UTI tests done for all of the women
  xtabs(~d$laburuti_1)
  xtabs(~d$laburutirep_1)
  #counting blood glucose observations
  listOfUtivars <- names(d)[stringr::str_detect(names(d),"^uti_")]
  d[,temp_totalUtiobs:=0]
  #start loop through all bp variables
  for(i in listOfUtivars){
    d[get(i)>0,temp_totalUtiobs:=temp_totalUtiobs+1]
  }
  xtabs(~d$temp_totalUtiobs)
  
  
  # number of women
  NROW(d)
  
  
  
  
  #resLiveBirths <- d[cpopregoutcome_1=="LIVE",.(
  # numberoflivebirths=.N
  #),by=.(
  # bookorgname, cpoyearmonth_1
  #)]
  
  ###here we will sum the live births per pregnancy 
  resBPobs <- d[,.(
    numberofBPobs=sum(temp_totalBPobs,na.rm=T)
  ),by=.(
   bookorgname,bookdate
    
  )]
  
  
  
  setorder(resLiveBirths,cpoorgname_1, cpoyearmonth_1)
  
  openxlsx::write.xlsx(resLiveBirths, 
                       file.path(FOLDER_DATA_RESULTS,
                                 "dashboard_indicators",
                                 sprintf("%s_live_births.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
  
}





