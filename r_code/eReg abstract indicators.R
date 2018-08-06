

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
  

  #Diabetes testing
  
  xtabs(~d$labbloodglu_1)
  xtabs(~d$labfastbloodglu_1)
  xtabs(~d$laburglu_1)
  xtabs(~d$labogct_1)
  #counting glood glucose observations
  listOfGluvars <- names(d)[stringr::str_detect(names(d),"^glu_")]
  
 
  
 #of UTI tests done for all of the women
  xtabs(~d$laburuti_1)
  xtabs(~d$laburutirep_1)
  
  
  
  
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





