

# this one makes nice excel tables containing
# the summary indicators (e.g. proportions)
IndicatorsPNIPHDashboard <- function(d=NULL){
  #if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  xtabs(~d$cpopregoutcome_1)
  xtabs(~d$cpopregoutcome_2)
  xtabs(~d$cpopregoutcome_3)
  #turn the date into year-month format
  d[,cpoyearmonth_1:=YearMonth(cpodate_1)]
  
  #here we say how many live births did each pregnancy have
  listOfcpopregvars <- names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]
  #create number of observations per pregnancy
  d[,temp_cpolivebirths:=0]
 #start loop through all bp variables
   for(i in listOfcpopregvars){
    d[get(i)=="LIVE",temp_cpolivebirths:=temp_cpolivebirths+1]
  }
  xtabs(~d$temp_cpolivebirths)
  
  stata <- c(
    850095407,
    401611173,
    852543990,
    852188531,
    401911615,
    401761564,
    859820466,
    850089574,
    936854348,
    37083367,
    950912295,
    858567571,
    403430671
  )
  stata[!stata %in% d$motheridno]
  
  #resLiveBirths <- d[cpopregoutcome_1=="LIVE",.(
  # numberoflivebirths=.N
  #),by=.(
  # bookorgname, cpoyearmonth_1
  #)]
  
  ###here we will sum the live births per pregnancy 
  resLiveBirths <- d[,.(
    numberoflivebirths=sum(temp_cpolivebirths,na.rm=T)
  ),by=.(
    cpoorgname_1, cpoyearmonth_1
    
  )]
  
  
  
  setorder(resLiveBirths,cpoorgname_1, cpoyearmonth_1)

  openxlsx::write.xlsx(resLiveBirths, 
                       file.path(FOLDER_DATA_RESULTS,
                                 "dashboard_indicators",
                                 sprintf("%s_live_births.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
    
}





