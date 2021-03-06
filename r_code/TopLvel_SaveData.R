Save2CasesPerMonthToNetwork <- function(d){
  suppressWarnings(dir.create(file.path(FOLDER_DATA_RESULTS,"quality_control")))
  # this saves the file to the network
  print("SAVING 4 CASES PER MONTH TO NETWORK")
  
  #hopefully lets us keep the same random sample
  set.seed(4)
  d[ident_TRIAL_1==TRUE & ident_dhis2_control==FALSE,
    randomNum:=order(runif(.N)),by=bookyearmonth]
  setorder(d,bookyearmonth)
  
  desiredFile <- file.path(
    FOLDER_DATA_RESULTS,"quality_control",
    sprintf("%s_random_quality_control_from_intervention.xlsx",DATA_DATE))
  
  if(!file.exists(desiredFile)) openxlsx::write.xlsx(d[randomNum<=4],desiredFile)
  print("FINISHED SAVING 4 CASES PER MONTH TO NETWORK")
  d[,randomNum:=NULL]
  
  # Did this for cases in 2018 so we can open their file
  # set.seed(4)
  # d[bookyear=="2018" & ident_dhis2_control==FALSE,
  #   randomNum:=order(runif(.N)),by=bookyearmonth]
  # setorder(d,bookyearmonth)
  # 
  # desiredFile <- file.path(
  #   FOLDER_DATA_RESULTS,"quality_control",
  #   sprintf("%s_random_quality_control_from_intervention.xlsx",lubridate::today()))
  #  if(!file.exists(desiredFile)) openxlsx::write.xlsx(d[randomNum<=4],desiredFile)
  # print("FINISHED SAVING 4 CASES PER MONTH TO NETWORK")
  # d[,randomNum:=NULL]
  # 
  
  
}

SaveFullFileToNetwork <- function(d){
  # this saves the file to the network
  print("SAVING FILES TO NETWORK")
  saveRDS(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
  saveRDS(d,file.path(FOLDER_DATA_CLEAN,sprintf("archive_%s.rds",CLINIC_INTERVENTION_DATE)))
  fwrite(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.csv"))
  #openxlsx::write.xlsx(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.xlsx"))
  print("FINISHED SAVING FILES TO NETWORK")
}

SaveAnonymousOslo <- function(d){
  
  ## here we delete the sensitive variables
  d[,trackedentity:=NULL]
  d[,dummy:=NULL]
  d[,idtype:=NULL]
  d[,motheridno:=NULL]
  d[,firstname:=NULL]
  d[,datecreated:=NULL]
  d[,fathersname:=NULL]
  d[,middlename:=NULL]
  d[,familyname1:=NULL]
  d[,familyname2:=NULL]
  d[,husbandsname:=NULL]
  d[,street:=NULL]
  d[,village:=NULL]
  d[,city:=NULL]
  d[,camp:=NULL]
  d[,mobile:=NULL]
  d[,phone:=NULL]
  d[,email:=NULL]
  d[,cosang:=NULL]
  d[,dob:=NULL]
  d[,income:=NULL]
  d[,education:=NULL]
  d[,agemarriage:=NULL]
  d[,agepregnancy:=NULL]
  d[,members:=NULL]
  d[,age:=NULL]
  
  # this is what i do if i know the entire name of the variable
  d[,bookdate:=NULL]
  d[,booklong:=NULL]
  d[,booklat:=NULL]
  d[,bookorgname:=NULL]
  d[,bookorgcode:=NULL]
  #d[,bookorgunit:=NULL]
  d[,bookidnumber:=NULL]
  d[,demoorgname:=NULL]
  d[,demoorgunit:=NULL]
  d[,bookorgdistrict:=NULL]
  
  # this is what i do if i know part of the variable name
  d[,names(d)[stringr::str_detect(names(d),"latitude")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"organisationunit")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"womanfirst")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"womanfamily")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"address")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"dataextractor")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"alternateidentificationnum")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"firstname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"fathersname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandsfamilyname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandsname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"middlename")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"village")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"city")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"dateofbirth")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"mobile")]:=NULL]
  #d[,names(d)[stringr::str_detect(names(d),"education")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"ageatmarriage")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"ageatfirstpreg")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"monthlyhouseholdincome")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"numberofmembers")]:=NULL]
  
  badNames <- names(d)[stringr::str_detect(names(d),"name")]
  goodNames <- badNames[stringr::str_detect(badNames,"labtestname")]
  badNames <- badNames[!badNames %in% goodNames]
  d[,(badNames):=NULL]
  
  d[,booklmp:=NULL]
  d[,bookdatelastbirth:=NULL]
  d[,dateupdated:=NULL]
  d[,expecteddateofdelivery:=NULL]
  d[,calc_expected_due_delivery:=NULL]
  d[,avgincome:=NULL]
 

  print("FINISHED SAVING FILES TO DROPBOX")
  
  # this saves the file to the server
  print("SAVING ANON FILES TO server")
  saveRDS(d[ident_dhis2_booking==1],file.path(FOLDER_DATA_CLEAN,"oslo_anon_data_from_r.rds"))
  fwrite(d[ident_dhis2_booking==1],file.path(FOLDER_DATA_CLEAN,"oslo_anon_data_from_r.csv"))
  print("FINISHED SAVING ANON FILES TO server")
  
  print("REMOVING D FROM MEMORY AS IT IS NOW DESTROYED BY ANONYMIZING")
  # delete the dataset "d" to make space in the memory
  rm("d", envir=.GlobalEnv)
  
  # garbage cleaning
  gc()
}

LoadDataFileFromNetwork <- function(){
  print("RELOADING DATASET FROM NETWORK DRIVE")
  d <- readRDS(file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
  print("FINISHED RELOADING DATASET FROM NETWORK DRIVE")
  
  return(d)
}

LoadDataFileFromNetworkGaza <- function(){
  print("RELOADING GAZA DATASET FROM NETWORK DRIVE")
  d <- readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,"full_data_from_r.rds"))
  print("FINISHED RELOADING DATASET FROM NETWORK DRIVE")
  
  return(d)
}

LoadDataFileFromNetworkWB <- function(){
  print("RELOADING WB DATASET FROM NETWORK DRIVE")
  d <- readRDS(file.path(FOLDER_DATA_CLEAN_WB,"full_data_from_r.rds"))
  print("FINISHED RELOADING DATASET FROM NETWORK DRIVE")
  
  return(d)
}

LoadDataFileFromNetworkPal <- function(){
  dWB <- LoadDataFileFromNetworkWB()
  dWB[,ident_gaza:=FALSE]
  dG <- LoadDataFileFromNetworkGaza()
  dG[,ident_gaza:=TRUE]
  dPal <- rbind(dWB,dG,fill=T)
  
  return(dPal)
}

LoadAnonDataFileFromNetwork<- function(){
  print("RELOADING ANON DATASET FROM NETWORK DRIVE")
  d <- readRDS(file.path(FOLDER_DATA_CLEAN,"oslo_anon_data_from_r.rds"))
  print("FINISHED RELOADING ANON DATASET FROM NETWORK DRIVE")
  
  return(d)
}

SaveCISMACDataBase<- function(){
  rm("d", envir=.GlobalEnv)
  d=LoadAnonDataFileFromNetwork()
  
  d[ident_TRIAL_1==T,]
  
#in order to keep the ident_control_variable (as requested), recoding it#
#ident_dhis2_control will be replaced in these datasets with Exposure_Trial_1
  d[,Exposure_Trial_1:=as.character()]
  d[ident_dhis2_control==T,Exposure_Trial_1:="A"]
  d[ident_dhis2_control==F,Exposure_Trial_1:="B"]
  
#bookhisthtn for control to be changed to bookhisthtn
  d[ident_dhis2_control==T,bookhisthtn:=confamilyhistoryofhypertension]

#gravida, para, abo can get from prev pregnancies, no need to keep control variables for them
  d[bookweight>140|bookweight<35, bookweight:=as.numeric(NA)]
  
  #paraCat
  d[,paracat:=cut(para,
                  breaks=c(0,0.9,4,15),
                  include.lowest=T)]

  varsKeep <- c(
    #"bookdate",
    #"bookorgcode",
    "bookorgdistricthashed",
    "bookorgunit",
    #"demoorgunit",
    "agecat",
    "agemarriagecat",
    "agepregnancycat",
    "incomecat",
    #"education",
    "educationcat",
    "paracat",
    #"members",
    "avicennanum",
    "motheridbooknum",
    "uniqueid",
    "bookevent",
    "booknum",
    #"booklmp",
    #"booklmp_original",
    "bookgestage",
    "bookgestagedays",
    "booklabhb",
    "bookprimi",
    "bookpprom",
    "bookprom",
    "bookeclamp",
    "bookvagbleed",
    "bookpallor",
    "bookexamhead",
    "bookexamheadabn",
    "bookexamheart",
    "bookexamheartabn",
    "bookexamabd",
    "bookexamabdabn",
    "bookexamlimb",
    "bookexamlimbabn",
    "usrecommendationscommentsx",
    "gravida",
    "para",
    "lmpT1",
    #"conabortion",
    #"congravida",
    #"conld",
    #"conlivingchildren",
    #"conpara",
    "bookhistmed",
    "bookhistdm",
    "bookhistabort",
    "bookhistperi",
    #"confamilyhistoryofhypertension",
    "bookfamdm",
    "bookhistcs",
    "bookhistcscompl",
    "bookparity",
    "bookhistpuersep",
    "bookhisteclamp",
    "bookhistantparthemprevpreg",
    "bookhistpph",
    "bookhistgdm",
    "bookhistghtn",
    "bookhistpreecl",
    "bookhistpreterm",
    "bookhistaph",
    "bookhisthtn",
    "bookfamhtn",
    "bookhistotherch",
    "bookhistblood",
    "bookhistotherchronic",
    "bookhistbloodspec",
    #"conage16or40",
    "bookheight",
    "bookbpsyst",
    "bookbpdiast",
    "bookweight",
    #"conancgestationaageatvisitweeks",
    #"conancgestationaageatvisitsize",
    "usrecommendationscommentsy",
    "bookexamfh",
    "bookexampalp",
    "consupplementsyesno",
    "conancsupplementprescription",
    "bookfetalmove",
    "bookexamsfh",
    "bookexamedema",
    "bookrefchronic",
    "bookmedpres",
    "bookhighrisk",
    "bookseenby",
    "bookbackupfile",
    #"ident_dhis2_df_no_gf_or_bf",
    "ident_dhis2_booking",
    "booking_number",
    "ident_expected_delivered",
    "Exposure_Trial_1",
    "ident_dhis2_b4_2017_01_15",
    #"ident_phase1clinic",
    #"ident_phase2clinic",
    #"ident_phase3clinic",
    "ident_TRIAL_1",
    "ident_dhis2_control",
   "str_TRIAL_1_Cluster",
   #"ident_hr_clinic",
    "ident_TRIAL_1_clinics",
    names(d)[stringr::str_detect(names(d),"^anevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anprogstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^andate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anbpdiast_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anweight_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anexamfh_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anexampalp_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anothermed_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhisthtn_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistblood_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anfetalmove_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhisthypothyr_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistbloodspec_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anexamsfh_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anexamedema_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anrefchronic_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anvisitweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anmedpres_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhighrisk_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistthr_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anchistclex_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anseenby_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anbackupfile_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^aneclamp_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anvagbleed_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^angestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anT1gestagedays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anothercond_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistrd_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistasthma_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistepi_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistcardiac_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistpsy_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistrti_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistchronicspec_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhomeorclinic_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistdm_[0-9]*")],
    #"ident_dhis2_an",
    names(d)[stringr::str_detect(names(d),"^labevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labprogstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^labdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laborgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laborgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labrh_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labict_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labhct_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labrecommendationscomments_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labbloodglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labfastbloodglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laburglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laburpro_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labother1_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labotherres1_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labother2_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labotherres2_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labother3_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labotherres3_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labgestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labT1gestagedays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labplace_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labplacespec_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labanemresp_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labogct_[0-9]*")],
    "ident_dhis2_lab",
    names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usprogstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^usdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegaweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegadays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usedd_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usnumberfetus_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfh_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uscomments_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uspres_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usiugr_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uslga_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamniquant_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamnideeppoc_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamniindex_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsac_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsacmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsacweek_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uscrlmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uscrlweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usbpdmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usbpdweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfemurmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfemurweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usacmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usacweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usefw_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usT1gestagedays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usreason_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usplace_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usmultifetdesignation_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanom_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanomspec_[0-9]*")],
    "ident_dhis2_us",
    names(d)[stringr::str_detect(names(d),"^riskevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskprogstage_1[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^riskdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskgestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^risktype_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskdesx_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskdesy_[0-9]*")],
    "ident_dhis2_risk",
    names(d)[stringr::str_detect(names(d),"^manevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manprogstage_1[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^mandate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mangestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manT1gestagedays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mandetail_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypex_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manperf_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypey_[0-9]*")],
    "ident_dhis2_man",
    names(d)[stringr::str_detect(names(d),"^prevevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevprogstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^prevdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevoutcome_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevmodedelivery_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevgdm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevhtn_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevpreeclampsia_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^preveclampsia_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevpuersep_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevaph_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevpph_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevnocompl_[0-9]*")],
    "ident_dhis2_prev",
   
    "ident_dhis2_dhis2hbo",
    names(d)[stringr::str_detect(names(d),"^pcnevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnprogstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^pcndate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnfileclosurereason_[0-9]*")],
    "ident_dhis2_pcn",
    "isExpectedToHaveDelivered",
    "motheridbook_earlyDate",
    "motheridbook_lateDate",
    "bookyearmonth",
    "bookyear",
    "bookmonth",
    "bookorgdistricthashed",
    "abbeventnum",
    "alabeventnum",
    "acseventnum",
    "ident_avic_abb",
    "ident_avic_any",
    "mahima_alab_hb_closest_to_delivery_1",
    "merged_is_hosp_gov",
    "hboeventnum",
    "ident_hbo",
    "mahima_gestageatbirthwk_1",
    "matching",
    "merged_is_hosp_gov",
    "merged_bpsyst",
    "merged_bpdiast",
    names(d)[stringr::str_detect(names(d),"^merged_namehospbirth_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_pregoutcome_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_abortion[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_gestageatdelivery_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_birthweight_[0-9]*")],
    
    names(d)[stringr::str_detect(names(d),"^merged_hbatadmission_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_modeofdelivery_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_fetalpresentation_[0-9]*")],
   
    names(d)[stringr::str_detect(names(d),"^merged_indicationforcsection_[0-9]*")]
   
  )
  
  #### THESE ARE THE VARIABLES THAT DONT EXIST!!!!!!
  varsDontExist <- varsKeep[!varsKeep %in% names(d)]
  if(length(varsDontExist)>0){
    message("THESE VARIABLES DONT EXIST IN CISMAC DATABASE")
    for(i in varsDontExist) message(i)
    stop("THESE VARIABLES DONT EXIST IN CISMAC DATABASE")
  }
  
  
  smallD <-d[ident_TRIAL_1==T,varsKeep,with=F]
  
  #save copy to dropbox
  openxlsx::write.xlsx(smallD, file.path(FOLDER_DROPBOX_RESULTS,
                                      "mahima",
                                      "trial_1",
                                      "anonymized_data.xlsx"))
  
  # A = controls, B= intervention
  A <-d[ident_dhis2_control==T]
  B <-d[ident_dhis2_control==F]
  
  A[,ident_dhis2_control:=NULL]
  B[,ident_dhis2_control:=NULL]
  
  n <- unique(d$bookyearmonth)
  n
  
  set.seed(4)
  stack <- data.table(yearmonth=sort(n))
  stack[,randomoffset:=sample(c(0,1),size=.N,replace=T)]
  stack[,interventionName:=10*c(1:.N)+randomoffset]
  stack[,controlName:=10*c(1:.N)+(1-randomoffset)]
  
  dir.create(file.path(FOLDER_DATA_CLEAN,
                       "CISMACdataset",
                       lubridate::today()))
  
  openxlsx::write.xlsx(stack,file.path(FOLDER_DATA_CLEAN,
                                       "CISMACdataset",
                                       lubridate::today(),
                                       "KEY_FILE.xlsx"))
  
  for(i in seq_along(n)){
    print(i)
    tempA <- A[bookyearmonth==stack$yearmonth[i]]
    tempB <-B[bookyearmonth==stack$yearmonth[i]]
    
    # identifying how many rows the smaller dataset has
    smallestNumber <- min(c(nrow(tempA),nrow(tempB)))
    
    # take a random sample of ROW NUMBERS of size 'smallestNumber' from both control/interverntion
    sampleRowsA <- sample(1:nrow(tempA), size=smallestNumber)
    sampleRowsB <- sample(1:nrow(tempB), size=smallestNumber)
    
    # here we say "please give me the 5th, the 7th, the 20th... etc rows"
    #tempA <- tempA[sampleRowsA]
    #tempB <- tempB[sampleRowsB]
    
   saveRDS(tempA,
           file.path(FOLDER_DATA_CLEAN,
                     "CISMACdataset",
                     lubridate::today(),
                     sprintf("%s_CISMAC_%s.rds",stack$controlName[i], stack$yearmonth[i])))
   
   openxlsx::write.xlsx(tempA,file.path(FOLDER_DATA_CLEAN,
                                       "CISMACdataset",
                                       lubridate::today(),
                                       sprintf("%s_CISMAC_%s.xlsx", stack$controlName[i], stack$yearmonth[i]))) 
 
    
    saveRDS(tempB,
            file.path(FOLDER_DATA_CLEAN,
                      "CISMACdataset",
                      lubridate::today(),
                      sprintf("%s_CISMAC_%s.rds",stack$interventionName[i], stack$yearmonth[i])))
    
    openxlsx::write.xlsx(tempB,file.path(FOLDER_DATA_CLEAN,
                                        "CISMACdataset",
                                        lubridate::today(),
                                        sprintf("%s_CISMAC_%s.xlsx", stack$interventionName[i], stack$yearmonth[i]))) 
    
  }
  
 
  return(d)
  
}


SaveTRIAL2DataSet<- function(){
  rm("d", envir=.GlobalEnv)
  d=LoadAnonDataFileFromNetwork()
  
    d <-d[bookyear=="2018" &
          !is.na(str_TRIAL_2_Cluster) &
            ident_TRIAL_2_and_3==T &
          isExpectedToHaveDelivered==TRUE]
  
  varsKeep <- c(
   
    "bookgestage",
    "str_TRIAL_2_Cluster",
    
    names(d)[stringr::str_detect(names(d),"^anevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anbpsyst_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anbpdiast_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^angestage_[0-9]*")],
    
    names(d)[stringr::str_detect(names(d),"^labevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laborgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labhb_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labbloodglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labfastbloodglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^laburglu_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labgestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labogct_[0-9]*")],
    
    names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegaweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegadays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usnumberfetus_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfh_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uspres_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usiugr_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uslga_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamniquant_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamnideeppoc_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usamniindex_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsac_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsacmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestsacweek_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uscrlmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^uscrlweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usbpdmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usbpdweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfemurmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usfemurweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usacmm_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usacweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usefw_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usgestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usreason_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usmultifetdesignation_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanom_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanomspec_[0-9]*")],
    
    
    names(d)[stringr::str_detect(names(d),"^riskevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^risktype_[0-9]*")],
    
    names(d)[stringr::str_detect(names(d),"^manevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mangestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mandetail_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manperf_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypey_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypex_[0-9]*")],
    "isExpectedToHaveDelivered"
    
    
  )
  
    t2 <-d[,varsKeep,with=F]
        
    file <- file.path(FOLDER_DATA_CLEAN,
                        "Trial2DataSet.xlsx")
    print(file)
    openxlsx::write.xlsx(t2,file)
  
    
  
}



SaveNamesOfVariables <- function(d){
  x <- data.frame(names=names(d))
  openxlsx::write.xlsx(x,file.path(FOLDER_DROPBOX_RESULTS,
                                     "variablenames.xlsx"))
}

SaveNamesOfANCAndBook <- function(d){
  # this will print out all names starting with an
  a <- stringr::str_subset(names(d),"^an[a-z\\_]+_1$")
  # this will print out all names starting with book
  b <- stringr::str_subset(names(d),"^book")
  ## turning book variables into anc_0
  x <- data.frame(booking=b)
  x$an <- ""
  x$an[1:length(a)] <- a
  openxlsx::write.xlsx(x,file.path(FOLDER_DROPBOX_RESULTS,
                                   "variablenames_ANCAndBook.xlsx"))
}

SaveAllDataFiles <- function(d, IS_GAZA=FALSE){
  
  # this is a bit weird, ignore it for the moment
  lists <- c()
  for(i in 1:ncol(d)) if(is.list(d[[i]])) lists <- c(lists,i)
  for(i in lists){
    n <- names(d)[i]
    d[,(n):=as.character(get(n))]
  }
  
  # start saving our files
  SaveNamesOfANCAndBook(d)
  SaveNamesOfVariables(d)
  SaveFullFileToNetwork(d)
  Save2CasesPerMonthToNetwork(d)
  SaveAnonymousOslo(d)
  SaveTRIAL2DataSet()


}