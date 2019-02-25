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
  d[,bookorgunit:=NULL]
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
  d[,names(d)[stringr::str_detect(names(d),"education")]:=NULL]
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
 
  
  
  # this saves the file to the dropbox
  # print("SAVING FILES TO DROPBOX")
  #saveRDS(d[ident_dhis2_booking==1],"~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_data_from_r.rds")
  # fwrite(d[ident_dhis2_booking==1],"~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_data_from_r.csv")
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
  
  varsKeep <- c(
    "avicennanum",
    "motheridbooknum",
    "uniqueid",
    "bookevent",
    "booknum",
    "bookprogstage",
    "booklmpknown",
    "bookgestage",
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
    "conabortion",
    "congravida",
    "conld",
    "conlivingchildren",
    "conpara",
    "bookhistmed",
    "bookhistdm",
    "bookhistabort",
    "bookhistperi",
    "confamilyhistoryofhypertension",
    "bookfamhistdiab",
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
    "conage16or40",
    "bookhistaph",
    "bookheight",
    "bookbpsyst",
    "bookbpdiast",
    "bookweight",
    "conancgestationaageatvisitweeks",
    "conancgestationaageatvisitsize",
    "usrecommendationscommentsy",
    "bookexamfh",
    "bookexampalp",
    "consupplementsyesno",
    "conancsupplementprescription",
    "bookhisthtn",
    "bookhistotherch",
    "bookhistblood",
    "bookfetalmove",
    "bookhistbloodspec",
    "bookexamsfh",
    "bookexamedema",
    "bookrefchronic",
    "bookmedpres",
    "bookhistotherchronic",
    "bookhighrisk",
    "bookseenby",
    "bookbackupfile",
    "ident_dhis2_df_no_gf_or_bf",
    "ident_dhis2_booking",
    "booking_number",
    "ident_expected_delivered",
    "ident_dhis2_control",
    "ident_dhis2_b4_2017_01_15",
    "ident_phase1clinic",
    "ident_phase2clinic",
    "ident_phase3clinic",
    "ident_TRIAL_1",
    "ident_hr_clinic",
    "ident_TRIAL_1_clinics",
    names(d)[stringr::str_detect(names(d),"^anevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anprogstage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^andate_[0-9]*")],
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
    names(d)[stringr::str_detect(names(d),"^anothercond_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistrd_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistasthma_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistepi_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistcardiac_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistpsy_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistrti_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhistchronicspec_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^anhomeorclinic_[0-9]*")],
    "ident_dhis2_an",
    names(d)[stringr::str_detect(names(d),"^labevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labprogstage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labdate_[0-9]*")],
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
    names(d)[stringr::str_detect(names(d),"^labplace_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labplacespec_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labanemresp_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^labogct_[0-9]*")],
    "ident_dhis2_lab",
    names(d)[stringr::str_detect(names(d),"^usevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usprogstage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegaweeks_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usegadays_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usedd_1[0-9]*")],
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
    names(d)[stringr::str_detect(names(d),"^usreason_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usplace_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usmultifetdesignation_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanom_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^usanomspec_[0-9]*")],
    "ident_dhis2_us",
    names(d)[stringr::str_detect(names(d),"^riskevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskprogstage_1[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^risktype_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskdesx_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^riskdesy_[0-9]*")],
    "ident_dhis2_risk",
    names(d)[stringr::str_detect(names(d),"^manevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manprogstage_1[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mandate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manorgcode_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manorgunit_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mangestage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mandetail_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypex_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^manperf_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^mantypey_[0-9]*")],
    "ident_dhis2_man",
    names(d)[stringr::str_detect(names(d),"^prevevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevprogstage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevdate_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevorgcode_[0-9]*")],
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
    names(d)[stringr::str_detect(names(d),"^prevnocompl_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevnocompl_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevnocompl_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^prevnocompl_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hboprogramstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbodate_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hboconwomandob_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbodateofdeliveryhospital_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbogestagedeliv_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbopregoutcome_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbopregbweight_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbosystbp_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbodiastbp_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbomodedeliv_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbousfetalpresentation_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hboindicforcsec_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbolabcbchemoglobin_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbolaburinestickprotein_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^dhis2hbousrecommendcomment_[0-9]*")],
    "ident_dhis2_dhis2hbo",
    names(d)[stringr::str_detect(names(d),"^pcnevent_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcnprogstage_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^pcndate_[0-9]*")],
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
    #names(d)[stringr::str_detect(names(d),"^amdbabyrecorddatecreation_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^amddate_[0-9]*")],
     "ident_avic_amd",
    #names(d)[stringr::str_detect(names(d),"^acsdatatext_1[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^acsdatecreated_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^acsstatus_1[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^acsdate_1[0-9]*")],
    "ident_avic_acs",
    #names(d)[stringr::str_detect(names(d),"^alabtestname_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^alabtestunit_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^alabtestminval_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^alabtestmaxval_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^alabtestresult_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^alabdate_[0-9]*")],
    "ident_avic_alab",
    #names(d)[stringr::str_detect(names(d),"^abbbabyweight_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabybirthresult_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabybirthtype_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbdatatext_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabybirthmark_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabybirthcomment_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabybirthdate_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyheight_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyapgar1minscore_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyapgar5minscore_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyapgar10minscore_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabypregnancynoofweeks_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyanusstatus_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbbabyrecorddatecreation_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^abbdate_[0-9]*")],
    "ident_avic_abb",
    "ident_avic_any",
    "mahima_alab_hb_closest_to_delivery_1",
    "hboeventnum",
    #names(d)[stringr::str_detect(names(d),"^hbouniqueid_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodcreated_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodlastupdated_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodtrackedentity_[0-9]*")],
    
    #names(d)[stringr::str_detect(names(d),"^hbodinactive_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodidentificationdocumenttype_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboevent_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboprogramstage_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboeventdate_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodateofdeliveryhospital_[0-9]*")],
    
    #names(d)[stringr::str_detect(names(d),"^hbogestagedeliv_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboprevpregoutcome_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboprevpregbweight_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbomodeprevdeliv_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbousfetalpresentation_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboindiccsectioninanycol_[0-9]*")],
    
    #names(d)[stringr::str_detect(names(d),"^hbolaburinestickprotein_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboindiccsectioninanycol_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbosystbp_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hbodiastbp_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboconreasonforcs_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboconlabcbchemoglobin_[0-9]*")],
    #names(d)[stringr::str_detect(names(d),"^hboconlabcbchematocrit_[0-9]*")],
    "ident_hbo",
    "matching",
    "agecat",
    "agemarriagecat",
    "agepregnancycat",
    "incomecat",
    names(d)[stringr::str_detect(names(d),"^merged_namehospbirth_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_pregoutcome_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_abortion[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_gestageatdelivery_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_birthweight_[0-9]*")],
    names(d)[stringr::str_detect(names(d),"^merged_babybirthdate_[0-9]*")],
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
  
  d <-d[ident_TRIAL_1==T,varsKeep,with=F]
  
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
    temp <-A[bookyearmonth==stack$yearmonth[i]]
   
   saveRDS(temp,
           file.path(FOLDER_DATA_CLEAN,
                     "CISMACdataset",
                     lubridate::today(),
                     sprintf("%s_CISMAC_%s.rds",stack$controlName[i], stack$yearmonth[i])))
   
   openxlsx::write.xlsx(temp,file.path(FOLDER_DATA_CLEAN,
                                       "CISMACdataset",
                                       lubridate::today(),
                                       sprintf("%s_CISMAC_%s.xlsx", stack$controlName[i], stack$yearmonth[i]))) 
  
    print(i)
    
    temp <-B[bookyearmonth==stack$yearmonth[i]]
    
    saveRDS(temp,
            file.path(FOLDER_DATA_CLEAN,
                      "CISMACdataset",
                      lubridate::today(),
                      sprintf("%s_CISMAC_%s.rds",stack$interventionName[i], stack$yearmonth[i])))
    
    openxlsx::write.xlsx(temp,file.path(FOLDER_DATA_CLEAN,
                                        "CISMACdataset",
                                        lubridate::today(),
                                        sprintf("%s_CISMAC_%s.xlsx", stack$interventionName[i], stack$yearmonth[i]))) 
    
  }
  
 
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


}