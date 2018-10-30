Analyse_pniph_abstract_2018_ppc<- function(){
  # remove d from the global env as it is too big
  rm("d", envir=.GlobalEnv)

  d <- CleanAllData(includePPC=T,
                    minBookDate="2017-01-01",
                    maxBookDate="2018-09-01",
                    delete=c("^lab",
                              "^us"
                             ))
  
  d <- d[
    ident_dhis2_ppc==1 & 
      ident_dhis2_an==1 & 
      ident_dhis2_control==F]
  
  tryCatch({
    # number of visits
    tab <- d[,.(
      NWomen=.N,
      numVisit1=sum(!is.na(ppcevent_1)),
      numVisit2=sum(!is.na(ppcevent_2)),
      numVisit3=sum(!is.na(ppcevent_3)),
      numVisit4=sum(!is.na(ppcevent_4))
    ),keyby=.(
      ppcorgname_1
    )]
    
    openxlsx::write.xlsx(tab, 
                         file.path(
                           FOLDER_DROPBOX_RESULTS,
                           "pniph",
                           "abstracts_2018",
                           "ppc_list_of_clinics.xlsx"))
  },error = function(e){
    print("*****Analyse_pniph_abstract_2018_ppc() NOT WORKING ppc_list_of_clinics.xlsx")  
  })
  
  xtabs(~d$ident_dhis2_ppc)
  xtabs(~d$ident_dhis2_booking)
  
  xtabs(~d$ident_dhis2_ppc)
  #d$ident_dhis2_ppc
  #TRUE 
  #7217 
  #> xtabs(~d$ident_dhis2_booking)
  #d$ident_dhis2_booking
  #1 
  #19499 
  #> 
  
  #for demographic
  #agecat
  #avgincomecat
  #incomecat
  #agemarriagecat
  #agepregnancycat
  #educationcat
  #village
  #city
  #camp
  #members <3   3-5   >5
  
  
  #categories(numbers and percents)			
  #ppcbirthoutcome_1
    # NA                                           
    # "0", "",                                           
  # "2",                                          
  # "Abortion (<24 weeks)",                       
  # "1",                                          
  # "3",                                          
  # "Infant death (>28 days and befored one year)"
  #ppcrespiratoryraterr
     #unique(d$ppcrespiratoryraterr_1)
  #ppcmodeofdelivery_1 
  #ppcplaceofdelivery_1
  #ppclochiaamount_1=NULL
  #ppcrisktype_1
  #ppcwhichppcvisit_1 how many total, first, second, by district
    #NA,"First visit","Second visit","Beyond second visit","" 
  #ppcvisitundertakenbywhom_1
    # NA, 1,2,3
  
  #ranges, means, etc
  #ppcpulsebeatmin_1	
  #ppcbirthweight_1	
  #ppctemperaturecelsius_1	
  #ppcfundalmeasurementcm_1	
 	#ppcdiastolicbloodpressuremmhg_1	
  #ppcdaysafterdelivery_1, can also calculate this ranges below	
  
  #mean gestational age at visit, days after delivery
  			
  vars <- names(d)[stringr::str_detect(names(d),"^ppcwhichppcvisit_")]
  for(v in vars){
    d[get(v)=="First visit",(v):="1"]
    d[get(v)=="Second visit",(v):="2"]
    d[get(v)=="Beyond second visit",(v):="3"]
  }
  
  #bookevent
  
  d[,pniph_000EVERYONE:=1]
  vars_demo <- c(
    "pniph_000EVERYONE",
    "pniph_agecat",
    "pniph_avgincomecat",
    "pniph_incomecat",
    "pniph_agemarriagecat",
    "pniph_agepregnancycat",
    "pniph_educationcat"
  )
  
  ###fix these for what she wants
  vars_demo2 <- c(
      "age",
      "avgincome",
      "agemarriage",
      "agepregnancy"
  )
  
  #categories for breast problems
  #numbers and percents
  #Avg days after delivery
  unique(d$ppcdaysafterdelivery_1)
  #or we can calculate them because the numbers are off??
  #d$ppcdateofdelivery_1 and d$ppcdate_1
  #cats: (1-7),(8-15),(16-22),(23-30),(31-37),(38-42)
  
  #Want to know these
  vars_ppcbreastinspectionabnormalsecretion	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionabnormalsecretion_")]
  vars_breastinspectionbreastswelling	<- names(d)[stringr::str_detect(names(d),"^breastinspectionbreastswelling_")]
  vars_breastinspectioncrackednipples	<- names(d)[stringr::str_detect(names(d),"^breastinspectioncrackednipples_")]
  vars_ppcbreastinspectionhotsensation	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionhotsensation_")]
  vars_ppcbreastinspectionredness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionredness_")]
  vars_ppcbreastinspectiontenderness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectiontenderness_")]
  vars_ppcbreastnoproblems <- names(d)[stringr::str_detect(names(d),"^ppcbreastnoproblems_")]
  vars_ppcdaysafterdelivery <- names(d)[stringr::str_detect(names(d),"^ppcdaysafterdelivery_")]
  vars_cpoantepartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
  vars_cpoevent <- names(d)[stringr::str_detect(names(d),"^cpoevent_")]
  vars_ppclochiaamount <- names(d)[stringr::str_detect(names(d),"^ppclochiaamount_")]
  
#d$ppcincisiontearinspectionbleeding_1
#d$ppcincisiontearcondition_1
  
  
  #How many of these women had a fever
  #how would we do this for more than one reading??
  #d[,ppctemp:=cut(ppctemperaturecelsius_1,
  #               breaks=c(0,38,41),
  #              include.lowest=T)]
  #d$ppctemperaturecelsius_1
 
 # for ppc peaple 
  
  
  vars_ppcsevereabdominal	<- names(d)[stringr::str_detect(names(d),"^ppcsevereabdominal_")]
  vars_ppclungauscultation	<- names(d)[stringr::str_detect(names(d),"^ppclungauscultation_")]
  vars_ppcverypaleconjunctivapalmsmucuousmembraneornailbeds	<- names(d)[stringr::str_detect(names(d),"^ppcverypaleconjunctivapalmsmucuousmembraneornailbeds_")]
  vars_ppccoughing	<- names(d)[stringr::str_detect(names(d),"^ppccoughing_")]
  vars_ppcdifficultybreathing	<- names(d)[stringr::str_detect(names(d),"^ppcdifficultybreathing_")]
  vars_ppccounselingonfamilyplanning	<- names(d)[stringr::str_detect(names(d),"^ppccounselingonfamilyplanning_")]
  vars_ppccounselingonvitamins	<- names(d)[stringr::str_detect(names(d),"^ppccounselingonvitamins_")]
  vars_ppcinscisionorperinealtear	<- names(d)[stringr::str_detect(names(d),"^ppcinscisionorperinealtear_")]
  vars_ppcexcessivetiredness	<- names(d)[stringr::str_detect(names(d),"^ppcexcessivetiredness_")]
  vars_ppcfundallevel	<- names(d)[stringr::str_detect(names(d),"^ppcfundallevel_")]
  vars_ppcsevereheadache	<- names(d)[stringr::str_detect(names(d),"^ppcsevereheadache_")]
  vars_ppcblurryvision	<- names(d)[stringr::str_detect(names(d),"^ppcblurryvision_")]
  vars_ppcheartauscultationabnormalitiesspecified	<- names(d)[stringr::str_detect(names(d),"^ppcheartauscultationabnormalitiesspecified_")]
  vars_ppcheartauscultation	<- names(d)[stringr::str_detect(names(d),"^ppcheartauscultation_")]
  vars_ppcextremelylightheaded	<- names(d)[stringr::str_detect(names(d),"^ppcextremelylightheaded_")]
  vars_ppclungauscultationabnormalitiesspecified	<- names(d)[stringr::str_detect(names(d),"^ppclungauscultationabnormalitiesspecified_")]
  vars_ppcseizures	<- names(d)[stringr::str_detect(names(d),"^ppcseizures_")]
  vars_ppclossofconsciousness	<- names(d)[stringr::str_detect(names(d),"^ppclossofconsciousness_")]
  vars_ppchistoryofepilepsy	<- names(d)[stringr::str_detect(names(d),"^ppchistoryofepilepsy_")]
  vars_ppcsignsofsepsis	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofsepsis_")]
  vars_ppcsleeplessnessapathyorsymptomsofdepression	<- names(d)[stringr::str_detect(names(d),"^ppcsleeplessnessapathyorsymptomsofdepression_")]
  vars_ppcvomiting	<- names(d)[stringr::str_detect(names(d),"^ppcvomiting_")]
  vars_ppccounselingaboutppcfollowupvisitinsixweeks	<- names(d)[stringr::str_detect(names(d),"^ppccounselingaboutppcfollowupvisitinsixweeks_")]
  vars_ppccounselingaboutbreastcare	<- names(d)[stringr::str_detect(names(d),"^ppccounselingaboutbreastcare_")]
  vars_ppcsignsofedemainhandsorface	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofedemainhandsorface_")]
  vars_ppclochiavaginaldischargecolor	<- names(d)[stringr::str_detect(names(d),"^ppclochiavaginaldischargecolor_")]
  vars_ppcvaginaldischargewithunpleasantodor	<- names(d)[stringr::str_detect(names(d),"^ppcvaginaldischargewithunpleasantodor_")]
  vars_ppcbreastinspection	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspection_")]
  vars_ppcbloodtransfusion	<- names(d)[stringr::str_detect(names(d),"^ppcbloodtransfusion_")]
  vars_ppccalfswelling	<- names(d)[stringr::str_detect(names(d),"^ppccalfswelling_")]
  vars_ppcruptureduterus	<- names(d)[stringr::str_detect(names(d),"^ppcruptureduterus_")]
  vars_ppcsignsofdvt	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofdvt_")]
  vars_ppccontinuousleakageofstool	<- names(d)[stringr::str_detect(names(d),"^ppccontinuousleakageofstool_")]
  vars_ppccontinuousleakageofurine	<- names(d)[stringr::str_detect(names(d),"^ppccontinuousleakageofurine_")]
  vars_ppcsignsofshock	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofshock_")]
  vars_ppcdvtsymptomscalfpain	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalfpain_")]
  vars_ppcdvtsymptomscalfswelling	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalfswelling_")]
  vars_ppcdvtsymptomscalftenderness	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalftenderness_")]
  vars_ppcdvtnosigns	<- names(d)[stringr::str_detect(names(d),"^ppcdvtnosigns_")]
  vars_ppcbreastinspectionabnormalsecretion	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionabnormalsecretion_")]
  vars_ppcbreastinspectioncrackednipples	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectioncrackednipples_")]
  vars_ppcbreastinspectionhotsensation	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionhotsensation_")]
  vars_ppcbreastnoproblems	<- names(d)[stringr::str_detect(names(d),"^ppcbreastnoproblems_")]
  vars_ppcbreastinspectionredness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionredness_")]
  vars_ppcbreastinspectionbreastswelling	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionbreastswelling_")]
  vars_ppcbreastinspectiontenderness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectiontenderness_")]
  vars_ppccounselingondangersignsinthepostpartumperiod	<- names(d)[stringr::str_detect(names(d),"^ppccounselingondangersignsinthepostpartumperiod_")]
  vars_ppccounselingaboutbreastfeeding	<- names(d)[stringr::str_detect(names(d),"^ppccounselingaboutbreastfeeding_")]
  vars_ppccounselingaboutpostpartumdepression	<- names(d)[stringr::str_detect(names(d),"^ppccounselingaboutpostpartumdepression_")]
  vars_ppcincisiontearinspectionbleeding	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionbleeding_")]
  vars_ppcincisiontearinspectionabnormaldischarge	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionabnormaldischarge_")]
  vars_ppcincisiontearinspectionnormal	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionnormal_")]
  vars_ppcincisiontearinspectionpain	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionpain_")]
  vars_ppcincisiontearinspectionswelling	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionswelling_")]
  vars_ppcdefecation	<- names(d)[stringr::str_detect(names(d),"^ppcdefecation_")]
  vars_ppcabnormalurination	<- names(d)[stringr::str_detect(names(d),"^ppcabnormalurination_")]
  vars_ppcskin	<- names(d)[stringr::str_detect(names(d),"^ppcskin_")]
  vars_ppcclammyskin 	<- names(d)[stringr::str_detect(names(d),"^ppcclammyskin _")]
  vars_ppcskintemperature	<- names(d)[stringr::str_detect(names(d),"^ppcskintemperature_")]
  vars_ppccounselingonpelvicfloormuscleexerciseskegel	<- names(d)[stringr::str_detect(names(d),"^	 ppccounselingonpelvicfloormuscleexerciseskegel_")]
  vars_ppcspesificationofabnormaldefecation	<- names(d)[stringr::str_detect(names(d),"^ppcspesificationofabnormaldefecation_")]
  vars_ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem	<- names(d)[stringr::str_detect(names(d),"^ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem_")]
  vars_ppcmchhandbookavailableforthewoman	<- names(d)[stringr::str_detect(names(d),"^ppcmchhandbookavailableforthewoman_")]
  vars_ppcmchhandbookprovided	<- names(d)[stringr::str_detect(names(d),"^ppcmchhandbookprovided_")]
  vars_ppcincisiontearcondition	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearcondition_")]
  vars_ppcvisitundertakenbywhom	<- names(d)[stringr::str_detect(names(d),"^ppcvisitundertakenbywhom_")]
  vars_ppcwhichppcvisit <- names(d)[stringr::str_detect(names(d),"^ppcwhichppcvisit_")]
  vars_ppcmodeofdelivery	<- names(d)[stringr::str_detect(names(d),"^ppcmodeofdelivery_")]
  vars_ppcplaceofdelivery	<- names(d)[stringr::str_detect(names(d),"^ppcplaceofdelivery_")]
  vars_ppcevent<- names(d)[stringr::str_detect(names(d),"^ppcevent_")]
  
  smallD <- d[
              ,
              c(
                "ppcorgdistrict_1",
                vars_ppcsevereabdominal,
                #vars_ppclungauscultation,
                #vars_ppcverypaleconjunctivapalmsmucuousmembraneornailbeds,
                vars_ppccoughing,
                vars_ppcdifficultybreathing,
                #vars_ppccounselingonfamilyplanning,
                #vars_ppccounselingonvitamins,
                #vars_ppcinscisionorperinealtear,
                #vars_ppcexcessivetiredness,
                #vars_ppcfundallevel,
                vars_ppcsevereheadache,
                vars_ppcblurryvision,
                #vars_ppcheartauscultationabnormalitiesspecified,
                #vars_ppcheartauscultation,
                vars_ppcextremelylightheaded,
                #vars_ppclungauscultationabnormalitiesspecified,
                #vars_ppcseizures,
                #vars_ppclossofconsciousness,
                #vars_ppchistoryofepilepsy,
                vars_ppcsignsofsepsis,
                vars_ppcsleeplessnessapathyorsymptomsofdepression,
                #vars_ppcvomiting,
                #vars_ppccounselingaboutppcfollowupvisitinsixweeks,
                #vars_ppccounselingaboutbreastcare,
                #vars_ppcsignsofedemainhandsorface,
                #vars_ppclochiavaginaldischargecolor,
                #vars_ppcvaginaldischargewithunpleasantodor,
                vars_ppcbreastinspection,
                #vars_ppcbloodtransfusion,
                #vars_ppccalfswelling,
                #vars_ppcruptureduterus,
                vars_ppcsignsofdvt,
                #vars_ppccontinuousleakageofstool,
                #vars_ppccontinuousleakageofurine,
                #vars_ppcsignsofshock,
                #vars_ppcdvtsymptomscalfpain,
                #vars_ppcdvtsymptomscalfswelling,
                #vars_ppcdvtsymptomscalftenderness,
                vars_ppcdvtnosigns,
                vars_ppcbreastinspectionabnormalsecretion,
                vars_ppcbreastinspectioncrackednipples,
                vars_ppcbreastinspectionhotsensation,
                vars_ppcbreastnoproblems,
                #vars_ppcbreastinspectionredness,
                #vars_ppcbreastinspectionbreastswelling,
                #vars_ppcbreastinspectiontenderness,
                #vars_ppccounselingondangersignsinthepostpartumperiod,
                #vars_ppccounselingaboutbreastfeeding,
                #vars_ppccounselingaboutpostpartumdepression,
                vars_ppcincisiontearinspectionbleeding,
                #vars_ppcincisiontearinspectionabnormaldischarge,
                #vars_ppcincisiontearinspectionnormal,
                #vars_ppcincisiontearinspectionpain,
                #vars_ppcincisiontearinspectionswelling,
                #vars_ppcdefecation,
                #vars_ppcabnormalurination,
                #vars_ppcskin,
                #vars_ppcclammyskin ,
                vars_ppcskintemperature,
                #vars_ppccounselingonpelvicfloormuscleexerciseskegel,
                #vars_ppcspesificationofabnormaldefecation,
                #vars_ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem,
                #vars_ppcmchhandbookavailableforthewoman,
                #vars_ppcmchhandbookprovided,
                vars_ppcincisiontearcondition,
                vars_ppcvisitundertakenbywhom,
                vars_ppcwhichppcvisit,
                vars_ppcmodeofdelivery,
                vars_ppcplaceofdelivery,
                vars_ppcdaysafterdelivery,
                vars_ppclochiaamount,
                vars_ppcevent,
                vars_cpoevent,
                vars_cpoantepartumhemorrhage,
                vars_demo,
                vars_demo2,
                "pniph_num_ppc_visits"
              ), with=F]

  
  # please create me a 7 unit long list
  res <- vector("list",length=length(vars_demo))
  
  # i want all the vars that are not in vars_demo
  varsIwant <- names(smallD)
  varsIwant <- varsIwant[!varsIwant %in% vars_demo]
  
  for(i in 1:length(res)){
    var <- vars_demo[i]
    res[[i]] <- copy(smallD)
    res[[i]][,DEMO:=sprintf("%s=%s",var,get(var))]
    # only keep the vars that I want
    res[[i]] <- res[[i]][,c(varsIwant,"DEMO"),with=F]
  }
  # compress the list into 1 data.table
  smallD <- rbindlist(res)
  
  
  smallD[,id:=1:.N]
  long <- melt.data.table(smallD,
                          id.vars=c(
                            "id",
                            "DEMO",
                            "ppcorgdistrict_1"
                            ))
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      value2=sum(value==2,na.rm=T),
                      value3=sum(value==3,na.rm=T),
                      valueMean=mean(as.numeric(value),na.rm=T),
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
                            DEMO,
                            variable)
                    ]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "ppc.xlsx"))
  
  # bookorgdistricthashed,	bookorgdistrict
  # reload D back into the global env
  d <- LoadDataFileFromNetwork()
  assign("d",d,envir=.GlobalEnv)
}

Analyse_pniph_abstract_2018_nbc<- function(d){
  
  
  vars_nbcmodeofdelivery	<- names(d)[stringr::str_detect(names(d),"^nbcmodeofdelivery_")]
  vars_nbcplaceofdelivery	<- names(d)[stringr::str_detect(names(d),"^nbcplaceofdelivery_")]
  
  ###For making categories
  
  
  #nbc risks need to add in our data set and make categories
  #NBCrisktype below
  #Umbilicalstumpinfected,Jaundice,Abnormaldefecation,
  #Congenitalanomalies,PKUpositive,Cyanosis,Abnormaldefection,
  #Abnormalurination
  
  #NBC Managments need to add this into the data set as well
  #nbcManagement detail(same as above but with spaces)
  #nbcRisk Type: RefALC or empty (referred to appropriate level of care)
  #management performed (yes or no)
  
  #bookevent
  
  vars_nbcnewbornfeeding	<- names(d)[stringr::str_detect(names(d),"^nbcnewbornfeeding_")]
  vars_bcconditionofbaby	<- names(d)[stringr::str_detect(names(d),"^bcconditionofbaby_")]
  vars_nbcsex	<- names(d)[stringr::str_detect(names(d),"^nbcsex_")]
  vars_nbcsuspectedcongenitalmalformation	<- names(d)[stringr::str_detect(names(d),"^nbcsuspectedcongenitalmalformation_")]
  vars_nbcsuspectedjaundice	<- names(d)[stringr::str_detect(names(d),"^nbcsuspectedjaundice_")]
  vars_nbcnewbornfeeding	<- names(d)[stringr::str_detect(names(d),"^nbcnewbornfeeding_")]
  vars_nbcmorbidity	<- names(d)[stringr::str_detect(names(d),"^nbcmorbidity_")]
  vars_nbcumbilicalstump	<- names(d)[stringr::str_detect(names(d),"^nbcumbilicalstump_")]
  vars_nbccyanosis	<- names(d)[stringr::str_detect(names(d),"^nbccyanosis_")]
  vars_nbcdefecation	<- names(d)[stringr::str_detect(names(d),"^nbcdefecation_")]
  vars_nbchepb	<- names(d)[stringr::str_detect(names(d),"^nbchepb_")]
  vars_nbclatching	<- names(d)[stringr::str_detect(names(d),"^nbclatching_")]
  vars_nbcnewborn	<- names(d)[stringr::str_detect(names(d),"^nbcnewborn_")]
  vars_nbclabscreeningperformedforpku	<- names(d)[stringr::str_detect(names(d),"^nbclabscreeningperformedforpku_")]
  vars_nbcurination	<- names(d)[stringr::str_detect(names(d),"^nbcurination_")]
  vars_nbclabscreeningperformedforcongenitalhypothyreoidism	<- names(d)[stringr::str_detect(names(d),"^nbclabscreeningperformedforcongenitalhypothyreoidism_")]
  vars_nbccounselingondangersignsfornewborn	<- names(d)[stringr::str_detect(names(d),"^nbccounselingondangersignsfornewborn_")]
  vars_nbccounselingaboutumbilicalcordcare	<- names(d)[stringr::str_detect(names(d),"^nbccounselingaboutumbilicalcordcare_")]
  vars_nbccounselingaboutvitaminsandsupplements	<- names(d)[stringr::str_detect(names(d),"^nbccounselingaboutvitaminsandsupplements_")]
  vars_nbcvitaminaddropsgiven	<- names(d)[stringr::str_detect(names(d),"^nbcvitaminaddropsgiven_")]
  vars_nbcchilddesignation	<- names(d)[stringr::str_detect(names(d),"^nbcchilddesignation_")]
  vars_nbclabresultofpkuscreening	<- names(d)[stringr::str_detect(names(d),"^nbclabresultofpkuscreening_")]
  vars_nbclabresultofcongenitalhypothyreoidismchscreening	<- names(d)[stringr::str_detect(names(d),"^nbclabresultofcongenitalhypothyreoidismchscreening_")]
  vars_nbcbreastfeedingposition	<- names(d)[stringr::str_detect(names(d),"^nbcbreastfeedingposition_")]
  
  vars_nbcheadcircumferencecm	<- names(d)[stringr::str_detect(names(d),"^nbcheadcircumferencecm_")]
  vars_nbcheightcm	<- names(d)[stringr::str_detect(names(d),"^nbcheightcm_")]
  vars_nbcrespiratoryratebreathmin	<- names(d)[stringr::str_detect(names(d),"^nbcrespiratoryratebreathmin_")]
  vars_nbcpulsebeatmin	<- names(d)[stringr::str_detect(names(d),"^nbcpulsebeatmin_")]
  vars_nbcweightgrams	<- names(d)[stringr::str_detect(names(d),"^nbcweightgrams_")]
  vars_nbcbirthweightgrams	<- names(d)[stringr::str_detect(names(d),"^nbcbirthweightgrams_")]
  vars_nbcgestationalageatdelivery	<- names(d)[stringr::str_detect(names(d),"^nbcgestationalageatdelivery_")]
  vars_nbcevent	<- names(d)[stringr::str_detect(names(d),"^nbcevent_")]
  
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
    v	<- names(d)[stringr::str_detect(names(d),"^nbmandetail_")]
    v <- stringr::str_remove(v,"nbmandetail_")
    for(i in v){
      d[get(sprintf("nbmandetail_%s",i))==risk,
        (newVar):=1]
      d[get(newVar)==1 & get(sprintf("nbmantypex_%s",i))=="RefALC",
        (newVar):=2]
    }
  }
  var_risks <- names(links)
  
  d[,pniph_000EVERYONE:=1]
  vars_demo <- c(
    "pniph_000EVERYONE",
    "pniph_agecat",
    "pniph_avgincomecat",
    "pniph_incomecat",
    "pniph_agemarriagecat",
    "pniph_agepregnancycat",
    "pniph_educationcat"
  )
  
  smallD <-d[ident_dhis2_control==F &
               bookdate>="2017-01-01" & bookdate<="2018-09-01" &
               ident_dhis2_nbc==T
             ,
              c(
                "bookorgdistrict",
                vars_nbcnewbornfeeding,
                vars_nbcsuspectedcongenitalmalformation,
                vars_nbcsuspectedjaundice,
                vars_nbcumbilicalstump,
                vars_nbccyanosis,
                vars_nbchepb,
                vars_nbclatching,
                vars_nbclabscreeningperformedforpku,
                vars_nbclabscreeningperformedforcongenitalhypothyreoidism,
                vars_nbccounselingondangersignsfornewborn,
                vars_nbccounselingaboutumbilicalcordcare,
                vars_nbccounselingaboutvitaminsandsupplements,
                vars_nbcvitaminaddropsgiven,
                vars_nbcchilddesignation,
                vars_nbclabresultofpkuscreening,
                vars_nbclabresultofcongenitalhypothyreoidismchscreening,
                vars_nbcbreastfeedingposition,
                vars_nbcmodeofdelivery,
                vars_nbcplaceofdelivery,
                vars_nbcevent,
                vars_nbcheadcircumferencecm,
                vars_nbcheightcm,
                vars_nbcrespiratoryratebreathmin,
                vars_nbcpulsebeatmin,
                vars_nbcweightgrams,
                vars_nbcbirthweightgrams,
                vars_nbcgestationalageatdelivery,
                
                "pniph_nbcdaysatvisit",
                var_risks,
                vars_demo
              ),with=F]
  #vars_bcconditionofbaby
  #vars_nbcurination,
  #vars_nbcnewborn,
  #vars_nbcdefecation,
  #vars_nbcmorbidity
  #vars_nbcsex,
  
  # duplicate the dataset
  # make one of them have a district of palestine
  # then put them on top of each other
  smallDPalestine <- copy(smallD)
  smallDPalestine[,bookorgdistrict:="0PALESTINE"]
  
  smallD <- rbind(smallD,smallDPalestine)
  smallD[,id:=1:.N]
  
  # please create me a 7 unit long list
  res <- vector("list",length=length(vars_demo))
  
  # i want all the vars that are not in vars_demo
  varsIwant <- names(smallD)
  varsIwant <- varsIwant[!varsIwant %in% vars_demo]
  
  for(i in 1:length(res)){
    var <- vars_demo[i]
    res[[i]] <- copy(smallD)
    res[[i]][,DEMO:=sprintf("%s=%s",var,get(var))]
    # only keep the vars that I want
    res[[i]] <- res[[i]][,c(varsIwant,"DEMO"),with=F]
  }
  # compress the list into 1 data.table
  smallD <- rbindlist(res)
  
  long <- melt.data.table(smallD, id.vars=c(
    "id",
    "DEMO",
    "bookorgdistrict"
    ),variable.factor = F, value.factor = F)
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
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
                      bookorgdistrict,
                      DEMO,
                      variable)
                    ]

  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "nbc.xlsx"))
  
  
}


Analyse_clex_abstract_2018_clex<- function(d){
  
  ###############the last analyaia for clexane##

  
  d[,x_thrombolytic_problems:=
      (bookhistthrom==1) |
      stringr::str_detect(tolower(bookhistbloodspec),"thromb") |
      (bookhistprevdvt==1)
      ]

  d[,x_bookmedpress_clex:=as.numeric(stringr::str_detect(tolower(bookmedpres),"cle"))]
  d[,x_bookhistmed_clex:=as.numeric(stringr::str_detect(tolower(bookhistmed),"cle"))]
  d[,x_booktext_clex:= x_bookmedpress_clex | x_bookhistmed_clex]

  res<-d[ident_dhis2_control==F &
      bookyear %in% c(2017,2018) &
      ident_dhis2_booking==TRUE,
    .(
      N=.N,
      
      x_thrombolytic_problems_NA=sum(is.na(x_thrombolytic_problems)),
      x_thrombolytic_problems_0=sum(x_thrombolytic_problems==FALSE,na.rm=T),
      x_thrombolytic_problems_1=sum(x_thrombolytic_problems==TRUE,na.rm=T),
      
     bookabortionnothromb_1=sum(bookhistabort==1 & x_thrombolytic_problems==F,na.rm=T),
     bookivfnoabortionnothromb_1=sum(bookhistivf==1 & bookhistabort==0 & x_thrombolytic_problems==F,na.rm=T),
     bookinfernoivfnoabortionnothromb_1=sum(bookhistinfert==1& bookhistivf==0 & bookhistabort==0 & x_thrombolytic_problems==F,na.rm=T)
     
     ),
    keyby=.(
      bookhistclex,
      x_booktext_clex
      
    )]
  
  
  openxlsx::write.xlsx(res, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "clexbookcases.xlsx"))
  
  
  
  
}
  


Analyse_pniph_abstract_2018<- function(d){
  Analyse_pniph_abstract_2018_nbc(d)
  Analyse_pniph_abstract_2018_cpo(d)
  Analyse_clex_abstract_2018_clex(d)
  Analyse_pniph_abstract_2018_ppc()
}

