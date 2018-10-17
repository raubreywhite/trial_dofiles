Analyse_pniph_abstract_2018_ppc<- function(){
  # remove d from the global env as it is too big
  rm("d", envir=.GlobalEnv)
  d <- CleanAllData(includePPC=T,
                    minBookDate="2017-09-01",
                    maxBookDate="2018-09-01")
  
  d <- d[ident_dhis2_ppc==1]
  
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
  
  #bookevent
 
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
  
  
  
  
  
  smallD <- d[ident_dhis2_control==F &
                bookdate>="2017-09-01" & bookdate<="2018-09-01"
              ,
              c(
                vars_ppcsevereabdominal,
                vars_ppclungauscultation,
                vars_ppcverypaleconjunctivapalmsmucuousmembraneornailbeds,
                vars_ppccoughing,
                vars_ppcdifficultybreathing,
                vars_ppccounselingonfamilyplanning,
                vars_ppccounselingonvitamins,
                vars_ppcinscisionorperinealtear,
                vars_ppcexcessivetiredness,
                vars_ppcfundallevel,
                vars_ppcsevereheadache,
                vars_ppcblurryvision,
                vars_ppcheartauscultationabnormalitiesspecified,
                vars_ppcheartauscultation,
                vars_ppcextremelylightheaded,
                vars_ppclungauscultationabnormalitiesspecified,
                vars_ppcseizures,
                vars_ppclossofconsciousness,
                vars_ppchistoryofepilepsy,
                vars_ppcsignsofsepsis,
                vars_ppcsleeplessnessapathyorsymptomsofdepression,
                vars_ppcvomiting,
                vars_ppccounselingaboutppcfollowupvisitinsixweeks,
                vars_ppccounselingaboutbreastcare,
                vars_ppcsignsofedemainhandsorface,
                vars_ppclochiavaginaldischargecolor,
                vars_ppcvaginaldischargewithunpleasantodor,
                vars_ppcbreastinspection,
                vars_ppcbloodtransfusion,
                vars_ppccalfswelling,
                vars_ppcruptureduterus,
                vars_ppcsignsofdvt,
                vars_ppccontinuousleakageofstool,
                vars_ppccontinuousleakageofurine,
                vars_ppcsignsofshock,
                vars_ppcdvtsymptomscalfpain,
                vars_ppcdvtsymptomscalfswelling,
                vars_ppcdvtsymptomscalftenderness,
                vars_ppcdvtnosigns,
                vars_ppcbreastinspectionabnormalsecretion,
                vars_ppcbreastinspectioncrackednipples,
                vars_ppcbreastinspectionhotsensation,
                vars_ppcbreastnoproblems,
                vars_ppcbreastinspectionredness,
                vars_ppcbreastinspectionbreastswelling,
                vars_ppcbreastinspectiontenderness,
                vars_ppccounselingondangersignsinthepostpartumperiod,
                vars_ppccounselingaboutbreastfeeding,
                vars_ppccounselingaboutpostpartumdepression,
                vars_ppcincisiontearinspectionbleeding,
                vars_ppcincisiontearinspectionabnormaldischarge,
                vars_ppcincisiontearinspectionnormal,
                vars_ppcincisiontearinspectionpain,
                vars_ppcincisiontearinspectionswelling,
                vars_ppcdefecation,
                vars_ppcabnormalurination,
                vars_ppcskin,
                vars_ppcclammyskin ,
                vars_ppcskintemperature,
                vars_ppccounselingonpelvicfloormuscleexerciseskegel,
                vars_ppcspesificationofabnormaldefecation,
                vars_ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem,
                vars_ppcmchhandbookavailableforthewoman,
                vars_ppcmchhandbookprovided,
                vars_ppcincisiontearcondition
                
              ), with=F]
  smallD[,id:=1:.N]
  long <- melt.data.table(smallD,
                          id.vars=c("id"))
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      value2=sum(value==2,na.rm=T),
                      value3=sum(value==3,na.rm=T)
                    ),
                    keyby=.(variable)
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
  
  smallD <-d[ident_dhis2_control==F &
               bookdate>="2017-09-01" & bookdate<="2018-09-01" &
               ident_dhis2_nbc==T
             ,
              c(
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
                vars_nbcbreastfeedingposition
              ),with=F]
  #vars_bcconditionofbaby
  #vars_nbcurination,
  #vars_nbcnewborn,
  #vars_nbcdefecation,
  #vars_nbcmorbidity
  #vars_nbcsex,
  
  smallD[,id:=1:.N]
  long <- melt.data.table(smallD, id.vars=c("id"),variable.factor = F, value.factor = F)
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      value2=sum(value==2,na.rm=T),
                      value3=sum(value==3,na.rm=T)
                    ),
                    keyby=.(variable)
                    ]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "nbc.xlsx"))
  
  
}


Analyse_pniph_abstract_2018_cpo<- function(d){  

  #this is for cpo people

  
  vars_cpodvt <- names(d)[stringr::str_detect(names(d),"^cpodvt_")]
  vars_cpoeclampsia <-names(d)[stringr::str_detect(names(d),"^cpoeclampsia_")]
  vars_cpopreeclampsia <- names(d)[stringr::str_detect(names(d),"^cpopreeclampsia_")]
  vars_cpocomplicationsnone <- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
  vars_cpoantepartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
  vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
  vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
  
  #vars_cpopregoutcome <-names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]
  
  smallD <- d[ident_dhis2_control==F &
                bookdate>="2017-09-01" & bookdate<="2018-09-01" &
                ident_dhis2_booking==TRUE & 
                ident_dhis2_an==TRUE & 
                ident_dhis2_cpo==TRUE & 
                ident_dhis2_ppc==TRUE,
              c(
                "bookevent",
                vars_cpodvt,
                vars_cpoeclampsia,
                vars_cpopreeclampsia,
                vars_cpocomplicationsnone,
                vars_cpoantepartumhemorrhage,
                vars_cpopostpartumhemorrhage,
                vars_cpopuerpalsepsis
              ),with=F]
  
  
  
  smallD[,id:=1:.N]
  long <- melt.data.table(smallD, id.vars=c("id"),variable.factor = F, value.factor = F)
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      value2=sum(value==2,na.rm=T),
                      value3=sum(value==3,na.rm=T)
                    ),
                    keyby=.(variable)
                    ]
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "cpo.xlsx"))
  
# 
#   
#   
#   
#   
#   
#   
#   
#   #for demographic
#   agecat
#   avgincomecat
#   incomecat
#   agemarriagecat
#   agepregnancycat
#   educationcat
#   village
#   city
#   camp
#   #members <3   3-5   >5
#   
#   bookevent
#   bookhighrisk
#   anchighrisk
#   
#   #unique bookrogname 
#   
#   #gest age at last visit (less than 36 and more than 36 weeks)
#   #avg anc visits per woman
#   
#   
#   vars_cpoantepartumhemorrhage	<- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
#   vars_cpopuerpalsepsis	<- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
#   vars_cpoantepartumhemorrhage	<- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
#   vars_cpocomplicationsnone	<- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
#   vars_ancounsdanger	<- names(d)[stringr::str_detect(names(d),"^ancounsdanger_")]
#   vars_ancounsnut	<- names(d)[stringr::str_detect(names(d),"^ancounsnut_")]
#   vars_ancounslabor	<- names(d)[stringr::str_detect(names(d),"^ancounslabor_")]
#   
#   
#   
#   
#   smallD <- d[ident_dhis2_control==F &
#                 bookyearmonth>=09-2017 & bookyearmonth<=09-2018,
#               ident_dhis2_anc==T,
#               ident_dhis2_ppc==T,
#               ident_dhis2_cpo==T,
#               
#               # ident_dhis2_df_no_gf_or_bf==F
#               
#               c(agecat,
#                 avgincomecat,
#                 incomecat,
#                 agemarriagecat,
#                 agepregnancycat,
#                 educationcat,
#                 vars_cpoantepartumhemorrhage,
#                 vars_cpopuerpalsepsis,
#                 vars_cpoantepartumhemorrhage,
#                 vars_cpocomplicationsnone,
#                 vars_ancounsdanger,
#                 vars_ancounsnut,
#                 vars_ancounslabor
#                 
#               )] 
}


Analyse_clex_abstract_2018_clex<- function(d){
    
    # this only for booking people
    # for converting text to numbers(go inside this variable and lookfor cle 
    # to lower function::::Translate characters in character vectors
    # in particular from upper to lower case or vice versa.)
    
    
    d[,x_bookmedpress_clex:=as.numeric(stringr::str_detect(tolower(bookmedpres),"cle"))]
    d[,x_bookhistmed_clex:=as.numeric(stringr::str_detect(tolower(bookhistmed),"cle"))]
    
    # initialize the variable as NA
    d[,pniph_bookclex:=as.logical(NA)]
    
    # if any of these three variables are 0/FALSE, then pniph_bookclex=FALSE
    d[bookhistclex==0,pniph_bookclex:=FALSE]
    d[x_bookmedpress_clex==FALSE,pniph_bookclex:=FALSE]
    d[x_bookhistmed_clex==FALSE,pniph_bookclex:=FALSE]
    
    # if any of these three variables are 1/TRUE, then pniph_bookclex=TRUE
    d[bookhistclex==TRUE,pniph_bookclex:=TRUE]
    d[x_bookmedpress_clex==TRUE,pniph_bookclex:=TRUE]
    d[x_bookhistmed_clex==TRUE,pniph_bookclex:=TRUE]
    
    xtabs(~d$pniph_bookclex,addNA=T)
    
    ########################
    # combine anchistclex and anmedpres_
    vars <- c(
      names(d)[stringr::str_detect(names(d),"^anchistclex_")],
      names(d)[stringr::str_detect(names(d),"^anmedpres_")]
      )
    outcome <- "pniph_anchistclex_medpres"
    
    d[,(outcome):=as.logical(NA)]
    # if they respond at all, set them to FALSE
    for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
    # if they respond with 1, set them to TRUE
    for(i in vars) d[get(i)==1,(outcome):=TRUE]
    for(i in vars) d[stringr::str_detect(tolower(get(i)),"clex"),(outcome):=TRUE]
    
    ########################
    # combine anhistthrom
    vars <- c(
      names(d)[stringr::str_detect(names(d),"^anhistthr_")],
      names(d)[stringr::str_detect(names(d),"^anhistbloodspec_")]
    )
    outcome <- "pniph_anhistthrom_bloodspec"
    
    d[,(outcome):=as.logical(NA)]
    # if they respond at all, set them to FALSE
    for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
    # if they respond with 1, set them to TRUE
    for(i in vars) d[get(i)==1,(outcome):=TRUE]
    for(i in vars) d[stringr::str_detect(tolower(get(i)),"thromb"),(outcome):=TRUE]
    
    ########################
    # combine anhistblood
    vars <- names(d)[stringr::str_detect(names(d),"^anhistblood_")]
    outcome <- "pniph_anhistblood"
    
    d[,(outcome):=as.logical(NA)]
    # if they respond at all, set them to FALSE
    for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
    # if they respond with 1, set them to TRUE
    for(i in vars) d[get(i)==1,(outcome):=TRUE]
    
  
    ############################
    #### UGLY TABLE FOR BOOKING
    
    vars <- c(
      "ident_dhis2_booking",
      "bookhistthrom",
      "bookhistabort",
      "bookhistivf",
      "bookhistinfert",
      "bookhistblood",
      "bookhistprevdvt",
      "bookhistpreterm"
    )
    res <- vector("list",length=length(vars))
    
    for(i in 1:length(res)){
      newData1 <- d[ident_dhis2_control==F &
                      bookyear %in% c(2017,2018) &
                      ident_dhis2_booking==TRUE
                    ,
                    c(
                      "pniph_bookclex",
                      "pniph_anchistclex_medpres",
                      "bookorgdistrict",
                      vars[i]
                    ),
                    with=F
                    ]
      newData2 <- copy(newData1)
      newData2[,bookorgdistrict:="0PALESTINE"]
      
      newData <- rbind(newData1,newData2)
      
      setnames(newData,vars[i],"variableOfInterest")
      
      temp <- newData[,
                .(
                  N=.N,
                  bookClexYes=sum(pniph_bookclex==TRUE,na.rm=T),
                  ancClexYes=sum(pniph_anchistclex_medpres==TRUE,na.rm=T),
                  bookandancClexYes=sum(pniph_bookclex==TRUE & pniph_anchistclex_medpres==TRUE,na.rm=T)
                ),
                keyby=.(
                  bookorgdistrict,
                  variableOfInterest
                )]
      temp[,variable:=sprintf("%s - %s",vars[i],variableOfInterest)]
      temp[,variableOfInterest:=NULL]
      
      res[[i]] <- temp
    }
    res <- rbindlist(res)
    setcolorder(res,c("bookorgdistrict","variable"))
    setorder(res,bookorgdistrict,variable)
    
    openxlsx::write.xlsx(res, 
                         file.path(
                           FOLDER_DROPBOX_RESULTS,
                           "pniph",
                           "abstracts_2018",
                           "clexbookcases.xlsx"))

    ############################
    #### UGLY TABLE FOR ANC
    
    vars <- c(
      "pniph_anhistthrom_bloodspec",
      "pniph_anhistblood",
      "ident_dhis2_an"
    )
    res <- vector("list",length=length(vars))
    
    for(i in 1:length(res)){
      newData1 <- d[ident_dhis2_control==F&
                      bookyear %in% c(2017,2018) &  #ask richard---orbookyear==2018&
                      ident_dhis2_booking==TRUE &
                      ident_dhis2_an==TRUE
                    ,
                    c(
                      "pniph_bookclex",
                      "pniph_anchistclex_medpres",
                      "bookorgdistrict",
                      vars[i]
                    ),
                    with=F
                    ]
      newData2 <- copy(newData1)
      newData2[,bookorgdistrict:="0PALESTINE"]
      
      newData <- rbind(newData1,newData2)
      
      setnames(newData,vars[i],"variableOfInterest")
      
      temp <- newData[,
                      .(
                        N=.N,
                        bookClexYes=sum(pniph_bookclex==TRUE,na.rm=T),
                        ancClexYes=sum(pniph_anchistclex_medpres==TRUE,na.rm=T),
                        bookandancClexYes=sum(pniph_bookclex==TRUE & pniph_anchistclex_medpres==TRUE,na.rm=T)
                      ),
                      keyby=.(
                        bookorgdistrict,
                        variableOfInterest
                      )]
      temp[,variable:=sprintf("%s - %s",vars[i],variableOfInterest)]
      temp[,variableOfInterest:=NULL]
      
      res[[i]] <- temp
    }
    res <- rbindlist(res)
    setcolorder(res,c("bookorgdistrict","variable"))
    setorder(res,bookorgdistrict,variable)
    
    openxlsx::write.xlsx(res, 
                         file.path(
                           FOLDER_DROPBOX_RESULTS,
                           "pniph",
                           "abstracts_2018",
                           "clexanccases.xlsx"))
    
    
}


Analyse_pniph_abstract_2018<- function(d){
  Analyse_pniph_abstract_2018_nbc(d)
  Analyse_pniph_abstract_2018_cpo(d)
  Analyse_clex_abstract_2018_clex(d)
  Analyse_pniph_abstract_2018_ppc()
}


