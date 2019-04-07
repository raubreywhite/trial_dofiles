

Analyse_HBO_Completeness <-function(d){
    warning("THIS ONLY TAKES INTO ACCOUNT THE FIRST BIRTH (i.e. IGNORES THE SECOND BABY WITH A TWIN")
    tokeepeveryone <- d[
      ident_dhis2_booking==1]
      
    tokeep <- d[
      ident_dhis2_booking==1 &
      #isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE,]
    
   setorder(tokeep,bookorgname,bookdate)
# 
#     # name of hospital
#     # -dhis2hboconnamehospbirth_1,
#     tokeep[matching=="Avicenna",merged_namehospbirth:=abbname_1]
#     tokeep[matching=="Governmental",merged_namehospbirth:=hboorganisationunitname_1]
#     tokeep[matching=="Private",merged_namehospbirth:=dhis2hboconnamehospbirth_1]
#     tokeep[matching=="PaperHBO",merged_namehospbirth:=paperhbo_placeofdelivery_1]
#     
#     # denominator (number sent)
#     
#     # outcome
#     # - dhis2hbopregoutcome_1
#     tokeep[matching=="Avicenna",merged_pregoutcome:=abbbabybirthresult_1]
#     tokeep[matching=="Governmental",merged_pregoutcome:=hboprevpregoutcome_1]
#     tokeep[matching=="Private",merged_pregoutcome:=dhis2hbopregoutcome_1]
#     tokeep[matching=="PaperHBO",merged_pregoutcome:=paperhbo_outcome_1]
#     xtabs(~tokeep$merged_pregoutcome+tokeep$matching)
#     
#     # abortion
#     # - dhis2hbopregoutcome_1
#     tokeep[!is.na(merged_pregoutcome),merged_abortion:=merged_pregoutcome %in% c("ABO")]
#     xtabs(~tokeep$merged_abortion+tokeep$matching)
#     
#     # gestational age
#     # - dhis2hbogestagedeliv_1
#     tokeep[matching=="Avicenna",merged_gestagedeliv:=as.numeric(stringr::str_extract(abbbabypregnancynoofweeks_1,"^[0-9][0-9]"))]
#     tokeep[matching=="Governmental",merged_gestagedeliv:=hbogestagedeliv_1]
#     tokeep[matching=="Private",merged_gestagedeliv:=dhis2hbogestagedeliv_1]
#     tokeep[matching=="PaperHBO",merged_gestagedeliv:=paperhbo_gestationalageatbirthweeks]
#     
#     # weight
#     # - dhis2hbopregbweight_1
#     tokeep[matching=="Avicenna",merged_pregbweight:=as.numeric(abbbabyweight_1)]
#     tokeep[matching=="Governmental",merged_pregbweight:=hboprevpregbweight_1]
#     tokeep[matching=="Private",merged_pregbweight:=dhis2hbopregbweight_1]
#     tokeep[matching=="PaperHBO",merged_pregbweight:=paperhbo_weightgrams]
#     
#     # date of delivery
#     # - dhis2hbodateofdeliveryhospital_1
#     tokeep[matching=="Avicenna",merged_datedeliv:=abbbabybirthdate_1]
#     tokeep[matching=="Governmental",merged_datedeliv:=as.character(hbodateofdeliveryhospital_1)]
#     tokeep[matching=="Private",merged_datedeliv:=dhis2hbodateofdeliveryhospital_1]
#     tokeep[matching=="PaperHBO",merged_datedeliv:=paperhbo_birthdate]
#     xtabs(~tokeep$matching+tokeep$merged_datedeliv)
#     
#     # hemo
#     # - dhis2hbolabcbchemoglobin_1
#     tokeep[matching=="Avicenna",merged_birthhemo:=alabtestresult_1]
#     tokeep[matching=="Governmental",merged_birthhemo:=hboconlabcbchemoglobin_1]
#     tokeep[matching=="Private",merged_birthhemo:=dhis2hbolabcbchemoglobin_1]
#     tokeep[matching=="PaperHBO",merged_birthhemo:=paperhbo_hbgatadmissiontohospital]
#     
#     # blood p
#     # - dhis2hbosystbp_1
#     # - dhis2hbodiastbp_1
#     warning("need to fix blood pressure")
#     #tokeep[matching=="Avicenna",merged_birthhemo:=alabtestresult_1]
#     #tokeep[matching=="Governmental",merged_birthhemo:=hboconlabcbchemoglobin_1]
#     #tokeep[matching=="Private",merged_birthhemo:=dhis2hbolabcbchemoglobin_1]
#     
#     # mode of deliv
#     # - dhis2hbomodedeliv_1
#     tokeep[matching=="Avicenna",merged_modedeliv:=abbbabybirthtype_1]
#     tokeep[matching=="Governmental",merged_modedeliv:=hbomodeprevdeliv_1]
#     tokeep[matching=="Private",merged_modedeliv:=dhis2hbomodedeliv_1]
#     tokeep[matching=="PaperHBO",merged_modedeliv:=paperhbo_modeofdelivery]
#     
#     # presentation at deliv
#     # - dhis2hbousfetalpresentation_1
#     tokeep[matching=="Avicenna",merged_presentationdeliv:=abbbabybirthtype_1]
#     tokeep[matching=="Governmental",merged_presentationdeliv:=hbousfetalpresentation_1]
#     tokeep[matching=="Private",merged_presentationdeliv:=dhis2hbousfetalpresentation_1]
#     tokeep[matching=="PaperHBO",merged_presentationdeliv:=paperhbo_presentationatdelivery]
#     
#     # indic for csection
#     # - dhis2hboindicforcsec_1
#     warning("merged_indic_csection:=abbbabybirthtype_1")
#     tokeep[matching=="Avicenna",merged_indic_csection:=abbbabybirthtype_1]
#     tokeep[matching=="Governmental",merged_indic_csection:=hboindiccsectioninanycol_1]
#     tokeep[matching=="Private",merged_indic_csection:=dhis2hboindicforcsec_1]
#     tokeep[matching=="PaperHBO",merged_indic_csection:=paperhbo_indicationforcesarian]
#     
    missing <- tokeep[matching=="Not",c("motheridno",
                                        "bookevent",
                                        "bookyearmonth",
                                        "bookdate",
                                        "bookorgdistrict",
                                        "bookorgname",
                                        "firstname",
                                        "familyname1")]
    setorder(missing,bookyearmonth,bookorgdistrict)
    
    openxlsx::write.xlsx(x=missing,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_HBO_Completeness_NOT_MATCHING.xlsx",DATA_DATE)))
    
    
    results <- tokeep[,.(
      denominator=.N,
      merged_namehospbirth_notmiss=sum(!is.na(merged_namehospbirth)),
      merged_pregoutcome_notmiss=sum(!is.na(merged_pregoutcome)),
      merged_abortion_numerator=sum(merged_abortion,na.rm=T),
      merged_gestagedeliv_notmiss=sum(!is.na(merged_gestagedeliv)),
      merged_pregbweight_notmiss=sum(!is.na(merged_pregbweight)),
      merged_datedeliv_notmiss=sum(!is.na(merged_datedeliv)),
      merged_birthhemo_notmiss=sum(!is.na(merged_birthhemo)),
      merged_modedeliv_notmiss=sum(!is.na(merged_modedeliv)),
      merged_presentationdeliv_notmiss=sum(!is.na(merged_presentationdeliv)),
      merged_indic_csection_notmiss=sum(!is.na(merged_indic_csection))
      
    ),by=.(bookyearmonth,
           ident_dhis2_control,
           matching)]
    
    setorder(results,bookyearmonth,matching)
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_HBO_Completeness.xlsx",DATA_DATE)))
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DROPBOX_RESULTS,
      "hbo_completeness",
      sprintf("%s_HBO_Completeness.xlsx",DATA_DATE)))
    
# unmatched govt hospital cases from hbo sheet
    
     #makes a HBO with women who had a missing id
    h <-HBO_Master(deleteMissingMotherIDNO = FALSE)
    
    # list of the motherid nos that are in HBO but did not match
    h$motheridno[!h$motheridno %in% tokeep$motheridno]
    
    # this is the same as above, but the real dataset
    h[!motheridno %in% tokeep$motheridno & hboeventdate_1>="2017-01-15"]
    
    nrow(h)
    nrow(h[hboeventdate_1>="2017-01-15"])
    nrow(h[!motheridno %in% tokeep$motheridno & hboeventdate_1>="2017-01-15"])
    
    openxlsx::write.xlsx(x=h[!motheridno %in% tokeepeveryone$motheridno & 
                               hboeventdate_1>="2017-01-15"],
      file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_HBO_Completeness_unmatched_govt_cases.xlsx",DATA_DATE)))
    
    h <- paperhbo(
      src="bookeventsfound",
      tagWithPaperHBO=TRUE)
    
    nrow(h[!bookevent %in% tokeepeveryone$bookevent & 
             paperhbo_birthdate>="2017-01-15"])
    
    openxlsx::write.xlsx(x=h[!bookevent %in% tokeepeveryone$bookevent & 
                               paperhbo_birthdate>="2017-01-15"],
                         file=file.path(
                           FOLDER_DATA_RESULTS,
                           "hbo_completeness",
                           sprintf("%s_HBO_Completeness_unmatched_paperhbo_cases.xlsx",DATA_DATE)))
    
  
    
}