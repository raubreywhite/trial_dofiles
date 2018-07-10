

HBO_Completeness <-function(d){
  
    tokeep <- d[
      isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE,]
    
   setorder(tokeep,bookorgname,bookdate)

    tokeep[,matching:=as.character(NA)]
    xtabs(~tokeep$matching)
    tokeep[ident_avic_any==TRUE & is.na(matching),matching:="Avicenna"]
    xtabs(~tokeep$matching)
    tokeep[ident_hbo==TRUE & is.na(matching),matching:="Governmental"]
    xtabs(~tokeep$matching)
    tokeep[ident_dhis2_dhis2hbo==TRUE & is.na(matching),matching:="Private"]
    xtabs(~tokeep$matching)
    tokeep[is.na(matching),matching:="Not"]
    xtabs(~tokeep$matching)
    
    # name of hospital
    # -dhis2hboconnamehospbirth_1,
    
    # denominator (number sent)
    
    # abortion
    # - dhis2hbopregoutcome_1
    
    # outcome
    # - dhis2hbopregoutcome_1
    
    # gestational age
    # - dhis2hbogestagedeliv_1
    
    # weight
    # - dhis2hbopregbweight_1
    
    # date of delivery
    # - dhis2hbodateofdeliveryhospital_1
    
    # hemo
    # - dhis2hbolabcbchemoglobin_1
    
    # blood p
    # - dhis2hbosystbp_1
    # - dhis2hbodiastbp_1
    
    # mode of deliv
    # - dhis2hbomodedeliv_1
    
    # presentation at deliv
    # - dhis2hbousfetalpresentation_1
    
    # indic for csection
    # - dhis2hboindicforcsec_1
    
    tokeep[matching=="Private"]$dhis2hbopregoutcome_1
    tokeep[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),lubridate::month(bookdate))]
    #### PRIVATE
    xtabs(~tokeep$dhis2hbogestagedeliv_1,addNA = T)
    
    ##### NOTE, THIS IS NOT CORRECT
    ## NEED TO CHANGE IT TO
    ## dhis2hbogestagedeliv_1[!is.na(dhis2hbogestagedeliv_1)]!=0
    # FOR WHEN 0 IS MISSING
    
    results <- tokeep[matching=="Private",.(
      denom=.N,
      abortions=
        sum(dhis2hbopregoutcome_1=="ABO",na.rm=T) +
        sum(dhis2hbopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_1=="ABO",na.rm=T) +
        sum(cpopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_3=="ABO",na.rm=T) +
        sum(cpopregoutcome_4=="ABO",na.rm=T),
      dhis2hboconnamehospbirth_1=sum(!is.na(dhis2hboconnamehospbirth_1)),
      dhis2hbopregoutcome_1=sum(!is.na(dhis2hbopregoutcome_1)),
      dhis2hbogestagedeliv_1=sum(!is.na(dhis2hbogestagedeliv_1)),
      dhis2hbopregbweight_1=sum(!is.na(dhis2hbopregbweight_1)),
      dhis2hbodateofdeliveryhospital_1=sum(!is.na(dhis2hbodateofdeliveryhospital_1)),
      dhis2hbolabcbchemoglobin_1=sum(!is.na(dhis2hbolabcbchemoglobin_1)),
      dhis2hbosystbp_1=sum(!is.na(dhis2hbosystbp_1)),
      dhis2hbodiastbp_1=sum(!is.na(dhis2hbodiastbp_1)),
      dhis2hbomodedeliv_1=sum(!is.na(dhis2hbomodedeliv_1)),
      dhis2hbousfetalpresentation_1=sum(!is.na(dhis2hbousfetalpresentation_1)),
      dhis2hboindicforcsec_1=sum(!is.na(dhis2hboindicforcsec_1))
    ),by=.(bookyearmonth)]
    
    setorder(results,bookyearmonth)
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_PRIVATE_HBO_Completeness.xlsx",lubridate::today())))
    
    #### GOVERNMENTAL
    results <- tokeep[matching=="Governmental",.(
      denom=.N,
      abortions=
        sum(dhis2hbopregoutcome_1=="ABO",na.rm=T) +
        sum(dhis2hbopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_1=="ABO",na.rm=T) +
        sum(cpopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_3=="ABO",na.rm=T) +
        sum(cpopregoutcome_4=="ABO",na.rm=T),
      dhis2hboconnamehospbirth_1=sum(!is.na(dhis2hboconnamehospbirth_1)),
      dhis2hbopregoutcome_1=sum(!is.na(dhis2hbopregoutcome_1)),
      dhis2hbogestagedeliv_1=sum(!is.na(dhis2hbogestagedeliv_1)),
      dhis2hbopregbweight_1=sum(!is.na(dhis2hbopregbweight_1)),
      dhis2hbodateofdeliveryhospital_1=sum(!is.na(dhis2hbodateofdeliveryhospital_1)),
      dhis2hbolabcbchemoglobin_1=sum(!is.na(dhis2hbolabcbchemoglobin_1)),
      dhis2hbosystbp_1=sum(!is.na(dhis2hbosystbp_1)),
      dhis2hbodiastbp_1=sum(!is.na(dhis2hbodiastbp_1)),
      dhis2hbomodedeliv_1=sum(!is.na(dhis2hbomodedeliv_1)),
      dhis2hbousfetalpresentation_1=sum(!is.na(dhis2hbousfetalpresentation_1)),
      dhis2hboindicforcsec_1=sum(!is.na(dhis2hboindicforcsec_1))
    ),by=.(bookyearmonth)]
    
    setorder(results,bookyearmonth)
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_GOVERNMENTAL_HBO_Completeness.xlsx",lubridate::today())))
    
    #### AVICENNA
    results <- tokeep[matching=="Avicenna",.(
      denom=.N,
      abortions=
        sum(dhis2hbopregoutcome_1=="ABO",na.rm=T) +
        sum(dhis2hbopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_1=="ABO",na.rm=T) +
        sum(cpopregoutcome_2=="ABO",na.rm=T) +
        sum(cpopregoutcome_3=="ABO",na.rm=T) +
        sum(cpopregoutcome_4=="ABO",na.rm=T),
      dhis2hboconnamehospbirth_1=sum(!is.na(dhis2hboconnamehospbirth_1)),
      dhis2hbopregoutcome_1=sum(!is.na(dhis2hbopregoutcome_1)),
      dhis2hbogestagedeliv_1=sum(!is.na(dhis2hbogestagedeliv_1)),
      dhis2hbopregbweight_1=sum(!is.na(dhis2hbopregbweight_1)),
      dhis2hbodateofdeliveryhospital_1=sum(!is.na(dhis2hbodateofdeliveryhospital_1)),
      dhis2hbolabcbchemoglobin_1=sum(!is.na(dhis2hbolabcbchemoglobin_1)),
      dhis2hbosystbp_1=sum(!is.na(dhis2hbosystbp_1)),
      dhis2hbodiastbp_1=sum(!is.na(dhis2hbodiastbp_1)),
      dhis2hbomodedeliv_1=sum(!is.na(dhis2hbomodedeliv_1)),
      dhis2hbousfetalpresentation_1=sum(!is.na(dhis2hbousfetalpresentation_1)),
      dhis2hboindicforcsec_1=sum(!is.na(dhis2hboindicforcsec_1))
    ),by=.(bookyearmonth)]
    
    setorder(results,bookyearmonth)
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_AVICENNAL_HBO_Completeness.xlsx",lubridate::today())))
    
    #### NOT
    results <- tokeep[matching=="Not",.(
      denom=.N
      ),by=.(bookyearmonth)]
    
    setorder(results,bookyearmonth)
    
    openxlsx::write.xlsx(x=results,file=file.path(
      FOLDER_DATA_RESULTS,
      "hbo_completeness",
      sprintf("%s_NOTL_HBO_Completeness.xlsx",lubridate::today())))
  

}