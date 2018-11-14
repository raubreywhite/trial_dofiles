d[ident_dhis2_an==TRUE & 
    ident_dhis2_control==F &
    ident_dhis2_nbc,
  .(
    numVisits1=sum(!is.na(nbcevent_1)),
    numVisits2=sum(!is.na(nbcevent_2)),
    numVisits3=sum(!is.na(nbcevent_3))
  )]


uglytable <- d[bookyear>="2016"& 
    ident_dhis2_control==F &
    ident_dhis2_nbc &
    !is.na(nbcdate_1) &
    !is.na(nbcdate_2),
    
  c("nbcidnumber_1",
    "nbcidnumber_2",
    "nbcdate_1",
    "nbcdate_2",
    "nbcbirthweightgrams_1",
    "nbcbirthweightgrams_2",
    "nbcnameofnewborn_1",
    "nbcnameofnewborn_2"
 
)]


openxlsx::write.xlsx(x=uglytable,file=file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  sprintf("%s_nbc.xlsx",lubridate::today())))