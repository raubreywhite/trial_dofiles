Analyse_MissingBirthOutcomes <- function(d){
    
  ####
  # table <- d[,.(
  #   Total=.N,
  #   uglyname=3
  # )]
  # 
  # setnames(table,"uglyname","What a pretty name!")
  d[,prettyTrial1:="Trial 1"]
  d[ident_TRIAL_1==FALSE,prettyTrial1:="_Not trial 1"]
  tab <- d[ident_dhis2_booking==1 & !is.na(ident_bad_all),.(
    Total=.N,
    "Total expected delivery"=sum(isExpectedToHaveDelivered,na.rm=T),
    "Number of women who have a missing or false EDD"=
      sum(is.na(isExpectedToHaveDelivered)) + sum(isExpectedToHaveDelivered==F,na.rm=T),
    "Number match in avicenna" = sum(matching=="Avicenna"),
    "Number match in gov" = sum(matching=="Governmental"),
    "Number match in private" = sum(matching=="Private"),
    "Number match in paperhbo" = sum(matching=="PaperHBO"),
    "Number not matched" = sum(matching=="Not")
  ),
  keyby=.(prettyTrial1,bookyearmonth)]
  tab[,"% Missing":=round(`Number not matched`/Total*100)]
  
  openxlsx::write.xlsx(x=tab,
                       file=file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "hbo_completeness",
                         sprintf("%s_missing_birth_outcomes.xlsx",DATA_DATE)))
  
  
  # setnames(table,"uglyname","What a pretty name!")
  d[ident_dhis2_control==F,Arm:= "A"]
  d[ident_dhis2_control==T,Arm:="B"]
  tab <- d[ident_dhis2_booking==1 & ident_TRIAL_1==T,.(
    Total=.N,
    "Total expected delivery"=sum(isExpectedToHaveDelivered,na.rm=T),
    "Number of women who have a missing or false EDD"=
      sum(is.na(isExpectedToHaveDelivered)) + sum(isExpectedToHaveDelivered==F,na.rm=T),
    "Number match in avicenna" = sum(matching=="Avicenna"),
    "Number match in gov" = sum(matching=="Governmental"),
    "Number match in private" = sum(matching=="Private"),
    "Number match in paperhbo" = sum(matching=="PaperHBO"),
    "Number not matched" = sum(matching=="Not")
  ),
  keyby=.(bookyearmonth, Arm)]
  tab[,"% Missing":=round(`Number not matched`/Total*100)]
  
  openxlsx::write.xlsx(x=tab,
                       file=file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "hbo_completeness",
                         sprintf("%s_matched_report.xlsx",DATA_DATE)))
  
  
  ###analyze data points per woman
  ##variables per woman and total of combinations
  #
  
  tokeep <- d[
    ident_dhis2_booking==1 &
      #isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE &
      bookdate >= "2017-01-15"&
      bookdate<="2017-09-15",]
  
  setorder(tokeep,bookorgname,bookdate)
  
  
  results <- tokeep[,.(
    denominator=.N,
    NoneMissing=sum(!is.na(merged_pregoutcome) &
                      (!is.na(merged_gestagedeliv)|
                         !is.na(merged_datedeliv) ) &
                      !is.na(merged_pregbweight) &
                      !is.na(merged_birthhemo) &
                      !is.na(merged_modedeliv) &
                      (!is.na(merged_bpsyst) | !is.na(merged_bpdiast)) &
                      !is.na(merged_presentationdeliv), na.rm=TRUE),
    MissingGABDONLY=sum((is.na(merged_gestagedeliv)|
                           is.na(merged_datedeliv)) &
                          !is.na(merged_pregoutcome) &
                          !is.na(merged_pregbweight) &
                          !is.na(merged_birthhemo) &
                          !is.na(merged_modedeliv) &
                          (!is.na(merged_bpsyst)|!is.na(merged_bpdiast)) &
                          !is.na(merged_presentationdeliv), na.rm=TRUE),
    BWMissingOnly=sum(is.na(merged_pregbweight) &
                        (!is.na(merged_gestagedeliv)|
                           !is.na(merged_datedeliv)) &
                        !is.na(merged_pregoutcome) &
                        !is.na(merged_birthhemo) &
                        !is.na(merged_modedeliv) &
                        (!is.na(merged_bpsyst)|!is.na(merged_bpdiast)) &
                        !is.na(merged_presentationdeliv), na.rm=TRUE),
    HBGMissingOnly=sum(is.na(merged_birthhemo) &
                         (!is.na(merged_gestagedeliv)|
                            !is.na(merged_datedeliv)) &
                         !is.na(merged_pregoutcome) &
                         !is.na(merged_pregbweight) &
                         !is.na(merged_modedeliv) &
                         (!is.na(merged_bpsyst)|!is.na(merged_bpdiast)) &
                         !is.na(merged_presentationdeliv), na.rm=TRUE),
    MissingMODOnly=sum(is.na(merged_modedeliv) &
                         (!is.na(merged_gestagedeliv)|
                            !is.na(merged_datedeliv)) &
                         !is.na(merged_pregoutcome) &
                         !is.na(merged_pregbweight) &
                         !is.na(merged_birthhemo) &
                         (!is.na(merged_bpsyst)|!is.na(merged_bpdiast)) &
                         !is.na(merged_presentationdeliv), na.rm=TRUE),
    MissingBPONLY=sum((is.na(merged_bpsyst)|
                         is.na(merged_bpdiast)) &
                        (!is.na(merged_gestagedeliv)|
                           !is.na(merged_datedeliv)) &
                        !is.na(merged_pregoutcome) &
                        !is.na(merged_pregbweight) &
                        !is.na(merged_birthhemo) &
                        !is.na(merged_modedeliv) &
                        !is.na(merged_presentationdeliv), na.rm=TRUE),
    MissingPresONLY=sum(is.na(merged_modedeliv) &
                          (!is.na(merged_gestagedeliv)|
                             !is.na(merged_datedeliv)) &
                          !is.na(merged_pregoutcome) &
                          !is.na(merged_pregbweight) &
                          !is.na(merged_birthhemo) &
                          (!is.na(merged_bpsyst)|!is.na(merged_bpdiast)) &
                          !is.na(merged_presentationdeliv), na.rm=TRUE),
    MergedModeCS=sum(merged_modedeliv=="Cesarean Section (with General Anesthesia)"|
                       merged_modedeliv=="C.S"|
                       merged_modedeliv=="C.S."|
                       merged_modedeliv=="Caesarian section"|
                       merged_modedeliv=="Cesarean Section (with Local Anesthesia)"|
                       merged_modedeliv=="CS"|
                       merged_modedeliv=="C/S"|
                       merged_modedeliv=="C/S/",
                       na.rm=TRUE)
  ),by=.(ident_dhis2_control)]
  
  openxlsx::write.xlsx(results,
                       file=file.path(
                         FOLDER_DATA_RESULTS,
                         "hbo_completeness",
                         sprintf("%s_birth_outcomes_per_woman.xlsx",lubridate::today())))
 
  
  ### maybe you want this, maybe you dont??
  
  phbo <- paperhbo(
    src="bookeventsfound",
    tagWithPaperHBO=TRUE)
  nrow(phbo)
  phbo <- merge(phbo,
                d[ident_dhis2_booking==T,c("bookevent","ident_TRIAL_1","ident_dhis2_booking","isExpectedToHaveDelivered")],
                by="bookevent",
                all.x=T)
  nrow(phbo)
  sum(is.na(phbo$ident_dhis2_booking))
  
  tab <- phbo[paperhbo_birthdate>="2017-01-15",.(
    N=.N,
    matched=sum(ident_dhis2_booking,na.rm=T)
  ),keyby=.(paperhbo_fileoriginal)]
  tab[,"not matched":=N-matched]
  sum(tab$`not matched`)
  
  
  
}