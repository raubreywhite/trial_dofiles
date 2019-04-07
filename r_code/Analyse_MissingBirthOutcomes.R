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
  d[,prettyTrial1:="Trial 1"]
  d[ident_TRIAL_1==FALSE,prettyTrial1:="_Not trial 1"]
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
  keyby=.(prettyTrial1,bookyearmonth)]
  tab[,"% Missing":=round(`Number not matched`/Total*100)]
  
  openxlsx::write.xlsx(x=tab,
                       file=file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "hbo_completeness",
                         sprintf("%s_matched_report.xlsx",DATA_DATE)))
  
  
  ### maybe you want this, maybe you don??
  
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