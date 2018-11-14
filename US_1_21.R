sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "mahima_gA_us and hbo_dob(us_21wksAndBefore).txt"))

analysisDatasetUSgA <- d[bookyearmonth<="2017-03" & 
                           ident_TRIAL_1==TRUE & 
                           !is.na(mahima_hospenteredgestage_1) &
                           !is.na(mahima_gestageatbirthwk_1) &
                           !is.na(first_1_21_usedd_gA)&
                           !is.na(first_1_21_usedd_gA_cats),
                         c("mahima_dateofbirth_1",
                           "first_1_21_usedd_gA",
                           "first_1_21_usedd_gA_cats")]
cat("\nDenominator_first_1_21_usedd\n")
nrow(analysisDatasetUSgA)
cat("\nMean_first_1_21_usedd\n")
mean(analysisDatasetUSgA$first_1_21_usedd_gA)

cat("\nXtabs first_1_21_usedd_gA\n")
xtabs(~analysisDatasetUSgA$first_1_21_usedd_gA)

cat("\nXtabs first_1_21_usedd_gA_cats\n")
xtabs(~analysisDatasetUSgA$first_1_21_usedd_gA_cats)

cat("\nIQRcalculatedgestage\n")
quantile(x=analysisDatasetUSgA$first_1_21_usedd_gA, 
         probs = seq(0, 1, 0.25), 
         na.rm = TRUE)

cat("\nTRIALcats\n")
xtabs(~analysisDatasetUSgA$first_1_21_usedd_gA)

cat("\nbelow0US\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="[-30,0]",
  c(first_1_21_usedd_gA)])
cat("\nABO\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="(0,24.7]",
  c(first_1_21_usedd_gA)])
cat("\nreallyPRE\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="(24.7,32.7]",
  c(first_1_21_usedd_gA)])
cat("\nPRE\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="(32.7,37.7]",
  c(first_1_21_usedd_gA)])
cat("\nTerm\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="(37.7,41.7]",
  c(first_1_21_usedd_gA)])
cat("\nPost\n")
xtabs(~analysisDatasetUSgA[
  first_1_21_usedd_gA_cats=="(41.7,44]",
  c(first_1_21_usedd_gA)])




cat("\nDenominators for first_1_21_usedd\n")
cat("\nDenominators for TRIAL first_1_21_usedd\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(first_1_21_usedd_gA)&
         !is.na(first_1_21_usedd_gA_cats),
       c("mahima_dateofbirth_1",
         "first_1_21_usedd_gA",
         "first_1_21_usedd_gA_cats")])

cat("\nDenominators for ARM A first_1_21_usedd\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         ident_dhis2_control==T &
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(first_1_21_usedd_gA_cats),
       c("mahima_dateofbirth_1",
         "first_1_21_usedd_gA",
         "first_1_21_usedd_gA_cats")])



cat("\nDenominators for ARM B first_1_21_usedd\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         ident_dhis2_control==F &
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(first_1_21_usedd_gA)&
         !is.na(first_1_21_usedd_gA_cats),
       c("mahima_dateofbirth_1",
         "first_1_21_usedd_gA",
         "first_1_21_usedd_gA_cats")])

sink()



