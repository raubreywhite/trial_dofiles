sink(file.path(FOLDER_DROPBOX_RESULTS,
                                "mahima",
                                "trial_1",
                                "mahima_gA_us and hbo_dob(comboUsAndLMPgA).txt"))

   
cat("\n")
analysisDatasetUSgA <- d[bookyearmonth<="2017-03" & 
                         ident_TRIAL_1==TRUE & 
                         !is.na(mahima_hospenteredgestage_1) &
                         !is.na(mahima_gestageatbirthwk_1) &
                         !is.na(comboUSandLMPgA_cats) &
                         !is.na(comboUSandLMPgA),
                         c("mahima_dateofbirth_1",
                           "comboUSandLMPgA",
                           "comboUSandLMPgA_cats")]
cat("\nDENOMINATOR\n")
nrow(analysisDatasetUSgA)
 cat("\nIQRcalculatedgestage\n")
 quantile(x=analysisDatasetUSgA$comboUSandLMPgA, 
                        probs = seq(0, 1, 0.25), 
                        na.rm = TRUE)
 
 cat("\n")
 mean(analysisDatasetUSgA$comboUSandLMPgA)
 print(comboUSandLMPgA_mean)

cat("\nTRIAL\n")
   xtabs(~analysisDatasetUSgA$comboUSandLMPgA)
   
     #cat("\nbelow0US\n")
     #xtabs(~analysisDatasetUSgA[
     # first_1_21_usedd_gA_cats=="[-30,0]",
     # c(first_1_21_usedd_gA)])
     
     
cat("\nBelow0\n")
xtabs(~analysisDatasetUSgA[
     comboUSandLMPgA_cats=="[-30,0]",
     c(comboUSandLMPgA)])

cat("\nABO\n")
   xtabs(~analysisDatasetUSgA[
     comboUSandLMPgA_cats=="(0,24.7]",
     c(comboUSandLMPgA)])
   
 cat("\nreallyPRE\n")
 xtabs(~analysisDatasetUSgA[
   comboUSandLMPgA_cats=="(24.7,32.7]",
   c(comboUSandLMPgA)])
 
 cat("\nPRE\n")
 xtabs(~analysisDatasetUSgA[
   comboUSandLMPgA_cats=="(32.7,37.7]",
   c(comboUSandLMPgA)])
 
 cat("\nTerm\n")
 xtabs(~analysisDatasetUSgA[
   comboUSandLMPgA_cats=="(37.7,41.7]",
   c(comboUSandLMPgA)])
 
 cat("\nPost\n")
 xtabs(~analysisDatasetUSgA[
   comboUSandLMPgA_cats=="(41.7,44]",
   c(comboUSandLMPgA)])
 
 
 cat("\nDenominators comboUSandLMPgA\n")
 cat("\nDenominators for TRIAL lastuseddminusdob\n")
 nrow(analysisDatasetUSgA[, c("mahima_dateofbirth_1",
                               "comboUSandLMPgA",
                               "comboUSandLMPgA_cats")])
      
cat("\nDenominators for ARM A comboUSandLMPgA\n")
nrow(d[bookyearmonth<="2017-03" & 
                 ident_TRIAL_1==TRUE & 
                 !is.na(mahima_hospenteredgestage_1) &
                 !is.na(mahima_gestageatbirthwk_1) &
                 !is.na(comboUSandLMPgA_cats) &
                 !is.na(comboUSandLMPgA),
             c("mahima_dateofbirth_1",
                "comboUSandLMPgA",
                "comboUSandLMPgA_cats")])
      
      
cat("\nDenominators for ARM B comboUSandLMPgA\n")
nrow(d[bookyearmonth<="2017-03" & 
               ident_TRIAL_1==FALSE & 
               !is.na(mahima_hospenteredgestage_1) &
               !is.na(mahima_gestageatbirthwk_1) &
               !is.na(comboUSandLMPgA_cats) &
               !is.na(comboUSandLMPgA),
             c("mahima_dateofbirth_1",
               "comboUSandLMPgA",
               "comboUSandLMPgA_cats")])
      
  sink()

 