Analyse_BookingDescriptives <- function(d=NULL){
  #RColorBrewer::display.brewer.all() 
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # to make graphs
  
  toPlot <- d[ident_dhis2_booking==1,
              .(numWomen=.N),
              by=.(bookyearmonth)
              ]
  
  setorder(toPlot,bookyearmonth)
  
  p <- ggplot(data=toPlot, mapping=aes(x=bookyearmonth,y=numWomen))
  p <- p + geom_bar(stat="identity")
  p <- p + geom_label(mapping=aes(label=numWomen), size=1)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
  ggsave(
    filename=file.path(
      FOLDER_DROPBOX_RESULTS,
      "mahima",
      "trial_1",
      "bookings_by_month.png"),
    plot=p, 
    width = 297, 
    height = 210, 
    units = "mm")
  
  # if we use "by=.()" it wont be sorted
  # if we use "keyby=.()" it will be sorted
  variablesIWant <- c(
    "bookyearmonth",
    "ident_dhis2_control",
    "bookorgname",
    "bookgestage"
  )
  data_clinic <- d[ident_dhis2_booking==1 & ident_TRIAL_1==1,variablesIWant,with=F]
  data_palestine <- d[ident_dhis2_booking==1 & ident_TRIAL_1==1,variablesIWant,with=F]
  data_palestine[,bookorgname:="0Palestine"]
  
  dataForTable_month <- rbind(data_clinic,data_palestine)
  dataForTable_year <- rbind(data_clinic,data_palestine)
  dataForTable_year[,bookyearmonth:="Total"]
  
  dataForTable <- rbind(dataForTable_month,dataForTable_year)
  
  tab <- dataForTable[,
                      .(
                        meangestage=mean(bookgestage, na.rm=T),
                        mediangestage=median(bookgestage, na.rm=T),
                        numWomen=.N
                      ),
                      keyby=.(
                        bookyearmonth, ident_dhis2_control, bookorgname
                      )
                      ]
  tab[,meangestage:=round(meangestage,digits=0)]
  tab[,mediangestage:=round(mediangestage,digits=0)]
  tab
  
  openxlsx::write.xlsx(tab, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "booking_descriptives",
                         "numberandmeangestage.xlsx"))
  
  p <- ggplot(tab[bookyearmonth!="Total"], aes(x=ident_dhis2_control, y=meangestage))
  p <- p + geom_boxplot()
  p <- p + scale_x_discrete("Arm A clinics")
  p <- p + scale_y_continuous("Mean gestage")
  p1nocaption <- p + labs(title="Clinics' Mean Gestational Age in Trial 1")
  p1withcaption <- p1nocaption + labs(caption=GraphCaption())
  #p1withcaption <- coord_flip()
  p1withcaption
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1bycontrol.png"),
         plot=p1withcaption,
         height=210,
         width=297,
         units="mm")
  
  p <- ggplot(tab[bookorgname!="0Palestine"], aes(x=bookyearmonth, y=meangestage))
  p <- p + geom_boxplot()
  p <- p + scale_x_discrete("Booking Month")
  p <- p + scale_y_continuous("Mean gestage")
  p <- p + labs(title="Clinics' Mean Gestational Age in Trial 1")
  p2 <- p + labs(caption=GraphCaption())
  p2
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1bymonth.png"),
         plot=p2,
         height=210,
         width=297,
         units="mm")
  
  p3 <- gridExtra::grid.arrange(p1nocaption,p2)
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1_together.png"),
         plot=p3,
         height=210,
         width=297,
         units="mm")
  
}

Analyse_EnteredVsCalculated <- function(d){
  ##Creating differences between the calculated and entered gest ages
  d[,difference:= mahima_gestageatbirthwk_1-mahima_hospenteredgestage_1]
  #difference<- d$mahima_gestageatbirthwk_1-d$mahima_hospenteredgestage_1
  #creates variable outside of d, the first one created the variable inside d
  
  res <- list()
  
  
  f <- t.test(d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE & 
             ident_dhis2_control==T]$difference)
  res[[length(res)+1]] <- data.frame("label"="t.test control gestage diff",
                                     "pvalue"=f$p.value)
  
  f <- t.test(d[ bookyearmonth<="2017-03" & 
              ident_TRIAL_1==TRUE & 
              ident_dhis2_control==F]$difference)
  res[[length(res)+1]] <- data.frame("label"="t.test inter gestage diff",
                                     "pvalue"=f$p.value)
  
##is the calculated different than the entered. non parametric testing
  f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                  ident_TRIAL_1==TRUE & 
                  ident_dhis2_control==T]$difference,
              mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test control gestage diff",
                                     "pvalue"=f$p.value)
  
 f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                  ident_TRIAL_1==TRUE & 
                  ident_dhis2_control==F]$difference,
              mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test inter gestage diff",
                                     "pvalue"=f$p.value)
  
  f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                       ident_TRIAL_1==TRUE]$difference,
                   mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test control&inter gestage diff",
                                     "pvalue"=f$p.value)
##removing outlier and comparing significance in intervention and control gestages
 f <- t.test(difference ~ ident_dhis2_control, 
                      data = d[bookyearmonth<="2017-03" & 
                                 difference<400 &
                                 ident_TRIAL_1==TRUE ])
  res[[length(res)+1]] <- data.frame("label"="t.test (no outlier) btwn gestage diff in two groups",
                                     "pvalue"=f$p.value)
  
  
  f<- wilcox.test(difference ~ ident_dhis2_control, 
         data = d[bookyearmonth<="2017-03" & 
                    ident_TRIAL_1==TRUE ])
  res[[length(res)+1]] <- data.frame("label"="t.test (w/outlier) btwn gestage diff in two groups",
                                     "pvalue"=f$p.value)
  #if want to add df from wilcoxon, set it equal to 99 because it doesnt exist here
  
  res <- rbindlist(res)
  
  
  
  #unique(d$bookyearmonth)
  openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                      "mahima",
                                      "trial_1",
                                      "entered_and_calculated_gest_ages_statisticaltests.xlsx"))
 
  p<-ggplot(d[bookyearmonth<="2017-03" & ident_TRIAL_1==TRUE],aes(x=difference, colour = ident_dhis2_control)) 
  p <- p+ geom_density()
  p <-p + xlim(-25, 25)
  
  
  
  ## comparing mahima gestational age calculated vs entered
  
  #dev.off() try to run this code if get weird graphic errors
  p <- ggplot(d[ident_TRIAL_1==TRUE], aes(x=mahima_gestageatbirthwk_1, y=mahima_hospenteredgestage_1))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + labs(title="Entered and Calculated Gestational Ages")
  p <- p + scale_x_continuous("Calculated Gestational Ages")
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "entered_and_calculated_gest_ages.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  ####Graphs for distribution of gestational ages with categories
  p <- ggplot(d[ident_TRIAL_1==TRUE & 
                  ident_dhis2_control==FALSE &
                  bookyearmonth>= "2017-01" &
                  bookyearmonth<= "2017-03"
                  ], 
                aes(x=mahima_gestageatbirthwk_1_cats, y=mahima_gestageatbirthwk_1))
 

####differences with rounded entered gestages
##Creating differences between the rounded calculated and entered gest ages
  d[,difference_rounded:= mahima_gestageatbirthwk_1_rounded-mahima_hospenteredgestage_1]
 
  
  res <- list()
  
  
  f <- t.test(d[bookyearmonth<="2017-03" & 
                  ident_TRIAL_1==TRUE & 
                  ident_dhis2_control==T]$difference_rounded)
  res[[length(res)+1]] <- data.frame("label"="t.test control gestage diff_rounded",
                                     "pvalue"=f$p.value)
  
  f <- t.test(d[ bookyearmonth<="2017-03" & 
                   ident_TRIAL_1==TRUE & 
                   ident_dhis2_control==F]$difference_rounded)
  res[[length(res)+1]] <- data.frame("label"="t.test inter gestage diff_rounded",
                                     "pvalue"=f$p.value)
  
  ##is the rounded calculated different than the entered. non parametric testing
  f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                       ident_TRIAL_1==TRUE & 
                       ident_dhis2_control==T]$difference_rounded,
                   mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test control gestage diff_rounded",
                                     "pvalue"=f$p.value)
  
  f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                       ident_TRIAL_1==TRUE & 
                       ident_dhis2_control==F]$difference_rounded,
                   mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test inter gestage diff_rounded",
                                     "pvalue"=f$p.value)
  
  f <- wilcox.test(d[bookyearmonth<="2017-03" & 
                       ident_TRIAL_1==TRUE]$difference_rounded,
                   mu = 0, alternative = "two.sided")
  res[[length(res)+1]] <- data.frame("label"="wilcoxon.test control&inter diff_rounded gestage diff_rounded",
                                     "pvalue"=f$p.value)
  ##removing outlier and comparing significance in intervention and control gestages
  f <- t.test(difference_rounded ~ ident_dhis2_control, 
              data = d[bookyearmonth<="2017-03" & 
                         difference_rounded<400 &
                         ident_TRIAL_1==TRUE ])
  res[[length(res)+1]] <- data.frame("label"="t.test (no outlier) btwn rounded gestage diff in two groups",
                                     "pvalue"=f$p.value)
  
  
  f<- wilcox.test(difference_rounded ~ ident_dhis2_control, 
                  data = d[bookyearmonth<="2017-03" & 
                             ident_TRIAL_1==TRUE ])
  res[[length(res)+1]] <- data.frame("label"="t.test (w/outlier) btwn rounded gestage diff in two groups",
                                     "pvalue"=f$p.value)
  #if want to add df from wilcoxon, set it equal to 99 because it doesnt exist here
  
  res <- rbindlist(res)
  
  
  
  #unique(d$bookyearmonth)
  openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                     "mahima",
                                     "trial_1",
                                     "entered_and_rounded_calculated_gest_ages_statisticaltests.xlsx"))
  
  p<-ggplot(d[bookyearmonth<="2017-03" & ident_TRIAL_1==TRUE],aes(x=difference_rounded, colour = ident_dhis2_control)) 
  p <- p+ geom_density()
  p <-p + xlim(-25, 25)
  
  
  
  ## comparing mahima gestational age rounded calculated vs entered
  
  #dev.off() try to run this code if get weird graphic errors
  p <- ggplot(d[ident_TRIAL_1==TRUE], aes(x=mahima_gestageatbirthwk_1_rounded, y=mahima_hospenteredgestage_1))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + labs(title="Entered and Calculated Gestational Ages_rounded")
  p <- p + scale_x_continuous("Calculated Gestational Ages")
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "entered_and_rounded_calculated_gest_ages.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  
  
  
  
###printing out prevalences for the hospital birth gest age stuff ROUNDED
###sink() is like capture in STATA
  sink()
  sink(file.path(FOLDER_DROPBOX_RESULTS,
                                     "mahima",
                                     "trial_1",
                                     "hospital gA calculated and entered(lmp).txt"))
  
  cat("\ncreating a smaller dataset just for this purpose\n")
  analysisDataset <- d[bookyearmonth<="2017-03"&
                         ident_TRIAL_1==TRUE &
                         !is.na(mahima_gestageatbirthwk_1) &
                         !is.na(mahima_hospenteredgestage_1)&
                         !is.na(mahima_gestageatbirthwk_1_rounded),
                       c("bookevent",
                         "mahima_hospenteredgestage_1",
                         "mahima_gestageatbirthwk_1",
                         "mahima_gestageatbirthwk_1_rounded")]
  cat("\n\n Denonimator in the dataset that have both variables\n\n") 
  cat("\n\n") 
  print(nrow(d[bookyearmonth<="2017-03" & 
                 ident_TRIAL_1==TRUE & 
                 !is.na(mahima_hospenteredgestage_1) & 
                 !is.na(mahima_gestageatbirthwk_1_rounded) &
                 !is.na(mahima_gestageatbirthwk_1)]))
  
  #xtabs(~d$mahima_gestageatbirthwk_1_cats)
  #xtabs(~d$mahima_hospenteredgestage_1_cats)
  xtabs(~d$mahima_hospenteredgestage_1_cats + d$mahima_gestageatbirthwk_1_cats)
  
  cat("\nMeancalculatedgestAge\n")
  mean(analysisDataset$mahima_gestageatbirthwk_1)
  
  cat("\nIQRcalculatedgestage\n")
  quantile(x=analysisDataset$mahima_gestageatbirthwk_1, 
           probs = seq(0, 1, 0.25), 
           na.rm = TRUE)
  cat("\nMean_mahima_hospenteredgestage_1")
  mean(analysisDataset$mahima_hospenteredgestage_1)
  
  cat("\nIQRroundedcalculatedgestage\n")
  quantile(x=analysisDataset$mahima_gestageatbirthwk_1_rounded, 
           probs = seq(0, 1, 0.25), 
           na.rm = TRUE)
  
  cat("\nMahima_hospenteredgestage_1\n")
  mean(analysisDataset$mahima_hospenteredgestage_1, na.rm=T)
  
  
  cat("\nIQRenteredgestage\n")
  quantile(x=analysisDataset$mahima_hospenteredgestage_1, 
           probs = seq(0, 1, 0.25), 
           na.rm = TRUE)
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1),
           c(mahima_hospenteredgestage_1_cats)])
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1),
           c(mahima_gestageatbirthwk_1_cats)])
  
 
 cat("\nentered_cats_dist\n") 

  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="[-30,0]",
           c(mahima_hospenteredgestage_1)])
  
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="(0,24]",
           c(mahima_hospenteredgestage_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="(24,32]",
           c(mahima_hospenteredgestage_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="(32,37]",
           c(mahima_hospenteredgestage_1)])
  
  
 xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="(37,41]",
           c(mahima_hospenteredgestage_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_hospenteredgestage_1_cats=="(41,44]",
           c(mahima_hospenteredgestage_1)])
  
  cat("\nTerm_entered\n")
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1) &
             mahima_gestageatbirthwk_1_cats=="[-30,0]",
           c(mahima_gestageatbirthwk_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1) &
             mahima_gestageatbirthwk_1_cats=="(0,24.7]",
           c(mahima_gestageatbirthwk_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_gestageatbirthwk_1_cats=="(24.7,32.7]",
           c(mahima_gestageatbirthwk_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1) &
             mahima_gestageatbirthwk_1_cats=="(32.7,37.7]",
           c(mahima_gestageatbirthwk_1)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1) &
             mahima_gestageatbirthwk_1_cats=="(37.7,41.7]",
           c(mahima_gestageatbirthwk_1)])
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1) &
             mahima_gestageatbirthwk_1_cats=="(41.7,44]",
           c(mahima_gestageatbirthwk_1)])
  
 # cat("\n\nDistributions_of_Cats\n")
 # cat("\nPostterm_41-42_rounded distribution\n")
  #xtabs(~d[bookyearmonth<="2017-03" & 
          #   ident_TRIAL_1==TRUE &
           #  !is.na(mahima_hospenteredgestage_1)&
           #  mahima_gestageatbirthwk_1_cats=="(40.7,42.7]",
           #c(mahima_gestageatbirthwk_1_rounded)])
  #xtabs(~d[bookyearmonth<="2017-03" & 
             #ident_TRIAL_1==TRUE &
            # !is.na(mahima_hospenteredgestage_1)&
             #mahima_gestageatbirthwk_1_cats=="(42.7,44]",
           #c(mahima_gestageatbirthwk_1_rounded)])
 
  #cat("\nCrosstabCats\n") 
  #xtabs(~d$mahima_gestageatbirthwk_1_cats)
  #xtabs(~d$mahima_hospenteredgestage_1_cats)
  
  #cat("\nCrosstabVars\n") 
  #xtabs(~d$mahima_hospenteredgestage_1)
  #xtabs(~d$mahima_gestageatbirthwk_1)
  #xtabs(~d$mahima_gestageatbirthwk_1_rounded)
  
  
#Control 
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE & 
             !is.na(mahima_gestageatbirthwk_1) &
             ident_dhis2_control==T,
           c(mahima_hospenteredgestage_1_cats)])
  #Intervention 
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE & 
             !is.na(mahima_gestageatbirthwk_1) &
             ident_dhis2_control==F,
           c(mahima_hospenteredgestage_1_cats)])
  #Both 
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_gestageatbirthwk_1),
          c(mahima_hospenteredgestage_1_cats)])
  
  xtabs(~d[bookyearmonth<="2017-03" & 
             ident_TRIAL_1==TRUE &
             !is.na(mahima_hospenteredgestage_1),
           c(mahima_gestageatbirthwk_1_cats)])
  
 
  
sink()

######US edd of last visit for woman###
sink()
sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "mahima_gA_1_us_LASTvisit and hbo_dob(us).txt"))




cat("\n")
cat("\nDiffbetweenuseddAndHospdateinWeeks\n")
mahima_gA_1_us__mean <- mean(d[bookyearmonth<="2017-03" & 
                                ident_TRIAL_1==TRUE]$mahima_gA_1_us, 
                                na.rm=TRUE)
print(mahima_gA_1_us__mean)

analysisDatasetUSgA <- d[bookyearmonth<="2017-03" & 
    ident_TRIAL_1==TRUE & 
    !is.na(mahima_hospenteredgestage_1) &
    !is.na(mahima_gestageatbirthwk_1) &
    !is.na(mahima_gA_1_us) &
    !is.na(lastuseddminusdob)&
    !is.na(mahima_gA_1_us_cats),
  c("mahima_dateofbirth_1",
    "lastusedd",
    "mahima_gA_1_us",
    "lastuseddminusdob",
    "mahima_gA_1_us_cats")]


cat("\nIQRcalculatedgestage\n")
quantile(x=analysisDatasetUSgA$mahima_gA_1_us, 
         probs = seq(0, 1, 0.25), 
         na.rm = TRUE)

cat("\n mahima_gA_1_us_mean\n")
mean(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE]$mahima_gA_1_us, na.rm=TRUE)

cat("\nTRIAL\n")
xtabs(~analysisDatasetUSgA$mahima_gA_1_us_cats)

cat("\nbelow0US\n")
xtabs(~analysisDatasetUSgA[
              mahima_gA_1_us_cats=="[-30,0]",
              c(mahima_gA_1_us)])
cat("\nABO\n")
xtabs(~analysisDatasetUSgA[
               mahima_gA_1_us_cats=="(0,24.7]",
              c(mahima_gA_1_us)])
cat("\nreallyPRE\n")
xtabs(~analysisDatasetUSgA[
  mahima_gA_1_us_cats=="(24.7,32.7]",
  c(mahima_gA_1_us)])
cat("\nPRE\n")
xtabs(~analysisDatasetUSgA[
          mahima_gA_1_us_cats=="(32.7,37.7]",
           c(mahima_gA_1_us)])
cat("\nTerm\n")
xtabs(~analysisDatasetUSgA[
  mahima_gA_1_us_cats=="(37.7,41.7]",
  c(mahima_gA_1_us)])
cat("\nPost\n")
xtabs(~analysisDatasetUSgA[
  mahima_gA_1_us_cats=="(41.7,44]",
  c(mahima_gA_1_us)])


cat("\nDenominators for mahima_gA_1_us\n")
cat("\nDenominators for TRIAL mahima_gA_1_us\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(mahima_gA_1_us) &
         !is.na(lastuseddminusdob)&
         !is.na(mahima_gA_1_us_cats),
       c("mahima_dateofbirth_1",
         "lastusedd",
         "mahima_gA_1_us",
         "lastuseddminusdob",
         "mahima_gA_1_us_cats")])

cat("\nDenominators for ARM A mahima_gA_1_us\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         ident_dhis2_control==TRUE &
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(mahima_gA_1_us) &
         !is.na(lastuseddminusdob)&
         !is.na(mahima_gA_1_us_cats),
       c("mahima_dateofbirth_1",
         "lastusedd",
         "mahima_gA_1_us",
         "lastuseddminusdob",
         "mahima_gA_1_us_cats")])



cat("\nDenominators for ARM B mahima_gA_1_us\n")
nrow(d[bookyearmonth<="2017-03" & 
        ident_TRIAL_1==TRUE & 
        ident_dhis2_control==TRUE &
        !is.na(mahima_hospenteredgestage_1) &
        !is.na(mahima_gestageatbirthwk_1) &
        !is.na(mahima_gA_1_us) &
        !is.na(lastuseddminusdob)&
        !is.na(mahima_gA_1_us_cats),
      c("mahima_dateofbirth_1",
        "lastusedd",
        "mahima_gA_1_us",
        "lastuseddminusdob",
        "mahima_gA_1_us_cats")])


cat("\nDenominators forlastuseddminusdob\n")
cat("\nDenominators for TRIAL lastuseddminusdob\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE & 
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(mahima_gA_1_us) &
         !is.na(lastuseddminusdob),
          c("mahima_dateofbirth_1",
            "lastusedd",
            "mahima_gA_1_us",
            "lastuseddminusdob")])

cat("\nDenominators for ARM A lastuseddminusdob\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE &
         ident_dhis2_control==T &
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(mahima_gA_1_us) &
         !is.na(lastuseddminusdob),
       c("mahima_dateofbirth_1",
         "lastusedd",
         "mahima_gA_1_us",
         "lastuseddminusdob")])



cat("\nDenominators for ARM B lastuseddminusdob\n")
nrow(d[bookyearmonth<="2017-03" & 
         ident_TRIAL_1==TRUE &
         ident_dhis2_control==F &
         !is.na(mahima_hospenteredgestage_1) &
         !is.na(mahima_gestageatbirthwk_1) &
         !is.na(mahima_gA_1_us) &
         !is.na(lastuseddminusdob),
       c("mahima_dateofbirth_1",
         "lastusedd",
         "mahima_gA_1_us",
         "lastuseddminusdob")])

sink()


res <- list()


f <- t.test(d[bookyearmonth<="2017-03" & 
                ident_TRIAL_1==TRUE & 
                ident_dhis2_control==T &
                !is.na(mahima_hospenteredgestage_1) &
                !is.na(mahima_gestageatbirthwk_1) &
                !is.na(mahima_gA_1_us) &
                !is.na(lastuseddminusdob)]$mahima_gA_1_us,
                mu=40, paired=FALSE)
res[[length(res)+1]] <- data.frame("label"="t.test mahima_gA_1_us",
                                   "pvalue"=f$p.value)

f <- t.test(d[bookyearmonth<="2017-03" & 
                ident_TRIAL_1==TRUE & 
                ident_dhis2_control==F &
                !is.na(mahima_hospenteredgestage_1) &
                !is.na(mahima_gestageatbirthwk_1) &
                !is.na(mahima_gA_1_us) &
                !is.na(lastuseddminusdob)]$mahima_gA_1_us,
            mu=40, paired=FALSE)
res[[length(res)+1]] <- data.frame("label"="t.test intervention mahima_gA_1_us",
                                   "pvalue"=f$p.value)


f <- t.test(d[bookyearmonth<="2017-03" & 
                ident_TRIAL_1==TRUE & 
                !is.na(mahima_hospenteredgestage_1) &
                !is.na(mahima_gestageatbirthwk_1) &
                !is.na(mahima_gA_1_us) &
                !is.na(lastuseddminusdob)]$mahima_gA_1_us,
            mu=40, paired=FALSE)
res[[length(res)+1]] <- data.frame("label"="t.test TRIAL mahima_gA_1_us",
                                   "pvalue"=f$p.value)




res <- rbindlist(res)

openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                   "mahima",
                                   "trial_1",
                                   "us_gest_ages_statisticaltests.xlsx"))



  
  ###Kappa test for reliability not rounded
  ###Using intraclass correlation here because these are continuous variables
  str(d$mahima_hospenteredgestage_1)
  str(d$mahima_gestageatbirthwk_1)
  #want kappa for continuous variables, normal kappa wont work
  # this one needs long format so want wide to long
  #creating a smaller dataset just for this purpose
  analysisDataset <- d[bookyearmonth<="2017-03"&
                         ident_TRIAL_1==TRUE &
                         !is.na(mahima_gestageatbirthwk_1) &
                         !is.na(mahima_hospenteredgestage_1),
                       c("bookevent",
                         "mahima_hospenteredgestage_1",
                         "mahima_gestageatbirthwk_1")]
  
  long <-melt.data.table(analysisDataset,
                         id.vars = "bookevent")
  
  ###if any ones have an empty value, should check them this way
# d[bookevent=="----", c("motheridno", 
#                        "bookyearmonth", 
#                        "mahima_hospenteredgestage_1",
#                       "mahima_hospenteredgestage_1")]
  
  f <- ICC::ICCest("bookevent", value, data = long, 
                   alpha = 0.05, 
                   CI.type = c("THD", "Smith"))
  
  res<-list()
  
  res[[length(res)+1]] <- data.frame("label"="intraclass correlation test btwn gestage entered and calculated",
                                     "denominator"=f$N,
                                     "correlationcoefficient"=f$ICC,
                                     "lowerCI"=f$LowerCI,
                                     "upperCI"=f$UpperCI,
                                     "kappa"=f$k,
                                     "w/inGroupOrIndivVar"=f$varw,
                                     "amongIndivOrGroup"=f$vara)
  
  ###covariance--this is bounded unlike variance and covariance not rounded
  ###in the analysis dataset we have bookevent
  ###so choose only the variables you want to compare or else it will break it
  ###this one will only give you a pearsons value because its "pairwise.complete.obs"
  f <- cor(x=analysisDataset$mahima_hospenteredgestage_1, 
           analysisDataset$mahima_gestageatbirthwk_1, 
           use = "pairwise.complete.obs",
           method = c("pearson"))
  
  res[[length(res)+1]] <- data.frame("label"=" pearson correlation coeff btwn gestage entered and calculated",
                                     "correlationcoefficient"=f)
  
  ###here we want spearman coefficient
  f <- cor(x=analysisDataset$mahima_hospenteredgestage_1, 
           analysisDataset$mahima_gestageatbirthwk_1, 
           use = "pairwise.complete.obs",
           method = c("spearman"))
  res[[length(res)+1]] <- data.frame("label"="spearman correlation coeff btwn gestage entered and calculated",
                                     "correlationcoefficient"=f)
  res <- rbindlist(res, fill=T)
  
  openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                     "mahima",
                                     "trial_1",
                                     "entered_and_calculated_gest_ages_ICC.xlsx"))
  
  
  ###Kappa test for reliability ROUNDED
  ###Using intraclass correlation here because these are continuous variables
  str(d$mahima_hospenteredgestage_1)
  str(d$mahima_gestageatbirthwk_1_rounded)
  #want kappa for continuous variables, normal kappa wont work
  # this one needs long format so want wide to long
  #creating a smaller dataset just for this purpose
  analysisDataset <- d[bookyearmonth<="2017-03"&
                         ident_TRIAL_1==TRUE &
                         !is.na(mahima_gestageatbirthwk_1_rounded) &
                         !is.na(mahima_hospenteredgestage_1),
                       c("bookevent",
                         "mahima_hospenteredgestage_1",
                         "mahima_gestageatbirthwk_1_rounded")]
  
  long <-melt.data.table(analysisDataset,
                         id.vars = "bookevent")
  
  ###if any ones have an empty value, should check them this way
#d[bookevent=="----", c("motheridno", 
#                       "bookyearmonth", 
#                       "mahima_hospenteredgestage_1",
#                       "mahima_gestageatbirthwk_1_rounded")]
  
  f <- ICC::ICCest("bookevent", value, data = long, 
                   alpha = 0.05, 
                   CI.type = c("THD", "Smith"))
  
  res<-list()
  
  res[[length(res)+1]] <- data.frame("label"="intraclass correlation test btwn gestage entered and rounded calculated",
                                     "denominator"=f$N,
                                     "correlationcoefficient"=f$ICC,
                                     "lowerCI"=f$LowerCI,
                                     "upperCI"=f$UpperCI,
                                     "kappa"=f$k,
                                     "w/inGroupOrIndivVar"=f$varw,
                                     "amongIndivOrGroup"=f$vara)
  
  ###covariance--this is bounded unlike variance and covariance ROUNDED
  ###in the analysis dataset we have bookevent
  ###so choose only the variables you want to compare or else it will break it
  ###this one will only give you a pearsons value because its "pairwise.complete.obs"
  f <- cor(x=analysisDataset$mahima_hospenteredgestage_1, 
           analysisDataset$mahima_gestageatbirthwk_1_rounded, 
           use = "pairwise.complete.obs",
           method = c("pearson"))
  
  res[[length(res)+1]] <- data.frame("label"=" pearson correlation coeff btwn gestage entered and rounded calculated",
                                     "correlationcoefficient"=f)
  
  ###here we want spearman coefficient
  f <- cor(x=analysisDataset$mahima_hospenteredgestage_1, 
           analysisDataset$mahima_gestageatbirthwk_1_rounded, 
           use = "pairwise.complete.obs",
           method = c("spearman"))
  res[[length(res)+1]] <- data.frame("label"="spearman correlation coeff btwn gestage entered and rounded calculated",
                                     "correlationcoefficient"=f)
  res <- rbindlist(res, fill=T)
  
  openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                     "mahima",
                                     "trial_1",
                                     "entered_and_rounded_calculated_gest_ages_ICC.xlsx"))
  
  
}


Analyse_BookingRawVsClean <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # directly pull in the raw data
  FOLDERS <- DHIS2_Folders(isControl = T)
  rawC <- fread(
    sprintf(
      "%s/%s ANC Green File.csv",
      FOLDERS$FOLDER_DATA,
      FOLDERS$CLINICAL_OR_CONTROL
    ),
    encoding = "UTF-8"
  )
  setnames(rawC, 2, "uniqueid")
  setnames(rawC, "Event date","bookdate")
  setnames(rawC, "Event", "bookevent")
  rawC[,ident_dhis2_control:=TRUE]
  
  FOLDERS <- DHIS2_Folders(isControl = F)
  rawI <- fread(
    sprintf(
      "%s/%s Booking Visit.csv",
      FOLDERS$FOLDER_DATA,
      FOLDERS$CLINICAL_OR_CONTROL
    ),
    encoding = "UTF-8"
  )
  setnames(rawI, 2, "uniqueid")
  setnames(rawI, "Event date","bookdate")
  setnames(rawI, "Event", "bookevent")
  rawI[,ident_dhis2_control:=TRUE]
  
  # now we have a "minimum viable dataset" for the raw data
  raw <- rbind(rawC[,c("uniqueid","bookevent","bookdate","ident_dhis2_control")],
               rawI[,c("uniqueid","bookevent","bookdate","ident_dhis2_control")])
  # now just do a little bit of cleaning so that it can be graphed
  raw[,bookyearmonth:=YearMonth(bookdate)]
  raw[,bookdate:=NULL]
  
  raw[,type:="Raw"]
  d[,type:="Clean"]
  
  # create a joint dataset, only using the variable names that
  # exist in "raw"
  plotData <- rbind(
    raw,
    d[,names(raw),with=F]
  )
  

  
  # to make graphs
  
  toPlot <- plotData[,.(numBookings=.N),
                      by=.(bookyearmonth,type)]
  # specify that "raw" should be first, not "clean"
  toPlot[,type:=factor(type,levels=c("Raw","Clean"))]
  
  p <- ggplot(data=toPlot[bookyearmonth>="2017-01"],
              mapping=aes(x=bookyearmonth,y=numBookings,fill=type))
  p <- p + geom_bar(stat="identity",position="dodge",width=0.7,colour="white")
  p <- p + scale_x_discrete("Booking Month")
  p <- p + scale_y_continuous("Number of bookings")
  p <- p + scale_fill_brewer("Data",palette="Dark2")
  p <- p + labs(title="Comparing number of bookings in raw versus clean data")
  p <- p + labs(caption=GraphCaption())
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "compare_raw_to_clean_bookings.png"),
         plot=p,
         height=210,
         width=297,
         units="mm")
}


IndicatorsOsloDemographics <- function(d){
  
  
  toAnalyse <- copy(d[ident_TRIAL_1==TRUE])
  toAnalyse[,bookorgdistrict:="Palestine"]
  toAnalyse[,bookorgdistricthashed:="Palestine"]
  toAnalyse <- rbind(toAnalyse,d[ident_TRIAL_1==TRUE])
  
  # we will just switch this one over
  toAnalyse[,aggregationVariable:=bookorgdistricthashed]
  # this is how to run a t-test
  # t.test(OUTCOME ~ GROUPINGVARIABLE, data=DATA)
  # res <- t.test(education~ident_dhis2_control,data=toAnalyse)
  
  ### YEARS OF EDUCATION
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(education~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable
  ]
  pvalueInfo[,pvalue:=sprintf("p=%s",formatC(pvalue,digits=2,format="f"))]
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable))
  p <- p + geom_boxplot(mapping=aes(y=education,fill=ident_dhis2_control))
  p <- p + geom_text(data=pvalueInfo,mapping=aes(y=0,label=pvalue))
  p <- p + scale_fill_brewer("Trial Arm",palette="Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Education differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "education_years.png"),
    plot=p)
  
  ### AGE
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(age~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=age))
  p <- p+ geom_boxplot(mapping = aes(y=age, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "age_years.png"),
    plot=p)
  
  
  ### AGE at marrige
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agemarriage~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agemarriage))
  p <- p+ geom_boxplot(mapping = aes(y=agemarriage, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at marriage differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "agemarriage_years.png"),
    plot=p)
  
  
  ### AGE at first pregnancy
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agepregnancy~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agepregnancy))
  p <- p+ geom_boxplot(mapping = aes(y=agepregnancy, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at first pregnancy differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "agepregnancy_years.png"),
    plot=p)
  
  #income
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(income~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=income))
  p <- p+ geom_boxplot(mapping = aes(y=income, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=-7000,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p+scale_y_continuous("Income")
  p <- p + labs(title="Income differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "income_continuous.png"),
    plot=p)
  
}

Analyse_Medical_History_Trial_1 <- function(d){
  xtabs(~d$bookhistgdm,addNA=T)
  
  xtabs(~d$bookhistdm,addNA = T)
  
  xtabs(~d$bookhisthtn,addNA=T)
  
  xtabs(~d$bookhistperi, addNA=T)
  
  xtabs(~d$bookhistcs, addNA=T)
  
  sum(d$ident_TRIAL_1==1,na.rm=T)
  d[,rowid:=1:.N]
  #tamara keep on adding new variables below
  temp <- d[ident_TRIAL_1==TRUE,c(varsKeep==T)]
  
  
  long <- melt.data.table(
    temp,
    id.vars = c("rowid","ident_dhis2_control"))
  
  tab <- long[,.(
    has_var=sum(value, na.rm=T),
    notmissing_var=sum(!is.na(value)),
    missing_var=sum(is.na(value))
  ),by=.(variable,ident_dhis2_control)]
  tab
  
  # this is the harder way
  tab <- d[ident_TRIAL_1==TRUE,.(
    has_bookhistgdm=sum(bookhistgdm, na.rm=T),
    notmissing_bookhistgdm=sum(!is.na(bookhistgdm)),
    missing_bookhistgdm=sum(is.na(bookhistgdm)),
    
    has_bookhistdm=sum(bookhistdm, na.rm=T),
    notmissing_bookhistdm=sum(!is.na(bookhistdm)),
    missing_bookhistdm=sum(is.na(bookhistdm)),
    
    
    has_bookhisthtn=sum(bookhisthtn, na.rm = T),
    notmissing_bookhisthtn=sum(!is.na(bookhisthtn)),
    missing_bookhisthtn=sum(is.na(bookhisthtn)),
    
    has_bookhistperi=sum(bookhistperi, na.rm=T),
    notmissing_bookhistperi=sum(!is.na(bookhistperi)),
    missing_bookhistperi=sum(is.na(bookhistperi)),
    
    has_bookhistcs=sum(bookhistcs, na.rm=T),
    notmissing_bookhistcs=sum(!is.na(bookhistcs)),
    missing_bookhistcs=sum(is.na(bookhistcs))
    
    
  ),
  by=.(ident_dhis2_control)]
  
  tab
  
  
}

Analyse_Mahima_Trial_1 <- function(d){
  Analyse_BookingRawVsClean(d[ident_dhis2_booking==1])
  Analyse_EnteredVsCalculated(d)
  Analyse_BookingDescriptives(d[ident_dhis2_booking==1])
  IndicatorsOsloDemographics(d)
  #Analyse_Medical_History_Trial_1(d)
  warning("Analyse_Medical_History_Trial_1(d)")
  
}

