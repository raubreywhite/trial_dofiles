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

#########GRAPHS GA##########################################
#making plots for gestage distributions
analysisDatasetUSgA <- d[bookyearmonth<="2017-03" & 
                           ident_TRIAL_1==TRUE & 
                           !is.na(mahima_hospenteredgestage_1) &
                           !is.na(mahima_gestageatbirthwk_1) &
                           !is.na(first_1_21_usedd_gA)&
                           !is.na(first_1_21_usedd_gA_cats),
                         c("mahima_dateofbirth_1",
                           "mahima_hospenteredgestage_1",
                           "mahima_gestageatbirthwk_1",
                           "first_1_21_usedd_gA",
                           "first_1_21_usedd_gA_cats")]

#making plots for gestage distributions
analysisDatasetUSgA <- d[bookyearmonth<="2017-03" & 
                           ident_TRIAL_1==TRUE,
                         c("mahima_hospenteredgestage_1",
                           "mahima_gestageatbirthwk_1",
                           "first_1_21_usedd_gA"
                           )]

#dont need an id.vars here because not retaining information for each woman
long <- melt.data.table(analysisDatasetUSgA)
head(long)

#ran this and got 3 levels, so must do levels
long$variable

#see the levels, which one comes first so we can rename them accordingly
levels(long$variable)
levels(long$variable) <-c("Entered by HCP",
                          "Calculated by LMP",
                          "Calculated from Ultrasound")



long <- long[!is.na(value)]
long[,category:=cut(value,
                    breaks=c(-300,0,24.7,32.7,37.7,41.7,44,9999999999),
                    include.lowest=T)]

xtabs(~long$category, addNA = T)
levels(long$category) <- c("<=0",
                           "1-24",
                           "25-32",
                           "33-37",
                           "38-41",
                           "42-44",
                           ">44")

levels(long$category)

#if restrict more rows, cant produce both continous and categorical
p <- ggplot(long[value<50 & value>0], aes(x=value, fill=variable)) 
p <- p + geom_density(alpha=0.3)
p <- p + labs(title="Distribution of Gestational Age by Source",
              subtitle = "Adjusted") +
              xlab("Weeks") +
              ylab("Density")
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Dark2")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=40))
              
p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Distribution_Adjusted.png"
), plot = p, width = 297, height = 210, unit = "mm")



#####Non-Adjusted Distribution#####
#if restrict more rows, cant produce both continous and categorical
p <- ggplot(long[value<300], aes(x=value, fill=variable)) 
p <- p + geom_density(alpha=0.3)
p <- p + labs(title="Distribution of Gestational Age by Source",
              subtitle= "Non-Adjusted") +
          xlab("Weeks")
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Set1")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=40))


p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Distribution_NON_Adjusted.png"
), plot = p, width = 297, height = 210, unit = "mm")


###Adjusting the table so we can aggregate and get percentages
uglytable <- long[,.(
                    N=.N),
                  keyby=.(
                    variable,
                    category
                  
              )]


#####FIX THIS######
#creating denominator
#run bottom code when tells you reached elapsed time limit
#dev.off(
#ggsave with the restrictions helps save it in higher resolution
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Bar_graph_Nonadjusted.png"
), plot = p, width = 297, height = 210, unit = "mm")

####Bar Graph with restriction###
###Removing outliers...other values that arent possible

p <- ggplot(uglytable[!category %in% c("<=0",">44")], aes(x= category, y=percentage, fill=variable))
p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
p <- p + scale_x_discrete("Weeks")
p <- p + scale_y_continuous("Frequency")
p <- p + labs(title="Distribtion of Gestational Age by Category",
              subtitle="Adjusted")
#p <- p + geom_text(aes(label = percentage), vjust = -0.5)
p
#makes everything really big
p <- p + theme_gray(22)
p 

#ggsave with the restrictions helps save it in higher resolution
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Bar_graph_Adjusted_with_labels.png"
), plot = p, width = 297, height = 210, unit = "mm")


