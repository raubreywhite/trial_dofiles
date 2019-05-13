sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "gA_paper_cats.txt"))

analysisDatasetUSgA <- d[bookyearmonth>="2017-01" &
                           bookyearmonth<="2017-12" &
                           ident_avic_abb==T &
                           !is.na(mahima_hospenteredgestage_1) &
                           !is.na(mahima_gestageatbirthwk_1) &
                           !is.na(first_1_21_usedd_gA)&
                           !is.na(first_1_21_usedd_gA_cats),
                         c("mahima_dateofbirth_1",
                           "first_1_21_usedd_gA",
                           "first_1_21_usedd_gA_cats",
                           "mahima_hospenteredgestage_1",
                           "mahima_gestageatbirthwk_1")]

# we pull ou thte 3 variables that we care about
long <- analysisDatasetUSgA[,c(
  "first_1_21_usedd_gA",
  "mahima_hospenteredgestage_1",
  "mahima_gestageatbirthwk_1"
)]
# i create my own id variable (per row number)
long[,id:=1:.N]
# reshape to long
long <- melt.data.table(long, id.vars="id")

# mixed effects linear regression with random effect for person
# this takes into account the 'paired nature' of the data
# (i.e. multiple observations per woman/pregnancy)
summary(lme4::lmer(value ~ variable + (1|id), data=long))
summary(lme4::lmer(value ~ variable + (1|id), data=long[value >= 0 & value <= 45])) # removing outliers

# this is a normal linear regression
# (but not appropriate due to the paired nature)
# outcome ~ exposure
summary(lm(value ~ variable, data=long))
# remember to look at the overall pvalue 
# for each of the variables first!
# lm generally just reports pair-wise comparisons
# which are a second step in the analyses
anova(lm(value ~ variable, data=long))

### 


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
nrow(d[bookyearmonth>="2017-01" & 
         bookyearmonth<="2017-12" &
         ident_avic_abb==T & 
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
analysisDatasetUSgA <- d[bookyearmonth>="2017-01" & 
                           bookyearmonth<="2017-12" &
                           ident_avic_abb==T & 
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
analysisDatasetUSgA <- d[[bookyearmonth>="2017-01" & 
                            bookyearmonth<="2017-12" &
                            ident_avic_abb==T,
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
              caption = "Outliers Removed") +
  xlab("Weeks") +
  ylab("Density")
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Dark2")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=24))

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
              caption= "Outliers Not Removed") +
  xlab("Weeks")
p <- p + scale_y_continuous("Frequency", labels=scales::percent)
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Set1")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=24))

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

####Bar Graph with restriction###
uglytable[,percentage:=N/sum(N),by=.(variable)]
p <- ggplot(uglytable, aes(x= category, y=percentage, fill=variable))
p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
p <- p + scale_x_discrete("Weeks")
p <- p + scale_y_continuous("Frequency", labels=scales::percent)
p <- p + labs(title="Distribtion of Gestational Age by Category",
              caption="Outliers not removed")
p <- p + geom_text(aes(label = round(100*percentage)), 
                   size=6.5,
                   vjust = -0.5,
                   position=position_dodge(width=1))
p <- p + theme(text = element_text(size=44))
p <- p + theme_gray(22)
p 

#ggsave with the restrictions helps save it in higher resolution
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Bar_graph_Not_Adjusted_with_labels.png"
), plot = p, width = 297, height = 210, unit = "mm")


####Bar Graph with restriction###
###Removing outliers...other values that arent possible
uglytable[,percentage:=N/sum(N),by=.(variable)]
p <- ggplot(uglytable[!category %in% c("<=0",">44")], aes(x= category, y=percentage, fill=variable))
p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
p <- p + scale_x_discrete("Weeks")
p <- p + scale_y_continuous("Frequency", labels=scales::percent)
p <- p + labs(title="Distribtion of Gestational Age by Category",
              caption="Outliers removed")
p <- p + geom_text(aes(label = round(100*percentage)),
                   size=6.5,
                   vjust = -0.5,
                   position=position_dodge(width=1))
p <- p + theme(text = element_text(size=44))
p <- p + theme_gray(22)
p 

#ggsave with the restrictions helps save it in higher resolution
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "mbo_r",
  "GA_Abstract_Bar_graph_Adjusted_with_labels.png"
), plot = p, width = 297, height = 210, unit = "mm")


