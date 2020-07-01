# Allow the code to edit files on your computer
org::AllowFileManipulationFromInitialiseProject()
# Specifies where all the required folders are
# HOME=where the code is
# SHARED=results
# DATA=datas
org::InitialiseProject(
  HOME="C:/data processing/projects_that_will_be_published_on_github/git/gestational_age",
  SHARED = "C:/data processing/projects_that_will_be_published_on_github/results/gestational_age",
  DATA = "C:/data processing/data_clean/"
)

# say which packages we want, and install if neccessary
# (note, this should really only happen once)
desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64",
                     "haven",
                     "lubridate",
                     "ggplot2",
                     "irr",
                     "rel",                      
                     "gridExtra",
                     "openssl",
                     "fmsb",
                     "ICC",
                     "arabicStemR",
                     "lme4",
                     "fs",
                     "ggplot2",
                     "ggplot",
                     "e1071"
                     
)
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

# MERETT/TAMARA CHECK OUT IF THIS IS ACTUALLY CORRECT
#FOLDER_DROPBOX <- file.path("~","..","eRegistry CRCT Dropbox")
# MERVETT/TAMARA DELETE THIS ONCE DROPBOX IS INSTALLED
FOLDER_DROPBOX <<- "C:/data processing/FAKE_DROPBOX/eRegistry CRCT Dropbox"

library(data.table)

d <- readRDS(file.path(org::PROJ$DATA,"full_data_from_r.rds"))

#sink(file.path(org::PROJ$SHARED_TODAY,
#               "gA_paper_cats.txt"))



########## Data set definition ################

#gA <- d[bookyear<=2019 & 
#         (ident_avic_abb==T |
#        matching=="Governmental"|
#       matching=="PaperHBO"|
#        matching=="Private") &
#        comboUSandLMPgA>=20 &
#        comboUSandLMPgA<=44 &
#        mahima_hospenteredgestage_1>=20 &
#        mahima_hospenteredgestage_1<=44,]

gA <- d[((bookyear==2015) | (ident_TRIAL_1==T & bookdate >= "2017-01-15"&
                               bookdate<="2017-09-15")) &
          (matching=="Avicenna"|
             matching=="Governmental"|
             matching=="PaperHBO"|
             matching=="Private"),]


################ END  ################

############################# ABSTRACT ############################# 
gA[!is.na(mahima_gestageatbirthwk_1) & 
     is.na(mahima_gestageatbirthwk_1_cats), 
   c("mahima_gestageatbirthwk_1",
     "mahima_gestageatbirthwk_1_cats")]

sum(!is.na(gA$mahima_gestageatbirthwk_1))
sum(!is.na(gA$mahima_gestageatbirthwk_1_cats))

sum(!is.na(gA$mahima_gestageatbirthwk_1) & !is.na(gA$mahima_gestageatbirthwk_1_cats))
sum(!is.na(gA$mahima_gestageatbirthwk_1) & is.na(gA$mahima_gestageatbirthwk_1_cats))
sum(is.na(gA$mahima_gestageatbirthwk_1) & !is.na(gA$mahima_gestageatbirthwk_1_cats))


analysisDatasetUSgA <- gA[ident_TRIAL_1==T &
                            !is.na(mahima_hospenteredgestage_1) &
                            mahima_hospenteredgestage_1>0,
                          c("mahima_dateofbirth_1",
                            "first_1_21_usedd_gA",
                            "first_1_21_usedd_gA_cats",
                            "first_1_21_usedd_gA_cats_wide",
                            "mahima_hospenteredgestage_1",
                            "mahima_hospenteredgestage_1_cats",
                            "mahima_hospenteredgestage_1_cats_wide",
                            "mahima_gestageatbirthwk_1",
                            "mahima_gestageatbirthwk_1_cats",
                            "mahima_gestageatbirthwk_1_cats_wide",
                            "merged_is_hosp_gov")]

sum(!is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1) & 
      !is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats))
sum(!is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1) & 
      is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats))
sum(is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1) & 
      !is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats))


nrow(analysisDatasetUSgA)
sum(!is.na(analysisDatasetUSgA$first_1_21_usedd_gA))
sum(!is.na(analysisDatasetUSgA$first_1_21_usedd_gA_cats))
sum(!is.na(analysisDatasetUSgA$first_1_21_usedd_gA_cats_wide))

sum(!is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1))
sum(!is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats))
sum(!is.na(analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats_wide))

sum(!is.na(analysisDatasetUSgA$mahima_hospenteredgestage_1))
sum(!is.na(analysisDatasetUSgA$mahima_hospenteredgestage_1_cats))
sum(!is.na(analysisDatasetUSgA$mahima_hospenteredgestage_1_cats_wide))

#qqplots for normality
#huge outlier at 4,000, so we should get rid of it
qqnorm(analysisDatasetUSgA$difference_us); qqline(analysisDatasetUSgA$difference_us, col = 2)


unique(analysisDatasetUSgA$first_1_21_usedd_gA)
#1. qqplots, crazy distributions so do the nonparametric analysis
#not normal distribution, so will have to do nonparametric testing
qqnorm(analysisDatasetUSgA[abs(first_1_21_usedd_gA)<100]$difference_us)
qqline(analysisDatasetUSgA[abs(first_1_21_usedd_gA)<100]$difference_us, col = 2)

#2.nonparametic
#true location is not equal to zero
#since not significant
wilcox.test(analysisDatasetUSgA$difference_us)

#since they arent significant, we can present summary statistics of the data

# creating analysis variables
analysisDatasetUSgA[,difference_us := mahima_hospenteredgestage_1 - first_1_21_usedd_gA]

# paired t.test
t.test(analysisDatasetUSgA$difference_us)

#equivalence test
#two one sided test, how much do we really care about for them to be the same?
#ex: test 1 is the difference than -10?
#dont have equivalence with a region of equivalence of +2 and -2
#(in our data set because 90% cI is -8.9 to 0.03)
t.test(analysisDatasetUSgA$difference_us, conf.level = 0.9)

#paired t.test excluding outliers
unique(analysisDatasetUSgA$mahima_hospenteredgestage_1)

# linear regression
fit <- lm(difference_us ~ merged_is_hosp_gov, data=analysisDatasetUSgA)
# (Intercept) is when hospital is private. It says "US estimate is 11 days earlier than entered"
# merged_is_hosp_govTRUE is the difference between private and government estimates. it is NOT signifant, because the p-value is 0.13!!
# this means we CANNOT interpret private results and government results separately
summary(fit)


####Table 3: CHIsqtest by ultrasound and then LMP
#used wider categories to decrease lower numbers in categories
#tried the chi.sq test which is an estimation but it might be incorrect
#so we constrained the limits further by merging categories with small numbers
#so tried fishers exact test and bc sample size is too large so it didnt work

xtabs(~first_1_21_usedd_gA_cats+mahima_hospenteredgestage_1_cats,data=analysisDatasetUSgA)
chisq.test(xtabs(~first_1_21_usedd_gA_cats+mahima_hospenteredgestage_1_cats,data=analysisDatasetUSgA))


xtabs(~first_1_21_usedd_gA_cats_wide+mahima_hospenteredgestage_1_cats_wide,data=analysisDatasetUSgA)
chisq.test(xtabs(~first_1_21_usedd_gA_cats_wide+mahima_hospenteredgestage_1_cats_wide,data=analysisDatasetUSgA))


xtabs(~analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats+analysisDatasetUSgA$mahima_hospenteredgestage_1_cats)
chisq.test(xtabs(~analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats+analysisDatasetUSgA$mahima_hospenteredgestage_1_cats))

xtabs(~analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats_wide+analysisDatasetUSgA$mahima_hospenteredgestage_1_cats_wide)
chisq.test(xtabs(~analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats_wide+analysisDatasetUSgA$mahima_hospenteredgestage_1_cats_wide))




# we pull ou thte 3 variables that we care about
#should we add is hosp gov variable here too?
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

###also compare difference between governmental and private
#matching=="Governmental" or matching=="Private"
#merged_is_hosp_gov, true or false



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
xtabs(~analysisDatasetUSgA$first_1_21_usedd_gA_cats+analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats)

xtabs(~analysisDatasetUSgA$first_1_21_usedd_gA_cats_wide)
xtabs(~analysisDatasetUSgA$mahima_gestageatbirthwk_1_cats_wide)


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

################################# GRAPHS GA #################################

#making plots for gestage distributions
analysisDatasetUSgA <- d[bookdate>="2017-01-15" &
                           bookyearmonth<="2017-09-15" &
                           ident_TRIAL_1==T &
                           ident_avic_abb==T &
                           !is.na(merged_is_hosp_gov) &
                           !is.na(mahima_hospenteredgestage_1) &
                           !is.na(mahima_gestageatbirthwk_1) &
                           !is.na(first_1_21_usedd_gA)&
                           !is.na(first_1_21_usedd_gA_cats),
                         c("mahima_hospenteredgestage_1",
                           "mahima_gestageatbirthwk_1",
                           "first_1_21_usedd_gA")]



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
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Distribution_Adjusted.png"
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
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Distribution_NON_Adjusted.png"
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
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Bar_graph_Not_Adjusted_with_labels.png"
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
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Bar_graph_Adjusted_with_labels.png"
), plot = p, width = 297, height = 210, unit = "mm")


########## completeness reports ########## 

gA[,termcats:=cut(comboUSandLMPgA,
                  breaks=c(-100,0,20,32.9,36.9,42.9,44,50,60,200,5000),
                  include.lowest=T)]
xtabs(~gA$termcats, addNA=T)

gAcomp <- gA[,.(N=.N,
                LMP=sum(!is.na(booklmp)),
                US=sum(!is.na(first_1_21_usedd)),
                LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                USgA=sum(!is.na(first_1_21_usedd_gA)),
                COMBOgA=sum(!is.na(comboUSandLMPgA)),
                HOSPgA=sum(!is.na(mahima_hospenteredgestage_1)),
                LMPandEntered=sum(!is.na(booklmp) & 
                                    !is.na(mahima_hospenteredgestage_1)),
                USandEntered=sum(!is.na(first_1_21_usedd_gA) & 
                                   !is.na(mahima_hospenteredgestage_1)),
                notMissingLMP=sum(!is.na(booklmp)),
                notMissingUS=sum(!is.na(first_1_21_usedd)),
                notMissingEDD=sum(!is.na(first_1_21_usedd)),
                notMissingDateofDeliv=sum(!is.na(merged_datedeliv)),
                notMissingcomboGA=sum(!is.na(comboUSandLMPgA)),
                MissingLMP=sum(is.na(booklmp)),
                MissingUS=sum(is.na(first_1_21_usedd)),
                MissingEDD=sum(is.na(first_1_21_usedd)),
                MissingDateofDeliv=sum(is.na(merged_datedeliv)),
                MissingcomboGA=sum(is.na(comboUSandLMPgA)),
                MissingGAhosp=sum(is.na(mahima_hospenteredgestage_1))),
             keyby=.(termcats)]

openxlsx::write.xlsx(gAcomp,file.path(
  org::PROJ$SHARED_TODAY,
  sprintf("Completeness_%s.xlsx",
          lubridate::today())))

library("e1071")
# statistical  info
gAstats <- gA[(comboUSandLMPgA<=44 & 
                 comboUSandLMPgA>=20) & 
                (mahima_gestageatbirthwk_1<=44 & mahima_gestageatbirthwk_1>=20) &
                (first_1_21_usedd_gA<=44 & first_1_21_usedd_gA>=20) &
                (mahima_hospenteredgestage_1>=20 & mahima_hospenteredgestage_1<=44),
              .(N=.N,
                MeanLMPgA=mean(mahima_gestageatbirthwk_1, na.rm=T),
                MedianLMPgA=median(mahima_gestageatbirthwk_1, na.rm=T),
                SDLMPgA=sd(mahima_gestageatbirthwk_1, na.rm=T),
                KurtLMPgA=kurtosis(mahima_gestageatbirthwk_1,na.rm=T),
                SkewLMPgA=skewness(mahima_gestageatbirthwk_1, na.rm=T),
                MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                MeanCombogA=mean(comboUSandLMPgA, na.rm=T),
                MedianCombogA=median(comboUSandLMPgA, na.rm=T),
                SDCombogA=sd(comboUSandLMPgA, na.rm=T),
                KurtCombogA=kurtosis(comboUSandLMPgA,na.rm=T),
                SkewCombogA=skewness(comboUSandLMPgA, na.rm=T),
                MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                MedianHospgA=median(mahima_hospenteredgestage_1, na.rm=T),
                SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                KurtHospgA=kurtosis(mahima_hospenteredgestage_1,na.rm=T),
                SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T))]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_20_44_ALL_%s.xlsx",lubridate::today())))



# term categorical data based on variable
gAtermcounts <- gA[comboUSandLMPgA<=44 & 
                     comboUSandLMPgA>=20,.(
                       N=.N,
                       LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                       UsGA=sum(!is.na(first_1_21_usedd_gA)),
                       CombogA=sum(!is.na(comboUSandLMPgA)),
                       HospentgA=sum(!is.na(mahima_hospenteredgestage_1))),
                   keyby=.(termcats)]

openxlsx::write.xlsx(gAtermcounts,file.path(org::PROJ$SHARED_TODAY,
                                            sprintf("gA_TermCounts_20_44_%s.xlsx",
                                                    lubridate::today())))

# birth type
gAtype <- gA[comboUSandLMPgA<=44 & 
               comboUSandLMPgA>=20,.(
                 N=.N,
                 Missing=sum(is.na(merged_pregoutcome)| merged_pregoutcome==""),
                 Alive=sum(merged_pregoutcome=="Live Birth" |
                             merged_pregoutcome=="Alive" |
                             merged_pregoutcome=="ALIVE" |
                             merged_pregoutcome=="LIVE" |
                             merged_pregoutcome=="Live",na.rm=T),
                 Stillbirth=sum(merged_pregoutcome=="Stillbirth"|
                                  merged_pregoutcome=="STILL" |
                                  merged_pregoutcome=="Still Birth" |
                                  merged_pregoutcome=="StillBirth", na.rm=T),
                 IUFD=sum(merged_pregoutcome=="I.U.F.D" |
                            merged_pregoutcome=="IUFD", na.rm=T),
                 Other=sum(merged_pregoutcome=="DEAD"|
                             merged_pregoutcome=="Female"|
                             merged_pregoutcome=="NEO_DEATH"|
                             merged_pregoutcome=="Early Neonatal Death"|
                             merged_pregoutcome=="Dead baby"|
                             merged_pregoutcome=="Dead"|
                             merged_pregoutcome=="Died"|
                             merged_pregoutcome=="Male", na.rm=T)),
             keyby=.(termcats)]

openxlsx::write.xlsx(gAtype,file.path(org::PROJ$SHARED_TODAY,
                                      sprintf("birthoutcome_20_44_%s.xlsx",
                                              lubridate::today())))

# birth type CPO 
CPO <- gA[comboUSandLMPgA<=44 & 
            comboUSandLMPgA>=20,.(
              N=.N,
              Alive=sum(cpopregoutcome_1=="LIVE", na.rm=T),
              Stillbirth=sum(cpopregoutcome_1=="STILL", na.rm=T),
              ABO=sum(cpopregoutcome_1=="ABO", na.rm=T),
              Death=sum(cpopregoutcome_1=="NEO_DEATH"|
                          cpopregoutcome_1=="LATE_DEATH"|
                          cpopregoutcome_1=="INF_DEATH",na.rm=T),
              Missing=sum(is.na(cpopregoutcome_1))),
          keyby=.(termcats)]

openxlsx::write.xlsx(CPO,file.path(org::PROJ$SHARED_TODAY,
                                   sprintf("cpo_20_44_%s.xlsx",
                                           lubridate::today())))




# concordance
gA[,comboUSandLMPgA_rounded:= floor(comboUSandLMPgA)]

gA[,gAdif:=(comboUSandLMPgA-mahima_hospenteredgestage_1)]
xtabs(~gA$gAdif, addNA=T)

gAconcordance <- gA[(comboUSandLMPgA<=44 & 
                       comboUSandLMPgA>=20) &
                      !is.na(gAdif),.(N=.N,
                                      noweekdiff=sum(gAdif==0, na.rm=T),
                                      oneweekdif=sum(abs(gAdif)>=1 &
                                                       abs(gAdif)<2, na.rm=T),
                                      twoweekdiff=sum(abs(gAdif)>=2 &
                                                        abs(gAdif)<3, na.rm=T),
                                      morethan1wkdif=sum(abs(gAdif)>=2, na.rm=T)
                      )]

openxlsx::write.xlsx(gAconcordance,file.path(org::PROJ$SHARED_TODAY,
                                             sprintf("gA_Concordance_20_44_%s.xlsx",
                                                     lubridate::today())))

# concordance and birthweight

bwdiff <- gA[gAdif>=2, 
             c("bookdate",
               "booklmp", 
               "usedd_1",
               "merged_datedeliv",
               "cpodate_1",
               "mahima_hospenteredgestage_1",
               "comboUSandLMPgA",
               "mahima_gestageatbirthwk_1",
               "bw")]

openxlsx::write.xlsx(bwdiff,file.path(org::PROJ$SHARED_TODAY,
                                      sprintf("bwdiff_gAdiff_37_44_%s.xlsx",
                                              lubridate::today())))



########## Data Quality ########## 
gAover3weeks <- gA[(mahima_gestageatbirthwk_1<=44 & 
                      mahima_gestageatbirthwk_1>=37) & abs(gAdif)>=3, 
                   c("uniqueid",
                     "ident_dhis2_an",
                     "ident_dhis2_ppc",
                     "booknum",
                     "bookdate",
                     "booklmp",
                     "first_1_21_usedd",
                     "merged_datedeliv",
                     "comboUSandLMPgA",
                     "mahima_hospenteredgestage_1",
                     "gAdif",
                     "bw")]


openxlsx::write.xlsx(gAover3weeks,file.path(org::PROJ$SHARED_TODAY,
                                            sprintf("gA_DQ_over3weeks_37_44_%s.xlsx",
                                                    lubridate::today())))


# birth weights and birth outcomes
gA[,USorLMPdateCombogAdays:= as.numeric(difftime(
  mahima_dateofbirth_1,
  USorLMPdate,
  units ="days"))]


#merged_pregbweight
#weights not unified, get them all to grams
#gA[,abbbabyweight_1:=as.numeric(abbbabyweight_1)]
gA[,bw:=as.numeric(merged_pregbweight)]
gA[merged_pregbweight>0 & merged_pregbweight<7, bw:=1000*merged_pregbweight]
#gA[merged_pregbweight>=1 & merged_pregbweight<7, bw:=1000*merged_pregbweight]
gA[merged_pregbweight>=7 & merged_pregbweight<=60, bw:=100*merged_pregbweight]
gA[merged_pregbweight>6000,bw:=merged_pregbweight*0.1]

# cleaning gAs that are less than 2300 at birth, but greater than than 1000
#gA[,bwreplaced:=as.numeric(bw)]
gA[bw>1000 & bw<2300 & !is.na(ppcbirthweight_1), 
   bw:=as.numeric(ppcbirthweight_1)]

#xtabs(~gA$bw)

quantile(gA$bw, na.rm=T)


bwstats <-gA[comboUSandLMPgA>=40 & 
               comboUSandLMPgA<41 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_combo_%s.xlsx",
                                               lubridate::today())))
# hospentered
bwstats <-gA[mahima_hospenteredgestage_1>=40 & 
               mahima_hospenteredgestage_1<41 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_hospent_%s.xlsx",
                                               lubridate::today())))

# ultrasound based
bwstats <-gA[first_1_21_usedd_gA>=40 & 
               first_1_21_usedd_gA<41 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_us_based_%s.xlsx",
                                               lubridate::today())))


# calculated lmp
bwstats <-gA[mahima_gestageatbirthwk_1>=40 & 
               mahima_gestageatbirthwk_1<41 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_calculated_%s.xlsx",
                                               lubridate::today())))


###### SGA and LGA proportions  ######

###### sga ###### 

#making sgA variables combo days
gA[,sga:=as.logical(NA)]
gA[!is.na(bw) & !is.na(USorLMPdateCombogAdays),sga:=FALSE]

#identifying sgas via combogAs
#37 weeks
gA[sga==FALSE &
     bw>0 & 
     bw<2396 & 
     USorLMPdateCombogAdays>=259 & 
     USorLMPdateCombogAdays<=265, sga:=TRUE]

#38 week
gA[sga==FALSE &
     bw<2552 & 
     USorLMPdateCombogAdays>=266 & 
     USorLMPdateCombogAdays<=272, sga:=TRUE]

#39 weeks
gA[sga==FALSE &
     bw<2698 &
     USorLMPdateCombogAdays>=273 & 
     USorLMPdateCombogAdays<=279, sga:=TRUE]

#40 weeks
gA[sga==FALSE &
     bw<2832 & 
     USorLMPdateCombogAdays>=280 & 
     USorLMPdateCombogAdays<=286, sga:=TRUE]

#41 weeks
gA[sga==FALSE &
     bw<2953 & 
     USorLMPdateCombogAdays>=287 &
     USorLMPdateCombogAdays<=293, sga:=TRUE]


#### sga based on hospital entered gA ####
gA[,sga2:=as.logical(NA)]
gA[!is.na(bw) & !is.na(mahima_hospenteredgestage_1),sga2:=FALSE]

#identifying sgas via hospitalentered gA
#37 weeks
gA[sga2==FALSE &
     bw>0 & 
     bw<2389 & 
     mahima_hospenteredgestage_1==37, sga2:=TRUE]

#38 week
gA[sga2==FALSE &
     bw<2554 & 
     mahima_hospenteredgestage_1==38, sga2:=TRUE]

#39 weeks
gA[sga2==FALSE &
     bw<2691 &
     mahima_hospenteredgestage_1==39, sga2:=TRUE]

#40 weeks
gA[sga2==FALSE &
     bw<2825 & 
     mahima_hospenteredgestage_1==40, sga2:=TRUE]

#41 weeks
gA[sga2==FALSE &
     bw<2945 & 
     mahima_hospenteredgestage_1==41, sga2:=TRUE]



#### sga based on ultrasound entered gA ####
gA[,sga3:=as.logical(NA)]
gA[!is.na(bw) & !is.na(first_1_21_usedd_gA),sga3:=FALSE]

#identifying sgas via ultrasound gA
#37 weeks
gA[sga3==FALSE &
     bw>0 & 
     bw<2399 & 
     first_1_21_usedd_gA>=37 &
     first_1_21_usedd_gA<38, sga3:=TRUE]

#38 week
gA[sga3==FALSE &
     bw<2555 & 
     first_1_21_usedd_gA>=38 &
     first_1_21_usedd_gA<39, sga3:=TRUE]

#39 weeks
gA[sga3==FALSE &
     bw<2702 &
     first_1_21_usedd_gA>=39 &
     first_1_21_usedd_gA<40, sga3:=TRUE]

#40 weeks
gA[sga3==FALSE &
     bw<2837 & 
     first_1_21_usedd_gA>=40 &
     first_1_21_usedd_gA<41, sga3:=TRUE]

#41 weeks
gA[sga3==FALSE &
     bw<2958 & 
     first_1_21_usedd_gA>=41 &
     first_1_21_usedd_gA<42, sga3:=TRUE]




###### lga ###### 

#making lgA variables
gA[,lga:=NA]

#everyone who isnt missing gA and weight is given a false
gA[!is.na(bw) & !is.na(USorLMPdateCombogAdays), lga:=FALSE]

#identifying lgas
#37
gA[lga==FALSE &
     bw>3261 & 
     USorLMPdateCombogAdays>=259 &
     USorLMPdateCombogAdays<=265, lga:=TRUE]

#38
gA[lga==FALSE &
     bw>3473 & 
     USorLMPdateCombogAdays>=266 & 
     USorLMPdateCombogAdays<=272, lga:=TRUE]

#39
gA[lga==FALSE &
     bw>3672 & 
     USorLMPdateCombogAdays>=273 &
     USorLMPdateCombogAdays<=279, lga:=TRUE]

#40
gA[lga==FALSE &
     bw>3856 & 
     USorLMPdateCombogAdays>=280 & 
     USorLMPdateCombogAdays<=286, lga:=TRUE]

#41
gA[lga==FALSE &
     bw>4020 &
     USorLMPdateCombogAdays>=287 & 
     USorLMPdateCombogAdays<=293, lga:=TRUE]


#making lgA variables (Hospital entered)
gA[,lga2:=NA]

#everyone who isnt missing gA and weight is given a false
gA[!is.na(bw) & !is.na(mahima_hospenteredgestage_1), lga2:=FALSE]

#identifying lgas
#37
gA[lga2==FALSE &
     bw>3249 & 
     mahima_hospenteredgestage_1==37, lga2:=TRUE]

#38
gA[lga2==FALSE &
     bw>3461 & 
     mahima_hospenteredgestage_1==38, lga2:=TRUE]

#39
gA[lga2==FALSE &
     bw>3660 & 
     mahima_hospenteredgestage_1==39, lga2:=TRUE]

#40
gA[lga2==FALSE &
     bw>3843 & 
     mahima_hospenteredgestage_1==40, lga2:=TRUE]

#41
gA[lga2==FALSE &
     bw>4006 &
     mahima_hospenteredgestage_1==41, lga2:=TRUE]


#### lga based on ultrasound entered gA ####
gA[,lga3:=as.logical(NA)]
gA[!is.na(bw) & !is.na(first_1_21_usedd_gA),lga3:=FALSE]

#identifying sgas via ultrasound gA
#37 weeks
gA[lga3==FALSE &
     bw>0 & 
     bw>3268 & 
     first_1_21_usedd_gA>=37 &
     first_1_21_usedd_gA<38, lga3:=TRUE]

#38 week
gA[lga3==FALSE &
     bw>3480 & 
     first_1_21_usedd_gA>=38 &
     first_1_21_usedd_gA<39, lga3:=TRUE]

#39 weeks
gA[lga3==FALSE &
     bw>3680 &
     first_1_21_usedd_gA>=39 &
     first_1_21_usedd_gA<40, lga3:=TRUE]

#40 weeks
gA[lga3==FALSE &
     bw>3864 & 
     first_1_21_usedd_gA>=40 &
     first_1_21_usedd_gA<41, lga3:=TRUE]

#41 weeks
gA[lga3==FALSE &
     bw>4029 & 
     first_1_21_usedd_gA>=41 &
     first_1_21_usedd_gA<42, lga3:=TRUE]



##### lga and sga proportions ####
sgalga <-gA[,.(N=.N,
               sgacomboT=sum(sga==T, na.rm=T),
               sgacomboF=sum(sga==F, na.rm=T),
               missingsga=sum(is.na(sga)),
               sgaenteredT=sum(sga2==T, na.rm=T),
               sgaenteredF=sum(sga2==F, na.rm=T),
               missingsgaentered=sum(is.na(sga2)),
               sgaUST=sum(sga3==T, na.rm=T),
               sgaUSF=sum(sga3==F, na.rm=T),
               missingsgaUS=sum(is.na(sga3)),
               lgaT=sum(lga==T, na.rm=T),
               lgaF=sum(lga==F, na.rm=T),
               missingsga=sum(is.na(lga)),
               lgaentT=sum(lga2==T, na.rm=T),
               lgaentF=sum(lga2==F, na.rm=T),
               missingsgaent=sum(is.na(lga2)),
               lgaUST=sum(lga3==T, na.rm=T),
               lgaUSF=sum(lga3==F, na.rm=T),
               missinglgaUS=sum(is.na(lga3)))]

openxlsx::write.xlsx(sgalga,file.path(org::PROJ$SHARED_TODAY,
                                      sprintf("sgalga_%s.xlsx",
                                              lubridate::today())))



###################### GRAPHICS ###################### 


#at 24 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))

p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(24,24.86))
p <- p + theme_grey (base_size = 16)
p10 <- 555
p90 <- 756

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_24 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 25 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(25,25.86))
p <- p + theme_grey (base_size = 16)
p10 <- 648
p90 <- 882

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_25 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 26 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(26,26.86))
p <- p + theme_grey (base_size = 16)
p10 <- 751
p90 <- 1023

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_26 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 27 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(27,27.86))
p <- p + theme_grey (base_size = 16)
p10 <- 865
p90 <- 1178

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_27 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 28 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(28,28.86))
p <- p + theme_grey (base_size = 16)
p10 <- 989
p90 <- 1346

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_28 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 29 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(29,29.86))
p <- p + theme_grey (base_size = 16)
p10 <- 1122
p90 <- 1528

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_29 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 30 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(30,30.86))
p <- p + theme_grey (base_size = 16)
p10 <- 1265
p90 <- 1722

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_30 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 31 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(31,31.86))
p <- p + theme_grey (base_size = 16)
p10 <- 1416
p90 <- 1927

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_31 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 32 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(32,32.86))
p <- p + theme_grey (base_size = 16)
p10 <- 1573
p90 <- 2142

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_32 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")




#at 33 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgA, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(33,33.86))
p <- p + theme_grey (base_size = 16)
p10 <- 1736
p90 <- 2363

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_33 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 34 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(34,34.86))
p <- p + theme_grey (base_size = 16)

p10 <- 1901
p90 <- 2589

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

p

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_34 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 35 Weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(35,35.86))
p <- p + theme_grey (base_size = 16)

p10 <- 2068
p90 <- 2816

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_35 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



# 36 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(36,36.86))
p <- p + theme_grey (base_size = 16)

p10 <- 2234
p90 <- 3042

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_36 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 37 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(37,37.86))
p <- p + theme_grey (base_size = 16)

p10 <- 2396
p90 <- 3263

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_37 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 38 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(38,38.86))
p <- p + theme_grey (base_size = 16)

p10 <- 2552
p90 <- 3475

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_38 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 39 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(39,39.86))

p10 <- 2699
p90 <- 3675

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_39 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 40 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(40,40.86))
p <- p + theme_grey (base_size = 16)

p10 <- 2834
p90 <- 3858

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_40 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 41 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 comboUSandLMPgA>0 &
                 comboUSandLMPgA<44], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(41,41.86)) 
p <- p + theme_grey (base_size = 16)

p10 <- 2954
p90 <- 4023

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_41 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")





################ gA Paper Distribution Graphs ################ 

#dont need an id.vars here because not retaining information for each woman
tab <- gA[,c("mahima_hospenteredgestage_1", "comboUSandLMPgA")]

long <- melt.data.table(tab)
head(long)

#ran this and got 3 levels, so must do levels
long$variable

#see the levels, which one comes first so we can rename them accordingly
levels(long$variable)
levels(long$variable) <-c("Entered by HCP",
                          "Best Estimate")



long <- long[!is.na(value)]
long[,category:=cut(value,
                    breaks=c(-300,0,20,32.9,36.9,42.9,44,5000),
                    include.lowest=T)]

xtabs(~long$category, addNA = T)
levels(long$category) <- c("<=0",
                           "1-20",
                           "21-32",
                           "33-36",
                           "37-42",
                           "43-44",
                           ">44")

levels(long$category)

xtabs(~long$category, addNA = T)

library(ggplot2)

#if restrict more rows, cant produce both continous and categorical
p <- ggplot(long[value<50 & value>0], aes(x=value, fill=variable)) 
p <- p + geom_density(alpha=0.3)
p <- p + labs(title="Distribution of Gestational Age by Source",
              caption = "Outliers Removed") +
  xlab("Weeks") +
  ylab("Density")
p <- p + scale_x_continuous("Weeks", lim=c(36, 44))
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Dark2")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=24))

p

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Distribution_Adjusted_36_onwards.png"
), plot = p, width = 297, height = 210, unit = "mm")



#####Non-Adjusted Distribution#####
#if restrict more rows, cant produce both continous and categorical
p <- ggplot(long[value<300], aes(x=value, fill=variable)) 
p <- p + geom_density(alpha=0.3)
p <- p + labs(title="Distribution of Gestational Age by Source",
              caption= "Outliers Not Removed") +
  xlab("Weeks")
p <- p + scale_x_continuous("Weeks", lim=c(32, 44))
p <- p + scale_y_continuous("Frequency", labels=scales::percent)
p <- p + scale_fill_brewer("Gestational Age Source", palette ="Set1")
#centers title 
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + theme(text = element_text(size=24))

p

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "GA_Paper_Distribution_NON_Adjusted.png"
), plot = p, width = 297, height = 210, unit = "mm")



















