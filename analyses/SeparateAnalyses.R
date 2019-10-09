#RColorBrewer::display.brewer.all() 
###### SETUP STARTS
###################
###################
###################

# define our dates
CLINIC_INTERVENTION_DATE <- "2018-09-27"
CLINIC_CONTROL_DATE <- "2018-09-27"

# define the folders
tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})
FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
FOLDER_DATA_RESULTS <- file.path(getwd(),"../results/")
FOLDER_DATA_MBO <- file.path(getwd(),"../results/mbo_r/")
FOLDER_DROPBOX_RESULTS <- file.path(
  "~",
  "..",
  "eRegistry CRCT Dropbox",
  "Data management eRegQual",
  "Results_From_PNIPH",
  "Results",
  lubridate::today())


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
                     "fmsb"
)
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)


# from net but the above already in R

library(data.table)
library(ggplot2)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)...WE CAN deal it as library but it doesnot

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
# make sure that all of the file sources go DIRECTLY
# into the **global** environment
sapply(fileSources, source, .GlobalEnv)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()
DATA_DATE <- min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)

weekyear <- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
yearmonth <- sprintf("%s-%s",
                     lubridate::year(lubridate::today()),
                     lubridate::month(lubridate::today()))
### SETUP ENDS

# Load in datafile

d <- LoadDataFileFromNetwork()

smalldataset <- d[ident_dhis2_control==F &
                  ident_dhis2_an==T &
                  ident_dhis2_ppc==T &
                  ident_dhis2_cpo==T &
                  bookdate>="2017-09-01" & bookdate<="2018-09-01",
              c("cpocomplicationsnone_1",
                "cpopregoutcome_1",
                "cpopuerpalsepsis_2"
               )
                  
          ]

vars_cpodvt <- names(d)[stringr::str_detect(names(d),"^cpodvt_")]
vars_cpoeclampsia <-names(d)[stringr::str_detect(names(d),"^cpoeclampsia_")]
vars_cpopreeclampsia <- names(d)[stringr::str_detect(names(d),"^cpopreeclampsia_")]
vars_cpocomplicationsnone <- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
vars_cpoantepartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]

#vars_cpopregoutcome <-names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]

smallD <- d[ident_dhis2_control==F &
              bookdate>="2017-09-01" & bookdate<="2018-09-01" &
              ident_dhis2_booking==TRUE & 
              ident_dhis2_an==TRUE & 
              ident_dhis2_cpo==TRUE & 
              ident_dhis2_ppc==TRUE,
            c(
              "bookevent",
              vars_cpodvt,
              vars_cpoeclampsia,
              vars_cpopreeclampsia,
              vars_cpocomplicationsnone,
              vars_cpoantepartumhemorrhage,
              vars_cpopostpartumhemorrhage,
              vars_cpopuerpalsepsis
            ),with=F]



smallD[,id:=1:.N]
long <- melt.data.table(smallD, id.vars=c("id"),variable.factor = F, value.factor = F)

uglytable <- long[,
                  .(
                    denominator=.N,
                    is_NA=sum(is.na(value)),
                    not_NA=sum(!is.na(value)),
                    value0=sum(value==0,na.rm=T),
                    value1=sum(value==1,na.rm=T),
                    value2=sum(value==2,na.rm=T),
                    value3=sum(value==3,na.rm=T)
                  ),
                  keyby=.(variable)
                  ]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "pniph",
                       "abstracts_2018",
                       "cpo.xlsx"))

# 
#   

nrow(smalldataset)




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




###printing out prevalences for the hospital birth gest age stuff
###sink() is like capture in STATA
sink()
sink(file.path(FOLDER_DROPBOX_RESULTS,
               "mahima",
               "trial_1",
               "hospital gA calculated and entered.txt"))
cat("Control Alone \n")
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE & 
           ident_dhis2_control==T &
           !is.na(mahima_hospenteredgestage_1)]$mahima_gestageatbirthwk_1_cats)
print("Intervention Alone")
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE & 
           ident_dhis2_control==F &
           !is.na(mahima_hospenteredgestage_1)]$mahima_gestageatbirthwk_1_cats)
print("both")
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE &
           !is.na(mahima_hospenteredgestage_1)]$mahima_gestageatbirthwk_1_cats)

###below command makes spaces between this and before it
cat("\n\n")
print("Control") 
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE & 
           ident_dhis2_control==T &
           !is.na(mahima_gestageatbirthwk_1)]$mahima_hospenteredgestage_1_cats)
print("Intervention") 
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE & 
           ident_dhis2_control==F &
           !is.na(mahima_gestageatbirthwk_1)]$mahima_hospenteredgestage_1_cats)
print("Both") 
xtabs(~d[bookyearmonth<="2017-03" & 
           ident_TRIAL_1==TRUE &
           !is.na(mahima_gestageatbirthwk_1)]$mahima_hospenteredgestage_1_cats)
cat("\n\n Denonimator in the dataset that have both variables\n\n") 
cat("\n\n") 
print(nrow(d[bookyearmonth<="2017-03" & 
               ident_TRIAL_1==TRUE & 
               !is.na(mahima_hospenteredgestage_1) & 
               !is.na(mahima_gestageatbirthwk_1)]))


cat('\n\n')       
print("general tabs")
xtabs(~d$mahima_gestageatbirthwk_1_cats) 
xtabs(~d$mahima_hospenteredgestage_1_cats)
xtabs(~d$mahima_hospenteredgestage_1_cats + d$mahima_gestageatbirthwk_1_cats)

print("creating a smaller dataset just for this purpose")
analysisDataset <- d[bookyearmonth<="2017-03"&
                       ident_TRIAL_1==TRUE &
                       !is.na(mahima_gestageatbirthwk_1) &
                       !is.na(mahima_hospenteredgestage_1),
                     c("bookevent",
                       "mahima_hospenteredgestage_1",
                       "mahima_hospenteredgestage_1_cats",
                       "mahima_gestageatbirthwk_1",
                       "mahima_gestageatbirthwk_1_cats")]





cat('\n\n')
print("IQRs for these variables")
quantile(x=analysisDataset$mahima_gestageatbirthwk_1, 
         probs = seq(0, 1, 0.25), 
         na.rm = TRUE)

quantile(x=analysisDataset$mahima_hospenteredgestage_1, 
         probs = seq(0, 1, 0.25), 
         na.rm = TRUE)




sink()




###Kappa test for reliability
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
d[bookevent=="----", c("motheridno", 
                              "bookyearmonth", 
                              "mahima_hospenteredgestage_1",
                              "mahima_hospenteredgestage_1")]

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

###covariance--this is bounded unlike variance and covariance
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











