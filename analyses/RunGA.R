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



####### Setup ends ####### 

# LOAD IN DATA

d <- readRDS(file.path(org::PROJ$DATA,"full_data_from_r.rds"))



# book gA proportions
#nrow(ar[ident_dhis2_booking==T & bookdate>="2019-01-01" & bookdate<="2019-12-31"])
#[1] 28097
#> nrow(ar[ident_dhis2_booking==T & bookdate>="2019-01-01" & bookdate<="2019-12-31" & bookgestage>0 & bookgestage<16])

########## Data set definitions ################

gA <- d[ident_TRIAL_1==T & bookdate>= "2017-01-15"&
           bookdate<="2017-09-15" & 
           (matching=="Avicenna"|
              matching=="Governmental"|
              matching=="PaperHBO"|
              matching=="Private"),]




################ END  ################

bookgAcats <- gA[,.(N=.N,
                    bookgAlessthan16=sum(bookgestage<16, na.rm=T),
                    bookgAgreaterthanorequalto16=sum(bookgestage>=16, na.rm=T))]

openxlsx::write.xlsx(bookgAcats,
                     file.path(org::PROJ$SHARED_TODAY,
                               sprintf("bookgA_%s.xlsx",
                                       lubridate::today())))

nrow(gA[!is.na(mahima_dateofbirth_1)])


################  Creating new vars   ################
###### correct the us or lmp date
# to mimick the  eRegistry, we have used the eReg rule
# the rule takes an ultrasound less than 23 weeks and an lmp if no ultrasound is present
nam <- names(d)[stringr::str_detect(names(gA),"^usedd_[0-9]*$")]
num <- stringr::str_replace(nam,"usedd_","")
gA[,first_1_21_usedd:=as.Date(NA)]
for(i in num ){
  print(i)
  
  var_usedd <- sprintf("usedd_%s",i)
  var_usgestage <- sprintf("usgestage_%s",1)
  
  gA[!is.na(get(var_usedd)) &
      
      !is.na(get(var_usgestage)) &
      get(var_usgestage) > 0 &
      get(var_usgestage) < 21 &
      is.na(first_1_21_usedd),
    first_1_21_usedd:=as.Date(get(var_usedd),format="%Y-%m-%d")]
}
#unique(d$usgestage_1)
#unique(d$first_1_21_usedd)
#sum(!is.na(d$first_1_21_usedd))

#Making gA for first_1_21_usedd

gA[,first_1_21_usedd_diffbtwnHBO:=round(as.numeric(
  difftime(mahima_dateofbirth_1,first_1_21_usedd, units="weeks")),digits=1)]

gA[,first_1_21_usedd_gA:=first_1_21_usedd_diffbtwnHBO+40]

#unique(d$first_1_21_usedd_gA)


#TO DO: make sure we get the 87 women who are missing an lmp or bookdate
gA[,usedddays:=first_1_21_usedd - as.difftime(280, unit="days")]

gA[,USorLMPdate:=booklmp]
gA[!is.na(usedddays),USorLMPdate:=usedddays]
#d[is.na(USorLMPdate), USorLMPdate:=]

# USorLMPdatecombogAdays variable (put in creating further variables)
gA[,USorLMPdateCombogAdays:= as.numeric(difftime(
  mahima_dateofbirth_1,
  USorLMPdate,
  units ="days"))]




# hospital entered gestational age in days with avg value
gA[,hospentavgdays:=3+mahima_hospenteredgestage_1*7]
gA[,hospentdays:=mahima_hospenteredgestage_1*7]



# termcats
gA[,termcats:=cut(USorLMPdateCombogAdays,
                 breaks=c(-300,0,140,167,195,230,258,294,314,50000),
                 include.lowest=T)]
#xtabs(~gA$termcats, addNA=T)


uscattab <- gA[,.(N=.N),
               keyby=.(termcats)]

openxlsx::write.xlsx(uscattab,
                     file.path(org::PROJ$SHARED_TODAY,
                               sprintf("bestestimate_%s.xlsx",
                                       lubridate::today())))


# hospenteredcats
gA[,hospentcats:=cut(hospentdays,
                  breaks=c(-300,0,140,167,195,230,258,294,314,50000),
                  include.lowest=T)]
# xtabs(~gA$hospentcats, addNA=T)


uscattab <- gA[,.(N=.N),
               keyby=.(hospentcats)]

openxlsx::write.xlsx(uscattab,
                     file.path(org::PROJ$SHARED_TODAY,
                               sprintf("hospentcat_%s.xlsx",
                                       lubridate::today())))



# usedd days
gA[,first_1_21_usedd_days:=floor(7*first_1_21_usedd_gA)]
# uscats
gA[,uscats:=cut(first_1_21_usedd_days,
                  breaks=c(-300,0,140,167,195,230,258,294,314,50000),
                  include.lowest=T)]
xtabs(~gA$uscats, addNA=T)

uscattab <- gA[,.(N=.N),
               keyby=.(uscats)]

openxlsx::write.xlsx(uscattab,
                     file.path(org::PROJ$SHARED_TODAY,
                    sprintf("uscat_%s.xlsx",
                    lubridate::today())))

## making term variables
gA[,termcatcombo:=as.character(NA)]
gA[USorLMPdateCombogAdays>=259 & 
     USorLMPdateCombogAdays<=293, termcatcombo:="term"]
gA[USorLMPdateCombogAdays>=168 & 
     USorLMPdateCombogAdays<=258, termcatcombo:="preterm"]
gA[USorLMPdateCombogAdays>=294 & 
     USorLMPdateCombogAdays<=314, termcatcombo:="postterm"]

gA[,termcathospavg:=as.character(NA)]
gA[hospentavgdays>=259 & 
     hospentavgdays<=293, termcathospavg:="term"]
gA[hospentavgdays>=168 & 
     hospentavgdays<=258, termcathospavg:="preterm"]
gA[hospentavgdays>=294 & 
     hospentavgdays<=314, termcathospavg:="postterm"]

# can also make one for first21useddday?


gA[,gAdif2:=abs((hospentdays-USorLMPdateCombogAdays))]

##### BW #####
#merged_pregbweight
#weights not unified, get them all to grams
#gA[,abbbabyweight_1:=as.numeric(abbbabyweight_1)]
gA[,bw:=as.numeric(merged_pregbweight)]
gA[merged_pregbweight>0.1 & merged_pregbweight<7, bw:=1000*merged_pregbweight]
#gA[merged_pregbweight>=1 & merged_pregbweight<7, bw:=1000*merged_pregbweight]
gA[merged_pregbweight>=7 & merged_pregbweight<=60, bw:=100*merged_pregbweight]
gA[merged_pregbweight>6000,bw:=merged_pregbweight*0.1]

# cleaning gAs that are less than 2300 at birth, but greater than than 1000
#gA[,bwreplaced:=as.numeric(bw)]
gA[bw>1000 & bw<2300 & !is.na(merged_pregbweight), 
   bw:=as.numeric(merged_pregbweight)]

#xtabs(~gA$bw)

quantile(gA$bw, na.rm=T)


bwstats <-gA[USorLMPdateCombogAdays==280 & 
                    bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_combo_%s.xlsx",
                                               lubridate::today())))


# only do results for people with both hospentered and usorlmpcombo
# only using restricted dataset for termcounts and sga and lga datasets
gArestricted <- gA[!is.na(USorLMPdateCombogAdays) & 
                     !is.na(hospentavgdays)]

########## completeness reports ##########
gAcomp <- gA[,.(N=.N,
                          LMP=sum(!is.na(booklmp)),
                          US=sum(!is.na(first_1_21_usedd)),
                          LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                          USgA=sum(!is.na(first_1_21_usedd_gA)),
                          COMBOgA=sum(!is.na(comboUSandLMPgA)),
                          usorlmpcombo=sum(!is.na(USorLMPdateCombogAdays)),
                          HOSPgA=sum(!is.na(mahima_hospenteredgestage_1)),
                          LMPandEntered=sum(!is.na(booklmp) & 
                                              !is.na(mahima_hospenteredgestage_1)),
                          USandEntered=sum(!is.na(first_1_21_usedd_gA) & 
                                             !is.na(mahima_hospenteredgestage_1)),
                          notMissingLMP=sum(!is.na(booklmp)),
                          notMissingUS=sum(!is.na(first_1_21_usedd)),
                          notMissingEDD=sum(!is.na(first_1_21_usedd)),
                          notMissingDateofDeliv=sum(!is.na(mahima_dateofbirth_1)),
                          notMissingcomboGA=sum(!is.na(comboUSandLMPgA)),
                          MissingLMP=sum(is.na(booklmp)),
                          MissingUS=sum(is.na(first_1_21_usedd)),
                          MissingEDD=sum(is.na(first_1_21_usedd)),
                          MissingDateofDeliv=sum(is.na(mahima_dateofbirth_1)),
                          MissingcomboGA=sum(is.na(comboUSandLMPgA)),
                          MissingGAhosp=sum(is.na(mahima_hospenteredgestage_1))),
                       keyby=.(termcats)]

openxlsx::write.xlsx(gAcomp,file.path(
  org::PROJ$SHARED_TODAY,
  sprintf("Completeness_%s.xlsx",
          lubridate::today())))

# do completeness with restricted and non restricted
gAcomp <- gArestricted[,.(N=.N,
                LMP=sum(!is.na(booklmp)),
                US=sum(!is.na(first_1_21_usedd)),
                LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                USgA=sum(!is.na(first_1_21_usedd_gA)),
                COMBOgA=sum(!is.na(comboUSandLMPgA)),
                usorlmpcombo=sum(!is.na(USorLMPdateCombogAdays)),
                HOSPgA=sum(!is.na(mahima_hospenteredgestage_1)),
                LMPandEntered=sum(!is.na(booklmp) & 
                                    !is.na(mahima_hospenteredgestage_1)),
                USandEntered=sum(!is.na(first_1_21_usedd_gA) & 
                                    !is.na(mahima_hospenteredgestage_1)),
                notMissingLMP=sum(!is.na(booklmp)),
                notMissingUS=sum(!is.na(first_1_21_usedd)),
                notMissingEDD=sum(!is.na(first_1_21_usedd)),
                notMissingDateofDeliv=sum(!is.na(mahima_dateofbirth_1)),
                notMissingcomboGA=sum(!is.na(comboUSandLMPgA)),
                MissingLMP=sum(is.na(booklmp)),
                MissingUS=sum(is.na(first_1_21_usedd)),
                MissingEDD=sum(is.na(first_1_21_usedd)),
                MissingDateofDeliv=sum(is.na(mahima_dateofbirth_1)),
                MissingcomboGA=sum(is.na(comboUSandLMPgA)),
                MissingGAhosp=sum(is.na(mahima_hospenteredgestage_1))),
         keyby=.(termcats)]

openxlsx::write.xlsx(gAcomp,file.path(
                                  org::PROJ$SHARED_TODAY,
                                  sprintf("Completeness_restricted_%s.xlsx",
                                          lubridate::today())))



library("e1071")
# statistical  info

gAstats <- gA[(USorLMPdateCombogAdays<=314 & 
                 USorLMPdateCombogAdays>=168) & 
                (hospentdays>=168 & hospentdays<=314),
              .(N=.N,
                note="allinweeksexceptcombogAdays",
                maxlmpgA=max(mahima_gestageatbirthwk_1, na.rm=T),
                minlmpgA=min(mahima_gestageatbirthwk_1, na.rm=T),
                MeanLMPgA=mean(mahima_gestageatbirthwk_1, na.rm=T),
                MedianLMPgA=median(mahima_gestageatbirthwk_1, na.rm=T),
                SDLMPgA=sd(mahima_gestageatbirthwk_1, na.rm=T),
                KurtLMPgA=kurtosis(mahima_gestageatbirthwk_1,na.rm=T),
                SkewLMPgA=skewness(mahima_gestageatbirthwk_1, na.rm=T),
                maxUS=max(first_1_21_usedd_gA, na.rm=T),
                minUS=min(first_1_21_usedd_gA, na.rm=T),
                MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                maxUsCombogAdays=max(USorLMPdateCombogAdays, na.rm=T),
                minUsCombogAdays=min(USorLMPdateCombogAdays, na.rm=T),
                MeanCombogAdays=mean(USorLMPdateCombogAdays, na.rm=T),
                MedianCombogAdays=median(USorLMPdateCombogAdays, na.rm=T),
                SDCombogAdays=sd(USorLMPdateCombogAdays, na.rm=T),
                KurtCombogAdays=kurtosis(USorLMPdateCombogAdays,na.rm=T),
                SkewCombogAdays=skewness(USorLMPdateCombogAdays, na.rm=T),
                maxhospent=max(mahima_hospenteredgestage_1, na.rm=T),
                minhospent=min(mahima_hospenteredgestage_1, na.rm=T),
                MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                MedianHospgA=median(mahima_hospenteredgestage_1, na.rm=T),
                SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                KurtHospgA=kurtosis(mahima_hospenteredgestage_1,na.rm=T),
                SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                maxhospentdays=max(hospentavgdays, na.rm=T),
                minhospentdays=min(hospentavgdays, na.rm=T),
                MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                MedianHospgAavg=median(hospentavgdays, na.rm=T),
                SDHospgAavg=sd(hospentavgdays, na.rm=T),
                KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
              )]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_24_44_hospentandcombo_%s.xlsx",lubridate::today())))


gAstats <- gA[(USorLMPdateCombogAdays<=314 & 
                 USorLMPdateCombogAdays>=168) & 
                (mahima_gestageatbirthwk_1<=44 & 
                   mahima_gestageatbirthwk_1>=24) &
                (first_1_21_usedd_gA<=44 & first_1_21_usedd_gA>=24) &
                (mahima_hospenteredgestage_1>=24 & 
                   mahima_hospenteredgestage_1<=44),
              .(N=.N,
                maxlmpgA=max(mahima_gestageatbirthwk_1, na.rm=T),
                minlmpgA=min(mahima_gestageatbirthwk_1, na.rm=T),
                MeanLMPgA=mean(mahima_gestageatbirthwk_1, na.rm=T),
                MedianLMPgA=median(mahima_gestageatbirthwk_1, na.rm=T),
                SDLMPgA=sd(mahima_gestageatbirthwk_1, na.rm=T),
                KurtLMPgA=kurtosis(mahima_gestageatbirthwk_1,na.rm=T),
                SkewLMPgA=skewness(mahima_gestageatbirthwk_1, na.rm=T),
                maxUS=max(first_1_21_usedd_gA, na.rm=T),
                minUS=min(first_1_21_usedd_gA, na.rm=T),
                MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                maxUsCombogA=max(USorLMPdateCombogAdays, na.rm=T),
                minUsCombogA=min(USorLMPdateCombogAdays, na.rm=T),
                MeanCombogA=mean(USorLMPdateCombogAdays, na.rm=T),
                MedianCombogA=median(USorLMPdateCombogAdays, na.rm=T),
                SDCombogA=sd(USorLMPdateCombogAdays, na.rm=T),
                KurtCombogA=kurtosis(USorLMPdateCombogAdays,na.rm=T),
                SkewCombogA=skewness(USorLMPdateCombogAdays, na.rm=T),
                maxhospent=max(mahima_hospenteredgestage_1, na.rm=T),
                minhospent=min(mahima_hospenteredgestage_1, na.rm=T),
                MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                MedianHospgA=median(mahima_hospenteredgestage_1, na.rm=T),
                SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                KurtHospgA=kurtosis(mahima_hospenteredgestage_1,na.rm=T),
                SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                maxhospentdays=max(hospentavgdays, na.rm=T),
                minhospentdays=min(hospentavgdays, na.rm=T),
                MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                MedianHospgAavg=median(hospentavgdays, na.rm=T),
                SDHospgAavg=sd(hospentavgdays, na.rm=T),
                KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
              )]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                    sprintf("gA_Stats_24_44_ALLrestricted_%s.xlsx",lubridate::today())))



#### stats with restricted data set and overall data set

gAstats <- gArestricted[,
                        .(N=.N,
                          maxlmpgA=max(mahima_gestageatbirthwk_1, na.rm=T),
                          minlmpgA=min(mahima_gestageatbirthwk_1, na.rm=T),
                          MeanLMPgA=mean(mahima_gestageatbirthwk_1, na.rm=T),
                          MedianLMPgA=median(mahima_gestageatbirthwk_1, 
                                             na.rm=T),
                          SDLMPgA=sd(mahima_gestageatbirthwk_1, na.rm=T),
                          KurtLMPgA=kurtosis(mahima_gestageatbirthwk_1,na.rm=T),
                          SkewLMPgA=skewness(mahima_gestageatbirthwk_1, 
                                             na.rm=T),
                          maxUS=max(first_1_21_usedd_gA, na.rm=T),
                          minUS=min(first_1_21_usedd_gA, na.rm=T),
                          MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                          MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                          SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                          KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                          SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                          maxUsCombogA=max(USorLMPdateCombogAdays, na.rm=T),
                          minUsCombogA=min(USorLMPdateCombogAdays, na.rm=T),
                          MeanCombogA=mean(USorLMPdateCombogAdays, na.rm=T),
                          MedianCombogA=median(USorLMPdateCombogAdays, na.rm=T),
                          SDCombogA=sd(USorLMPdateCombogAdays, na.rm=T),
                          KurtCombogA=kurtosis(USorLMPdateCombogAdays,na.rm=T),
                          SkewCombogA=skewness(USorLMPdateCombogAdays, na.rm=T),
                          maxhospent=max(mahima_hospenteredgestage_1, na.rm=T),
                          minhospent=min(mahima_hospenteredgestage_1, na.rm=T),
                          MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                          MedianHospgA=median(mahima_hospenteredgestage_1,
                                              na.rm=T),
                          SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                          KurtHospgA=kurtosis(mahima_hospenteredgestage_1,
                                              na.rm=T),
                          SkewHospgA=skewness(mahima_hospenteredgestage_1, 
                                              na.rm=T),
                          maxhospentdays=max(hospentavgdays, na.rm=T),
                          minhospentdays=min(hospentavgdays, na.rm=T),
                          MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                          MedianHospgAavg=median(hospentavgdays, na.rm=T),
                          SDHospgAavg=sd(hospentavgdays, na.rm=T),
                          KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                          SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
                        )]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_restricted_data_set_only_%s.xlsx",lubridate::today())))


# overall data set
gAstats <- gA[,.(N=.N,
                  maxlmpgA=max(mahima_gestageatbirthwk_1, na.rm=T),
                  minlmpgA=min(mahima_gestageatbirthwk_1, na.rm=T),
                  MeanLMPgA=mean(mahima_gestageatbirthwk_1, na.rm=T),
                  MedianLMPgA=median(mahima_gestageatbirthwk_1, na.rm=T),
                  SDLMPgA=sd(mahima_gestageatbirthwk_1, na.rm=T),
                  KurtLMPgA=kurtosis(mahima_gestageatbirthwk_1,na.rm=T),
                  SkewLMPgA=skewness(mahima_gestageatbirthwk_1, na.rm=T),
                  maxUS=max(first_1_21_usedd_gA, na.rm=T),
                  minUS=min(first_1_21_usedd_gA, na.rm=T),
                  MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                  MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                  SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                  KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                  SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                  maxUsCombogA=max(USorLMPdateCombogAdays, na.rm=T),
                  minUsCombogA=min(USorLMPdateCombogAdays, na.rm=T),
                  MeanCombogA=mean(USorLMPdateCombogAdays, na.rm=T),
                  MedianCombogA=median(USorLMPdateCombogAdays, na.rm=T),
                  SDCombogA=sd(USorLMPdateCombogAdays, na.rm=T),
                  KurtCombogA=kurtosis(USorLMPdateCombogAdays,na.rm=T),
                  SkewCombogA=skewness(USorLMPdateCombogAdays, na.rm=T),
                  maxhospent=max(mahima_hospenteredgestage_1, na.rm=T),
                  minhospent=min(mahima_hospenteredgestage_1, na.rm=T),
                  MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                  MedianHospgA=median(mahima_hospenteredgestage_1, na.rm=T),
                  SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                  KurtHospgA=kurtosis(mahima_hospenteredgestage_1,na.rm=T),
                  SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                  maxhospentdays=max(hospentavgdays, na.rm=T),
                  minhospentdays=min(hospentavgdays, na.rm=T),
                  MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                  MedianHospgAavg=median(hospentavgdays, na.rm=T),
                  SDHospgAavg=sd(hospentavgdays, na.rm=T),
                  KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                  SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
)]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_original_data_set_%s.xlsx",lubridate::today())))
# overall data set
gAstats <- gA[first_1_21_usedd_days>=168 &
                first_1_21_usedd_days<=314,.(N=.N,
                 
                 maxUS=max(first_1_21_usedd_gA, na.rm=T),
                 minUS=min(first_1_21_usedd_gA, na.rm=T),
                 MeanUsgA=mean(first_1_21_usedd_gA, na.rm=T),
                 MedianUsgA=median(first_1_21_usedd_gA, na.rm=T),
                 SDUsgA=sd(first_1_21_usedd_gA, na.rm=T),
                 KurtUsgA=kurtosis(first_1_21_usedd_gA,na.rm=T),
                 SkewUsgA=skewness(first_1_21_usedd_gA, na.rm=T),
                 maxUSdays=max(first_1_21_usedd_days, na.rm=T),
                 minUSdays=min(first_1_21_usedd_days, na.rm=T),
                 MeanUsgAdays=mean(first_1_21_usedd_days, na.rm=T),
                 MedianUsgAdays=median(first_1_21_usedd_days, na.rm=T),
                 SDUsgAdays=sd(first_1_21_usedd_days, na.rm=T),
                 KurtUsgAdays=kurtosis(first_1_21_usedd_days,na.rm=T),
                 SkewUsgAdays=skewness(first_1_21_usedd_days, na.rm=T))]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_US_restricted_data_set_%s.xlsx",lubridate::today())))



# overall data set
gAstats <- gA[hospentdays>=168 & 
                hospentdays<=314,.(N=.N,
                
                 
                 maxhospent=max(mahima_hospenteredgestage_1, na.rm=T),
                 minhospent=min(mahima_hospenteredgestage_1, na.rm=T),
                 MeanHospgA=mean(mahima_hospenteredgestage_1, na.rm=T),
                 MedianHospgA=median(mahima_hospenteredgestage_1, na.rm=T),
                 SDHospgA=sd(mahima_hospenteredgestage_1, na.rm=T),
                 KurtHospgA=kurtosis(mahima_hospenteredgestage_1,na.rm=T),
                 SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                 maxhospentdays=max(hospentdays, na.rm=T),
                 minhospentdays=min(hospentdays, na.rm=T),
                 MeanHospgA=mean(hospentdays, na.rm=T),
                 MedianHospgAavg=median(hospentdays, na.rm=T),
                 SDHospgA=sd(hospentdays, na.rm=T),
                 KurtHospgA=kurtosis(hospentdays,na.rm=T),
                 SkewHospgA=skewness(hospentdays, na.rm=T)
)]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_HospenetGaRestricted_%s.xlsx",lubridate::today())))



gAstats <- gA[(USorLMPdateCombogAdays<=314 & 
                 USorLMPdateCombogAdays>=168),
              .(N=.N,
               
                maxUsCombogAdays=max(USorLMPdateCombogAdays, na.rm=T),
                minUsCombogAdays=min(USorLMPdateCombogAdays, na.rm=T),
                MeanCombogAdays=mean(USorLMPdateCombogAdays, na.rm=T),
                MedianCombogAdays=median(USorLMPdateCombogAdays, na.rm=T),
                SDCombogAdays=sd(USorLMPdateCombogAdays, na.rm=T),
                KurtCombogAdays=kurtosis(USorLMPdateCombogAdays,na.rm=T),
                SkewCombogAdays=skewness(USorLMPdateCombogAdays, na.rm=T))]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_24_44_comborestricted_%s.xlsx",lubridate::today())))





gAstats <- gA[(comboUSandLMPgA<=44 & 
                 comboUSandLMPgA>=24),
              .(N=.N,
                
                maxUsCombogAdays=max(comboUSandLMPgA, na.rm=T),
                minUsCombogAdays=min(comboUSandLMPgA, na.rm=T),
                MeanCombogAdays=mean(comboUSandLMPgA, na.rm=T),
                MedianCombogAdays=median(comboUSandLMPgA, na.rm=T),
                SDCombogAdays=sd(comboUSandLMPgA, na.rm=T),
                KurtCombogAdays=kurtosis(comboUSandLMPgA,na.rm=T),
                SkewCombogAdays=skewness(comboUSandLMPgA, na.rm=T))]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_24_44_comborestrictedWeeks_%s.xlsx",lubridate::today())))

# descriptives for restricted data set
gArestrictedcats <- gArestricted[,.(N=.N),keyby=.(termcats)]

openxlsx::write.xlsx(gArestrictedcats,
                     file.path(org::PROJ$SHARED_TODAY,
                 sprintf("gA_restricted_termcats_%s.xlsx",lubridate::today())))

gArestrictedhospcounts <- gArestricted[,.(N=.N),keyby=.(termcathospavg)]

openxlsx::write.xlsx(gArestrictedhospcounts,
                     file.path(org::PROJ$SHARED_TODAY,
          sprintf("gA_restricted_hospavgtermcats_%s.xlsx",lubridate::today())))

gArestrictedcombocats <- gArestricted[,.(N=.N),keyby=.(termcatcombo)]
openxlsx::write.xlsx(gArestrictedcombocats,
                     file.path(org::PROJ$SHARED_TODAY,
              sprintf("gA_restricted_termcatcombo_%s.xlsx",lubridate::today())))


# term categorical data based on variable
gAtermcounts <- gArestricted[,.(N=.N,
                            LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                            UsGA=sum(!is.na(first_1_21_usedd_gA)),
                            CombogA=sum(!is.na(USorLMPdateCombogAdays)),
                            HospentgA=sum(!is.na(hospentdays))),
                          keyby=.(termcatcombo,termcathospavg)]



openxlsx::write.xlsx(gAtermcounts,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_TermCounts_24_42_%s.xlsx",
                                               lubridate::today())))

# birth type
gAtype <- gA[USorLMPdateCombogAdays<=314 & 
               USorLMPdateCombogAdays>=168,.(
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
             keyby=.(termcatcombo)]

openxlsx::write.xlsx(gAtype,file.path(org::PROJ$SHARED_TODAY,
                                            sprintf("birthoutcome_24_44_%s.xlsx",
                                                    lubridate::today())))

# birth type CPO 
CPO <- gA[USorLMPdateCombogAdays<=314 & 
            USorLMPdateCombogAdays>=168,.(
  N=.N,
  Alive=sum(cpopregoutcome_1=="LIVE", na.rm=T),
  Stillbirth=sum(cpopregoutcome_1=="STILL", na.rm=T),
  ABO=sum(cpopregoutcome_1=="ABO", na.rm=T),
  Death=sum(cpopregoutcome_1=="NEO_DEATH"|
              cpopregoutcome_1=="LATE_DEATH"|
              cpopregoutcome_1=="INF_DEATH",na.rm=T),
  Missing=sum(is.na(cpopregoutcome_1))),
  keyby=.(termcatcombo)]

openxlsx::write.xlsx(CPO,file.path(org::PROJ$SHARED_TODAY,
                                      sprintf("cpo_24_44_%s.xlsx",
                                              lubridate::today())))



# only do results for people with both hospentered and usorlmpcombo
# only using restricted dataset for termcounts and sga and lga datasets
gArestricted <- gA[!is.na(USorLMPdateCombogAdays) & 
                     !is.na(hospentavgdays)]


### concordance in weeks ###
#gA[,comboUSandLMPgA_rounded:= floor(comboUSandLMPgA)]

gA[,gAdif:=abs((USorLMPdateCombogAdays-hospentavgdays))]
xtabs(~gA$gAdif, addNA=T)
str(gA$gAdif)

gAconcordance <- gA[(USorLMPdateCombogAdays<=314 & 
                       USorLMPdateCombogAdays>=168) &
                      !is.na(gAdif),.(N=.N,
                       "No difference"=sum(gAdif==0, na.rm=T),
                       "One week difference"=sum(abs(gAdif)>=1 &
                                        abs(gAdif)<7, na.rm=T),
                       "Two week difference"=sum(abs(gAdif)>=7 &
                                         abs(gAdif)<14, na.rm=T),
                       "More than 2 weeks difference"=sum(abs(gAdif)>=14, na.rm=T))]

openxlsx::write.xlsx(gAconcordance,
                     file.path(org::PROJ$SHARED_TODAY,
                                            sprintf(
                                              "gA_Concordance_24_44_%s.xlsx",
                                                    lubridate::today())))

# concordance and birthweight

bwdiff <- gA[gAdif>=14 & 
               USorLMPdateCombogAdays<=308 & 
               USorLMPdateCombogAdays>=259, 
                       c("bookdate",
                         "booklmp", 
                         "usedd_1",
                         "mahima_dateofbirth_1",
                         "mahima_dateofbirth_1",
                         "cpodate_1",
                         "mahima_hospenteredgestage_1",
                         "comboUSandLMPgA",
                         "mahima_gestageatbirthwk_1",
                         "bw")]

openxlsx::write.xlsx(bwdiff,file.path(org::PROJ$SHARED_TODAY,
                      sprintf("bwdiff_gAdiff_37_44_%s.xlsx",
                        lubridate::today())))


## concordance via days variables and the hospavg variable

gArestricted[,gAdif2:=abs((hospentdays-USorLMPdateCombogAdays))]
#xtabs(~gA$gAdif2, addNA=T)


# do for both restricted and normal
gAconcordance <- gArestricted[(USorLMPdateCombogAdays<=314 & 
                       USorLMPdateCombogAdays>=168) &
                      !is.na(gAdif2),.(N=.N,
                                      "No difference"=sum(gAdif2==0, na.rm=T),
                                      "One week difference"=sum(abs(gAdif2)>=1 &
                                                       abs(gAdif2)<7, na.rm=T),
                                      "Two week difference"=sum(abs(gAdif2)>=7 &
                                                        abs(gAdif2)<14,
                                                      na.rm=T),
                                      "More than 2 week difference"=sum(abs(gAdif2)>=14, 
                                                         na.rm=T)
                      )]

openxlsx::write.xlsx(gAconcordance,file.path(org::PROJ$SHARED_TODAY,
                                             sprintf("gA_Concordance_days_24_44_restricted%s.xlsx",
                                                     lubridate::today())))


# gA concordance all
gAconcordance <- gA[,.(N=.N,
                    noweekdiff=sum(gAdif2==0, na.rm=T),
                    oneweekdif=sum(abs(gAdif2)>=1 &
                                      abs(gAdif2)<7, na.rm=T),
                    twoweekdiff=sum(abs(gAdif2)>=7 &
                                     abs(gAdif2)<14, na.rm=T),
                    morethan1wkdif=sum(abs(gAdif2)>=7, na.rm=T))]

openxlsx::write.xlsx(gAconcordance,file.path(org::PROJ$SHARED_TODAY,
                                             sprintf("gA_Concordance_days_all%s.xlsx",
                                                     lubridate::today())))



##### compare
compare <- gArestricted[gAdif>7, c("gAdif2",
                          "gAdif",
                          "mahima_gestageatbirthwk_1",
                          'hospentdays',
                          "mahima_hospenteredgestage_1",
                          "USorLMPdateCombogAdays",
                          "hospentavgdays",
                          "bw",
                          "merged_is_hosp_gov",
                          "merged_namehospbirth")]

openxlsx::write.xlsx(compare,file.path(org::PROJ$SHARED_TODAY,
                                    sprintf("concordance_problems_%s.xlsx",
                                              lubridate::today())))


#### preterm and post term defs



terms <- gArestricted[gAdif2>7 & 
              USorLMPdateCombogAdays>=224 &
              USorLMPdateCombogAdays<=314 &
              (termcatcombo!=termcathospavg), 
                        c("gAdif2",
                          "gAdif",
                          "termcatcombo",
                          "termcathospavg",
                          "booklmp",
                          "first_1_21_usedd",
                          "mahima_dateofbirth_1",
                          "USorLMPdateCombogAdays",
                          "hospentavgdays",
                          "mahima_gestageatbirthwk_1",
                          "mahima_hospenteredgestage_1",
                          "merged_namehospbirth",
                          "merged_is_hosp_gov",
                          "bw")]

openxlsx::write.xlsx(terms,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("terms3244_%s.xlsx",
                                               lubridate::today())))

nrow(gA[termcatcombo!=termcathospavg])




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
            "mahima_dateofbirth_1",
            "comboUSandLMPgA",
            "mahima_hospenteredgestage_1",
            "gAdif",
            "bw")]


openxlsx::write.xlsx(gAover3weeks,file.path(org::PROJ$SHARED_TODAY,
                                             sprintf("gA_DQ_over3weeks_37_44_%s.xlsx",
                                                     lubridate::today())))


# hospentered
bwstats <-gA[hospentdays>=280 & 
               hospentdays<286 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_hospent_%s.xlsx",
                                               lubridate::today())))

# hospenteredavg
# stats didnt change for sga and lga for this variable- same as hospentered
bwstats <-gA[hospentavgdays>=280 &
               hospentavgdays<=286 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_hospentavg_%s.xlsx",
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


# combo gA
bwstats <-gA[USorLMPdateCombogAdays>=280 & 
               USorLMPdateCombogAdays<=286 &
               bw>0,.(N=.N,
                      meanbw=mean(bw, na.rm=T),
                      sd=sd(bw, na.rm=T),
                      sderror=(sd(bw)/sqrt(sum(!is.na(bw)))),
                      tenthquantile=quantile(bw, probs=0.1, na.rm=T))]

openxlsx::write.xlsx(bwstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("bwstats_combo_%s.xlsx",
                                               lubridate::today())))

# mean bw from 37+0 to 41+6 for the different enterd ones
# proportion of error in each of the categories


###### SGA and LGA proportions  ######
# should probably change bw to be more than 17
###### sga ###### 

#making sgA variables combo days
gA[,sga:=as.logical(NA)]
gA[!is.na(bw) & !is.na(USorLMPdateCombogAdays),sga:=FALSE]

#identifying sgas via combogAs
#37 weeks
gA[sga==FALSE &
         bw>0 & 
         bw<2383 & 
         USorLMPdateCombogAdays>=259 & 
         USorLMPdateCombogAdays<=265, sga:=TRUE]

#38 week
gA[sga==FALSE &
         bw<2538 & 
         USorLMPdateCombogAdays>=266 & 
         USorLMPdateCombogAdays<=272, sga:=TRUE]

#39 weeks
gA[sga==FALSE &
         bw<2684 &
         USorLMPdateCombogAdays>=273 & 
         USorLMPdateCombogAdays<=279, sga:=TRUE]

#40 weeks
gA[sga==FALSE &
         bw<2818 & 
         USorLMPdateCombogAdays>=280 & 
         USorLMPdateCombogAdays<=286, sga:=TRUE]

#41 weeks
gA[sga==FALSE &
         bw<2938 & 
         USorLMPdateCombogAdays>=287 &
         USorLMPdateCombogAdays<=293, sga:=TRUE]


#### sga based on hospital entered gA ####
gA[,sga2:=as.logical(NA)]
gA[!is.na(bw) & !is.na(hospentavgdays),sga2:=FALSE]

#identifying sgas via hospitalentered gA
#37 weeks
gA[sga2==FALSE &
     bw>0 & 
     bw<2379 & 
     hospentavgdays>=259 &
     hospentavgdays<=265, sga2:=TRUE]

#38 week
gA[sga2==FALSE &
     bw<2534 & 
     hospentavgdays>=266 &
     hospentavgdays<=272, sga2:=TRUE]

#39 weeks
gA[sga2==FALSE &
     bw<2679 &
     hospentavgdays>=273 &
     hospentavgdays<=279, sga2:=TRUE]

#40 weeks
gA[sga2==FALSE &
     bw<2813 & 
     hospentavgdays>=280 &
     hospentavgdays<=286,sga2:=TRUE]

#41 weeks
gA[sga2==FALSE &
     bw<2933 & 
     hospentavgdays>=287 &
     hospentavgdays<=293, sga2:=TRUE]



#### sga based on ultrasound entered gA ####
gA[,sga3:=as.logical(NA)]
gA[!is.na(bw) & !is.na(first_1_21_usedd_gA),sga3:=FALSE]

#identifying sgas via ultrasound gA
#37 weeks
gA[sga3==FALSE &
     bw>0 & 
     bw<2394 & 
     first_1_21_usedd_gA>=37 &
     first_1_21_usedd_gA<38, sga3:=TRUE]

#38 week
gA[sga3==FALSE &
     bw<2550 & 
     first_1_21_usedd_gA>=38 &
     first_1_21_usedd_gA<39, sga3:=TRUE]

#39 weeks
gA[sga3==FALSE &
     bw<2696 &
     first_1_21_usedd_gA>=39 &
     first_1_21_usedd_gA<40, sga3:=TRUE]

#40 weeks
gA[sga3==FALSE &
     bw<2831 & 
     first_1_21_usedd_gA>=40 &
     first_1_21_usedd_gA<41, sga3:=TRUE]

#41 weeks
gA[sga3==FALSE &
     bw<2952 & 
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
         bw>3238 & 
         USorLMPdateCombogAdays>=259 &
         USorLMPdateCombogAdays<=265, lga:=TRUE]

#38
gA[lga==FALSE &
         bw>3448 & 
         USorLMPdateCombogAdays>=266 & 
         USorLMPdateCombogAdays<=272, lga:=TRUE]

#39
gA[lga==FALSE &
         bw>3647 & 
         USorLMPdateCombogAdays>=273 &
         USorLMPdateCombogAdays<=279, lga:=TRUE]

#40
gA[lga==FALSE &
         bw>3829 & 
         USorLMPdateCombogAdays>=280 & 
         USorLMPdateCombogAdays<=286, lga:=TRUE]

#41
gA[lga==FALSE &
         bw>3992 &
         USorLMPdateCombogAdays>=287 & 
         USorLMPdateCombogAdays<=293, lga:=TRUE]


#making lgA variables (Hospital entered)
gA[,lga2:=NA]

#everyone who isnt missing gA and weight is given a false
gA[!is.na(bw) & !is.na(mahima_hospenteredgestage_1), lga2:=FALSE]

#identifying lgas
#37
gA[lga2==FALSE &
     bw>3230 & 
     hospentavgdays>=259 &
     hospentavgdays<=265,lga2:=TRUE]

#38
gA[lga2==FALSE &
     bw>3440 & 
     hospentavgdays>=266 &
     hospentavgdays<=272,lga2:=TRUE]

#39
gA[lga2==FALSE &
     bw>3638 & 
     hospentavgdays>=273 &
     hospentavgdays<=279,lga2:=TRUE]

#40
gA[lga2==FALSE &
     bw>3820 & 
     hospentavgdays>=280 &
     hospentavgdays<=286, , lga2:=TRUE]

#41
gA[lga2==FALSE &
     bw>3983 &
     hospentavgdays>=287 &
     hospentavgdays<=293,lga2:=TRUE]


#### lga based on ultrasound entered gA ####
gA[,lga3:=as.logical(NA)]
gA[!is.na(bw) & !is.na(first_1_21_usedd_gA),lga3:=FALSE]

#identifying sgas via ultrasound gA
#37 weeks
gA[lga3==FALSE &
     bw>0 & 
     bw>3258 & 
     first_1_21_usedd_gA>=37 &
     first_1_21_usedd_gA<38, lga3:=TRUE]

#38 week
gA[lga3==FALSE &
     bw>3470 & 
     first_1_21_usedd_gA>=38 &
     first_1_21_usedd_gA<39, lga3:=TRUE]

#39 weeks
gA[lga3==FALSE &
     bw>3669 &
     first_1_21_usedd_gA>=39 &
     first_1_21_usedd_gA<40, lga3:=TRUE]

#40 weeks
gA[lga3==FALSE &
     bw>3853 & 
     first_1_21_usedd_gA>=40 &
     first_1_21_usedd_gA<41, lga3:=TRUE]

#41 weeks
gA[lga3==FALSE &
     bw>4017 & 
     first_1_21_usedd_gA>=41 &
     first_1_21_usedd_gA<42, lga3:=TRUE]



##### lga and sga proportions ####
sgalga <-gA[,.(N=.N,
               notmissbw=sum(!is.na(bw)),
               missingbw=sum(is.na(bw)),
               notmissusorlmpdays=sum(!is.na(USorLMPdateCombogAdays) &
                                        !is.na(bw)),
               sgacomboT=sum(sga==T, na.rm=T),
               sgacomboF=sum(sga==F, na.rm=T),
               missingsga=sum(is.na(sga)),
               notmissgAent=sum(!is.na(mahima_hospenteredgestage_1) &
                                  !is.na(bw)),
               sgaenteredT=sum(sga2==T, na.rm=T),
               sgaenteredF=sum(sga2==F, na.rm=T),
               missingsgaentered=sum(is.na(sga2)),
               notmissuseddbw=sum(!is.na(bw) & 
                                    !is.na(first_1_21_usedd_gA)),
               sgaUST=sum(sga3==T, na.rm=T),
               sgaUSF=sum(sga3==F, na.rm=T),
               missingsgaUS=sum(is.na(sga3)),
               lgaT=sum(lga==T, na.rm=T),
               lgaF=sum(lga==F, na.rm=T),
               missingsga=sum(is.na(lga2)),
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
library(ggplot2)


#################################################
# preterm from eReg data and term at hospentby bw
#################################################


tab <- gA[(comboUSandLMPgA>=33 &
             comboUSandLMPgA<37) &
            (mahima_hospenteredgestage_1>=37 &
               mahima_hospenteredgestage_1<43) &
            bw>1000,c("mahima_hospenteredgestage_1", 
             "comboUSandLMPgA",
             "bw")]

tabb <- melt.data.table(tab, id.vars = "bw")

setnames(tabb,c("variable",
                "value"),
         c("gestagecat",
           "gestage"))


tabb[gestagecat=="mahima_hospenteredgestage_1", gestagecat:="Hospital delivery units"]
tabb[gestagecat=="comboUSandLMPgA", gestagecat:="Best Estimate"]


p <- ggplot(tabb,
                aes(x=gestage, y=bw))

# Change point shapes and colors by groups

p <- p + geom_point(aes(x=gestage,
                        y=bw, 
                        shape=gestagecat,
                        color = gestagecat), size = 3)


p <- p + scale_x_continuous(name = "Gestational Age (weeks)", breaks = seq(30, 45, by = 1)) 
  
p <- p + scale_y_continuous(name = "Birth weight (grams)", breaks = seq(1500, 5000, by = 500))

p <- p + theme(legend.title = element_blank())



p


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "eReg Preterm and Term hosp by bw.png"
), plot = p, width = 297, height = 210, unit = "mm")





#at 24 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))

p <- ggplot(gA[bw>10 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(168,174))
p <- p + theme_grey (base_size = 16)
p10 <- 552
p90 <- 750

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_24 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 25 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(175,181))
p <- p + theme_grey (base_size = 16)
p10 <- 644
p90 <- 876

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_25 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 26 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(182,188))
p <- p + theme_grey (base_size = 16)
p10 <- 747
p90 <- 1015

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_26 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 27 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(189,195))
p <- p + theme_grey (base_size = 16)
p10 <- 860
p90 <- 1169

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_27 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 28 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(196,202))
p <- p + theme_grey (base_size = 16)
p10 <- 983
p90 <- 1336

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_28 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



#at 29 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(203,209))
p <- p + theme_grey (base_size = 16)
p10 <- 1116
p90 <- 1516

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_29 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 30 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(210,216))
p <- p + theme_grey (base_size = 16)
p10 <- 1258
p90 <- 1709

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_30 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


#at 31 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(217,223))
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
                 comboUSandLMPgA<308], 
            aes(x=comboUSandLMPgA, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(224,230))
p <- p + theme_grey (base_size = 16)
p10 <- 1564
p90 <- 2125

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_32 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")




#at 33 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=USorLMPdateCombogAdays, y=bw))
#p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(231,237))
p <- p + theme_grey (base_size = 16)
p10 <- 1726
p90 <- 2345

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")


ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_33 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 34 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("Calculated Gestational Ages", lim=c(238,244))
p <- p + theme_grey (base_size = 16)

p10 <- 1891
p90 <- 2569

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

p

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_34 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 35 Weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("35 weeks", lim=c(245,251))
p <- p + theme_grey (base_size = 16)

p <- p + theme(axis.text.x=element_blank())

p10 <- 2057
p90 <- 2795

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_35 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")



# 36 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("36 Weeks", lim=c(252,258))
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x=element_blank())


p10 <- 2222
p90 <- 3019

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_36 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 37 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("37 Weeks", lim=c(259,265))
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x=element_blank())


p10 <- 2383
p90 <- 3238

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_37 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 38 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("38 Weeks", lim=c(266,272))
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x=element_blank())


p10 <- 2538
p90 <- 3448

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_38 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 39 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("39 Weeks", lim=c(273,279))
p <- p + theme(axis.text.x=element_blank())


p10 <- 2684
p90 <- 3647

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_39 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 40 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("40 Weeks", lim=c(280,286))
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x=element_blank())


p10 <- 2818
p90 <- 3829

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_40 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")


# 41 weeks
p <- ggplot(gA[bw>0 & bw<6000 & 
                 USorLMPdateCombogAdays>0 &
                 USorLMPdateCombogAdays<308], 
            aes(x=USorLMPdateCombogAdays, y=bw))
p <- p + geom_point()
p <- p + labs(title="Gestational age vs. Birthweight",
              xlab="Calculated Gestational Ages",
              ylab="Birth Weight")
p <- p + scale_x_continuous("41 Weeks", lim=c(287,293)) 
p <- p + theme_grey (base_size = 16)
p <- p + theme(axis.text.x=element_blank())



p10 <- 2938
p90 <- 3992

p <- p + geom_hline(yintercept = p10, color="red")
p <- p + geom_hline(yintercept = p90, color="red")

ggsave(file.path(
  org::PROJ$SHARED_TODAY,
  "BW_41 weeks.png"
), plot = p, width = 297, height = 210, unit = "mm")





################ gA Paper Distribution Graphs ################ 
ExtractOnlyEnglishLetters <- function(var){
  unlist(lapply(stringr::str_extract_all(stringr::str_to_lower(var),"[a-z]"),paste0,collapse=""))
}
gA[,merged_pregoutcome:=ExtractOnlyEnglishLetters(merged_pregoutcome)]
xtabs(~gA$merged_pregoutcome, addNA=T)


#dont need an id.vars here because not retaining information for each woman
tab <- gA[merged_pregoutcome %in% c("live",
                                    "livebirth",
                                    "alive"),
                    c("hospentavgdays", 
             "USorLMPdateCombogAdays",
             "first_1_21_usedd_days")]


nrow(tab)

long <- melt.data.table(tab)
head(long)

#ran this and got 3 levels, so must do levels
long$variable

#see the levels, which one comes first so we can rename them accordingly
levels(long$variable)
levels(long$variable) <-c("Entered by HCP",
                          "Best Estimate",
                          "Ultrasound")



long <- long[!is.na(value)]
long[,category:=cut(value,
                    breaks=c(-300,0,167,195,230,258,294,314,50000),
                    include.lowest=T)]

xtabs(~long$category, addNA = T)
levels(long$category) <- c("<=0",
                           "1-23",
                           "24-27",
                           "28-32",
                           "33-36",
                           "37-41",
                           "42-44",
                            ">44")

levels(long$category)

xtabs(~long$category, addNA = T)

library(ggplot2)
# 
# 
# #if restrict more rows, cant produce both continous and categorical
# p <- ggplot(long[value<350 & value>0], aes(x=value, fill=variable)) 
# p <- p + geom_density(alpha=0.3)
# p <- p + labs(title="Distribution of Gestational Age by Source",
#               caption = "Outliers Removed") +
#   xlab("Days") +
#   ylab("Density")
# p <- p + scale_x_continuous("Weeks", lim=c(252, 308))
# p <- p + scale_fill_brewer("Gestational Age Source", palette ="Dark2")
# #centers title 
# p <- p + theme(plot.title = element_text(hjust = 0.5))
# p <- p + theme(text = element_text(size=24))
# 
# p
# 
# # ggsave(file.path(
# #   org::PROJ$SHARED_TODAY,
# #   "GA_Paper_Distribution_Adjusted_36_onwards.png"
# # ), plot = p, width = 297, height = 210, unit = "mm")
# 
# 
# ggsave(file.path(
#   org::PROJ$SHARED_TODAY,
#   "GA_Paper_Distribution_Adjusted_36_onwards.png"
# ), plot = p, width = 150, height = 105, unit = "mm")
# 
# 
# #####Non-Adjusted Distribution#####
# #if restrict more rows, cant produce both continous and categorical
# p <- ggplot(long[value<300], aes(x=value, fill=variable)) 
# p <- p + geom_density(alpha=0.3)
# p <- p + labs(title="Distribution of Gestational Age by Source",
#               caption= "Outliers Not Removed") +
#   xlab("Weeks")
# p <- p + scale_x_continuous("Weeks", lim=c(224, 308))
# p <- p + scale_y_continuous("Frequency", labels=scales::percent)
# p <- p + scale_fill_brewer("Gestational Age Source", palette ="Set1")
# #centers title 
# p <- p + theme(plot.title = element_text(hjust = 0.5))
# p <- p + theme(text = element_text(size=24))
# 
# p
# 
# ggsave(file.path(
#   org::PROJ$SHARED_TODAY,
#   "GA_Paper_Distribution_NON_Adjusted.png"
# ), plot = p, width = 297, height = 210, unit = "mm")
# 


uglytable <- long[,.(
  N=.N),
  keyby=.(
    variable,
    category
    
  )]

uglytable[category=="24-27", category:="Extremely Preterm"]
uglytable[category=="28-32", category:="Very Preterm"]
uglytable[category=="33-36", category:="Preterm"]
uglytable[category=="37-41", category:="Term"]
uglytable[category=="42-44", category:="Post term"]

xtabs(~uglytable$category, addNA=T)

#####FIX THIS######
#creating denominator
#run bottom code when tells you reached elapsed time limit
#dev.off(
#ggsave with the restrictions helps save it in higher resolution

#uglytable <- long[!is.na(category),.(N=.N),keyby=.(variable,category)]

####Bar Graph with restriction###
###Removing outliers...other values that arent possible
uglytable[,percentage:=round(100*N/sum(N),digits=1),by=.(variable)]
#uglytable[,percentage:=paste(percentage,"%")]

p <- ggplot(uglytable[!category %in% c("<=0","1-23",">44", NA)], 
            aes(x= category, 
                y=percentage, 
                fill=variable))

p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Frequency (%)")

#p <- p + scale_y_continuous("Frequency",label=scales::percent)
p <- p + labs(title="Distribution of Gestational Age by Category")
p <- p + geom_text(aes(label = paste(percentage,"%")), 
                   size=3.0,
                   vjust = -0.5,
                   position=position_dodge(width=1))

p <- p + theme(text = element_text(size=20))


p <- p + theme(axis.text.x=element_text(angle=45,hjust=1))


p 

# 
# #ggsave with the restrictions helps save it in higher resolution
 ggsave(file.path(
   org::PROJ$SHARED_TODAY,
   "GA_Paper_bargraph.png"
 ), plot = p, width = 297, height = 210, unit = "mm")

#########################################
###### Preterm only as one category###### 
#########################################
 
 long[,cat:=cut(value,
                     breaks=c(-300,0,167,195,258,294,314,50000),
                     include.lowest=T)]
 
 xtabs(~long$cat, addNA = T)
 
 levels(long$cat) <- c("<=0",
                            "1-23",
                            "24-27",
                            "28-36",
                            "37-41",
                            "42-44",
                            ">44")
 
 levels(long$cat)
 
 xtabs(~long$cat, addNA = T) 
 
 uglytable <- long[,.(
   N=.N),
   keyby=.(
     variable,
     cat
     
   )]
 
 uglytable[cat=="28-36", cat:="Preterm"]
 uglytable[cat=="37-41", cat:="Term"]
 uglytable[cat=="42-44", cat:="Post term"]
 
 xtabs(~uglytable$cat, addNA=T)
 
 #####FIX THIS######
 #creating denominator
 #run bottom code when tells you reached elapsed time limit
 #dev.off(
 #ggsave with the restrictions helps save it in higher resolution
 
 #uglytable <- long[!is.na(category),.(N=.N),keyby=.(variable,category)]
 
 ####Bar Graph with restriction###
 ###Removing outliers...other values that arent possible
 uglytable[,percentage:=round(100*N/sum(N),digits=1),by=.(variable)]
 #uglytable[,percentage:=paste(percentage,"%")]
 
 p <- ggplot(uglytable[!cat %in% c("<=0","1-23","24-27",">44", NA)], 
             aes(x= cat, 
                 y=percentage, 
                 fill=variable))
 
 p <- p + geom_col(position="dodge", alpha=0.75)
 p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
 p <- p + scale_x_discrete("")
 p <- p + scale_y_continuous("Frequency (%)")
 
 #p <- p + scale_y_continuous("Frequency",label=scales::percent)
 p <- p + labs(title="Distribution of Gestational Age by Category")
 p <- p + geom_text(aes(label = paste(percentage,"%")), 
                    size=3.0,
                    vjust = -0.5,
                    position=position_dodge(width=1))
 
 p <- p + theme(text = element_text(size=20))
 
 
 p <- p + theme(axis.text.x=element_text(angle=45,hjust=1))
 
 
 p 
 
 # 
 # #ggsave with the restrictions helps save it in higher resolution
 ggsave(file.path(
   org::PROJ$SHARED_TODAY,
   "GA_Paper_bargraph_preterm.png"
 ), plot = p, width = 297, height = 210, unit = "mm")
 
 



##############################################################################
sink()

sink(file.path("C:/data processing/projects_that_will_be_published_on_github/results","crosstabs.txt"))

#qqplots for normality
#huge outlier at 4,000, so we should get rid of it
qqnorm(gA$gAdif2); qqline(gA$gAdif2, col = 2)


unique(gA$first_1_21_usedd_gA)
#1. qqplots, crazy distributions so do the nonparametric analysis
#not normal distribution, so will have to do nonparametric testing
#qqnorm(gA[abs(first_1_21_usedd_gA)<100]$difference_us)
#qqline(analysisDatasetUSgA[abs(first_1_21_usedd_gA)<100]$difference_us, col = 2)

#2.nonparametic
#true location is not equal to zero
#since not significant
wilcox.test(gA$gAdif2)

#since they arent significant, we can present summary statistics of the data

# creating analysis variables
gA[,difference_us := first_1_21_usedd_days - hospentdays]

# paired t.test
t.test(gA$difference_us)

#equivalence test
#two one sided test, how much do we really care about for them to be the same?
#ex: test 1 is the difference than -10?
#dont have equivalence with a region of equivalence of +2 and -2
#(in our data set because 90% cI is -8.9 to 0.03)
t.test(gA$difference_us, conf.level = 0.9)

#paired t.test excluding outliers
unique(gA$mahima_hospenteredgestage_1)

# linear regression
fit <- lm(gAdif2 ~ merged_is_hosp_gov, data=gA)
# (Intercept) is when hospital is private. It says "US estimate is 11 days earlier than entered"
# merged_is_hosp_govTRUE is the difference between private and government estimates. it is NOT signifant, because the p-value is 0.13!!
# this means we CANNOT interpret private results and government results separately
summary(fit)


####Table 3: CHIsqtest by ultrasound and then LMP
#used wider categories to decrease lower numbers in categories
#tried the chi.sq test which is an estimation but it might be incorrect
#so we constrained the limits further by merging categories with small numbers
#so tried fishers exact test and bc sample size is too large so it didnt work

xtabs(~hospentcats+termcats,data=gA)
chisq.test(xtabs(~termcats+hospentcats,data=gA[!is.na(hospentdays) & 
                                             !is.na(USorLMPdateCombogAdays) & 
                                               USorLMPdateCombogAdays<=308 & 
                                                 hospentdays<=308]))


xtabs(~termcats+uscat,data=gA)
chisq.test(xtabs(~termcats+uscats,data=gA))


xtabs(~hospentcats+uscats,data=gA)
chisq.test(xtabs(~hospentcats+uscats,data=gA))

# we pull ou thte 3 variables that we care about
#should we add is hosp gov variable here too?
long <- gA[,c(
  "first_1_21_usedd_days",
  "USorLMPdateCombogAdays",
  "hospentdays"
)]
# i create my own id variable (per row number)
long[,id:=1:.N]
# reshape to long
long <- melt.data.table(long, id.vars="id")
xtabs(~long$variable)

# mixed effects linear regression with random effect for person
# this takes into account the 'paired nature' of the data
# (i.e. multiple observations per woman/pregnancy)
summary(lme4::lmer(value ~ variable + (1|id), data=long))
summary(lme4::lmer(value ~ variable + (1|id), data=long[value >= 0 & 
                                                          value <= 315]))
# removing outliers

# this is a normal linear regression
# (but not appropriate due to the paired nature)
# outcome ~ exposure
summary(lm(value ~ variable, data=long))
# remember to look at the overall pvalue 
# for each of the variables first!
# lm generally just reports pair-wise comparisons
# which are a second step in the analyses
anova(lm(value ~ variable, data=long))




sink()


### 




### remove negative values in chi square





















