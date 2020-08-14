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

gA <- d[ident_TRIAL_1==T & bookdate >= "2017-01-15"&
          bookdate<="2017-09-15" &
          (matching=="Avicenna"|
             matching=="Governmental"|
             matching=="PaperHBO"|
             matching=="Private"),]


################ END  ################



################  Creating new vars   ################


# USorLMPdatecombogAdays variable (put in creating further variables)
gA[,USorLMPdateCombogAdays:= as.numeric(difftime(
  mahima_dateofbirth_1,
  USorLMPdate,
  units ="days"))]




# hospital entered gestaational age in days with avg value
gA[,hospentavgdays:=3+mahima_hospenteredgestage_1*7]
gA[,hospentdays:=mahima_hospenteredgestage_1*7]



# termcats
gA[,termcats:=cut(USorLMPdateCombogAdays,
                  breaks=c(-300,0,140,167,230,258,300,308,50000),
                  include.lowest=T)]
# xtabs(~gA$termcats, addNA=T)


## making term variables
gA[,termcatcombo:=as.character(NA)]
gA[USorLMPdateCombogAdays>=259 & 
     USorLMPdateCombogAdays<=300, termcatcombo:="term"]
gA[USorLMPdateCombogAdays>=168 & 
     USorLMPdateCombogAdays<=258, termcatcombo:="preterm"]
gA[USorLMPdateCombogAdays>=301 & 
     USorLMPdateCombogAdays<=308, termcatcombo:="postterm"]

gA[,termcathospavg:=as.character(NA)]
gA[hospentavgdays>=259 & 
     hospentavgdays<=300, termcathospavg:="term"]
gA[hospentavgdays>=168 & 
     hospentavgdays<=258, termcathospavg:="preterm"]
gA[hospentavgdays>=301 & 
     hospentavgdays<=308, termcathospavg:="postterm"]




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


# only do results for people with both hospentered and usorlmpcombo
gA <- gA[!is.na(USorLMPdateCombogAdays) & !is.na(hospentavgdays)]

library("e1071")
# statistical  info

gAstats <- gA[(USorLMPdateCombogAdays<=308 & 
                 USorLMPdateCombogAdays>=168) & 
                (hospentdays>=168 & hospentdays<=308),
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
                SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                MedianHospgAavg=median(hospentavgdays, na.rm=T),
                SDHospgAavg=sd(hospentavgdays, na.rm=T),
                KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
              )]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_24_44_hospentandcombo_%s.xlsx",lubridate::today())))


gAstats <- gA[(USorLMPdateCombogAdays<=308 & 
                 USorLMPdateCombogAdays>=168) & 
                (mahima_gestageatbirthwk_1<=44 & mahima_gestageatbirthwk_1>=24) &
                (first_1_21_usedd_gA<=44 & first_1_21_usedd_gA>=24) &
                (mahima_hospenteredgestage_1>=24 & 
                   mahima_hospenteredgestage_1<=44),
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
                SkewHospgA=skewness(mahima_hospenteredgestage_1, na.rm=T),
                MeanHospgAavg=mean(hospentavgdays, na.rm=T),
                MedianHospgAavg=median(hospentavgdays, na.rm=T),
                SDHospgAavg=sd(hospentavgdays, na.rm=T),
                KurtHospgAavg=kurtosis(hospentavgdays,na.rm=T),
                SkewHospgAavg =skewness(hospentavgdays, na.rm=T)
              )]

openxlsx::write.xlsx(gAstats,file.path(org::PROJ$SHARED_TODAY,
                                       sprintf("gA_Stats_24_44_ALL_%s.xlsx",lubridate::today())))



# term categorical data based on variable
gAtermcounts <- gA[,.(N=.N,
                      LMPgA=sum(!is.na(mahima_gestageatbirthwk_1)),
                      UsGA=sum(!is.na(first_1_21_usedd_gA)),
                      CombogA=sum(!is.na(USorLMPdateCombogAdays)),
                      HospentgA=sum(!is.na(hospentdays))),
                   keyby=.(termcatcombo)]

openxlsx::write.xlsx(gAtermcounts,file.path(org::PROJ$SHARED_TODAY,
                                            sprintf("gA_TermCounts_24_42_%s.xlsx",
                                                    lubridate::today())))

# birth type
gAtype <- gA[USorLMPdateCombogAdays<=308 & 
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
CPO <- gA[USorLMPdateCombogAdays<=308 & 
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




### concordance in weeks ###
#gA[,comboUSandLMPgA_rounded:= floor(comboUSandLMPgA)]

gA[,gAdif:=abs((USorLMPdateCombogAdays-hospentavgdays))]
xtabs(~gA$gAdif, addNA=T)

gAconcordance <- gA[(USorLMPdateCombogAdays<=308 & 
                       USorLMPdateCombogAdays>=168) &
                      !is.na(gAdif),.(N=.N,
                                      noweekdiff=sum(gAdif==0, na.rm=T),
                                      oneweekdif=sum(abs(gAdif)>=1 &
                                                       abs(gAdif)<7, na.rm=T),
                                      twoweekdiff=sum(abs(gAdif)>=7 &
                                                        abs(gAdif)<14, na.rm=T),
                                      morethan1wkdif=sum(abs(gAdif)>=7, na.rm=T))]

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
               "merged_datedeliv",
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
gA[,gAdif2:=abs((hospentdays-USorLMPdateCombogAdays))]
xtabs(~gA$gAdif2, addNA=T)

gAconcordance <- gA[(USorLMPdateCombogAdays<=314 & 
                       comboUSandLMPgA>=168) &
                      !is.na(gAdif2),.(N=.N,
                                       noweekdiff=sum(gAdif2==0, na.rm=T),
                                       oneweekdif=sum(abs(gAdif2)>=1 &
                                                        abs(gAdif2)<7, na.rm=T),
                                       twoweekdiff=sum(abs(gAdif2)>=7 &
                                                         abs(gAdif2)<14, na.rm=T),
                                       morethan1wkdif=sum(abs(gAdif2)>=7, na.rm=T)
                      )]

openxlsx::write.xlsx(gAconcordance,file.path(org::PROJ$SHARED_TODAY,
                                             sprintf("gA_Concordance_days_24_44_%s.xlsx",
                                                     lubridate::today())))


##### compare
compare <- gA[gAdif>7, c("gAdif2",
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
terms <- gA[gAdif2>7 & 
              USorLMPdateCombogAdays>=224 &
              USorLMPdateCombogAdays<=308 &
              (termcatcombo!=termcathospavg), 
            c("gAdif2",
              "gAdif",
              "termcatcombo",
              "termcathospavg",
              "booklmp",
              "first_1_21_usedd",
              "merged_datedeliv",
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
                     "merged_datedeliv",
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
     bw>4024 &
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


#at 24 weeks
#dev.off() try to run this code if get weird graphic errors
p <- ggplot(gA, 
            aes(x=comboUSandLMPgAdays, y=bw))

p <- ggplot(gA[bw>10 & bw<6000 & 
                 comboUSandLMPgAdays>0 &
                 comboUSandLMPgAdays<308], 
            aes(x=comboUSandLMPgAdays, y=bw))
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
tab <- gA[,c("hospentavgdays", "USorLMPdateCombogAdays")]

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
                    breaks=c(-300,0,168,230,258,300,308,5000),
                    include.lowest=T)]

xtabs(~long$category, addNA = T)
levels(long$category) <- c("<=0",
                           "1-24",
                           "24-32",
                           "33-36",
                           "37-42",
                           "43-44",
                           ">44")

levels(long$category)

xtabs(~long$category, addNA = T)

library(ggplot2)

#if restrict more rows, cant produce both continous and categorical
p <- ggplot(long[value<350 & value>0], aes(x=value, fill=variable)) 
p <- p + geom_density(alpha=0.3)
p <- p + labs(title="Distribution of Gestational Age by Source",
              caption = "Outliers Removed") +
  xlab("Days") +
  ylab("Density")
p <- p + scale_x_continuous("Weeks", lim=c(252, 308))
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
p <- p + scale_x_continuous("Weeks", lim=c(224, 308))
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

uglytable <- long[,.(N=.N),keyby=.(variable,category)]

####Bar Graph with restriction###
###Removing outliers...other values that arent possible
uglytable[,percentage:=N/sum(N),by=.(variable)]
p <- ggplot(uglytable[!category %in% c("<=0",">44")], 
            aes(x= category, 
                y=percentage, 
                fill=variable))

p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Gestational Age Source", palette="Set1")
p <- p + scale_x_discrete("Days")
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
  "GA_Paper_bargraph.png"
), plot = p, width = 297, height = 210, unit = "mm")























