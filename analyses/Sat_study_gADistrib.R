# Satisfaction study gA distributions as of today
# change this to TRUE if you want to run for gaza
IS_GAZA <-F

# check al shuyukh clinic
###### SETUP STARTS ######
setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=IS_GAZA)


###### SETUP ENDS ######

## LOAD DATA
if(IS_GAZA){
  d <- LoadDataFileFromNetworkGaza()
  
  fileTag <- "gaza"
  
  
} else {
  d <- LoadDataFileFromNetworkWB() 
  
  fileTag <- "wb"
  
}


satGA <- d[bookdate>="2019-07-01"& ident_TRIAL_2_and_3==T & is.na(cpopregoutcome_1),]

# Calculating gA

## LMP
satGA[,gA_todaylmp:=round(as.numeric(
  difftime(lubridate::today(),booklmp, units="days")))]

nrow(satGA[!is.na(booklmp)])
nrow(satGA[!is.na(gA_todaylmp)])

# US
satGA[,estimatedgest_1:=round(as.numeric(
  difftime(lubridate::today(),usdate_1, units="days")))]
xtabs(~satGA$estimatedgest_1)

satGA[,usgestage_1days:=usgestage_1*7]

satGA[,gA_todayus_1:=estimatedgest_1 + usgestage_1days]

gAdist <- satGA[,.(N=.N,
                   LMP38wks=sum((gA_todaylmp>=266 & gA_todaylmp<=272)| 
                                (gA_todayus_1>=266 & gA_todayus_1<=272), na.rm=T),
                   LMP37wks=sum((gA_todaylmp>=259 & gA_todaylmp<=265)|
                                  (gA_todayus_1>=259 & gA_todayus_1<=265), na.rm=T),
                   LMP36wks=sum((gA_todaylmp>=252 & gA_todaylmp<=258)|
                                  (gA_todayus_1>=252 & gA_todayus_1<=258), na.rm=T),
                   LMP35wks=sum((gA_todaylmp>=245 & gA_todaylmp<=251)|
                                  (gA_todayus_1>=245 & gA_todayus_1<=251), na.rm=T),
                   LMP34wks=sum((gA_todaylmp>=238 & gA_todaylmp<=244)|
                                  (gA_todayus_1>=238 & gA_todayus_1<=244), na.rm=T),
                   LMP33wks=sum((gA_todaylmp>=231 & gA_todaylmp<=237)|
                                 (gA_todayus_1>=231 & gA_todayus_1<=237), na.rm=T),
                   LMP32wks=sum((gA_todaylmp>=224 & gA_todaylmp<=230)|
                                  (gA_todayus_1>=224 & gA_todayus_1<=230),na.rm=T),
                   LMP31wks=sum((gA_todaylmp>=217 & gA_todaylmp<=223)|
                                  (gA_todayus_1>=217 & gA_todayus_1<=223), na.rm=T),
                   LMP30wks=sum((gA_todaylmp>=210 & gA_todaylmp<=216)|
                                  (gA_todayus_1>=210 & gA_todayus_1<=216), na.rm=T)),
            
                keyby=.(str_TRIAL_2_ClusSize,str_TRIAL_2_Cluster)]



openxlsx::write.xlsx(gAdist, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_satGAdistrib_wb.xlsx",
                               lubridate::today())))


openxlsx::write.xlsx(gAdist, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "satisfaction",
                       sprintf("%s_satGAdistrib_gaza.xlsx",
                               lubridate::today(),fileTag)))
