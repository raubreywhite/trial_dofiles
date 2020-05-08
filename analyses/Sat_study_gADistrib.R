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
  
  satGA <- d[bookdate>="2019-07-01" & 
               is.na(cpopregoutcome_1) &
               ident_dhis2_booking==T & 
               (ident_TRIAL_2_and_3==T),]
  
  
} else {
  d <- LoadDataFileFromNetworkWB() 
  
  fileTag <- "wb"
  
  satGA <- d[bookdate>="2019-07-01" & 
               is.na(cpopregoutcome_1) &
               ident_dhis2_booking==T & 
               (ident_TRIAL_2_and_3==T) &
               (mobile!="" | phone!=""),]
  
}




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

gAdist <- satGA[,.(
                   LMP38wks0_3days=sum((gA_todaylmp>=266 & gA_todaylmp<=272)| 
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
                                  (gA_todayus_1>=210 & gA_todayus_1<=216), na.rm=T),
                   LMP29wks=sum((gA_todaylmp>=203 & gA_todaylmp<=209)|
                                  (gA_todayus_1>=203 & gA_todayus_1<=209), na.rm=T),
                   
                   LMP28wks=sum((gA_todaylmp>=196 & gA_todaylmp<=202)|
                                  (gA_todayus_1>=196 & gA_todayus_1<=202), na.rm=T),
                   
                   LMP27wks=sum((gA_todaylmp>=189 & gA_todaylmp<=195)|
                                  (gA_todayus_1>=189 & gA_todayus_1<=195), na.rm=T),
                   
                   LMP26wks=sum((gA_todaylmp>=182 & gA_todaylmp<=188)|
                                  (gA_todayus_1>=182 & gA_todayus_1<=188), na.rm=T),
                   
                   LMP25wks=sum((gA_todaylmp>=175 & gA_todaylmp<=181)|
                                  (gA_todayus_1>=175 & gA_todayus_1<=181), na.rm=T),
                   
                   LMP24wks=sum((gA_todaylmp>=168 & gA_todaylmp<=174)|
                                  (gA_todayus_1>=168 & gA_todayus_1<=174), na.rm=T),
                   
                   LMP23wks=sum((gA_todaylmp>=161 & gA_todaylmp<=167)|
                                  (gA_todayus_1>=161 & gA_todayus_1<=167), na.rm=T)),
            
                keyby=.(str_TRIAL_2_ClusSize,str_TRIAL_2_Cluster)]



openxlsx::write.xlsx(gAdist, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "satisfaction",
                       sprintf("%s_satGAdistrib_wb_withphone.xlsx",
                               lubridate::today())))


openxlsx::write.xlsx(gAdist, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "satisfaction",
                       sprintf("%s_satGAdistrib_gaza.xlsx",
                               lubridate::today(),fileTag)))
