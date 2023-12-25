

###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

#Setup(IS_GAZA=FALSE)
#Setup(IS_GAZA=TRUE)


###### SETUP ENDS ######
if(IS_GAZA==F){
  
  ar <- readRDS(file.path(FOLDER_DATA_CLEAN,"annual reports","annualreportdata.RDS"))
  
}else {
  
  ar <- readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,"annual reports","annualreportdata.RDS"))
  
  # reassign this folder directory so we dont have to change the code below
  FOLDER_DATA_RESULTS <<- FOLDER_DATA_RESULTS_GAZA
  
}

# when data is rerun
#ar <- smallD


# identify year for saving the stuff in

year_folder <- 2022

########## 
# demo
########## 

ar[bookgestage>40, bookgestage:=as.integer(NA)]

if(IS_GAZA){
  
  #BMI
  ar[bookheight!=0 & 
       bookweight!=0,
     bookbmi:=bookweight/(bookheight^2)]
  ar[,bookbmicat:=cut(bookbmi,
                      breaks=c(0,18.4,24.9,29.9,99999),
                      include.lowest=T)]
}

ar[is.infinite(bookbmi), bookbmi:=as.integer(NA)]
xtabs(~ar$bookbmicat,addNA=T)

##making a table for data we want to analyze from the analysis data set
##for things like parity, make an ugly table because its not a box plot
##or histogram, so better to make a ugly table.

if(IS_GAZA==F){
  tab <- ar[ident_dhis2_booking==1,
            .("Mean Age"=mean(age, na.rm=T),
              "Mean Age First Pregnancy"=mean(agepregnancy, na.rm=TRUE),
              "Mean Age at Marriage"=mean(agemarriage, na.rm=T),
              "Mean Average Monthly Income"= mean(avgincome, na.rm=T),
              "Mean Education"= mean(education, na.rm=T),
              "Mean Book Weight"= mean(bookweight, na.rm=T),
              "Mean Book Height"=mean(bookheight, na.rm=T),
              "Mean BMI"=mean(bookbmi, na.rm=T),
              "Mean Systolic BP at booking"= mean(bookbpsyst, na.rm=T),
              "Mean Diastolic BP at Booking"=mean(bookbpdiast, na.rm=T),
              "Mean Hb at Booking"= mean(labhb_1, na.rm=T),
              "Mean Income"=mean(income, na.rm=T),
              "Proportion NOt missing Book HB"=sum(!is.na(labhb_1)),
              "Proprtion Weights 0"= mean(bookweight==0, na.rm=T),
              "Proportion of weights over 100 KG"= mean(bookweight>100, na.rm=T),
              "Proprtion of Parity"= mean(bookparity, na.rm=T),
              "Number book parity"=sum(bookparity==1, na.rm=T),
              "Mean Bookgestage"= mean(bookgestage, na.rm=T),
              "Proprtion Primi"=mean(bookprimi, na.rm=T),
              "NUmber of Primi at booking"=sum(bookprimi==1, na.rm=T),
              "Number of Perinatal Death"=sum(bookhistperi, na.rm=T),
              "Number Uterine Surgery at Booking"=sum(bookhistutesur==1, na.rm=T),
              "Number History of C-section"=sum(bookhistcs==1, na.rm=T),
              "Number History of CS complications"=sum(bookhistcscompl==1, na.rm=T),
              "Number History of Preterm Birth"=sum(bookhistpreterm, na.rm=T),
              "Number History of Ute"=sum(bookhistute==1, na.rm=T),
              "Number History of Abortion"=sum(bookhistabort==1, na.rm=T),
              "Number History of DM in Family" =sum(bookfamdm==1, na.rm=T),
              "Number History of HTN in Family"=sum(bookfamhtn==1, na.rm=T),
              "Number History of APH"=sum(bookhistaph==1, na.rm=T),
              "Number of Women with History of Clexane Use"=sum(bookhistclex==1, na.rm=T),
              "Number History of GDM"=sum(bookhistgdm==1, na.rm=T),
              "Number History of GHTN"=sum(bookhistghtn==1, na.rm=T),
              "Number History of Preterm Birth"=sum(bookhistpreterm==1, na.rm=T)), 
            keyby=.(bookyear)]
  
  
  
  openxlsx::write.xlsx(tab,
                       file.path(
                         FOLDER_DATA_RESULTS,
                         "annual reports",
                         paste0(year_folder),
                         "BackgroundAndHistory.xlsx"))
  
} else {
  
  
  tab <- ar[ident_dhis2_booking==1,
            .("Mean Age"=mean(age, na.rm=T),
              "Mean Age First Pregnancy"=mean(agepregnancy, na.rm=TRUE),
              "Mean Age at Marriage"=mean(agemarriage, na.rm=T),
              "Mean Education"= mean(education, na.rm=T),
              "Mean Book Weight"= mean(bookweight, na.rm=T),
              "Mean Book Height"=mean(bookheight, na.rm=T),
              "Mean BMI"=mean(bookbmi, na.rm=T),
              "Mean Systolic BP at booking"= mean(bookbpsyst, na.rm=T),
              "Mean Diastolic BP at Booking"=mean(bookbpdiast, na.rm=T),
              "Mean Hb at Booking"= mean(labhb_1, na.rm=T),
              "Mean Income"=mean(income, na.rm=T),
              "Proportion NOt missing Book HB"=sum(!is.na(labhb_1)),
              "Proprtion Weights 0"= mean(bookweight==0, na.rm=T),
              "Proportion of weights over 100 KG"= mean(bookweight>100, na.rm=T),
              "Proprtion of Parity"= mean(bookparity, na.rm=T),
              "Number book parity"=sum(bookparity==1, na.rm=T),
              "Mean Bookgestage"= mean(bookgestage, na.rm=T),
              "Proprtion Primi"=mean(bookprimi, na.rm=T),
              "NUmber of Primi at booking"=sum(bookprimi==1, na.rm=T),
              "Number of Perinatal Death"=sum(bookhistperi, na.rm=T),
              "Number Uterine Surgery at Booking"=sum(bookhistutesur==1, na.rm=T),
              "Number History of C-section"=sum(bookhistcs==1, na.rm=T),
              "Number History of CS complications"=sum(bookhistcscompl==1, na.rm=T),
              "Number History of Preterm Birth"=sum(bookhistpreterm, na.rm=T),
              "Number History of Ute"=sum(bookhistute==1, na.rm=T),
              "Number History of Abortion"=sum(bookhistabort==1, na.rm=T),
              "Number History of DM in Family" =sum(bookfamdm==1, na.rm=T),
              "Number History of HTN in Family"=sum(bookfamhtn==1, na.rm=T),
              "Number History of APH"=sum(bookhistaph==1, na.rm=T),
              "Number of Women with History of Clexane Use"=sum(bookhistclex==1, na.rm=T),
              "Number History of GDM"=sum(bookhistgdm==1, na.rm=T),
              "Number History of GHTN"=sum(bookhistghtn==1, na.rm=T),
              "Number History of Preterm Birth"=sum(bookhistpreterm==1, na.rm=T)), 
            keyby=.(bookyear)]
  
  openxlsx::write.xlsx(tab,
                       file.path(
                         FOLDER_DATA_RESULTS,
                         "annual reports",
                         paste0(year_folder),
                         "BackgroundAndHistory.xlsx"))
  
}



# marriage cat

ar[bookorgdistrict=="Tulk", bookorgdistrict:="TULK"]

ar[,agemarriagecat_ar:=cut(agemarriage,
                           breaks=c(0,15,17,19,25,30,35,40,100),
                           include.lowest = T)]

xtabs(~ar$agemarriagecat_ar, addNA=T)

xtabs(~bookyear+agemarriagecat_ar, data=ar[booknum==1 & ident_dhis2_booking==1],addNA=T)

tab <- ar[ident_dhis2_booking==T,.(N=.N,
                                   "2019"=sum(bookyear==2019, na.rm=T),
                                   "2020"=sum(bookyear==2020, na.rm=T),
                                   "2021"=sum(bookyear==2021, na.rm=T),
                                   "2022"=sum(bookyear==2022, na.rm=T)),
          keyby=.(agemarriagecat_ar)]


openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "ageatmarriagecat_.xlsx"))


# find women who were booked the previous year
setorder(ar,motheridno,bookdate)
ar[,prevbookyear:=as.numeric(NA)]
ar[ident_dhis2_booking==1,prevbookyear:=0]

ar[ident_dhis2_booking==1,prevbookyear:=shift(bookyear), by=motheridno]

nrow(ar[!is.na(prevbookyear)])
xtabs(~bookyear+prevbookyear, data=ar[ident_dhis2_booking==1], addNA=T)


ar[,minbookyear:=as.numeric(NA)]
ar[ident_dhis2_booking==1,minbookyear:=min(bookyear), by=motheridno]
xtabs(~minbookyear+bookyear, data=ar[ident_dhis2_booking==T], addNA=T)

nrow(ar[minbookyear<bookyear])

# only include woman one time


tab <- ar[ident_dhis2_booking==T & 
            minbookyear==bookyear,.(N=.N,
                                    "2019"=sum(bookyear==2019, na.rm=T),
                                    "2020"=sum(bookyear==2020, na.rm=T),
                                    "2021"=sum(bookyear==2021,na.rm=T),
                                    "2022"=sum(bookyear==2022, na.rm=T)),
          keyby=.(agemarriagecat_ar)]

openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "ageatmarriagecat_perwoman_.xlsx"))




#Making ugly tables for parity
vars <- c("bookyear",
          "bookparity",
          "bookhistdm",
          "bookhistgdm",
          "bookhisthtn",
          "bookhistotherch",
          "bookhistcs",
          "bookhistperi",
          "bookprimi",
          "booklaburglu",
          "bookfamdm",
          "bookfamhtn",
          "bookhistabort",
          "bookhistaph",
          "bookhistclex",
          "bookhistgdm",
          "bookhistghtn",
          "bookhistpreterm",
          "bookprimi",
          "bookrefchronic",
          "bookhighrisk")





#Making ugly tables for parity
uglyTable <- ar[ident_dhis2_booking==T,
                .(N=.N), 
                keyby=.(bookyear,
                        bookparity,
                        bookhistdm,
                        bookhistgdm,
                        bookhisthtn,
                        bookhistotherch,
                        bookhistcs,
                        bookhistperi,
                        bookprimi,
                        booklaburglu,
                        bookfamdm,
                        bookfamhtn,
                        bookhistabort,
                        bookhistaph,
                        bookhistclex,
                        bookhistghtn,
                        bookhistpreterm,
                        bookprimi,
                        bookrefchronic,
                        bookhighrisk)]


histtab <- melt.data.table(uglyTable, id.vars = "bookyear")
long <- histtab[,.(N=.N),
                keyby=.(bookyear, variable, value)]







if(IS_GAZA==T){
  
  
  
  ar[,agecat:=cut(age,
                  breaks=c(0,20,25,30,35,40,100),
                  include.lowest=T)]
  
  ar[,agemarriagecat:=cut(agemarriage,
                          breaks=c(0,20,25,30,35,40,100),
                          include.lowest=T)]
  
  ##agepreg
  #d[agepregnancy<14, agepregnancy:=NA]
  #d[agepregnancy>45, agepregnancy:=NA]
  ar[,agepregnancycat:=cut(agepregnancy,
                           breaks=c(0,20,25,30,35,40,100),
                           include.lowest=T)]
  
  ##education is already less than 0 or missing
  ar[education<0, education:=NA]
  ar[,educationcat:=cut(education,
                        breaks=c(0,9,13,100),
                        include.lowest=T)]
  xtabs(~ar$agecat)
  xtabs(~ar$agemarriagecat)
  xtabs(~ar$agepregnancycat)
  xtabs(~ar$educationcat)
  
  #d[income<150, income:=NA]
  
  ar[,avgincome := income/members]
  
  ar[,avgincomecat:=cut(avgincome,
                        breaks=c(0,200,900,1824,3054,100000),
                        include.lowest=T)]
  ar[,incomecat:=cut(income,
                     breaks=c(0,200,900,1824,3054,100000),
                     include.lowest=T)]
  
  xtabs(~ar$avgincomecat)
  
  
  
}





########## 
# book gA
########## 

ar[,booktrimester:=cut(bookgestagedays,
                       breaks=c(0,91,189,280),
                       include.lowest=T)]

xtabs(~ar$booktrimester, addNA=T)


bookgA <- ar[ident_dhis2_booking==T,.(N=.N),
             keyby=.(bookyear,booktrimester)]



openxlsx::write.xlsx(bookgA,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BooTrimester_.xlsx"))





# bmi

# bookbmicat

tab <- ar[ident_dhis2_booking==T &
            bookgestage<12 & bookgestage>0,.(N=.N,
                                             "2019"=sum(bookyear==2019, na.rm=T),
                                             "2020"=sum(bookyear==2020, na.rm=T),
                                             "2021"=sum(bookyear==2021, na.rm=T),
                                             "2022"=sum(bookyear==2022, na.rm=T)),
          keyby=.(bookbmicat)]

openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookBmiCats_.xlsx"))


########## 
# bookHR #
##########

# high risk at booking


bookgAHR <- ar[ident_dhis2_booking==T,.(N=.N),
               keyby=.(bookyear,booktrimester,ident_dhis2_risk)]



openxlsx::write.xlsx(bookgAHR,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookTrimester_HR_.xlsx"))

########## 
# visits #
##########

# fix code for years to aggregate quicker #





# ultrasounds on time

usontime <-ar[ident_dhis2_booking==1,.(N=.N,
                                       FirstTrimesterUS=sum(TrialOne_us_exists_00_14==T,na.rm=T),
                                       US15to17weeksUS=sum(TrialOne_us_exists_15_17==T, na.rm=T),
                                       US18to18weeks=sum(TrialOne_us_exists_18_22==T, na.rm=T),
                                       SeconTrimesterUS=sum(TrialOne_us_exists_15_17==T|
                                                              TrialOne_us_exists_18_22==T|
                                                              TrialOne_us_exists_23_23==T|
                                                              TrialOne_us_exists_24_24==T|
                                                              TrialOne_us_exists_25_25==T|
                                                              TrialOne_us_exists_26_26==T, na.rm=T),
                                       FirstandsecTrimes=sum(TrialOne_us_exists_00_14==T &
                                                               (TrialOne_us_exists_15_17==T|
                                                                  TrialOne_us_exists_18_22==T|
                                                                  TrialOne_us_exists_23_23==T|
                                                                  TrialOne_us_exists_24_24==T|
                                                                  TrialOne_us_exists_25_25==T|
                                                                  TrialOne_us_exists_26_26==T), na.rm=T),
                                       ThirdTrimester=sum(TrialOne_us_exists_27_27==T|
                                                            TrialOne_us_exists_28_28==T|
                                                            TrialOne_us_exists_29_30==T|
                                                            TrialOne_us_exists_31_33==T |
                                                            TrialOne_us_exists_34_34==T|
                                                            TrialOne_us_exists_35_37==T, na.rm=T),
                                       All3US=sum(TrialOne_us_exists_00_14==T &
                                                    (TrialOne_us_exists_15_17==T|
                                                       TrialOne_us_exists_18_22==T|
                                                       TrialOne_us_exists_23_23==T|
                                                       TrialOne_us_exists_24_24==T|
                                                       TrialOne_us_exists_25_25==T|
                                                       TrialOne_us_exists_26_26==T) &
                                                    (TrialOne_us_exists_27_27==T|
                                                       TrialOne_us_exists_28_28==T|
                                                       TrialOne_us_exists_29_30==T|
                                                       TrialOne_us_exists_31_33==T |
                                                       TrialOne_us_exists_34_34==T|
                                                       TrialOne_us_exists_35_37==T), na.rm=T),
                                       US35to37Only=sum(TrialOne_us_exists_35_37==T, na.rm=T)),
              keyby=.(bookyear)]



openxlsx::write.xlsx(usontime,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               "usOntime_.xlsx"))






# ultrasounds on time by bookgA

usontimeGAcats <-ar[ident_dhis2_booking==1,.(N=.N,
                                             FirstTrimesterUS=sum(TrialOne_us_exists_00_14==T,na.rm=T),
                                             US15to17weeksUS=sum(TrialOne_us_exists_15_17==T, na.rm=T),
                                             US18to18weeks=sum(TrialOne_us_exists_18_22==T, na.rm=T),
                                             SeconTrimesterUS=sum(TrialOne_us_exists_15_17==T|
                                                                    TrialOne_us_exists_18_22==T|
                                                                    TrialOne_us_exists_23_23==T|
                                                                    TrialOne_us_exists_24_24==T|
                                                                    TrialOne_us_exists_25_25==T|
                                                                    TrialOne_us_exists_26_26==T, na.rm=T),
                                             FirstandsecTrimes=sum(TrialOne_us_exists_00_14==T &
                                                                     (TrialOne_us_exists_15_17==T|
                                                                        TrialOne_us_exists_18_22==T|
                                                                        TrialOne_us_exists_23_23==T|
                                                                        TrialOne_us_exists_24_24==T|
                                                                        TrialOne_us_exists_25_25==T|
                                                                        TrialOne_us_exists_26_26==T),
                                                                   na.rm=T),
                                             ThirdTrimester=sum(TrialOne_us_exists_27_27==T|
                                                                  TrialOne_us_exists_28_28==T|
                                                                  TrialOne_us_exists_29_30==T|
                                                                  TrialOne_us_exists_31_33==T |
                                                                  TrialOne_us_exists_34_34==T|
                                                                  TrialOne_us_exists_35_37==T, na.rm=T),
                                             All3US=sum(TrialOne_us_exists_00_14==T &
                                                          (TrialOne_us_exists_15_17==T|
                                                             TrialOne_us_exists_18_22==T|
                                                             TrialOne_us_exists_23_23==T|
                                                             TrialOne_us_exists_24_24==T|
                                                             TrialOne_us_exists_25_25==T|
                                                             TrialOne_us_exists_26_26==T) &
                                                          (TrialOne_us_exists_27_27==T|
                                                             TrialOne_us_exists_28_28==T|
                                                             TrialOne_us_exists_29_30==T|
                                                             TrialOne_us_exists_31_33==T |
                                                             TrialOne_us_exists_34_34==T|
                                                             TrialOne_us_exists_35_37==T), na.rm=T),
                                             US35to37Only=sum(TrialOne_us_exists_35_37==T, na.rm=T)),
                    keyby=.(bookyear, bookgestagedays_cats)]

openxlsx::write.xlsx(usontimeGAcats,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               "usOntimeBookGAcats.xlsx"))


##############
# anemia 
##############


##########
# Anemia
##########


#Oppt
ar[,Oppt_anemia_00_23:=as.logical(NA)]

ar[bookgestagedays_cats %in% c("(0,104]",
                               "(104,125]",
                               "(125,160]",
                               "(160,167]"), Oppt_anemia_00_23:=T]

xtabs(~ar$Oppt_anemia_00_23, addNA=T)

# 00_23
ar[,screeniningontime_anemia_00_23:=as.logical(NA)]
ar[Oppt_anemia_00_23==T,screeniningontime_anemia_00_23:=FALSE]
ar[screeniningontime_anemia_00_23==F & 
     !is.na(booklabhb) & 
     booklabhb>0,screeniningontime_anemia_00_23:=TRUE]

xtabs(~ar$screeniningontime_anemia_00_23, addNA=T)


# no anemia
ar[,screeniningontime_no_anemia_00_23:=as.logical(NA)]
ar[screeniningontime_anemia_00_23==T, screeniningontime_no_anemia_00_23:=FALSE]
ar[screeniningontime_no_anemia_00_23==F & 
     booklabhb>10.9 , screeniningontime_no_anemia_00_23:=TRUE]

xtabs(~ar$screeniningontime_no_anemia_00_23, addNA=T)

# 00-23 weeks severe anemia
ar[,mansevanemia_00_23:=as.logical(NA)]
ar[screeniningontime_no_anemia_00_23==F & 
     booklabhb<7 &
     booklabhb>0, mansevanemia_00_23:=F]
xtabs(~ar$mansevanemia_00_23, addNA=T)

# should probably use manhb variable here
ar[mansevanemia_00_23==F &
     (TrialOne_manhb_01_01==T |
        TrialOne_manhb_02_02==T |
        TrialOne_manhb_03_03==T |
        TrialOne_manhb_04_04==T |
        TrialOne_manhb_05_05==T |
        TrialOne_manhb_06_06==T |
        TrialOne_manhb_07_07==T |
        TrialOne_manhb_08_08==T |
        TrialOne_manhb_09_09==T |
        TrialOne_manhb_10_10==T |
        TrialOne_manhb_11_11==T |
        TrialOne_manhb_12_12==T |
        TrialOne_manhb_13_13==T |
        TrialOne_manhb_14_14==T |
        TrialOne_manhb_15_15==T |
        TrialOne_manhb_16_16==T |
        TrialOne_manhb_17_17==T |
        TrialOne_manhb_18_18==T |
        TrialOne_manhb_19_19==T |
        TrialOne_manhb_20_20==T |
        TrialOne_manhb_21_21==T |
        TrialOne_manhb_22_22==T |
        TrialOne_manhb_23_23==T),mansevanemia_00_23:=T]

xtabs(~ar$mansevanemia_00_23, addNA=T)

##########
#management
##########


# 00-23 weeks moderate anemia
ar[,manmilmodane_00_23:=as.logical(NA)]
ar[screeniningontime_no_anemia_00_23==F & 
     booklabhb>=7 & booklabhb<=10.9,manmilmodane_00_23:=F]
xtabs(~ar$manmilmodane_00_23, addNA=T)

ar[manmilmodane_00_23==F &
     (TrialOne_manhb_mildmodhbret_03_03==T|
        TrialOne_manhb_mildmodhbret_04_04==T|
        TrialOne_manhb_mildmodhbret_05_05==T|
        TrialOne_manhb_mildmodhbret_06_06==T|
        TrialOne_manhb_mildmodhbret_07_07==T|
        TrialOne_manhb_mildmodhbret_08_08==T|
        TrialOne_manhb_mildmodhbret_09_09==T|
        TrialOne_manhb_mildmodhbret_10_10==T|
        TrialOne_manhb_mildmodhbret_11_11==T|
        TrialOne_manhb_mildmodhbret_12_12==T|
        TrialOne_manhb_mildmodhbret_13_13==T|
        TrialOne_manhb_mildmodhbret_14_14==T|
        TrialOne_manhb_mildmodhbret_15_15==T|
        TrialOne_manhb_mildmodhbret_16_16==T|
        TrialOne_manhb_mildmodhbret_17_17==T|
        TrialOne_manhb_mildmodhbret_18_18==T|
        TrialOne_manhb_mildmodhbret_19_19==T|
        TrialOne_manhb_mildmodhbret_20_20==T|
        TrialOne_manhb_mildmodhbret_21_21==T|
        TrialOne_manhb_mildmodhbret_22_22==T|
        TrialOne_manhb_mildmodhbret_23_23==T|
        TrialOne_manhb_mildmodhbret_24_24==T|
        TrialOne_manhb_mildmodhbret_25_25==T|
        TrialOne_manhb_mildmodhbret_26_26==T),manmilmodane_00_23:=T]

xtabs(~ar$manmilmodane_00_23, addNA=T)

# screeniningontime_no_anemia_29_34 create this variable and at 24_28


################
#24-28 weeks
################

# 24-28 weeks
ar[,Oppt_anemia_24_28:=as.logical(NA)]
ar[TrialOne_anvisitnew_24_28==T, Oppt_anemia_24_28:=F]
ar[TrialOne_anvisitnew_24_28==T & 
     screeniningontime_no_anemia_00_23==T, Oppt_anemia_24_28:=TRUE ]

xtabs(~ar$Oppt_anemia_24_28)

# 24-28
ar[,screeniningontime_anemia_24_28:=as.logical(NA)]
ar[Oppt_anemia_24_28==T,screeniningontime_anemia_24_28:=FALSE]
ar[screeniningontime_anemia_24_28==F &
     TrialOne_labhb_exists_24_28==T,screeniningontime_anemia_24_28:=TRUE]

xtabs(~ar$screeniningontime_anemia_24_28, addNA=T)


# no anemia 24- 28 weeks

ar[,screeniningontime_no_anemia_24_28:=as.logical(NA)]
ar[screeniningontime_anemia_24_28==TRUE, screeniningontime_no_anemia_24_28:=FALSE]
ar[screeniningontime_no_anemia_24_28==F & TrialOne_labhb_normal_24_28==T, 
   screeniningontime_no_anemia_24_28:=TRUE]

xtabs(~ar$screeniningontime_no_anemia_24_28, addNA=T)


##########
#management
##########

ar[,manmildmodanemia_24_28:=as.logical(NA)]
ar[screeniningontime_no_anemia_24_28==F & 
     TrialOne_labhb_anemia_mild_mod_24_28==T,manmildmodanemia_24_28:=F]
xtabs(~ar$manmildmodanemia_24_28, addNA=T)

ar[manmildmodanemia_24_28==F &
     (TrialOne_manhb_mildmodhbret_27_27==T|
        TrialOne_manhb_mildmodhbret_28_28==T|
        TrialOne_manhb_mildmodhbret_29_29==T|
        TrialOne_manhb_mildmodhbret_30_30==T|
        TrialOne_manhb_mildmodhbret_31_31==T),manmildmodanemia_24_28:=T]

xtabs(~ar$manmildmodanemia_24_28, addNA=T)


# 24-28 weeks severe anemia
ar[,mansevanemia_24_28:=as.logical(NA)]
ar[screeniningontime_no_anemia_24_28==F & 
     TrialOne_labhb_anemia_sev_24_28==T,mansevanemia_24_28:=F]
xtabs(~ar$mansevanemia_24_28, addNA=T)

# should probably use manhb variable here
ar[mansevanemia_24_28==F &
     (TrialOne_manhb_24_24==T |
        TrialOne_manhb_25_25==T |
        TrialOne_manhb_26_26==T |
        TrialOne_manhb_27_27==T |
        TrialOne_manhb_28_28==T),mansevanemia_24_28:=T]
# manhb_sev==T or riskMildModAne

xtabs(~ar$mansevanemia_24_28)


################
#29-34 weeks
################

# only those booked at this category, so add an and statements that does this

#Oppt
ar[,Oppt_anemia_29_34:=as.logical(NA)]

ar[bookgestagedays_cats %in% c("(202,216]",
                               "(216,237]",
                               "(237,244]"), Oppt_anemia_29_34:=TRUE ]

xtabs(~ar$Oppt_anemia_29_34, addNA=T)

# 29-34 screening
ar[,screeniningontime_anemia_29_34:=as.logical(NA)]
ar[Oppt_anemia_29_34==T,screeniningontime_anemia_29_34:=FALSE]
ar[screeniningontime_anemia_29_34==F & !is.na(booklabhb) & booklabhb>0,
   screeniningontime_anemia_29_34:=TRUE]

xtabs(~ar$screeniningontime_anemia_29_34, addNA=T)

# no anemia
ar[,screeniningontime_no_anemia_29_34:=as.logical(NA)]
ar[screeniningontime_anemia_29_34==T, screeniningontime_no_anemia_29_34:=FALSE]
ar[screeniningontime_no_anemia_29_34==F & booklabhb>10.9, screeniningontime_no_anemia_29_34:=TRUE]

xtabs(~ar$screeniningontime_no_anemia_29_34, addNA=T)


# 29_34 weeks severe anemia
ar[,mansevanemia_29_34:=as.logical(NA)]
ar[screeniningontime_no_anemia_29_34==F & 
     booklabhb<7 & booklabhb>0,mansevanemia_29_34:=F]
xtabs(~ar$mansevanemia_29_34, addNA=T)

# should probably use manhb variable here
ar[mansevanemia_29_34==F &
     (TrialOne_manhb_29_29==T |
        TrialOne_manhb_30_30==T |
        TrialOne_manhb_31_31==T |
        TrialOne_manhb_32_32==T |
        TrialOne_manhb_33_33==T |
        TrialOne_manhb_34_34==T ),mansevanemia_29_34:=T]

xtabs(~ar$mansevanemia_29_34, addNA=T)
xtabs(~ar$Oppt_anemia_29_34)
# manhb_sev==T or riskMildModAne




##########
# mild/mod
##########


# 29-34 weeks severe anemia
ar[,manmildmodanemia_29_34:=as.logical(NA)]
ar[screeniningontime_no_anemia_29_34==F & 
     booklabhb>=7 & booklabhb<=10.9,manmildmodanemia_29_34:=F]
xtabs(~ar$manmildmodanemia_29_34, addNA=T)

ar[manmildmodanemia_29_34==F &
     (TrialOne_manhb_mildmodhbret_32_32==T|
        TrialOne_manhb_mildmodhbret_33_33==T|
        TrialOne_manhb_mildmodhbret_34_34==T|
        TrialOne_manhb_mildmodhbret_35_35==T|
        TrialOne_manhb_mildmodhbret_36_36==T),manmildmodanemia_29_34:=T]
xtabs(~ar$manmildmodanemia_29_34, addNA=T)


################
#35-37 weeks
################

#35-37 weeks
ar[,Oppt_anemia_35_37:=as.logical(NA)]
ar[TrialOne_anvisitnew_35_37==T, Oppt_anemia_35_37:=F]
ar[TrialOne_anvisitnew_35_37==T & (screeniningontime_no_anemia_24_28==T |
                                     screeniningontime_no_anemia_29_34==T), Oppt_anemia_35_37:=TRUE ]

xtabs(~ar$Oppt_anemia_35_37)

# 35_37 screening on time
ar[,screeniningontime_anemia_35_37:=as.logical(NA)]
ar[Oppt_anemia_35_37==T,screeniningontime_anemia_35_37:=FALSE]
ar[screeniningontime_anemia_35_37==F &
     TrialOne_labhb_exists_35_37==T,screeniningontime_anemia_35_37:=TRUE]

xtabs(~ar$screeniningontime_anemia_35_37, addNA=T)

# no anemia
ar[,screeniningontime_no_anemia_35_37:=as.logical(NA)]
ar[screeniningontime_anemia_35_37==T,screeniningontime_no_anemia_35_37:=FALSE]
ar[screeniningontime_anemia_35_37==T &
     TrialOne_labhb_normal_35_37==T, screeniningontime_no_anemia_35_37:=TRUE]
xtabs(~ar$screeniningontime_no_anemia_35_37, addNA=T)

# mild/mod anemia

ar[,manmildmodanemia_35_37:=as.logical(NA)]
ar[screeniningontime_no_anemia_35_37==F & 
     TrialOne_labhb_anemia_mild_mod_35_37==T,manmildmodanemia_35_37:=F]
xtabs(~ar$manmildmodanemia_35_37, addNA=T)

# keep them even if they give a false value
# maximum weeks for this in processing code is 37 weeks
ar[manmildmodanemia_35_37==F &
     (TrialOne_manhb_mildmodhbret_36_36==T|
        TrialOne_manhb_mildmodhbret_37_37==T|
        TrialOne_manhb_mildmodhbret_38_38==T),manmildmodanemia_35_37:=T]
xtabs(~ar$manmildmodanemia_35_37, addNA=T)



##########
# severe
##########

# 35_37 weeks severe anemia
ar[,mansevanemia_35_37:=as.logical(NA)]
ar[screeniningontime_no_anemia_35_37==F & 
     TrialOne_labhb_anemia_sev_35_37==T,mansevanemia_35_37:=F]
xtabs(~ar$mansevanemia_35_37, addNA=T)

ar[mansevanemia_35_37==F &
     (TrialOne_manhb_35_35==T |
        TrialOne_manhb_36_36==T |
        TrialOne_manhb_37_37==T),mansevanemia_35_37:=T]
# manhb_sev==T or riskMildModAne

xtabs(~ar$mansevanemia_35_37, addNA=T)




prelimHB <- ar[ident_dhis2_booking==1,.(N=.N,
                                        Opptb424=sum(Oppt_anemia_00_23, na.rm=T),
                                        screenb424=sum(screeniningontime_anemia_00_23, na.rm=T),
                                        notscreenb424=sum(screeniningontime_anemia_00_23==FALSE, na.rm=T),
                                        manmildmodaneb424=sum(manmilmodane_00_23, na.rm=T),
                                        notmanmildmodeaneb424=sum(manmilmodane_00_23==FALSE, na.rm=T),
                                        mansevanemiab424=sum(mansevanemia_00_23, na.rm=T),
                                        notmansevaneb424=sum(mansevanemia_00_23==FALSE, na.rm=T),
                                        Oppt2428weeks=sum(Oppt_anemia_24_28, na.rm=T),
                                        Screen2428weeks=sum(screeniningontime_anemia_24_28, na.rm=T),
                                        manmilmodane2428=sum(manmildmodanemia_24_28, na.rm=T),
                                        notmanmilmod2428=sum(manmildmodanemia_24_28==F, na.rm=T),
                                        mansevane2428=sum(mansevanemia_24_28, na.rm=T),
                                        notmansevane2428=sum(mansevanemia_24_28==F, na.rm=T),
                                        Opportun3537weeks=sum(Oppt_anemia_35_37, na.rm=T),
                                        screen3537weeks=sum(screeniningontime_anemia_35_37, na.rm=T),
                                        manmildmodane3537=sum(manmildmodanemia_35_37, na.rm=T),
                                        notmanmildmodane3537=sum(manmildmodanemia_35_37==FALSE, na.rm=T),
                                        mansevane3537=sum(mansevanemia_35_37, na.rm=T),
                                        notmansevane3537=sum(mansevanemia_35_37==F, na.rm=T)),
               keyby=.(bookyear)]

openxlsx::write.xlsx(prelimHB,file.path(FOLDER_DATA_RESULTS,
                                        "annual reports",
                                        paste0(year_folder),
                                        sprintf("%s_Hb.xlsx",
                                                lubridate::today()))) 






###########
# HTN
###########


########## 
# bookbp
########## 
# bookbp and bookGA

bptab <- ar[ident_dhis2_booking==T,.(NormalBP=sum((bookbpsyst>0 & bookbpsyst<140) &
                                                    (bookbpdiast>0 & bookbpdiast<90), na.rm=T),
                                     MildHTN=sum((bookbpsyst>=140 & bookbpsyst<=149) |
                                                   (bookbpdiast>90 & bookbpdiast<=100), na.rm=T),
                                     ModHTN=sum((bookbpsyst>=150 & bookbpsyst<=159) |
                                                  (bookbpdiast>100 & bookbpdiast<=110), na.rm=T),
                                     SevHTN=sum(bookbpsyst>=160 |
                                                  bookbpdiast>110, na.rm=T),
                                     MissingBookbp=sum(bookbpsyst==0|
                                                         bookbpdiast==0)),
            keyby=.(bookyear,booktrimester)]

xtabs(~bookbpsystcat+booktrimester,addNA=T,data=ar)



openxlsx::write.xlsx(bptab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookBPbyBookTM.xlsx"))




# making bookgA to differentiate between chronic and ghtn
ar[,bookHTNgAcutoff:=cut(bookgestagedays,
                         breaks=c(0,140,280),
                         include.lowest=T)]




bptab <- ar[ident_dhis2_booking==T,.(NormalBP=sum((bookbpsyst>0 & bookbpsyst<140) &
                                                    (bookbpdiast>0 & bookbpdiast<90), na.rm=T),
                                     MildHTN=sum((bookbpsyst>=140 & bookbpsyst<=149) |
                                                   (bookbpdiast>90 & bookbpdiast<=100), na.rm=T),
                                     ModHTN=sum((bookbpsyst>=150 & bookbpsyst<=159) |
                                                  (bookbpdiast>100 & bookbpdiast<=110), na.rm=T),
                                     SevHTN=sum(bookbpsyst>=160 |
                                                  bookbpdiast>110, na.rm=T),
                                     MissingBookbp=sum(bookbpsyst==0|
                                                         bookbpdiast==0)),
            keyby=.(bookyear,bookHTNgAcutoff)]

xtabs(~bookbpsystcat+bookHTNgAcutoff,addNA=T,data=ar)

bptab[bookHTNgAcutoff=="[0,140]",bookHTNgAcutoff:="<=20 weeks"]
bptab[bookHTNgAcutoff=="(140,280]",bookHTNgAcutoff:=">20 weeks"]

setnames(bptab,c("bookyear",
                 "bookHTNgAcutoff",
                 "NormalBP",
                 "MildHTN",
                 "ModHTN",
                 "SevHTN",
                 "MissingBookbp"),
         c("Year",
           "Gestational Age",
           "Normal",
           "Mild",
           "Moderate",
           "Severe",
           "Missing"))




openxlsx::write.xlsx(bptab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookBPbyHTNcutoff.xlsx"))




# age groups htn



bptab <- ar[ident_dhis2_booking==T,.(NormalBP=sum((bookbpsyst>0 & bookbpsyst<140) &
                                                    (bookbpdiast>0 & bookbpdiast<90), na.rm=T),
                                     MildHTN=sum((bookbpsyst>=140 & bookbpsyst<=149) |
                                                   (bookbpdiast>90 & bookbpdiast<=100), na.rm=T),
                                     ModHTN=sum((bookbpsyst>=150 & bookbpsyst<=159) |
                                                  (bookbpdiast>100 & bookbpdiast<=110), na.rm=T),
                                     SevHTN=sum(bookbpsyst>=160 |
                                                  bookbpdiast>110, na.rm=T),
                                     MissingBookbp=sum(bookbpsyst==0|
                                                         bookbpdiast==0)),
            keyby=.(bookyear,bookHTNgAcutoff,agecat)]

xtabs(~bookbpsystcat+bookHTNgAcutoff,addNA=T,data=ar)

bptab[bookHTNgAcutoff=="[0,140]",bookHTNgAcutoff:="<=20 weeks"]
bptab[bookHTNgAcutoff=="(140,280]",bookHTNgAcutoff:=">20 weeks"]

setnames(bptab,c("bookyear",
                 "bookHTNgAcutoff",
                 "agecat",
                 "NormalBP",
                 "MildHTN",
                 "ModHTN",
                 "SevHTN",
                 "MissingBookbp"),
         c("Year",
           "Gestational Age",
           "Age category",
           "Normal",
           "Mild",
           "Moderate",
           "Severe",
           "Missing"))




openxlsx::write.xlsx(bptab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookBPbyHTNcutoffwithAge.xlsx"))



####################
# Hypertension
####################

##########################################################################
# HTN
##########################################################################

########## Hypertension ########## 
#define chronic?
# high blood pressure before or on 22 weeks

################
#00-14 weeks
################

#screening
ar[,Oppt_bp_00_14:=as.logical(NA)]
ar[TrialOne_anvisitnew_00_14==T,Oppt_bp_00_14:=TRUE]
xtabs(~ar$Oppt_bp_00_14,addNA=T)

# numerator
ar[,bpontime_00_14:=as.logical(NA)]
ar[Oppt_bp_00_14==TRUE,bpontime_00_14:=FALSE]
ar[TrialOne_anbpsyst_present_00_14==T &
     TrialOne_anbpdiast_present_00_14==T &
     bpontime_00_14==F,bpontime_00_14:=TRUE]

xtabs(~ar$bpontime_00_14)



#management
#Oppt
ar[,manchronichtn_00_14:=as.logical(NA)]
ar[Oppt_bp_00_14==T & (TrialOne_anbpdiast_mildHTN_00_14==T|
                         TrialOne_anbpsyst_mildHTN_00_14==T|
                         TrialOne_anbpsyst_modSevHTN_00_14==T|
                         TrialOne_anbpdiast_modSevHTN_00_14==T),manchronichtn_00_14:=F]



ar[manchronichtn_00_14==F & (TrialOne_refHR_00_14==T|
                               TrialOne_refHosp_00_14==T|
                               TrialOne_refSpec_00_14==T),manchronichtn_00_14:=T]

xtabs(~ar$manchronichtn_00_14, addNA=T)



################
#15-17 weeks
################

#screening
ar[,Oppt_bp_15_17:=as.logical(NA)]
ar[TrialOne_anvisitnew_15_17==T &
     is.na(manchronichtn_00_14),Oppt_bp_15_17:=TRUE]
xtabs(~ar$Oppt_bp_15_17,addNA=T)

# numerator
ar[,bpontime_15_17:=as.logical(NA)]
ar[Oppt_bp_15_17==TRUE,bpontime_15_17:=FALSE]
ar[TrialOne_anbpsyst_present_15_17==T &
     TrialOne_anbpdiast_present_15_17==T &
     bpontime_15_17==F,bpontime_15_17:=TRUE]

xtabs(~ar$bpontime_15_17)


################
#15-17 weeks
################

#management
#needs to be done week by week

# mild 15 weeks
ar[,manmildchronichtn_15_15:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_mildHTN_15_15==T|
                         TrialOne_anbpsyst_mildHTN_15_15==T),manmildchronichtn_15_15:=F]

ar[manmildchronichtn_15_15==F & (TrialOne_refHR_15_15==T|
                                   TrialOne_refHosp_15_15==T|
                                   TrialOne_refSpec_15_15==T),manmildchronichtn_15_15:=T]

xtabs(~ar$manmildchronichtn_15_15, addNA=T)

# mod/sev 15 weeks
ar[,manmodsevchronichtn_15_15:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_modSevHTN_15_15==T|
                         TrialOne_anbpsyst_modSevHTN_15_15==T),manmodsevchronichtn_15_15:=F]

ar[manmodsevchronichtn_15_15==F & (TrialOne_refHR_15_15==T|
                                     TrialOne_refHosp_15_15==T|
                                     TrialOne_refSpec_15_15==T),manmodsevchronichtn_15_15:=T]

xtabs(~ar$manmodsevchronichtn_15_15, addNA=T)

# mild/mod/sev 16 weeks
ar[,manmildchronichtn_16_16:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_mildHTN_16_16==T|
                         TrialOne_anbpsyst_mildHTN_16_16==T),manmildchronichtn_16_16:=F]

ar[manmildchronichtn_16_16==F & (TrialOne_refHR_16_16==T|
                                   TrialOne_refHosp_16_16==T|
                                   TrialOne_refSpec_16_16==T),manmildchronichtn_16_16:=T]
xtabs(~ar$manmildchronichtn_16_16, addNA=T)

# mod/sev 15 weeks
ar[,manmodsevchronichtn_16_16:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_modSevHTN_16_16==T|
                         TrialOne_anbpsyst_modSevHTN_16_16==T),manmodsevchronichtn_16_16:=F]

ar[manmodsevchronichtn_16_16==F & (TrialOne_refHR_16_16==T|
                                     TrialOne_refHosp_16_16==T|
                                     TrialOne_refSpec_16_16==T),manmodsevchronichtn_16_16:=T]

xtabs(~ar$manmodsevchronichtn_16_16, addNA=T)

# mild/mod/sev 17 weeks
ar[,manmildchronichtn_17_17:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_mildHTN_17_17==T|
                         TrialOne_anbpsyst_mildHTN_17_17==T),manmildchronichtn_17_17:=F]

ar[manmildchronichtn_17_17==F & (TrialOne_refHR_17_17==T|
                                   TrialOne_refHosp_17_17==T|
                                   TrialOne_refSpec_17_17==T),manmildchronichtn_17_17:=T]

xtabs(~ar$manmildchronichtn_17_17, addNA=T)

# mod/sev 17 weeks
ar[,manmodsevchronichtn_17_17:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (TrialOne_anbpdiast_modSevHTN_17_17==T|
                         TrialOne_anbpsyst_modSevHTN_17_17==T),manmodsevchronichtn_17_17:=F]

ar[manmodsevchronichtn_17_17==F & (TrialOne_refHR_17_17==T|
                                     TrialOne_refHosp_17_17==T|
                                     TrialOne_refSpec_17_17==T),manmodsevchronichtn_17_17:=T]

xtabs(~ar$manmodsevchronichtn_17_17, addNA=T)


################
# combined
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildchronichtn_15_17:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (!is.na(manmildchronichtn_15_15)|
                         !is.na(manmildchronichtn_16_16)|
                         !is.na(manmildchronichtn_17_17)),manmildchronichtn_15_17:=F]

xtabs(~ar$manmildchronichtn_15_17, addNA=T)


ar[manmildchronichtn_15_17==F & (manmildchronichtn_15_15==T|
                                   manmildchronichtn_16_16==T|
                                   manmildchronichtn_17_17==T),manmildchronichtn_17_17:=T]

xtabs(~ar$manmildchronichtn_15_17, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevchronichtn_15_17:=as.logical(NA)]
ar[Oppt_bp_15_17==T & (!is.na(manmodsevchronichtn_15_15)|
                         !is.na(manmodsevchronichtn_16_16)|
                         !is.na(manmodsevchronichtn_17_17)),manmodsevchronichtn_15_17:=F]

xtabs(~ar$manmodsevchronichtn_15_17, addNA=T)


ar[manmodsevchronichtn_15_17==F & (manmodsevchronichtn_15_15==T|
                                     manmodsevchronichtn_16_16==T|
                                     manmodsevchronichtn_17_17==T),manmodsevchronichtn_15_17:=T]

xtabs(~ar$manmodsevchronichtn_15_17, addNA=T)

################
#18-22 weeks
################

#screening
ar[,Oppt_bp_18_22:=as.logical(NA)]
ar[TrialOne_anvisitnew_18_22==T &
     is.na(manmildchronichtn_15_17) &
     is.na(manmodsevchronichtn_15_17),Oppt_bp_18_22:=TRUE]
xtabs(~ar$Oppt_bp_18_22,addNA=T)

# numerator
ar[,bpontime_18_22:=as.logical(NA)]
ar[Oppt_bp_18_22==TRUE,bpontime_18_22:=FALSE]
ar[TrialOne_anbpsyst_present_18_22==T &
     TrialOne_anbpdiast_present_18_22==T &
     bpontime_18_22==F,bpontime_18_22:=TRUE]

xtabs(~ar$bpontime_18_22)


################
#18-22 weeks
################

#management

# mild 18 weeks
ar[,manmildchronichtn_18_18:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_mildHTN_18_18==T|
                         TrialOne_anbpsyst_mildHTN_18_18==T),manmildchronichtn_18_18:=F]

ar[manmildchronichtn_18_18==F & (TrialOne_anbpsyst_present_19_19==T &
                                   TrialOne_anbpdiast_present_19_19==T),manmildchronichtn_18_18:=T]

xtabs(~ar$manmildchronichtn_18_18, addNA=T)

# mod/sev 18 weeks
ar[,manmodsevchronichtn_18_18:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_modSevHTN_18_18==T|
                         TrialOne_anbpsyst_modSevHTN_18_18==T),manmodsevchronichtn_18_18:=F]

ar[manmodsevchronichtn_18_18==F & (TrialOne_refHR_18_18==T|
                                     TrialOne_refHosp_18_18==T|
                                     TrialOne_refSpec_18_18==T),manmodsevchronichtn_18_18:=T]

xtabs(~ar$manmodsevchronichtn_18_18, addNA=T)

# man 19 weeks
#mild 
ar[,manmildchronichtn_19_19:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_mildHTN_19_19==T|
                         TrialOne_anbpsyst_mildHTN_19_19==T),manmildchronichtn_19_19:=F]

ar[manmildchronichtn_19_19==F & (TrialOne_anbpsyst_present_20_20==T &
                                   TrialOne_anbpdiast_present_20_20==T),manmildchronichtn_19_19:=T]

xtabs(~ar$manmildchronichtn_19_19, addNA=T)

# mod/sev 19 weeks
ar[,manmodsevchronichtn_19_19:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_modSevHTN_19_19==T|
                         TrialOne_anbpsyst_modSevHTN_19_19==T),manmodsevchronichtn_19_19:=F]

ar[manmodsevchronichtn_19_19==F & (TrialOne_refHR_19_19==T|
                                     TrialOne_refHosp_19_19==T|
                                     TrialOne_refSpec_19_19==T),manmodsevchronichtn_19_19:=T]

xtabs(~ar$manmodsevchronichtn_19_19, addNA=T)

# man 20 weeks
#mild 20 weeks
ar[,manmildchronichtn_20_20:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_mildHTN_20_20==T|
                         TrialOne_anbpsyst_mildHTN_20_20==T),manmildchronichtn_20_20:=F]

ar[manmildchronichtn_20_20==F & (TrialOne_anbpsyst_present_21_21==T &
                                   TrialOne_anbpdiast_present_21_21==T),manmildchronichtn_20_20:=T]

xtabs(~ar$manmildchronichtn_20_20, addNA=T)

# mod/sev 20 weeks
ar[,manmodsevchronichtn_20_20:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_modSevHTN_20_20==T|
                         TrialOne_anbpsyst_modSevHTN_20_20==T),manmodsevchronichtn_20_20:=F]

ar[manmodsevchronichtn_20_20==F & (TrialOne_refHR_20_20==T|
                                     TrialOne_refHosp_20_20==T|
                                     TrialOne_refSpec_20_20==T),manmodsevchronichtn_20_20:=T]

xtabs(~ar$manmodsevchronichtn_20_20, addNA=T)

# man 21 weeks
#mild 21 weeks
ar[,manmildchronichtn_21_21:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_mildHTN_21_21==T|
                         TrialOne_anbpsyst_mildHTN_21_21==T),manmildchronichtn_21_21:=F]

ar[manmildchronichtn_21_21==F & (TrialOne_anbpsyst_present_22_22==T &
                                   TrialOne_anbpdiast_present_22_22==T),manmildchronichtn_21_21:=T]

xtabs(~ar$manmildchronichtn_21_21, addNA=T)


# mod/sev 21 weeks
ar[,manmodsevchronichtn_21_21:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_modSevHTN_21_21==T|
                         TrialOne_anbpsyst_modSevHTN_21_21==T),manmodsevchronichtn_21_21:=F]

ar[manmodsevchronichtn_21_21==F & (TrialOne_refHR_21_21==T|
                                     TrialOne_refHosp_21_21==T|
                                     TrialOne_refSpec_21_21==T),manmodsevchronichtn_21_21:=T]

xtabs(~ar$manmodsevchronichtn_21_21, addNA=T)

# man 22 weeks
#mild 22 weeks
ar[,manmildchronichtn_22_22:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_mildHTN_22_22==T|
                         TrialOne_anbpsyst_mildHTN_22_22==T),manmildchronichtn_22_22:=F]

ar[manmildchronichtn_22_22==F & (TrialOne_anbpsyst_present_23_23==T &
                                   TrialOne_anbpdiast_present_23_23==T),manmildchronichtn_22_22:=T]

xtabs(~ar$manmildchronichtn_22_22, addNA=T)

#
ar[,manmodsevchronichtn_22_22:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (TrialOne_anbpdiast_modSevHTN_22_22==T|
                         TrialOne_anbpsyst_modSevHTN_22_22==T),manmodsevchronichtn_22_22:=F]

ar[manmodsevchronichtn_22_22==F & (TrialOne_refHR_22_22==T|
                                     TrialOne_refHosp_22_22==T|
                                     TrialOne_refSpec_22_22==T),manmodsevchronichtn_22_22:=T]

xtabs(~ar$manmodsevchronichtn_22_22, addNA=T)



################
# combined 18-22
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildchronichtn_18_22:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (!is.na(manmildchronichtn_18_18)|
                         !is.na(manmildchronichtn_19_19)|
                         !is.na(manmildchronichtn_20_20)|
                         !is.na(manmildchronichtn_21_21)|
                         !is.na(manmildchronichtn_22_22)),manmildchronichtn_18_22:=F]

xtabs(~ar$manmildchronichtn_18_22, addNA=T)


ar[manmildchronichtn_18_22==F & (manmildchronichtn_18_18==T|
                                   manmildchronichtn_19_19==T|
                                   manmildchronichtn_20_20==T|
                                   manmildchronichtn_21_21==T|
                                   manmildchronichtn_22_22==T),manmildchronichtn_18_22:=T]

xtabs(~ar$manmildchronichtn_18_22, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevchronichtn_18_22:=as.logical(NA)]
ar[Oppt_bp_18_22==T & (!is.na(manmodsevchronichtn_18_18)|
                         !is.na(manmodsevchronichtn_19_19)|
                         !is.na(manmodsevchronichtn_20_20)|
                         !is.na(manmodsevchronichtn_21_21)|
                         !is.na(manmodsevchronichtn_22_22)),manmodsevchronichtn_18_22:=F]

xtabs(~ar$manmodsevchronichtn_18_22, addNA=T)


ar[manmodsevchronichtn_18_22==F & (manmodsevchronichtn_18_18==T|
                                     manmodsevchronichtn_19_19==T|
                                     manmodsevchronichtn_20_20==T|
                                     manmodsevchronichtn_21_21==T|
                                     manmodsevchronichtn_22_22==T),manmodsevchronichtn_18_22:=T]

xtabs(~ar$manmodsevchronichtn_18_22, addNA=T)



################
# 18-18 weeks
################


#screening
ar[,Oppt_bp_18_18:=as.logical(NA)]
ar[TrialOne_anvisitnew_18_18==T &
     is.na(manmildchronichtn_15_17) &
     is.na(manmodsevchronichtn_15_17),Oppt_bp_18_18:=TRUE]
xtabs(~ar$Oppt_bp_18_18,addNA=T)

# numerator
ar[,bpontime_18_18:=as.logical(NA)]
ar[Oppt_bp_18_18==TRUE,bpontime_18_18:=FALSE]
ar[TrialOne_anbpsyst_present_18_18==T &
     TrialOne_anbpdiast_present_18_18==T &
     bpontime_18_18==F,bpontime_18_18:=TRUE]

xtabs(~ar$bpontime_18_18)


#management

# mild 18 weeks
ar[,manmildchronichtn_18_18_new:=as.logical(NA)]
ar[Oppt_bp_18_18==T & (TrialOne_anbpdiast_mildHTN_18_18==T|
                         TrialOne_anbpsyst_mildHTN_18_18==T),manmildchronichtn_18_18_new:=F]

ar[manmildchronichtn_18_18_new==F & (TrialOne_anbpsyst_present_19_19==T &
                                       TrialOne_anbpdiast_present_19_19==T) ,manmildchronichtn_18_18_new:=T]

xtabs(~ar$manmildchronichtn_18_18_new, addNA=T)


# mod/sev 18-18 weeks
ar[,manmodsevchronichtn_18_18_new:=as.logical(NA)]
ar[Oppt_bp_18_18==T & (TrialOne_anbpdiast_modSevHTN_19_19==T|
                         TrialOne_anbpsyst_modSevHTN_19_19==T),manmodsevchronichtn_18_18_new:=F]


ar[manmodsevchronichtn_18_18_new==F &
     (TrialOne_refHR_18_18==T|
        TrialOne_refHosp_18_18==T|
        TrialOne_refSpec_18_18==T),manmodsevchronichtn_18_18_new:=T]

xtabs(~ar$manmodsevchronichtn_18_18_new, addNA=T)



################
# 19-19 weeks
################


#screening
ar[,Oppt_bp_19_19:=as.logical(NA)]
ar[TrialOne_anvisitnew_19_19==T &
     is.na(manmildchronichtn_18_18_new) &
     is.na(manmodsevchronichtn_18_18_new),Oppt_bp_19_19:=TRUE]
xtabs(~ar$Oppt_bp_19_19,addNA=T)

# numerator
ar[,bpontime_19_19:=as.logical(NA)]
ar[Oppt_bp_19_19==TRUE,bpontime_19_19:=FALSE]
ar[TrialOne_anbpsyst_present_19_19==T &
     TrialOne_anbpdiast_present_19_19==T &
     bpontime_19_19==F,bpontime_19_19:=TRUE]

xtabs(~ar$bpontime_19_19)


#management

# mild 19 weeks
ar[,manmildchronichtn_19_19_new:=as.logical(NA)]
ar[Oppt_bp_19_19==T & (TrialOne_anbpdiast_mildHTN_19_19==T|
                         TrialOne_anbpsyst_mildHTN_19_19==T),manmildchronichtn_19_19_new:=F]

ar[manmildchronichtn_19_19_new==F & (TrialOne_anbpsyst_present_20_20==T &
                                       TrialOne_anbpdiast_present_20_20==T) ,manmildchronichtn_19_19_new:=T]

xtabs(~ar$manmildchronichtn_19_19_new, addNA=T)


# mod/sev 19-19 weeks
ar[,manmodsevchronichtn_19_19_new:=as.logical(NA)]
ar[Oppt_bp_19_19==T & (TrialOne_anbpdiast_modSevHTN_19_19==T|
                         TrialOne_anbpsyst_modSevHTN_19_19==T),manmodsevchronichtn_19_19_new:=F]


ar[manmodsevchronichtn_19_19_new==F &
     (TrialOne_refHR_19_19==T|
        TrialOne_refHosp_19_19==T| 
        TrialOne_refSpec_19_19),manmodsevchronichtn_19_19_new:=T]

xtabs(~ar$manmodsevchronichtn_19_19_new, addNA=T)



################
# combined 18-19
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildchronichtn_18_19:=as.logical(NA)]
ar[(Oppt_bp_18_18==T & !is.na(manmildchronichtn_18_18_new))|
     (Oppt_bp_19_19==T & !is.na(manmildchronichtn_19_19_new)),manmildchronichtn_18_19:=F]

xtabs(~ar$manmildchronichtn_18_19, addNA=T)


ar[manmildchronichtn_18_19==F & (manmildchronichtn_18_18_new==T|
                                   manmildchronichtn_19_19_new==T),manmildchronichtn_18_19:=T]

xtabs(~ar$manmildchronichtn_18_19, addNA=T)





# severe chronic htn
ar[,manmodsevchronichtn_18_19:=as.logical(NA)]
ar[(Oppt_bp_18_18==T & !is.na(manmodsevchronichtn_18_18_new))|
     (Oppt_bp_19_19==T & !is.na(manmodsevchronichtn_19_19_new)),manmodsevchronichtn_18_19:=F]

xtabs(~ar$manmodsevchronichtn_18_19, addNA=T)


ar[manmodsevchronichtn_18_19==F & (manmodsevchronichtn_18_18_new==T|
                                     manmodsevchronichtn_19_19_new==T),manmodsevchronichtn_18_19:=T]

xtabs(~ar$manmodsevchronichtn_18_19, addNA=T)



################
# 20 weeks only
################

#screening
ar[,Oppt_bp_20_20:=as.logical(NA)]
ar[(TrialOne_anvisitnew_20_20==T) &
     is.na(manmodsevchronichtn_18_19) &
     is.na(manmildchronichtn_18_19),Oppt_bp_20_20:=TRUE]
xtabs(~ar$Oppt_bp_20_20,addNA=T)

# numerator
ar[,bpontime_20_20:=as.logical(NA)]
ar[Oppt_bp_20_20==TRUE,bpontime_20_20:=FALSE]
ar[TrialOne_anbpsyst_present_20_20==T &
     TrialOne_anbpdiast_present_20_20==T &
     bpontime_20_20==F,bpontime_20_20:=TRUE]

xtabs(~ar$bpontime_20_20)


#management

# mild 20 weeks only
ar[,manmildchronichtn_20_20_new:=as.logical(NA)]
ar[Oppt_bp_20_20==T & (TrialOne_anbpdiast_mildHTN_20_20==T|
                         TrialOne_anbpsyst_mildHTN_20_20==T),manmildchronichtn_20_20_new:=F]

ar[manmildchronichtn_20_20_new==F & (TrialOne_anbpsyst_present_21_21==T &
                                       TrialOne_anbpdiast_present_21_21==T),manmildchronichtn_20_20_new:=T]

xtabs(~ar$manmildchronichtn_20_20_new, addNA=T)

# mod/sev 23 weeks
ar[,manmodsevchronichtn_20_20_new:=as.logical(NA)]
ar[Oppt_bp_20_20==T & (TrialOne_anbpdiast_modSevHTN_20_20==T|
                         TrialOne_anbpsyst_modSevHTN_20_20==T),manmodsevchronichtn_20_20_new:=F]

ar[manmodsevchronichtn_20_20_new==F & (TrialOne_refHR_20_20==T|
                                         TrialOne_refHosp_20_20==T|
                                         TrialOne_refSpec_20_20==T),manmildchronichtn_20_20_new:=T]

xtabs(~ar$manmodsevchronichtn_20_20_new, addNA=T)




################
# 21 weeks only
################

#screening
ar[,Oppt_bp_21_21:=as.logical(NA)]
ar[(TrialOne_anvisitnew_21_21==T) &
     is.na(manmodsevchronichtn_20_20_new) &
     is.na(manmildchronichtn_20_20_new),Oppt_bp_21_21:=TRUE]
xtabs(~ar$Oppt_bp_21_21,addNA=T)

# numerator
ar[,bpontime_21_21:=as.logical(NA)]
ar[Oppt_bp_21_21==TRUE,bpontime_21_21:=FALSE]
ar[TrialOne_anbpsyst_present_21_21==T &
     TrialOne_anbpdiast_present_21_21==T &
     bpontime_21_21==F,bpontime_21_21:=TRUE]

xtabs(~ar$bpontime_21_21)


#management

# mild 21 weeks
ar[,manmildhtn_21_21_new:=as.logical(NA)]
ar[Oppt_bp_21_21==T & (TrialOne_anbpdiast_mildHTN_21_21==T|
                         TrialOne_anbpsyst_mildHTN_21_21==T),manmildhtn_21_21:=F]

ar[manmildhtn_21_21==F & (TrialOne_anbpsyst_present_22_22==T &
                            TrialOne_anbpdiast_present_22_22==T),manmildhtn_21_21:=T]

xtabs(~ar$manmildhtn_21_21, addNA=T)

# mod/sev 21 weeks only
ar[,manmodsevhtn_21_21:=as.logical(NA)]
ar[Oppt_bp_21_21==T & (TrialOne_anbpdiast_modSevHTN_21_21==T|
                         TrialOne_anbpsyst_modSevHTN_21_21==T),manmodsevhtn_21_21:=F]

ar[manmodsevhtn_21_21==F & (TrialOne_refHR_21_21==T|
                              TrialOne_refHosp_21_21==T),manmodsevhtn_21_21:=T]

xtabs(~ar$manmodsevhtn_21_21, addNA=T)





################
# 22 weeks only
################

#screening
ar[,Oppt_bp_22_22:=as.logical(NA)]
ar[(TrialOne_anvisitnew_22_22==T) &
     is.na(manmodsevhtn_21_21) &
     is.na(manmildhtn_21_21),Oppt_bp_22_22:=TRUE]
xtabs(~ar$Oppt_bp_22_22,addNA=T)

# numerator
ar[,bpontime_22_22:=as.logical(NA)]
ar[Oppt_bp_22_22==TRUE,bpontime_22_22:=FALSE]
ar[TrialOne_anbpsyst_present_22_22==T &
     TrialOne_anbpdiast_present_22_22==T &
     bpontime_22_22==F,bpontime_22_22:=TRUE]

xtabs(~ar$bpontime_22_22)


#management

# mild 22 weeks
ar[,manmildhtn_22_22:=as.logical(NA)]
ar[Oppt_bp_22_22==T & (TrialOne_anbpdiast_mildHTN_22_22==T|
                         TrialOne_anbpsyst_mildHTN_22_22==T),manmildhtn_22_22:=F]

ar[manmildhtn_22_22==F & (TrialOne_anbpsyst_present_23_23==T &
                            TrialOne_anbpdiast_present_23_23==T),manmildhtn_22_22:=T]

xtabs(~ar$manmildhtn_22_22, addNA=T)

# mod/sev 22 weeks
ar[,manmodsevhtn_22_22:=as.logical(NA)]
ar[Oppt_bp_22_22==T & (TrialOne_anbpdiast_modSevHTN_22_22==T|
                         TrialOne_anbpsyst_modSevHTN_22_22==T),manmodsevhtn_22_22:=F]

ar[manmodsevhtn_22_22==F & (TrialOne_refHR_22_22==T|
                              TrialOne_refHosp_22_22==T),manmodsevhtn_22_22:=T]

xtabs(~ar$manmodsevhtn_22_22, addNA=T)




################
# combined 20-22
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildhtn_20_22:=as.logical(NA)]
ar[(Oppt_bp_20_20==T & (!is.na(manmildchronichtn_20_20_new)))|
     (Oppt_bp_21_21==T & (!is.na(manmildhtn_21_21)))|
     (Oppt_bp_22_22==T & !is.na(manmildhtn_22_22)),manmildhtn_20_22:=F]

xtabs(~ar$manmildhtn_20_22, addNA=T)


ar[manmildhtn_20_22==F & (manmildchronichtn_20_20_new==T|
                            manmildhtn_21_21==T|
                            manmildhtn_22_22==T),manmildhtn_20_22:=T]

xtabs(~ar$manmildhtn_20_22, addNA=T)





# severe chronic htn
ar[,manmodsevhtn_20_22:=as.logical(NA)]
ar[(Oppt_bp_20_20==T & (!is.na(manmodsevchronichtn_20_20_new)))|
     (Oppt_bp_21_21==T & (!is.na(manmodsevhtn_21_21)))|
     (Oppt_bp_22_22==T & !is.na(manmodsevhtn_22_22)),manmodsevhtn_20_22:=F]

xtabs(~ar$manmodsevhtn_20_22, addNA=T)


ar[manmodsevhtn_20_22==F & (manmodsevchronichtn_20_20_new==T|
                              manmodsevhtn_21_21==T|
                              manmodsevhtn_22_22==T),manmodsevhtn_20_22:=T]

xtabs(~ar$manmodsevhtn_20_22, addNA=T)




################
# 23 weeks
################

#screening
ar[,Oppt_bp_23_23:=as.logical(NA)]
ar[TrialOne_anvisitnew_23_23==T &
     is.na(manmildhtn_20_22) &
     is.na(manmodsevhtn_20_22),Oppt_bp_23_23:=TRUE]
xtabs(~ar$Oppt_bp_23_23,addNA=T)

# numerator
ar[,bpontime_23_23:=as.logical(NA)]
ar[Oppt_bp_23_23==TRUE,bpontime_23_23:=FALSE]
ar[TrialOne_anbpsyst_present_23_23==T &
     TrialOne_anbpdiast_present_23_23==T &
     bpontime_23_23==F,bpontime_23_23:=TRUE]

xtabs(~ar$bpontime_23_23)


#management

# mild 23 weeks
ar[,manmildchronichtn_23_23:=as.logical(NA)]
ar[Oppt_bp_23_23==T & (TrialOne_anbpdiast_mildHTN_23_23==T|
                         TrialOne_anbpsyst_mildHTN_23_23==T),manmildchronichtn_23_23:=F]

ar[manmildchronichtn_23_23==F & (TrialOne_anbpsyst_present_24_24==T &
                                   TrialOne_anbpdiast_present_24_24==T),manmildchronichtn_23_23:=T]

xtabs(~ar$manmildchronichtn_23_23, addNA=T)

# mod/sev 23 weeks
ar[,manmodsevchronichtn_23_23:=as.logical(NA)]
ar[Oppt_bp_23_23==T & (TrialOne_anbpdiast_modSevHTN_23_23==T|
                         TrialOne_anbpsyst_modSevHTN_23_23==T),manmodsevchronichtn_23_23:=F]

ar[manmodsevchronichtn_23_23==F & (TrialOne_refHR_23_23==T|
                                     TrialOne_refHosp_23_23==T|
                                     TrialOne_refSpec_23_23==T),manmodsevchronichtn_23_23:=T]

xtabs(~ar$manmodsevchronichtn_23_23, addNA=T)


################
#24-28 weeks
################

#screening
ar[,Oppt_bp_24_28:=as.logical(NA)]
ar[TrialOne_anvisitnew_24_28==T &
     is.na(manmildchronichtn_23_23) &
     is.na(manmildchronichtn_23_23),Oppt_bp_24_28:=TRUE]
xtabs(~ar$Oppt_bp_24_28,addNA=T)

# numerator
ar[,bpontime_24_28:=as.logical(NA)]
ar[Oppt_bp_24_28==TRUE,bpontime_24_28:=FALSE]
ar[TrialOne_anbpsyst_present_24_28==T &
     TrialOne_anbpdiast_present_24_28==T &
     bpontime_24_28==F,bpontime_24_28:=TRUE]

xtabs(~ar$bpontime_24_28)


################
#24-28 weeks
################

#management
# 24 weeks
#mild 
ar[,manmildhtn_24_24:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_mildHTN_24_24==T|
                         TrialOne_anbpsyst_mildHTN_24_24==T),manmildhtn_24_24:=F]

ar[manmildhtn_24_24==F & (TrialOne_anbpsyst_present_25_25==T &
                            TrialOne_anbpdiast_present_25_25==T),manmildhtn_24_24:=T]

xtabs(~ar$manmildhtn_24_24, addNA=T)

# mod/sev  weeks
ar[,manmodsevhtn_24_24:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_modSevHTN_24_24==T|
                         TrialOne_anbpsyst_modSevHTN_24_24==T),manmodsevhtn_24_24:=F]

ar[manmodsevhtn_24_24==F & (TrialOne_refHR_24_24==T|
                              TrialOne_refHosp_24_24==T|
                              TrialOne_refSpec_24_24==T),manmodsevhtn_24_24:=T]

xtabs(~ar$manmodsevhtn_24_24, addNA=T)

# 25 weeks
#mild 
ar[,manmildhtn_25_25:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_mildHTN_25_25==T|
                         TrialOne_anbpsyst_mildHTN_25_25==T),manmildhtn_25_25:=F]

ar[manmildhtn_25_25==F & (TrialOne_anbpsyst_present_26_26==T &
                            TrialOne_anbpdiast_present_26_26==T),manmildhtn_25_25:=T]

xtabs(~ar$manmildhtn_25_25, addNA=T)

# mod/sev weeks
ar[,manmodsevhtn_25_25:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_modSevHTN_25_25==T|
                         TrialOne_anbpsyst_modSevHTN_25_25==T),manmodsevhtn_25_25:=F]

ar[manmodsevhtn_25_25==F & (TrialOne_refHR_25_25==T|
                              TrialOne_refHosp_25_25==T|
                              TrialOne_refSpec_25_25==T),manmodsevhtn_25_25:=T]

xtabs(~ar$manmodsevhtn_25_25, addNA=T)

# 26 weeks
#mild 
ar[,manmildhtn_26_26:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_mildHTN_26_26==T|
                         TrialOne_anbpsyst_mildHTN_26_26==T),manmildhtn_26_26:=F]

ar[manmildhtn_26_26==F & (!is.na(TrialOne_anbpsyst_present_27_27) &
                            !is.na(TrialOne_anbpdiast_present_27_27)),manmildhtn_26_26:=T]

xtabs(~ar$manmildhtn_26_26, addNA=T)


# mod/sev 
ar[,manmodsevhtn_26_26:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_modSevHTN_26_26==T|
                         TrialOne_anbpsyst_modSevHTN_26_26==T),manmodsevhtn_26_26:=F]

ar[manmodsevhtn_26_26==F & (TrialOne_refHR_26_26==T|
                              TrialOne_refHosp_26_26==T|
                              TrialOne_refSpec_26_26==T),manmodsevhtn_26_26:=T]

xtabs(~ar$manmodsevhtn_26_26, addNA=T)

# 27 weeks
#mild 
ar[,manmildhtn_27_27:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_mildHTN_27_27==T|
                         TrialOne_anbpsyst_mildHTN_27_27==T),manmildhtn_27_27:=F]

ar[manmildhtn_27_27==F & (TrialOne_anbpsyst_present_28_28==T &
                            TrialOne_anbpdiast_present_28_28==T),manmildhtn_27_27:=T]


xtabs(~ar$manmildhtn_27_27, addNA=T)

# mod/sev 19 weeks
ar[,manmodsevhtn_27_27:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_modSevHTN_27_27==T|
                         TrialOne_anbpsyst_modSevHTN_27_27==T),manmodsevhtn_27_27:=F]

ar[manmodsevhtn_27_27==F & (TrialOne_refHR_27_27==T|
                              TrialOne_refHosp_27_27==T|
                              TrialOne_refSpec_27_27==T),manmodsevhtn_27_27:=T]

xtabs(~ar$manmodsevhtn_27_27, addNA=T)


# 28 weeks
#mild 
ar[,manmildhtn_28_28:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_mildHTN_28_28==T|
                         TrialOne_anbpsyst_mildHTN_28_28==T),manmildhtn_28_28:=F]

ar[manmildhtn_28_28==F & (TrialOne_anbpsyst_present_29_29==T &
                            TrialOne_anbpdiast_present_29_29==T),manmildhtn_28_28:=T]

xtabs(~ar$manmildhtn_28_28, addNA=T)


# mod/sev 
ar[,manmodsevhtn_28_28:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (TrialOne_anbpdiast_modSevHTN_28_28==T|
                         TrialOne_anbpsyst_modSevHTN_28_28==T),manmodsevhtn_28_28:=F]

ar[manmodsevhtn_28_28==F & (TrialOne_refHR_28_28==T|
                              TrialOne_refHosp_28_28==T|
                              TrialOne_refSpec_28_28==T),manmodsevhtn_28_28:=T]

xtabs(~ar$manmodsevhtn_28_28, addNA=T)

# combine man var for 24-28 weeks
ar[,manmildhtn_24_28:=as.logical(NA)]
ar[manmildhtn_28_28==T|
     manmildhtn_27_27==T,manmildhtn_24_28:=T]
ar[,manmodsevhtn_24_28:=as.logical(NA)]



################
# combined 24-28
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildhtn_24_28:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (!is.na(manmildhtn_24_24)|
                         !is.na(manmildhtn_25_25)|
                         !is.na(manmildhtn_26_26)|
                         !is.na(manmildhtn_27_27)|
                         !is.na(manmildhtn_28_28)),manmildhtn_24_28:=F]

xtabs(~ar$manmildhtn_24_28, addNA=T)


ar[manmildhtn_24_28==F & (manmildhtn_24_24==T|
                            manmildhtn_25_25==T|
                            manmildhtn_26_26==T|
                            manmildhtn_27_27==T|
                            manmildhtn_28_28==T),manmildhtn_24_28:=T]

xtabs(~ar$manmildhtn_24_28, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevhtn_24_28:=as.logical(NA)]
ar[Oppt_bp_24_28==T & (!is.na(manmodsevhtn_24_24)|
                         !is.na(manmodsevhtn_25_25)|
                         !is.na(manmodsevhtn_26_26)|
                         !is.na(manmodsevhtn_27_27)|
                         !is.na(manmodsevhtn_28_28)),manmodsevhtn_24_28:=F]

xtabs(~ar$manmodsevhtn_24_28, addNA=T)


ar[manmodsevhtn_24_28==F & (manmodsevhtn_24_24==T|
                              manmodsevhtn_25_25==T|
                              manmodsevhtn_26_26==T|
                              manmodsevhtn_27_27==T|
                              manmodsevhtn_28_28==T),manmodsevhtn_24_28:=T]

xtabs(~ar$manmodsevhtn_24_28, addNA=T)


################
# 29-30 weeks
################

#screening
ar[,Oppt_bp_29_30:=as.logical(NA)]
ar[TrialOne_anvisitnew_29_30==T &
     is.na(manmildhtn_24_28) &
     is.na(manmodsevhtn_24_28),Oppt_bp_29_30:=TRUE]
xtabs(~ar$Oppt_bp_29_30,addNA=T)

# numerator
ar[,bpontime_29_30:=as.logical(NA)]
ar[Oppt_bp_29_30==TRUE,bpontime_29_30:=FALSE]
ar[TrialOne_anbpsyst_present_29_30==T &
     TrialOne_anbpdiast_present_29_30==T &
     bpontime_29_30==F,bpontime_29_30:=TRUE]

xtabs(~ar$bpontime_29_30)


#management
#29 weeks
#mild 
ar[,manmildhtn_29_29:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (TrialOne_anbpdiast_mildHTN_29_29==T|
                         TrialOne_anbpsyst_mildHTN_29_29==T),manmildhtn_29_29:=F]

ar[manmildhtn_29_29==F & (TrialOne_anbpsyst_present_30_30==T &
                            TrialOne_anbpdiast_present_30_30==T),manmildhtn_29_29:=T]

# mod/sev 
ar[,manmodsevhtn_29_29:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (TrialOne_anbpdiast_modSevHTN_29_29==T|
                         TrialOne_anbpsyst_modSevHTN_29_29==T),manmodsevhtn_29_29:=F]

ar[manmodsevhtn_29_29==F & (TrialOne_refHR_29_29==T|
                              TrialOne_refHosp_29_29==T|
                              TrialOne_refSpec_29_29==T),manmodsevhtn_29_29:=T]

xtabs(~ar$manmodsevhtn_29_29, addNA=T)

#30 weeks
#mild 
ar[,manmildhtn_30_30:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (TrialOne_anbpdiast_mildHTN_30_30==T|
                         TrialOne_anbpsyst_mildHTN_30_30==T),manmildhtn_30_30:=F]

ar[manmildhtn_30_30==F & (TrialOne_anbpsyst_present_31_31==T &
                            TrialOne_anbpdiast_present_31_31==T),manmildhtn_30_30:=T]

# mod/sev 
ar[,manmodsevhtn_30_30:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (TrialOne_anbpdiast_modSevHTN_30_30==T|
                         TrialOne_anbpsyst_modSevHTN_30_30==T),manmodsevhtn_30_30:=F]

ar[manmodsevhtn_30_30==F & (TrialOne_refHR_30_30==T|
                              TrialOne_refHosp_30_30==T|
                              TrialOne_refSpec_30_30==T),manmodsevhtn_30_30:=T]

xtabs(~ar$manmodsevhtn_30_30, addNA=T)




################
# combined 29-30
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildhtn_29_30:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (!is.na(manmildhtn_29_29)|
                         !is.na(manmildhtn_30_30)),manmildhtn_29_30:=F]

xtabs(~ar$manmildhtn_29_30, addNA=T)


ar[manmildhtn_29_30==F & (manmildhtn_29_29==T|
                            manmildhtn_30_30==T),manmildhtn_29_30:=T]

xtabs(~ar$manmildhtn_29_30, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevhtn_29_30:=as.logical(NA)]
ar[Oppt_bp_29_30==T & (!is.na(manmodsevhtn_29_29)|
                         !is.na(manmodsevhtn_30_30)),manmodsevhtn_29_30:=F]

xtabs(~ar$manmodsevhtn_29_30, addNA=T)


ar[manmodsevhtn_29_30==F & (manmodsevhtn_29_29==T|
                              manmodsevhtn_30_30==T),manmodsevhtn_29_30:=T]

xtabs(~ar$manmodsevhtn_29_30, addNA=T)



################
#31-33 weeks
################

#screening
ar[,Oppt_bp_31_33:=as.logical(NA)]
ar[TrialOne_anvisitnew_31_33==T &
     is.na(manmodsevhtn_29_30) & 
     is.na(manmildhtn_29_30),Oppt_bp_31_33:=TRUE]
xtabs(~ar$Oppt_bp_31_33,addNA=T)

# numerator
ar[,bpontime_31_33:=as.logical(NA)]
ar[Oppt_bp_31_33==TRUE,bpontime_31_33:=FALSE]
ar[TrialOne_anbpsyst_present_31_33==T &
     TrialOne_anbpdiast_present_31_33==T &
     bpontime_31_33==F,bpontime_31_33:=TRUE]

xtabs(~ar$bpontime_31_33)


################
#31-33 weeks
################

#management

#management
#31 weeks
#mild 
ar[,manmildhtn_31_31:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_mildHTN_31_31==T|
                         TrialOne_anbpsyst_mildHTN_31_31==T),manmildhtn_31_31:=F]

ar[manmildhtn_31_31==F & (TrialOne_anbpsyst_present_32_32==T &
                            TrialOne_anbpdiast_present_32_32==T),manmildhtn_31_31:=T]

# mod/sev 
ar[,manmodsevhtn_31_31:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_modSevHTN_31_31==T|
                         TrialOne_anbpsyst_modSevHTN_31_31==T),manmodsevhtn_31_31:=F]

ar[manmodsevhtn_31_31==F & (TrialOne_refHR_31_31==T|
                              TrialOne_refHosp_31_31==T|
                              TrialOne_refSpec_31_31==T),manmodsevhtn_31_31:=T]

xtabs(~ar$manmodsevhtn_31_31, addNA=T)

# 32 weeks
#mild 
ar[,manmildhtn_32_32:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_mildHTN_32_32==T|
                         TrialOne_anbpsyst_mildHTN_32_32==T),manmildhtn_32_32:=F]

ar[manmildhtn_32_32==F & (TrialOne_anbpsyst_present_33_33==T &
                            TrialOne_anbpdiast_present_33_33==T),manmildhtn_32_32:=T]

# mod/sev 
ar[,manmodsevhtn_32_32:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_modSevHTN_32_32==T|
                         TrialOne_anbpsyst_modSevHTN_32_32==T),manmodsevhtn_32_32:=F]

ar[manmodsevhtn_32_32==F & (TrialOne_refHR_32_32==T|
                              TrialOne_refHosp_32_32==T|
                              TrialOne_refSpec_32_32==T),manmodsevhtn_32_32:=T]

xtabs(~ar$manmodsevhtn_32_32, addNA=T)

# 33 weeks
#mild 
ar[,manmildhtn_33_33:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_mildHTN_33_33==T|
                         TrialOne_anbpsyst_mildHTN_33_33==T),manmildhtn_33_33:=F]

ar[manmildhtn_33_33==F & (TrialOne_anbpsyst_present_34_34==T &
                            TrialOne_anbpdiast_present_34_34==T),manmildhtn_33_33:=T]

# mod/sev 
ar[,manmodsevhtn_33_33:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (TrialOne_anbpdiast_modSevHTN_33_33==T|
                         TrialOne_anbpsyst_modSevHTN_33_33==T),manmodsevhtn_33_33:=F]

ar[manmodsevhtn_33_33==F & (TrialOne_refHR_33_33==T|
                              TrialOne_refHosp_33_33==T|
                              TrialOne_refSpec_33_33==T),manmodsevhtn_33_33:=T]

xtabs(~ar$manmodsevhtn_33_33, addNA=T)


################
# combined 31-33
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildhtn_31_33:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (!is.na(manmildhtn_31_31)|
                         !is.na(manmildhtn_32_32)|
                         !is.na(manmildhtn_33_33)),manmildhtn_31_33:=F]

xtabs(~ar$manmildhtn_31_33, addNA=T)


ar[manmildhtn_31_33==F & (manmildhtn_31_31==T|
                            manmildhtn_32_32==T|
                            manmildhtn_33_33==T),manmildhtn_31_33:=T]

xtabs(~ar$manmildhtn_31_33, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevhtn_31_33:=as.logical(NA)]
ar[Oppt_bp_31_33==T & (!is.na(manmodsevhtn_31_31)|
                         !is.na(manmodsevhtn_32_32)|
                         !is.na(manmodsevhtn_33_33)),manmodsevhtn_31_33:=F]

xtabs(~ar$manmodsevhtn_31_33, addNA=T)


ar[manmodsevhtn_31_33==F & (manmodsevhtn_31_31==T|
                              manmodsevhtn_32_32==T|
                              manmodsevhtn_33_33==T),manmodsevhtn_31_33:=T]

xtabs(~ar$manmodsevhtn_31_33, addNA=T)

################
# 34 weeks
################

#screening
ar[,Oppt_bp_34_34:=as.logical(NA)]
ar[TrialOne_anvisitnew_34_34==T &
     is.na(manmodsevhtn_31_33) & 
     is.na(manmildhtn_31_33),Oppt_bp_34_34:=TRUE]
xtabs(~ar$Oppt_bp_34_34,addNA=T)

# numerator
ar[,bpontime_34_34:=as.logical(NA)]
ar[Oppt_bp_34_34==TRUE,bpontime_34_34:=FALSE]
ar[TrialOne_anbpsyst_present_34_34==T &
     TrialOne_anbpdiast_present_34_34==T &
     bpontime_34_34==F,bpontime_34_34:=TRUE]

xtabs(~ar$bpontime_34_34)

#management
#34 weeks
#mild 
ar[,manmildhtn_34_34:=as.logical(NA)]
ar[bpontime_34_34==T & (TrialOne_anbpdiast_mildHTN_34_34==T|
                          TrialOne_anbpsyst_mildHTN_34_34==T),manmildhtn_34_34:=F]

ar[manmildhtn_34_34==F & (TrialOne_anbpsyst_present_35_35==T &
                            TrialOne_anbpdiast_present_35_35==T),manmildhtn_34_34:=T]

# mod/sev 
ar[,manmodsevhtn_34_34:=as.logical(NA)]
ar[Oppt_bp_34_34==T & (TrialOne_anbpdiast_modSevHTN_34_34==T|
                         TrialOne_anbpsyst_modSevHTN_34_34==T),manmodsevhtn_34_34:=F]

ar[manmodsevhtn_34_34==F & (TrialOne_refHR_34_34==T|
                              TrialOne_refHosp_34_34==T|
                              TrialOne_refSpec_34_34==T),manmodsevhtn_34_34:=T]

xtabs(~ar$manmodsevhtn_34_34, addNA=T)


################
#35-37 weeks
################

#screening
ar[,Oppt_bp_35_37:=as.logical(NA)]
ar[TrialOne_anvisitnew_35_37==T &
     is.na(manmildhtn_34_34) &
     is.na(manmodsevhtn_34_34),Oppt_bp_35_37:=TRUE]
xtabs(~ar$Oppt_bp_35_37,addNA=T)

# numerator
ar[,bpontime_35_37:=as.logical(NA)]
ar[Oppt_bp_35_37==TRUE,bpontime_35_37:=FALSE]
ar[TrialOne_anbpsyst_present_35_37==T &
     TrialOne_anbpdiast_present_35_37==T &
     bpontime_35_37==F,bpontime_35_37:=TRUE]

xtabs(~ar$bpontime_35_37)


################
#35-37 weeks
################
#management
#35-37 weeks
#mild 
ar[,manmildhtn_35_35:=as.logical(NA)]
ar[bpontime_35_37==T & (TrialOne_anbpdiast_mildHTN_35_35==T|
                          TrialOne_anbpsyst_mildHTN_35_35==T),manmildhtn_35_35:=F]

ar[manmildhtn_35_35==F & (TrialOne_anbpsyst_present_36_36==T &
                            TrialOne_anbpdiast_present_36_36==T),manmildhtn_35_35:=T]

xtabs(~ar$manmildhtn_35_35, addNA=T)

# mod/sev 
ar[,manmodsevhtn_35_35:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (TrialOne_anbpdiast_modSevHTN_35_35==T|
                         TrialOne_anbpsyst_modSevHTN_35_35==T),manmodsevhtn_35_35:=F]

ar[manmodsevhtn_35_35==F & (TrialOne_refHR_35_35==T|
                              TrialOne_refHosp_35_35==T|
                              TrialOne_refSpec_35_35==T),manmodsevhtn_35_35:=T]

xtabs(~ar$manmodsevhtn_35_35, addNA=T)

# 36
#mild 
ar[,manmildhtn_36_36:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (TrialOne_anbpdiast_mildHTN_36_36==T|
                         TrialOne_anbpsyst_mildHTN_36_36==T),manmildhtn_36_36:=F]

ar[manmildhtn_36_36==F & (TrialOne_anbpsyst_present_37_37==T &
                            TrialOne_anbpdiast_present_37_37==T),manmildhtn_36_36:=T]

xtabs(~ar$manmildhtn_36_36, addNA=T)


# mod/sev 
ar[,manmodsevhtn_36_36:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (TrialOne_anbpdiast_modSevHTN_36_36==T|
                         TrialOne_anbpsyst_modSevHTN_36_36==T),manmodsevhtn_36_36:=F]

ar[manmodsevhtn_36_36==F & (TrialOne_refHR_36_36==T|
                              TrialOne_refHosp_36_36==T|
                              TrialOne_refSpec_36_36==T),manmodsevhtn_36_36:=T]

xtabs(~ar$manmodsevhtn_36_36, addNA=T)



# 37
#mild 
ar[,manmildhtn_37_37:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (TrialOne_anbpdiast_mildHTN_37_37==T|
                         TrialOne_anbpsyst_mildHTN_37_37==T),manmildhtn_37_37:=F]

ar[manmildhtn_37_37==F & (TrialOne_anbpsyst_present_38_38==T &
                            TrialOne_anbpdiast_present_38_38==T),manmildhtn_37_37:=T]

xtabs(~ar$manmildhtn_37_37, addNA=T)

# mod/sev 
ar[,manmodsevhtn_37_37:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (TrialOne_anbpdiast_modSevHTN_37_37==T|
                         TrialOne_anbpsyst_modSevHTN_37_37==T),manmodsevhtn_37_37:=F]

ar[manmodsevhtn_37_37==F & (TrialOne_refHR_37_37==T|
                              TrialOne_refHosp_37_37==T|
                              TrialOne_refSpec_37_37==T),manmodsevhtn_37_37:=T]

xtabs(~ar$manmodsevhtn_37_37, addNA=T)




################
# combined 35-37
################


#manmildchronichtn 15-17 weeks combo variable
ar[,manmildhtn_35_37:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (!is.na(manmildhtn_35_35)|
                         !is.na(manmildhtn_36_36)|
                         !is.na(manmildhtn_37_37)),manmildhtn_35_37:=F]

xtabs(~ar$manmildhtn_35_37, addNA=T)


ar[manmildhtn_35_37==F & (manmildhtn_35_35==T|
                            manmildhtn_36_36==T|
                            manmildhtn_37_37==T),manmildhtn_35_37:=T]

xtabs(~ar$manmildhtn_35_37, addNA=T)





# severe chronic htn

#manmodsevchronichtn 15-17 weeks combo variable
ar[,manmodsevhtn_35_37:=as.logical(NA)]
ar[Oppt_bp_35_37==T & (!is.na(manmodsevhtn_35_35)|
                         !is.na(manmodsevhtn_36_36)|
                         !is.na(manmodsevhtn_37_37)),manmodsevhtn_35_37:=F]

xtabs(~ar$manmodsevhtn_35_37, addNA=T)


ar[manmodsevhtn_35_37==F & (manmodsevhtn_35_35==T|
                              manmodsevhtn_36_36==T|
                              manmodsevhtn_37_37==T),manmodsevhtn_35_37:=T]

xtabs(~ar$manmodsevhtn_35_37, addNA=T)





# we will only look at women who booked 22 weeks and before since that is the cut off for chronichtn

ar[,bookedbefore20:=as.logical(NA)]
ar[ident_dhis2_booking==T & bookgestage<20 & bookgestage>0,
   bookedbefore20:=TRUE]

ar[ident_dhis2_booking==T & bookgestage >=20,bookedbefore20:=FALSE]

xtabs(~ar$bookedbefore20, addNA=T)




ar[,bookedbefore22:=as.logical(NA)]
ar[ident_dhis2_booking==T & bookgestage<23 & bookgestage>0,
   bookedbefore22:=TRUE]

ar[ident_dhis2_booking==T & bookgestage >=22,bookedbefore22:=FALSE]

xtabs(~ar$bookedbefore22, addNA=T)

HTN <- ar[!is.na(bookedbefore20),.(N=.N,
                                   "chronichtn"=sum(!is.na(manmildchronichtn_15_17)|
                                                      !is.na(manmodsevchronichtn_15_17)|
                                                      !is.na(manchronichtn_00_14)|
                                                      !is.na(manmildchronichtn_18_19)|
                                                      !is.na(manmodsevchronichtn_18_19)),
                                   "gestational htn"=sum(!is.na(manmildhtn_24_28)|
                                                           !is.na(manmodsevhtn_24_28)|
                                                           !is.na(manmildhtn_29_30)|
                                                           !is.na(manmildchronichtn_23_23)|
                                                           !is.na(manmodsevchronichtn_23_23)|
                                                           !is.na(manmodsevhtn_29_30)|
                                                           !is.na(manmildhtn_31_33)|
                                                           !is.na(manmildhtn_34_34)|
                                                           !is.na(manmildhtn_35_37)|
                                                           !is.na(manmodsevhtn_31_33)|
                                                           !is.na(manmodsevhtn_34_34)|
                                                           !is.na(manmodsevhtn_35_37)|
                                                           !is.na(manmildhtn_20_22)|
                                                           !is.na(manmodsevhtn_20_22))),
          keyby=.(bookyear,
                  bookedbefore20)]


setorder(HTN,bookedbefore20)

openxlsx::write.xlsx(HTN, 
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               sprintf("%s_HTN_updated.xlsx",CLINIC_INTERVENTION_DATE)))


HTN <- ar[!is.na(bookedbefore20),.(N=.N,
                                   "chronichtn"=sum(!is.na(manmildchronichtn_15_17)|
                                                      !is.na(manmodsevchronichtn_15_17)|
                                                      !is.na(manchronichtn_00_14)|
                                                      !is.na(manmildchronichtn_18_19)|
                                                      !is.na(manmodsevchronichtn_18_19)),
                                   "gestational htn"=sum(!is.na(manmildhtn_24_28)|
                                                           !is.na(manmodsevhtn_24_28)|
                                                           !is.na(manmildhtn_29_30)|
                                                           !is.na(manmildchronichtn_23_23)|
                                                           !is.na(manmodsevchronichtn_23_23)|
                                                           !is.na(manmodsevhtn_29_30)|
                                                           !is.na(manmildhtn_31_33)|
                                                           !is.na(manmildhtn_34_34)|
                                                           !is.na(manmildhtn_35_37)|
                                                           !is.na(manmodsevhtn_31_33)|
                                                           !is.na(manmodsevhtn_34_34)|
                                                           !is.na(manmodsevhtn_35_37)|
                                                           !is.na(manmildhtn_20_22)|
                                                           !is.na(manmodsevhtn_20_22))),
          keyby=.(bookyear,
                  bookedbefore20,
                  agecat)]


setorder(HTN,bookedbefore20)

openxlsx::write.xlsx(HTN, 
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               sprintf("%s_HTNbyAgecat_updated.xlsx",CLINIC_INTERVENTION_DATE)))






##############################
# screening htn
##############################

ghtnsc <- ar[ident_dhis2_booking==T,.(N=.N,
                                      Oppt_b4_14=sum(Oppt_bp_00_14==T, na.rm=T),
                                      Screened_b4_14=sum(bpontime_00_14,na.rm=T),
                                      
                                      Oppt_15_17=sum(Oppt_bp_15_17==T, na.rm=T),
                                      Screened_15_17=sum(bpontime_15_17,na.rm=T),
                                      
                                      Oppt_18_22=sum(Oppt_bp_18_22==T, na.rm=T),
                                      Screened_18_22=sum(bpontime_18_22,na.rm=T),
                                      
                                      Oppt_24_28=sum(Oppt_bp_24_28==T, na.rm=T),
                                      Screened_24_28=sum(bpontime_24_28,na.rm=T),
                                      
                                      Oppt_31_33=sum(Oppt_bp_31_33==T, na.rm=T),
                                      Screened_31_33=sum(bpontime_31_33,na.rm=T),
                                      
                                      Oppt_35_37=sum(Oppt_bp_35_37==T, na.rm=T),
                                      Screened_35_37=sum(bpontime_35_37,na.rm=T)),
             keyby=.(bookyear)]



openxlsx::write.xlsx(ghtnsc, 
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               sprintf("%s_HTN_screening.xlsx",CLINIC_INTERVENTION_DATE)))


###################
# Attendance
###################



# making vars
ar[,refHRhosp:= FALSE]
ar[(TrialOne_manRef_HR_00_00==T|
      TrialOne_manRef_HR_01_01==T|
      TrialOne_manRef_HR_02_02==T|
      TrialOne_manRef_HR_03_03==T|
      TrialOne_manRef_HR_04_04==T|
      TrialOne_manRef_HR_05_05==T|
      TrialOne_manRef_HR_06_06==T|
      TrialOne_manRef_HR_07_07==T|
      TrialOne_manRef_HR_08_08==T|
      TrialOne_manRef_HR_09_09==T|
      TrialOne_manRef_HR_10_10==T|
      TrialOne_manRef_HR_11_11==T|
      TrialOne_manRef_HR_12_12==T)|
     TrialOne_manRef_HR_13_13==T|
     (TrialOne_manRef_Hosp_00_00==T|
        TrialOne_manRef_Hosp_01_01==T|
        TrialOne_manRef_Hosp_02_02==T|
        TrialOne_manRef_Hosp_03_03==T|
        TrialOne_manRef_Hosp_04_04==T|
        TrialOne_manRef_Hosp_05_05==T|
        TrialOne_manRef_Hosp_06_06==T|
        TrialOne_manRef_Hosp_07_07==T|
        TrialOne_manRef_Hosp_08_08==T|
        TrialOne_manRef_Hosp_09_09==T|
        TrialOne_manRef_Hosp_10_10==T|
        TrialOne_manRef_Hosp_11_11==T|
        TrialOne_manRef_Hosp_12_12==T|
        TrialOne_manRef_Hosp_13_13==T),refHRhosp:=TRUE]

xtabs(~ar$refHRhosp, addNA=T)

## Define Opportunities
ar[,Trimester1booking:=as.numeric(NA)]

ar[booktrimester=="[0,91]",Trimester1booking:=1]

xtabs(~ar$Trimester1booking, addNA=T)


# opportunity 2nd trimester
ar[,Trimester2visitopp:=as.numeric(NA)]
ar[Trimester1booking==1, Trimester2visitopp:=1]
ar[Trimester1booking==1 & refHRhosp==T,
   Trimester2visitopp:=0]

xtabs(~ar$Trimester2visitopp, addNA=T)


# opportunity third trimester
ar[,Trimester3visitopp:=as.numeric(NA)]
ar[booktrimester %in% c("[0,91]",
                        "(91,189]"),
   Trimester3visitopp:=1]

xtabs(~ar$Trimester3visitopp, addNA=T)

#removing opportunities
ar[Trimester3visitopp==1 & 
     ((TrialOne_manRef_HR_14_14==T|TrialOne_manRef_Hosp_14_14==T)|
        (TrialOne_manRef_HR_15_15==T|TrialOne_manRef_Hosp_15_15==T)|
        (TrialOne_manRef_HR_16_16==T|TrialOne_manRef_Hosp_16_16==T)|
        (TrialOne_manRef_HR_17_17==T|TrialOne_manRef_Hosp_17_17==T)|
        (TrialOne_manRef_HR_18_18==T|TrialOne_manRef_Hosp_18_18==T)|
        (TrialOne_manRef_HR_19_19==T|TrialOne_manRef_Hosp_19_19==T)|
        (TrialOne_manRef_HR_20_20==T|TrialOne_manRef_Hosp_20_20==T)|
        (TrialOne_manRef_HR_21_21==T|TrialOne_manRef_Hosp_21_21==T)|
        (TrialOne_manRef_HR_22_22==T|TrialOne_manRef_Hosp_22_22==T)|
        (TrialOne_manRef_HR_23_23==T|TrialOne_manRef_Hosp_23_23==T)|
        (TrialOne_manRef_HR_24_24==T|TrialOne_manRef_Hosp_24_24==T)|
        (TrialOne_manRef_HR_25_25==T|TrialOne_manRef_Hosp_25_25==T)|
        (TrialOne_manRef_HR_26_26==T|TrialOne_manRef_Hosp_26_26==T)), 
   Trimester3visitopp:=Trimester3visitopp-1]

xtabs(~ar$Trimester3visitopp, addNA=T)



# create referral in 3rd trimester

ar[,ref3rdTr:=as.logical(NA)]

ar[Trimester3visitopp==1 &
     ((TrialOne_manRef_HR_27_27==T|TrialOne_manRef_Hosp_27_27==T)|
        (TrialOne_manRef_HR_29_29==T|TrialOne_manRef_Hosp_29_29==T)|
        (TrialOne_manRef_HR_28_28==T|TrialOne_manRef_Hosp_28_28==T)|
        (TrialOne_manRef_HR_30_30==T|TrialOne_manRef_Hosp_30_30==T)|
        (TrialOne_manRef_HR_31_31==T|TrialOne_manRef_Hosp_31_31==T)|
        (TrialOne_manRef_HR_32_32==T|TrialOne_manRef_Hosp_32_32==T)|
        (TrialOne_manRef_HR_33_33==T|TrialOne_manRef_Hosp_33_33==T)|
        (TrialOne_manRef_HR_34_34==T|TrialOne_manRef_Hosp_34_34==T)|
        (TrialOne_manRef_HR_35_35==T|TrialOne_manRef_Hosp_35_35==T)|
        (TrialOne_manRef_HR_36_36==T|TrialOne_manRef_Hosp_36_36==T)|
        (TrialOne_manRef_HR_37_37==T|TrialOne_manRef_Hosp_37_37==T)), ref3rdTr:=TRUE]

xtabs(~ar$ref3rdTr, addNA=T)



################ successes ##########

# second Trimester
ar[,T2visit:=as.logical(NA)]
ar[Trimester2visitopp==1, T2visit:=FALSE]
ar[Trimester2visitopp==1 &
     (TrialOne_anvisitnew_14_14==T|
        TrialOne_anvisitnew_15_17==T|
        TrialOne_anvisitnew_18_22==T|
        TrialOne_anvisitnew_23_23==T|
        TrialOne_anvisitnew_24_24==T|
        TrialOne_anvisitnew_25_25==T|
        TrialOne_anvisitnew_26_26==T), T2visit:=TRUE]

xtabs(~ar$T2visit, addNA=T)




# third Trimester
ar[,T3visit:=as.logical(NA)]
ar[Trimester3visitopp==1, T3visit:=FALSE]
ar[Trimester3visitopp==1 &
     (TrialOne_anvisitnew_27_27==T|
        TrialOne_anvisitnew_29_29==T|
        TrialOne_anvisitnew_28_28==T|
        TrialOne_anvisitnew_29_29==T|
        TrialOne_anvisitnew_30_30==T|
        TrialOne_anvisitnew_31_31==T|
        TrialOne_anvisitnew_32_32==T|
        TrialOne_anvisitnew_33_33==T|
        TrialOne_anvisitnew_34_34==T|
        TrialOne_anvisitnew_35_35==T|
        TrialOne_anvisitnew_36_36==T|
        TrialOne_anvisitnew_37_37==T), T3visit:=TRUE]

xtabs(~ar$T3visit, addNA=T)



prelimAtt <- ar[ident_dhis2_booking==1,.(
  "First Trimester Bookings"=sum(Trimester1booking==1, na.rm=T),
  "Number Referred in First Trimester"=sum(refHRhosp==T, na.rm=T),
  "Opportunity 2nd Trimester Visited"=sum(Trimester2visitopp==1, na.rm=T),
  "Number Booked 2nd Trimester"=sum(booktrimester=="(91,189]", na.rm=T),
  "Number Visited 2nd Trimester"=sum(T2visit==T, na.rm=T),
  "Number Not Visited 2nd Trimester"=sum(T2visit==F, na.rm=T),
  "Number Referred 2nd Trimester"=sum(Trimester3visitopp==0, na.rm=T),
  "Opportunity 3rd Trimester"=sum(Trimester3visitopp==1, na.rm=T),
  "Number Booked 3rd Trimester"=sum(booktrimester=="(189,280]",na.rm=T),
  "Visited 3rd Trimester"=sum(T3visit==T, na.rm=T),
  "Not Visited 3rd Trimester"=sum(T3visit==F, na.rm=T)),
  
  keyby=.(bookyear)]

openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         paste0(year_folder),
                                         sprintf("%s_Attendance.xlsx",
                                                 lubridate::today()))) 






#################### 
# bookbp categories
####################  

bookbpsystdiast <- ar[ident_dhis2_booking==1,.(N=.N),
                      keyby=.(bookyear,
                              booktrimester,
                              bookbpsystcat,
                              bookbpdiastcat)]

xtabs(~bookbpsystcat+booktrimester,addNA=T,data=ar)



openxlsx::write.xlsx(bookbpsystdiast,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       paste0(year_folder),
                       "BookBPCatsbybookTM.xlsx"))








###########
# GDM
###########

###Redefining opportinites


################
# B4 24 weeks
################
ar[,Opportunity_GDM_screening_b4_24:=as.logical(NA)] 

# before 24
ar[bookgestagedays_cats %in% c("(0,104]",
                               "(104,125]",
                               "(125,160]",
                               "(160,167]"),Opportunity_GDM_screening_b4_24:=TRUE]

xtabs(~ar$Opportunity_GDM_screening_b4_24, addNA=T)

# high sugar at booking
ar[,bookhrhighsug:=as.logical(NA)]

ar[(mandate_1-bookdate)<=7,bookhrhighsug:=FALSE] 
ar[(mandate_1-bookdate)<=7 &
     manperf_1==1 &
     mantypex_1 %in% c("RefHighRisk","RefDiabetes","RefSpec"),bookhrhighsug:=TRUE] 

xtabs(~ar$bookhrhighsug, addNA=T)

################
# opportunities
################

#Screening before 24 weeks: Creating one var for 3 possibilities
#laburglu

ar[,GDMscreeningontime_b4_24_bookurglu_normal:=as.logical(NA)]

ar[Opportunity_GDM_screening_b4_24==TRUE & 
     (!is.na(booklaburglu)), GDMscreeningontime_b4_24_bookurglu_normal:=FALSE]


ar[Opportunity_GDM_screening_b4_24==TRUE & 
     GDMscreeningontime_b4_24_bookurglu_normal==FALSE &
     (booklaburglu=="NEG"), 
   GDMscreeningontime_b4_24_bookurglu_normal:=TRUE]

xtabs(~ar$GDMscreeningontime_b4_24_bookurglu_normal,addNA=T)



# fastbloodglu
ar[,GDMscreeningontime_b4_24_bookfastbloodglu_normal:=as.logical(NA)]

ar[Opportunity_GDM_screening_b4_24==TRUE & 
     (!is.na(booklabfastbloodglu)), GDMscreeningontime_b4_24_bookfastbloodglu_normal:=FALSE]


ar[Opportunity_GDM_screening_b4_24==TRUE & 
     GDMscreeningontime_b4_24_bookfastbloodglu_normal==FALSE &
     (booklabfastbloodglu_high=="FALSE"), 
   GDMscreeningontime_b4_24_bookfastbloodglu_normal:=TRUE]

xtabs(~ar$GDMscreeningontime_b4_24_bookfastbloodglu_normal,addNA=T)


# booklabbloodglu
ar[,GDMscreeningontime_b4_24_bookbloodglu_normal:=as.logical(NA)]

ar[Opportunity_GDM_screening_b4_24==TRUE & 
     (!is.na(booklabbloodglu)), GDMscreeningontime_b4_24_bookbloodglu_normal:=FALSE]


ar[Opportunity_GDM_screening_b4_24==TRUE & 
     GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
     (booklabbloodglu_high=="FALSE"), 
   GDMscreeningontime_b4_24_bookbloodglu_normal:=TRUE]


xtabs(~ar$GDMscreeningontime_b4_24_bookbloodglu_normal,addNA=T)

# managements
##################################### QUESTION ##################################### 
### question: are we keeping the rbs and fbs at booking as well? 
### or would it be at any time point?####
###################################################################################

# do this week by week, because management has to be up to one week later
# gdm screening if have pos urglu and have h
ar[,GDMscreeningontime_b4_24_manposurglu:=as.logical(NA)]
ar[GDMscreeningontime_b4_24_bookurglu_normal==FALSE &
     booklaburglu=="POS",GDMscreeningontime_b4_24_manposurglu:=FALSE]

ar[GDMscreeningontime_b4_24_manposurglu==F &
     (!is.na(booklabbloodglu)|
        !is.na(booklabfastbloodglu)), GDMscreeningontime_b4_24_manposurglu:=TRUE]

xtabs(~ar$GDMscreeningontime_b4_24_manposurglu, addNA=T)


# management of high rbg and fbs
ar[,GDMscreeningontime_b4_24_manhighrbs:=as.logical(NA)]
ar[GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
     is.na(GDMscreeningontime_b4_24_manposurglu) &
     (booklabbloodglu_high==T|booklabfastbloodglu_high==T),GDMscreeningontime_b4_24_manhighrbs:=FALSE]

ar[GDMscreeningontime_b4_24_manhighrbs==F &
     (bookhrhighsug==T), GDMscreeningontime_b4_24_manhighrbs:=TRUE]

xtabs(~ar$GDMscreeningontime_b4_24_manhighrbs, addNA=T)



################################
## 24-28 weeks
################################



###################
#  opportunity
###################
ar[,Opportunity_GDM_screening_24_28:=as.logical(NA)]


#24-28
ar[TrialOne_anvisitnew_24_28==T |
     bookgestagedays_cats %in% c("(167,202]"),Opportunity_GDM_screening_24_28:=TRUE]

## Remove opportunities for people who had high blood glu
ar[Opportunity_GDM_screening_24_28==TRUE &
     (TrialOne_labbloodglu_high_00_14==T|
        TrialOne_labbloodglu_high_15_17==T|
        TrialOne_labbloodglu_high_18_22==T|
        TrialOne_labbloodglu_high_23_23==T|
        TrialOne_labfastbloodglu_high_00_14==T|
        TrialOne_labfastbloodglu_high_15_17==T|
        TrialOne_labfastbloodglu_high_18_22==T|
        TrialOne_labfastbloodglu_high_23_23==T),
   Opportunity_GDM_screening_24_28:=FALSE]


xtabs(~ar$Opportunity_GDM_screening_24_28, addNA=T)

###################
# screening 24-28
###################


ar[,GDMscreeningontime_24_28:=as.logical(NA)]
ar[Opportunity_GDM_screening_24_28==TRUE &
     (TrialOne_labfastbloodglu_exists_24_28==T|
        TrialOne_labbloodglu_exists_24_28==T), GDMscreeningontime_24_28:=T]

xtabs(~ar$GDMscreeningontime_24_28, addNA=T)
# normal values
ar[,GDMscreeningontime_24_28_normal:=as.logical(NA)]


ar[GDMscreeningontime_24_28==T, GDMscreeningontime_24_28_normal:=F]

ar[Opportunity_GDM_screening_24_28==TRUE & 
     GDMscreeningontime_24_28_normal==F &
     (TrialOne_labfastbloodglu_normal_24_28==T),GDMscreeningontime_24_28_normal:=TRUE]

xtabs(~ar$GDMscreeningontime_24_28_normal, addNA=T)

###################
# management 24-28
###################


# highrbg 24 weeks
ar[,GDMscreeningontime_24_24_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (TrialOne_labbloodglu_high_24_24==T |
        TrialOne_labfastbloodglu_high_24_24==T),GDMscreeningontime_24_24_manhighrbg:=F]


ar[GDMscreeningontime_24_24_manhighrbg==F &
     (TrialOne_manRBGHigh_Diab_24_24==T|
        TrialOne_manRef_HR_24_24==T), GDMscreeningontime_24_24_manhighrbg:=T]


# highrbg 25 weeks
ar[,GDMscreeningontime_25_25_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (TrialOne_labbloodglu_high_25_25==T |
        TrialOne_labfastbloodglu_high_25_25==T),GDMscreeningontime_25_25_manhighrbg:=F]


ar[GDMscreeningontime_25_25_manhighrbg==F &
     (TrialOne_manRBGHigh_Diab_25_25==T|
        TrialOne_manRef_HR_25_25==T), GDMscreeningontime_25_25_manhighrbg:=T]



# highrbg 26 weeks
ar[,GDMscreeningontime_26_26_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (TrialOne_labbloodglu_high_26_26==T |
        TrialOne_labfastbloodglu_high_26_26==T),GDMscreeningontime_26_26_manhighrbg:=F]


ar[GDMscreeningontime_26_26_manhighrbg==F &
     (TrialOne_manRBGHigh_Diab_26_26==T|
        TrialOne_manRef_HR_26_26==T), GDMscreeningontime_26_26_manhighrbg:=T]


# highrbg 27 weeks
ar[,GDMscreeningontime_27_27_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (TrialOne_labbloodglu_high_27_27==T |
        TrialOne_labfastbloodglu_high_27_27==T),GDMscreeningontime_27_27_manhighrbg:=F]


ar[GDMscreeningontime_27_27_manhighrbg==F &
     (TrialOne_manRBGHigh_Diab_27_27==T|
        TrialOne_manRef_HR_27_27==T), GDMscreeningontime_27_27_manhighrbg:=T]


# highrbg 28 weeks
ar[,GDMscreeningontime_28_28_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (TrialOne_labbloodglu_high_28_28==T |
        TrialOne_labfastbloodglu_high_28_28==T),GDMscreeningontime_28_28_manhighrbg:=F]


ar[GDMscreeningontime_28_28_manhighrbg==F &
     (TrialOne_manRBGHigh_Diab_28_28==T|
        TrialOne_manRef_HR_28_28==T), GDMscreeningontime_28_28_manhighrbg:=T]


# combined group

ar[,GDMscreeningontime_24_28_manhighrbg:=as.logical(NA)]
ar[GDMscreeningontime_24_28==T &
     GDMscreeningontime_24_28_normal==FALSE &
     (!is.na(GDMscreeningontime_24_24_manhighrbg)|
        !is.na(GDMscreeningontime_25_25_manhighrbg)|
        !is.na(GDMscreeningontime_26_26_manhighrbg)|
        !is.na(GDMscreeningontime_27_27_manhighrbg)|
        !is.na(GDMscreeningontime_28_28_manhighrbg)),
   GDMscreeningontime_24_28_manhighrbg:=F]

xtabs(~ar$GDMscreeningontime_24_28_manhighrbg, addNA=T)


ar[GDMscreeningontime_24_28_manhighrbg==F & 
     (GDMscreeningontime_24_24_manhighrbg==T|
        GDMscreeningontime_25_25_manhighrbg==T|
        GDMscreeningontime_26_26_manhighrbg==T|
        GDMscreeningontime_27_27_manhighrbg==T|
        GDMscreeningontime_28_28_manhighrbg==T),
   GDMscreeningontime_24_28_manhighrbg:=T]

xtabs(~ar$GDMscreeningontime_24_28_manhighrbg, addNA=T)

########################
# intermediate values
########################

if(IS_GAZA){
  
  # intermediate values
  
  # intermediate values,  but dont want them for WB because management is in free text
  T2[,T2_GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28==T &
       T2_GDMscreeningontime_24_28_normal==FALSE &
       is.na(T2_GDMscreeningontime_24_28_manhighrbg) &
       (T2_labbloodglu_likelyGDM_24_28==T|
          T2_labfastbloodglu_likelyGDM_24_28==T),T2_GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_intmbg, addNA=T)
  # managment is repeat FBS with in 3 weeks
  
  # do this by one week intervals
  
  # 24 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_24_24==T|
          T2_labbloodglu_likelyGDM_24_24==T),T2_GDMscreeningontime_24_24_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_24_24_manintmbg==F &
       T2_repeatFBS_27_27==T,T2_GDMscreeningontime_24_24_manintmbg:=T]
  
  
  # 25 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_25_25==T|
          T2_labbloodglu_likelyGDM_25_25==T),T2_GDMscreeningontime_25_25_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_25_25_manintmbg==F &
       T2_repeatFBS_28_28==T,T2_GDMscreeningontime_25_25_manintmbg:=T]
  
  
  # 26 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_26_26==T|
          T2_labbloodglu_likelyGDM_26_26),T2_GDMscreeningontime_26_26_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_26_26_manintmbg==F &
       T2_repeatFBS_29_29==T,T2_GDMscreeningontime_26_26_manintmbg:=T]
  
  # 27 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_27_27==T |
          T2_labbloodglu_likelyGDM_27_27==T),T2_GDMscreeningontime_27_27_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_27_27_manintmbg==F &
       T2_repeatFBS_30_30==T,T2_GDMscreeningontime_27_27_manintmbg:=T]
  
  
  # 28 weeks
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (T2_labfastbloodglu_likelyGDM_28_28==T |
          T2_labbloodglu_likelyGDM_28_28==T),T2_GDMscreeningontime_28_28_manintmbg:=FALSE]
  
  T2[T2_GDMscreeningontime_28_28_manintmbg==F &
       T2_repeatFBS_31_31==T,T2_GDMscreeningontime_28_28_manintmbg:=T]
  
  
  
  # combined variable
  
  T2[,T2_GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  T2[T2_GDMscreeningontime_24_28_intmbg==T &
       (!is.na(T2_GDMscreeningontime_28_28_manintmbg)|
          !is.na(T2_GDMscreeningontime_27_27_manintmbg)|
          !is.na(T2_GDMscreeningontime_26_26_manintmbg)|
          !is.na(T2_GDMscreeningontime_25_25_manintmbg)|
          !is.na(T2_GDMscreeningontime_24_24_manintmbg)),T2_GDMscreeningontime_24_28_manintmbg:=FALSE ]
  
  T2[T2_GDMscreeningontime_24_28_manintmbg==F &
       (T2_GDMscreeningontime_24_24_manintmbg==T |
          T2_GDMscreeningontime_25_25_manintmbg==T|
          T2_GDMscreeningontime_26_26_manintmbg==T|
          T2_GDMscreeningontime_27_27_manintmbg==T|
          T2_GDMscreeningontime_28_28_manintmbg==T), T2_GDMscreeningontime_24_28_manintmbg:=TRUE]
  
  xtabs(~T2$T2_GDMscreeningontime_24_28_manintmbg, addNA=T) } else {
    
    # intermediate values,  but dont want them for WB because management is in free text
    ar[,GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
    ar[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         is.na(GDMscreeningontime_24_28_manhighrbg) &
         (TrialOne_labbloodglu_likelyGDM_24_28==T),GDMscreeningontime_24_28_intmbg:=TRUE]
    
    xtabs(~ar$GDMscreeningontime_24_28_intmbg, addNA=T)
    
    
    
    ar[,GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_24_24_manintmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_25_25_manintmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_26_26_manintmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_27_27_manintmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_28_28_manintmbg:=as.logical(NA)]
    ar[,GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
    
    
    
    xtabs(~ar$GDMscreeningontime_24_28, addNA = T)
    xtabs(~ar$GDMscreeningontime_24_28_normal, addNA = T)
    #xtabs(~ar$GDMscreeningontime_24_28_highrbg, addNA = T)
    xtabs(~ar$GDMscreeningontime_24_28_intmbg, addNA = T)
    
  }



################
# > 28 weeks
################
ar[,Opportunity_GDM_screening_after_28:=as.logical(NA)]


# checks
xtabs(~ar$Opportunity_GDM_screening_24_28, addNA=T)


##############
# after 28
##############
## defining opportunities
# after 28
ar[bookgestagedays_cats %in% c("(202,216]",
                               "(216,237]",
                               "(237,244]",
                               "(244,265]"), Opportunity_GDM_screening_after_28:=TRUE]
xtabs(~ar$Opportunity_GDM_screening_after_28, addNA=T)

## defining successes
ar[,GDMscreeningontime_after_28:=as.logical(NA)]
ar[,GDMscreeningontime_after_28_normal:=as.logical(NA)]
ar[,GDMscreeningontime_after_28_high:=as.logical(NA)]
ar[,GDMscreeningontime_after_28_intmd:=as.logical(NA)]



# anyone who has a fasting or blood glu value and booked after 28 weeks
ar[Opportunity_GDM_screening_after_28==TRUE, 
   GDMscreeningontime_after_28:=FALSE]

ar[GDMscreeningontime_after_28==F &
     (!is.na(booklabbloodglu)|
        !is.na(booklabfastbloodglu)), 
   GDMscreeningontime_after_28:=TRUE]

#xtabs(~ar$screenafter28, addNA=T)
xtabs(~ar$GDMscreeningontime_after_28, addNA=T)

#normal Values/ negative values
ar[,GDMscreeningontime_after_28_normal:=as.logical(NA)]

ar[Opportunity_GDM_screening_after_28==TRUE & 
     GDMscreeningontime_after_28==T,
   GDMscreeningontime_after_28_normal:=FALSE]

ar[GDMscreeningontime_after_28_normal==FALSE &
     ((booklabbloodglu_high==FALSE &
         !is.na(booklabbloodglu))|
        (booklabfastbloodglu_high==F &
           !is.na(booklabfastbloodglu))),GDMscreeningontime_after_28_normal:=T ]

xtabs(~ar$GDMscreeningontime_after_28_normal, addNA=T)


# high values
ar[,GDMscreeningontime_after_28_high:=as.logical(NA)]
ar[GDMscreeningontime_after_28_normal==FALSE,GDMscreeningontime_after_28_high:=FALSE]


# management
ar[GDMscreeningontime_after_28_high==FALSE &
     bookhrhighsug==T,
   GDMscreeningontime_after_28_high:=TRUE]
xtabs(~ar$GDMscreeningontime_after_28_high, addNA=T)


xtabs(~ar$GDMscreeningontime_after_28, addNA=T)
xtabs(~ar$GDMscreeningontime_after_28_normal, addNA=T)
xtabs(~ar$GDMscreeningontime_after_28_high, addNA=T)


# only look at those who booked before 24 weeks and after
ar[,bookedb424:=as.logical(NA)]
ar[ident_dhis2_booking==T & bookgestagedays_cats %in% c("(0,104]",
                                                        "(104,125]",
                                                        "(125,160]",
                                                        "(160,167]"), bookedb424:=TRUE]

ar[ident_dhis2_booking==T & bookgestagedays_cats %in% c("(202,216]",
                                                        "(216,237]",
                                                        "(237,244]",
                                                        "(244,265]",
                                                        "(265,293]"), bookedb424:=FALSE]
xtabs(~ar$bookedb424, addNA=T)

GDM <- ar[!is.na(bookedb424),.(N=.N,
                               oppt_b4_24=sum(Opportunity_GDM_screening_b4_24==T, na.rm=T),
                               screenedb424=sum(!is.na(GDMscreeningontime_b4_24_bookurglu_normal)),
                               posurglub424=sum(GDMscreeningontime_b4_24_bookurglu_normal==FALSE, na.rm=T),
                               Oppt_24_28_sc=sum(Opportunity_GDM_screening_24_28==T, na.rm=T),
                               Screen2428=sum(GDMscreeningontime_24_28==T,na.rm=T),
                               
                               DM=sum(!is.na(GDMscreeningontime_b4_24_manhighrbs)),
                               Oppt_after_28=sum(Opportunity_GDM_screening_after_28==T, na.rm=T),
                               screened28andafter=sum(GDMscreeningontime_after_28==T, na.rm=T),
                               
                               GDMDiabetes=sum(!is.na(GDMscreeningontime_24_28_manhighrbg)|
                                                 !is.na(GDMscreeningontime_after_28_high))),
          
          keyby=.(bookyear,bookedb424)]

setorder(GDM,bookedb424)

openxlsx::write.xlsx(GDM,file.path(FOLDER_DATA_RESULTS,
                                   "annual reports",
                                   paste0(year_folder),
                                   sprintf("%s_GDM.xlsx",
                                           lubridate::today()))) 




####################
# Bookmonths
####################

tab <- ar[ident_dhis2_booking==T,.(N=.N),
          keyby=.(bookyear,bookmonth)]

openxlsx::write.xlsx(tab,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               paste0(year_folder),
                               "bookingspermonth.xlsx"))



#### Total visits #### 

tab <- ar[bookyear>=2019 & ident_dhis2_control==F,.(
  #TotalRegisteredWomen=sum(ident_dhis2_booking==T, na.rm=T),
  #TotalBookingandAncVisits=sum(anevent_x, na.rm=T),
  #TotalPPCRegistrations=sum(ident_dhis2_ppc==T, na.rm=T),
  #TotalNBCregistrations=sum(ident_dhis2_nbc==T, na.rm=T),
  #TotalPPCvisits=sum(ppcevent_x, na.rm=T),
  # TotalNBCvisits=sum(nbcevent_x, na.rm=T),
  ANCvisits2019=sum(anevent_x_1, na.rm=T),
  ANCvisits2020=sum(anevent_x, na.rm=T),
  ANCvisits2021=sum(anevent_x2, na.rm=T),
  PPCvisits2019=sum(ppcevent_x_1,na.rm=T),
  PPCvisits2020=sum(ppcevent_x, na.rm=T),
  PPCvisits2021=sum(ppcevent_x2, na.rm=T),
  NBCvisits2019=sum(nbcevent_x, na.rm = T),
  NBCvisits2020=sum(nbcevent_x_1,na.rm=T),
  NBCvisits2021=sum(nbcevent_x2,na.rm=T))]

openxlsx::write.xlsx(tab, file.path(FOLDER_DATA_RESULTS,
                                    "annual reports",
                                    paste0(year_folder),
                                    "VisitsTotal_.xlsx"))





# visits by month and year for anc, ppc, and nbc by month and year


# Number of ANC, PPC, NBC per district per month

ar[,andate_0:=bookdate]

andate <- names(ar)[stringr::str_detect(names(ar),"^andate_[0-9]+")]
nbcdate <- names(ar)[stringr::str_detect(names(ar),"^nbcdate_[0-9]+")]
ppcdate <- names(ar)[stringr::str_detect(names(ar),"^ppcdate_[0-9]+")]

visits <- ar[,c("uniqueid",
                andate,
                nbcdate,
                ppcdate),
             with=F]

# melt it to long format to make the curve



long <- melt(visits,
             id.vars = c("uniqueid"),
             measure.vars = patterns("^andate", "^nbcdate","^ppcdate"),
             variable.name = "value",
             value.name = c("andate", "nbcdate","ppcdate"))

long[,value:=NULL]

# month and year for andate
long[,anmonth:=as.numeric(substr(andate,6,7))]
long[,anyear:=as.numeric(stringr::str_extract(andate,"^[0-9][0-9][0-9][0-9]"))]
xtabs(~long$anyear, addNA=T)
xtabs(~long$anmonth, addNA=T)


# month and year for nbcdate
long[,nbcmonth:=as.numeric(substr(nbcdate,6,7))]
long[,nbcyear:=as.numeric(stringr::str_extract(nbcdate,"^[0-9][0-9][0-9][0-9]"))]
xtabs(~long$nbcyear, addNA=T)
xtabs(~long$nbcmonth, addNA=T)


# month and year for ppc
long[,ppcmonth:=as.numeric(substr(ppcdate,6,7))]
long[,ppcyear:=as.numeric(stringr::str_extract(ppcdate,"^[0-9][0-9][0-9][0-9]"))]
xtabs(~long$ppcyear, addNA=T)
xtabs(~long$ppcmonth, addNA=T)

anc <- long[,.(ancvisits=.N),
            keyby=.(anyear, anmonth)]


nbc <- long[,.(nbcvisits=.N),
            keyby=.(nbcyear, nbcmonth)]


ppc <- long[,.(ppcvisits=.N),
            keyby=.(ppcyear, ppcmonth)]

all <- cbind(anc,
             nbc)

complete <- cbind(all,
                  ppc)

# remove other years and months stuff
complete[,nbcyear:=NULL]
complete[,nbcmonth:=NULL]

complete[,ppcyear:=NULL]
complete[,ppcmonth:=NULL]

setnames(complete,c("anyear","anmonth"),
         c("Year","Month"))

openxlsx::write.xlsx(complete, file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         paste0(year_folder),
                                         "VisitsbymonthandYear.xlsx"))


###############
# visits by year
###############
bookingsbyyear <- ar[,c("uniqueid",
                        "bookevent",
                        "bookdate")]

bookingsbyyear[,bookyear:=as.numeric(stringr::str_extract(bookdate,"^[0-9][0-9][0-9][0-9]"))]

bookingsbyyear <- bookingsbyyear[,.(bookingvisits=.N),
                                 keyby=.(bookyear)]

anc <- long[,.(ancvisits=.N),
            keyby=.(anyear)]
anc <-anc[!is.na(anyear)]

nbc <- long[,.(nbcvisits=.N),
            keyby=.(nbcyear)]
nbc <-nbc[!is.na(nbcyear)]


ppc <- long[,.(ppcvisits=.N),
            keyby=.(ppcyear)]
ppc <-ppc[!is.na(ppcyear)]

# bind all these data #

all <- cbind(bookingsbyyear,
             anc)

all <- cbind(all,
             nbc)

complete <- cbind(all,
                  ppc)



openxlsx::write.xlsx(complete, file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         paste0(year_folder),
                                         "VisitsbyYear.xlsx"))



###########

#################################  Attendance ################################
# making vars
ar[,refHRhosp:= FALSE]
ar[(TrialOne_manRef_HR_00_00==T|
      TrialOne_manRef_HR_01_01==T|
      TrialOne_manRef_HR_02_02==T|
      TrialOne_manRef_HR_03_03==T|
      TrialOne_manRef_HR_04_04==T|
      TrialOne_manRef_HR_05_05==T|
      TrialOne_manRef_HR_06_06==T|
      TrialOne_manRef_HR_07_07==T|
      TrialOne_manRef_HR_08_08==T|
      TrialOne_manRef_HR_09_09==T|
      TrialOne_manRef_HR_10_10==T|
      TrialOne_manRef_HR_11_11==T|
      TrialOne_manRef_HR_12_12==T|
      TrialOne_manRef_HR_13_13==T|
      TrialOne_manRef_HR_14_14==T)|
     (TrialOne_manRef_Hosp_00_00==T|
        TrialOne_manRef_Hosp_01_01==T|
        TrialOne_manRef_Hosp_02_02==T|
        TrialOne_manRef_Hosp_03_03==T|
        TrialOne_manRef_Hosp_04_04==T|
        TrialOne_manRef_Hosp_05_05==T|
        TrialOne_manRef_Hosp_06_06==T|
        TrialOne_manRef_Hosp_07_07==T|
        TrialOne_manRef_Hosp_08_08==T|
        TrialOne_manRef_Hosp_09_09==T|
        TrialOne_manRef_Hosp_10_10==T|
        TrialOne_manRef_Hosp_11_11==T|
        TrialOne_manRef_Hosp_12_12==T|
        TrialOne_manRef_Hosp_13_13==T|
        TrialOne_manRef_Hosp_14_14==T),refHRhosp:=TRUE]

xtabs(~ar$refHRhosp, addNA=T)

## Define Opportunities

# oppt 16 week visit
ar[,Opp_1:= as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(0,104]"),Opp_1:=1]
ar[bookgestagedays_cats %in% c("(0,104]") &
     refHRhosp==T,Opp_1:=0]
xtabs(~ar$Opp_1, addNA=T)



# oppt 18-22 visit
ar[,Opp_2:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(104,125]")| Opp_1==1, Opp_2:=1]

xtabs(~ar$Opp_2, addNA=T)

#removing opportunities
ar[Opp_2==1 & 
     (TrialOne_manRef_HR_15_15==T|TrialOne_manRef_Hosp_15_15==T)|
     (TrialOne_manRef_HR_16_16==T|TrialOne_manRef_Hosp_16_16==T)|
     (TrialOne_manRef_HR_17_17==T|TrialOne_manRef_Hosp_17_17==T),
   Opp_2:=Opp_2-1]

xtabs(~ar$Opp_2, addNA=T)


# 24-28 week visit
ar[,Opp_3:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(125,160]",
                               "(160,167]") | Opp_2==1, Opp_3:=1]

xtabs(~ar$Opp_3, addNA=T)

# removing opportunities
ar[Opp_3==1 & ((TrialOne_manRef_HR_18_18==T|TrialOne_manRef_Hosp_18_18==T)|
                 (TrialOne_manRef_HR_19_19==T|TrialOne_manRef_Hosp_19_19==T)|
                 (TrialOne_manRef_HR_20_20==T|TrialOne_manRef_Hosp_20_20==T)|
                 (TrialOne_manRef_HR_21_21==T |TrialOne_manRef_Hosp_21_21==T)|
                 (TrialOne_manRef_HR_22_22==T|TrialOne_manRef_Hosp_22_22==T)|
                 (TrialOne_manRef_HR_23_23==T|TrialOne_manRef_Hosp_23_23==T)), 
   Opp_3:=Opp_3-1]
xtabs(~ar$Opp_3, addNA=T)



# 31-33 week visit
ar[,Opp_4:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(160,167]",
                               "(167,202]",
                               "(202,216]")|Opp_3== 1, Opp_4:=1]

xtabs(~ar$Opp_4, addNA=T)

# removing opportunities 
ar[Opp_4==1 &
     ((TrialOne_manRef_HR_24_24==T|TrialOne_manRef_Hosp_24_24==T)|
        (TrialOne_manRef_HR_25_25==T|TrialOne_manRef_Hosp_25_25==T)|
        (TrialOne_manRef_HR_26_26==T|TrialOne_manRef_Hosp_26_26==T)|
        (TrialOne_manRef_HR_27_27==T|TrialOne_manRef_Hosp_27_27==T)|
        (TrialOne_manRef_HR_28_28==T|TrialOne_manRef_Hosp_28_28==T)|
        (TrialOne_manRef_HR_29_29==T|TrialOne_manRef_Hosp_29_29==T)|
        (TrialOne_manRef_HR_30_30==T|TrialOne_manRef_Hosp_30_30==T)), 
   Opp_4:=Opp_4-1]

xtabs(~ar$Opp_4, addNA=T)

# 35-37 week visit
ar[,Opp_5:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(216,237]",
                               "(237,244]") | Opp_4==1, Opp_5:=1]
xtabs(~ar$Opp_5, addNA=T)

ar[Opp_5==1 &
     ((TrialOne_manRef_HR_31_31==T|TrialOne_manRef_Hosp_31_31==T)|
        (TrialOne_manRef_HR_32_32==T|TrialOne_manRef_Hosp_32_32==T)|
        (TrialOne_manRef_HR_33_33==T|TrialOne_manRef_Hosp_33_33==T)|
        (TrialOne_manRef_HR_34_34==T|TrialOne_manRef_Hosp_34_34==T)), 
   Opp_5:=Opp_5-1]
xtabs(~ar$Opp_5, addNA=T)




################ successes ##########
# 15-17 week visit
ar[,Succ_1:=as.logical(NA)]
ar[Opp_1==1, Succ_1:=FALSE]
ar[Succ_1==F & 
     TrialOne_anvisitnew_15_17==T, Succ_1:=TRUE]

xtabs(~ar$Succ_1, addNA=T)

# 18-22 week visit
ar[,Succ_2:=as.logical(NA)]
ar[Opp_2==1, Succ_2:=FALSE]
ar[Succ_2==F & TrialOne_anvisitnew_18_22==T, Succ_2:=TRUE]

xtabs(~ar$Succ_2, addNA=T)

# 24-28 week visit
ar[,Succ_3:=as.logical(NA)]
ar[Opp_3==1, Succ_3:=as.logical(FALSE)]
ar[Succ_3==F & TrialOne_anvisitnew_24_28==T, Succ_3:=TRUE]

xtabs(~ar$Succ_3, addNA=T)

# 31-33 week visit
ar[,Succ_4:=as.logical(NA)]
ar[Opp_4==1, Succ_4:=FALSE]
ar[Succ_4==F & TrialOne_anvisitnew_31_33==T, Succ_4:=TRUE]

xtabs(~ar$Succ_4, addNA=T)

# 35-37
ar[,Succ_5:=as.logical(NA)]
ar[Opp_5==1, Succ_5:=FALSE]
ar[Succ_5==F & TrialOne_anvisitnew_35_37==T, Succ_5:=TRUE]

xtabs(~ar$Succ_5, addNA=T)


oppvars <- names(ar)[stringr::str_detect(names(ar),"^Opp_")]
succvars <-names(ar)[stringr::str_detect(names(ar),"^Succ_")]

ar[,numconsecvisits:=is.na(numeric)]

for(i in succvars){
  
  
  ar[get(i)==1, numconsecvisits:=numconsecvisits+1]
  
}


xtabs(~ar$numconsecvisits, addNA=T)


ar[,numopps:=0]
for(i in oppvars){
  
  
  ar[get(i)==1, numopps:=numopps+1]
  
}

xtabs(~ar$numopps, addNA=T)


newatt <- ar[,.(N=.N,
                "1 visit"=sum(numconsecvisits==1, na.rm=T),
                "2 visits"=sum(numconsecvisits==2, na.rm=T),
                "3 visits"=sum(numconsecvisits==3, na.rm=T),
                "4 visits"=sum(numconsecvisits==4, na.rm=T),
                "5 visits"=sum(numconsecvisits==5, na.rm=T)),
             keyby=.(bookyear,
                     numopps)]


anconly <-ar[ident_dhis2_booking==T]
long <- melt.data.table(anconly,
                        id.vars=c("bookyear",
                                 "bookevent",
                                 "uniqueid"),
                        measure.vars=patterns("Opp_","Succ_"),
                        value.name=c("Opp","Succ"))



numoppsandvisits <- long[,.(Opp=sum(Opp==1, na.rm=T),
                        Succ=sum(Succ==TRUE, na.rm=T)),
                     keyby=.(bookyear, variable)]
numoppsandvisits[,prop:=round(Succ/Opp, digits=3)]

setnames(numoppsandvisits, "variable","visitnumber")

openxlsx::write.xlsx(numoppsandvisits, file.path(FOLDER_DATA_RESULTS,
                                             "annual reports",
                                             paste0(year_folder),
                                             "numancvisits_by_guidelines.xlsx"))



# num opps and visits by uniqueid
totalbyid <- long[,.(Opp=sum(Opp==1, na.rm=T),
                     Succ=sum(Succ==1, na.rm=T)),
                  keyby=.(bookyear, bookevent)]
xtabs(~totalbyid$Opp, addNA=T)
xtabs(~totalbyid$Succ, addNA=T)



multipleopp <- long[,.(Opps=sum(Opp==1, na.rm=T),
                       Succ=sum(Succ==1, na.rm=T)),
                    keyby=.(bookyear, bookevent)]



multipvisits <- multipleopp[,.(N=.N),
                           keyby=.(bookyear,Opps,Succ)]








# multiple opportunities

newAtt <- ar[,.(N=.N,
                bookedb414=sum(bookgestagedays_cats=="(0,104]", na.rm = T),
                ANC15_17Opps=sum(Opp_1,na.rm=T),
                ANC15_17=sum(Succ_1, na.rm=T),
                Opp_1_2=sum(Opp_1==1 &
                              Opp_2==1, na.rm=T),
                Succ_1_2=sum(Succ_1==1 &
                               Succ_2==1, na.rm=T),
                Opp_1_2_3=sum(Opp_1==1 &
                                Opp_2==1 &
                                Opp_3, na.rm=T),
                Succ_1_2_3=sum(Succ_1==1 &
                                 Succ_2==1 &
                                 Succ_3==1, na.rm=T),
                Opp_1_2_3_4=sum(Opp_1==1 &
                                  Opp_2==1 &
                                  Opp_3==1 &
                                  Opp_4==1, na.rm=T),
                Succ_1_2_3_4=sum(Succ_1==1 &
                                   Succ_2==1 &
                                   Succ_3==1 &
                                   Succ_4==1, na.rm=T),
                Opp_1_to_5=sum(Opp_1==1 &
                                 Opp_2==1 &
                                 Opp_3==1 &
                                 Opp_4==1 &
                                 Opp_5==1, na.rm=T),
                Succ_1_to_5=sum(Succ_1==1 &
                                  Succ_2==1 &
                                  Succ_3==1 &
                                  Succ_4==1 &
                                  Succ_5==T, na.rm=T)),
                           keyby=.(bookyear)]

newAtt[,prop_1_visit:=round(ANC15_17/ANC15_17Opps, digits=3)]
newAtt[,prop_1_and_2_visits:=round(Succ_1_2/Opp_1_2, digits=3)]
newAtt[,prop_1_2_and_3_visits:=round(Succ_1_2_3/Opp_1_2_3, digits=3)]
newAtt[,prop_1_2_3_and_4_visits:=round(Succ_1_2_3_4/Opp_1_2_3_4, digits=3)]
newAtt[,prop_1_2_3_4_and_5_visits:=round(Succ_1_to_5/Opp_1_to_5, digits=3)]



openxlsx::write.xlsx(newAtt, file.path(FOLDER_DATA_RESULTS,
                                             "annual reports",
                                             paste0(year_folder),
                                             "numconsecancvisits.xlsx"))



prelimAtt <- ar[,.(N=.N,
                   bookedb414=sum(bookgestagedays_cats=="(0,104]", na.rm = T),
                   ANC15_17Opps=sum(Opp_1,na.rm=T),
                   ANC15_17=sum(Succ_1, na.rm=T),
                   ANC15_17FALSE=sum(Succ_1==F, na.rm=T),
                   booked1515=sum(bookgestagedays_cats=="(104,125]", na.rm = T),
                   ANC18_22Opps=sum(Opp_2, na.rm=T),
                   ANC18_22=sum(Succ_2, na.rm=T),
                   ANC18_22FALSE=sum(Succ_2==F, na.rm=T),
                   booked1822=sum(bookgestagedays_cats=="(125,160]", na.rm = T),
                   booked2323=sum(bookgestagedays_cats=="(160,167]", na.rm = T),
                   ANC2428Opps=sum(!is.na(Opp_3), na.rm=T),
                   ANC24_28TRUE=sum(Succ_3, na.rm=T),
                   ANC24_28FALSE=sum(Succ_3==F, na.rm=T),
                   booked2428=sum(bookgestagedays_cats=="(167,202]", na.rm = T),
                   booked2930=sum(bookgestagedays_cats=="(202,216]", na.rm = T),
                   ANC31_33Opps=sum(Opp_4, na.rm=T),
                   ANC31_33=sum(Succ_4, na.rm=T),
                   ANC31_33FALSE=sum(Succ_4==F, na.rm=T),
                   Booked31_33=sum(bookgestagedays_cats=="(216,237]", na.rm = T),
                   Booked34_34=sum(bookgestagedays_cats=="(237,244]", na.rm = T),
                   ANC3537Opps=sum(Opp_5, na.rm=T),
                   ANC3537=sum(Succ_5, na.rm=T),
                   Booked35_37=sum(bookgestagedays_cats=="(244,265]", na.rm = T)),
                
                keyby=.(bookyear)]





openxlsx::write.xlsx(prelimAtt, file.path(FOLDER_DATA_RESULTS,
                                    "annual reports",
                                    paste0(year_folder),
                                    "Attendanceoutcomes.xlsx"))




