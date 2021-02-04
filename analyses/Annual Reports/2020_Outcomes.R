# Demographics and risks at Booking

ar <- readRDS(file.path(FOLDER_DATA_CLEAN,"annual reports","annualreportdata.RDS"))
########## 
# demo
########## 

ar[bookgestage>40, bookgestage:=as.integer(NA)]

ar[is.infinite(bookbmi), bookbmi:=as.integer(NA)]

##making a table for data we want to analyze from the analysis data set
##for things like parity, make an ugly table because its not a box plot
##or histogram, so better to make a ugly table.
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
                       "2020",
                       "BackgroundAndHistory.xlsx"))

# bookbmicat

tab <- ar[ident_dhis2_booking==T &
            bookgestage<15 & bookgestage>0,.(N=.N,
             "2019"=sum(bookyear==2019, na.rm=T),
             "2020"=sum(bookyear==2020, na.rm=T)),
          keyby=.(bookbmicat)]



openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookBmiCats.xlsx"))



# marriage cat

ar[bookorgdistrict=="Tulk", bookorgdistrict:="TULK"]

ar[,agemarriagecat_ar:=cut(agemarriage,
                           breaks=c(0,15,17,19,25,30,35,40,100),
                           include.lowest = T)]

xtabs(~ar$agemarriagecat_ar, addNA=T)

tab <- ar[ident_dhis2_booking==T,.(N=.N,
                                   "2019"=sum(bookyear==2019, na.rm=T),
                                   "2020"=sum(bookyear==2020, na.rm=T)),
          keyby=.(agemarriagecat_ar)]



openxlsx::write.xlsx(tab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "ageatmarriagecat.xlsx"))


marriageDistricts <- ar[ident_dhis2_booking==T,.(N=.N,
                                                 "2019"=sum(bookyear==2019, na.rm=T),
                                                 "2020"=sum(bookyear==2020, na.rm=T)),
                        keyby=.(bookorgdistrict,agemarriagecat_ar)]



openxlsx::write.xlsx(marriageDistricts,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "ageatmarriagecatDistricts.xlsx"))





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




demoage <- ar[ident_dhis2_booking==T,.(
  Bookprimi=sum(bookprimi=="1", na.rm=T),
  BookPrimiNo=sum(bookprimi=="0", na.rm=T),
  BookPrimiNA=sum(is.na(bookprimi))),
  keyby=.(bookyear)]


openxlsx::write.xlsx(demoage,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimi.xlsx"))




demoage <- ar[ident_dhis2_booking==T,.(
                                    Bookprimi=sum(bookprimi=="1", na.rm=T),
                                    BookPrimiNo=sum(bookprimi=="0", na.rm=T),
                                    BookPrimiNA=sum(is.na(bookprimi))),
                  keyby=.(bookyear,agecat)]


openxlsx::write.xlsx(demoage,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimivAgecat.xlsx"))





demoagemarriage <- ar[ident_dhis2_booking==T,.(
                      Bookprimi=sum(bookprimi==1, na.rm=T),
                      BookPrimiNo=sum(bookprimi==0, na.rm=T),
                      BookPrimiNA=sum(is.na(bookprimi))),
               keyby=.(bookyear,agemarriagecat)]

openxlsx::write.xlsx(demoagemarriage,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimivAgemarriagecat.xlsx"))


demoagepreg <- ar[ident_dhis2_booking==T,.(
  Bookprimi=sum(bookprimi==1, na.rm=T),
  BookPrimiNo=sum(bookprimi==0, na.rm=T)),
  keyby=.(bookyear,agepregnancycat)]

openxlsx::write.xlsx(demoagepreg,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookPrimivAgepregcat.xlsx"))



########## 
# book gA
########## 
 

bookings <- ar[,.(Booked=sum(ident_dhis2_booking==T, na.rm=T),
                      BookedPPC=sum(ident_dhis2_ppc==T, na.rm=T),
                      BookePPCandANC=sum(ident_dhis2_PPC==T &
                                         ident_dhis2_booking==1, na.rm=T)),
                   keyby=.(bookyear,bookorgdistrict)]

openxlsx::write.xlsx(bookings,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookORdisvsYear.xlsx"))

bookgA <- ar[ident_dhis2_booking==T,.(N=.N),
                                        keyby=.(bookyear,bookgestagedays_cats)]

openxlsx::write.xlsx(bookgA,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookgACats.xlsx"))




bookgA <- ar[ident_dhis2_booking==T,.(N=.N),
             keyby=.(bookyear,bookgestagedays_cats,incomecat)]

openxlsx::write.xlsx(bookgA,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookgACats_incomeCats.xlsx"))




bookgAHR <- ar[ident_dhis2_booking==T,.(N=.N),
             keyby=.(bookyear,bookgestagedays_cats,ident_dhis2_risk)]

openxlsx::write.xlsx(bookgAHR,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookgACats_HR.xlsx"))




bookgAprimi <- ar[ident_dhis2_booking==T,.(N=.N),
               keyby=.(bookyear,bookgestagedays_cats,bookprimi)]

openxlsx::write.xlsx(bookgAHR,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookgACats_bookprimi.xlsx"))



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
                                         SevHTN=sum(bookbpsyst>=150 |
                                                      bookbpdiast>110, na.rm=T),
                                         MissingBookbp=sum(is.na(bookbpsyst)|
                                                             is.na(bookbpdiast))),
                                      keyby=.(bookyear,bookgestagedays_cats)]

xtabs(~bookbpsystcat+bookgestagedays_cats,addNA=T,data=ar)



openxlsx::write.xlsx(bptab,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookBPbyBookgAcats.xlsx"))




#################### 
# bookbp categories
####################  

bookbpsystdiast <- ar[ident_dhis2_booking==1,.(N=.N),
                          keyby=.(bookyear,
                                  bookgestagedays_cats,
                                  bookbpsystcat,
                                  bookbpdiastcat)]

xtabs(~bookbpsystcat+bookgestagedays_cats,addNA=T,data=ar)



openxlsx::write.xlsx(bookbpsystdiast,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "BookBPCatsbygAcats.xlsx"))




########## 
# us
########## 
# bookus

screenings <- ar[ident_dhis2_booking==1,.(N=.N),
                                      keyby=.(bookyear,
                                      bookgestagedays_cats,
                                      bookexamfh,
                                      bookexamsfh)]



openxlsx::write.xlsx(screenings,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "FHandSFHscreenings.xlsx"))

# CPO
#ppcbw lga and sga

#gA from ppcdeliv date or cpo gA, check them out
ar[,gestageatbirth:=difftime(cpodate_1,
                                 USorLMPdate,
                                 units="days"
                                 )]

xtabs(~ar$gestageatbirth, addNA=T)



############### 
# multifet preg
############### 
 
# uspres at 36 weeks 
ar[,hasan36plusweeks:=FALSE]
#ar[,hasanexampalp36:= FALSE]
vars <- stringr::str_subset(names(ar),"^angestage_")
for (i in vars){
  print(i)
  ar[get(i)>=36 & get(i)<=40, hasan36plusweeks:=TRUE]
  
}
#fetal presentation at term
ar[,presatterm:=as.character(NA)]

vars <- stringr::str_subset(names(ar),"^uspres_")


for (var_pres in vars){
  
  vargestage <-stringr::str_replace(var_pres,"uspres", "usgestage")
  
  ar[hasan36plusweeks==TRUE &
           get(vargestage)>=36 &
           get(vargestage)<=40 &
           !is.na(get(var_pres)) &
           get(var_pres)!="",
         presatterm:=get(var_pres)]
}

xtabs(~ar$presatterm, addNA=T)

############
# multifetus
############

# multifetus: cpo num (up to 3, the rest are empty) or us
ar[,multifet:=as.logical(NA)]
vars <- names(ar)[stringr::str_detect(names(ar),"^usnumberfetus_")]

for(i in vars){
  
  ar[get(i)>1, multifet:=TRUE]
  ar[get(i)<=1, multifet:=FALSE]
  
}

xtabs(~ar[ident_dhis2_booking==1]$multifet, addNA=T)

# gA validate self reported with those that come from ANC
vars <- names(ar)[stringr::str_detect(names(ar),"^cpo")]

# robson classifications
## fix the variables
#primi, single cephalic preg,at term (37 or more) & spont deliv
ar[bookprimi==1 &
         presatterm=="Cephalic" & 
         gestageatbirth>=259 &
         cpomodedelivery_1=="Spontaneous vaginal" &
         multifet==F,robsgp_1:=TRUE]
xtabs(~ar$robsgp_1, addNA=T)

#primi, single cephalic preg,at term (37 or more) & deliv==csec or induced
ar[bookprimi==1 &
         presatterm=="Cephalic" & 
         gestageatbirth>=259 &
         cpomodedelivery_1 %in% c("Caesarian section") &
         multifet==F,robsgp_2:=TRUE]

xtabs(~ar$robsgp_2, addNA=T)


#multiparous, single cephalic preg,at term (37 or more) & deliv= NVSD & no prev ut scar
ar[bookprimi==0 &
         presatterm=="Cephalic" & 
         gestageatbirth>=259 &
         cpomodedelivery_1=="Spontaneous vaginal" &
         (bookhistutesur==0 & bookhistcs==0) &
         multifet==F,robsgp_3:=TRUE]
xtabs(~ar$robsgp_3, addNA=T)

#multiparous, single cephalic preg,at term (37 or more) & deliv= induced or cs & no prev ut scar
ar[bookprimi==0 &
         presatterm=="Cephalic" & 
         gestageatbirth>=259 &
         cpomodedelivery_1 %in% c("Caesarian section") &
         (bookhistutesur==0 |bookhistcs==0) &
         multifet==F,robsgp_4:=TRUE]
xtabs(~ar$robsgp_4, addNA=T)


#multiparous,at term (37 or more) & deliv= induced or cs & no prev ut scar
ar[bookprimi==0 &
         presatterm=="Cephalic" & 
         gestageatbirth>=259 &
         cpomodedelivery_1 %in% c("Caesarian section","induced")&
        (bookhistutesur==1 | bookhistcs==1),robsgp_5:=TRUE]
xtabs(~ar$robsgp_5, addNA=T)

#nulliparous,single preg, breech
ar[bookprimi==1 &
         presatterm %in% c("Breech","Trasverse") &
         multifet==F,robsgp_6:=TRUE]
xtabs(~ar$robsgp_6, addNA=T)

#nulliparous,single preg, breech
ar[bookprimi==0 & 
         presatterm %in% c("Breech","Trasverse") &
         (bookhistutesur==1 | bookhistcs==1) &
         multifet==F,robsgp_7:=TRUE]
xtabs(~ar$robsgp_7, addNA=T)


# all women with multiple preg and uterine scars
ar[(bookhistutesur==1 | bookhistcs==1) &
         multifet==T,robsgp_8:=TRUE]
xtabs(~ar$robsgp_8, addNA=T)

# all women with single preg and uterine scar
ar[(bookhistutesur==1 | bookhistcs==1) &
         multifet==F,robsgp_9:=TRUE]
xtabs(~ar$robsgp_9, addNA=T)

# all women with single cephalic preg and less than 37 week 

ar[(gestageatbirth<259 & gestageatbirth>168) &
         presatterm=="Cephalic" & 
         multifet==F,robsgp_10:=TRUE]
xtabs(~ar$robsgp_10, addNA=T)


robsongrps <- ar[!is.na(cpoevent_1) ,.(
                         Denom=.N,
                         group1=sum(robsgp_1, na.rm=T),
                         group1F=sum(robsgp_1==FALSE, na.rm=T),
                         group2=sum(robsgp_2, na.rm=T),
                         group2F=sum(robsgp_2==F, na.rm=T),
                         group3=sum(robsgp_3, na.rm=T),
                         group3F=sum(robsgp_3==F, na.rm=T),
                         group4=sum(robsgp_4, na.rm=T),
                         group4F=sum(robsgp_4==F, na.rm=T),
                         group5=sum(robsgp_5, na.rm=T),
                         group5F=sum(robsgp_5==F, na.rm=T),
                         group6=sum(robsgp_6, na.rm=T),
                         group6F=sum(robsgp_6==F, na.rm=T),
                         group7=sum(robsgp_7, na.rm=T),
                         group7F=sum(robsgp_7==F, na.rm=T),
                         group8=sum(robsgp_8, na.rm=T),
                         group8F=sum(robsgp_8==F, na.rm=T),
                         group9=sum(robsgp_9, na.rm=T),
                         group9F=sum(robsgp_9==F, na.rm=T),
                         group10=sum(robsgp_10, na.rm=T),
                         group10=sum(robsgp_10==F, na.rm=T)
),
                     
                     keyby=.(bookyear,ident_dhis2_booking)]




openxlsx::write.xlsx(robsongrps,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "annual reports",
                       "2020",
                       "Robsongps.xlsx"))






#################### Visits overall Number ###########

# Number of ANC, PPC, NBC per district per month

ar[,anevent_0:=bookevent]
ar[,andate_0:=bookdate]

vars <- names(ar)[stringr::str_detect(names(ar),"^anevent_[0-9]+")]
ar[,anevent_x:=0]

ar[,anevent_x_1:=0]

print(vars)

for(i in vars){
  
  varsdate <- names(ar)[stringr::str_detect(names(ar),"andate")]
  
  ar[!is.na(get(i)) &
       get(varsdate)>="2020-01-01", anevent_x:=anevent_x + 1]
  
  
  ar[!is.na(get(i)) &
       get(varsdate)>="2019-01-01" &
       get(varsdate)<"2020-01-01", anevent_x_1:=anevent_x_1 + 1]
  
  
  
  
}

sum(ar[ident_dhis2_control==F]$anevent_x,na.rm=T)
sum(ar[ident_dhis2_control==F]$anevent_x_1,na.rm=T)


# total ppc events per woman
vars <- names(ar)[stringr::str_detect(names(ar),"^ppcevent_[0-9]+")]
ar[,ppcevent_x:=0]
ar[,ppcevent_x_1:=0]

print(vars)

for(i in vars){
  
  ppcdate <- names(ar)[stringr::str_detect(names(ar),"ppcdate")]
  ar[!is.na(get(i)) & 
       get(ppcdate)>="2020-01-01" &
       get(ppcdate)<="2020-12-31", ppcevent_x:=ppcevent_x + 1]
  
  ar[!is.na(get(i)) & 
       get(ppcdate)>="2019-01-01" &
       get(ppcdate)<="2019-12-31", ppcevent_x_1:=ppcevent_x_1 + 1]
  
  
}

sum(ar[ident_dhis2_control==F]$ppcevent_x,na.rm=T)
sum(ar[ident_dhis2_control==F]$ppcevent_x_1,na.rm=T)

# total nbc events per woman
#making variable for total nbc visits
vars <- names(ar)[stringr::str_detect(names(ar),"^nbcevent_[0-9]+")]
ar[,nbcevent_x:=0]
ar[,nbcevent_x_1:=0]

print(vars)

for(i in vars){
  
  nbcdate <- names(ar)[stringr::str_detect(names(ar),"^nbcdate")]
  
  
  ar[!is.na(get(i)) &
       get(nbcdate)>="2020-01-01" &
       get(nbcdate)<="2020-12-31", nbcevent_x:=nbcevent_x + 1]
  
  ar[!is.na(get(i)) &
       get(nbcdate)>="2019-01-01" &
       get(nbcdate)<="2019-12-31", nbcevent_x_1:=nbcevent_x_1 + 1]
  
  
}

sum(ar[ident_dhis2_control==F]$nbcevent_x,na.rm=T)




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
  PPCvisits2019=sum(ppcevent_x_1,na.rm=T),
  PPCvisits2020=sum(ppcevent_x, na.rm=T),
  NBCvisits2019=sum(nbcevent_x, na.rm = T),
  NBCvisits2020=sum(nbcevent_x_1,na.rm=T))]

openxlsx::write.xlsx(tab, file.path(FOLDER_DATA_RESULTS,
                                    "annual reports",
                                    "2020",
                                    "VisitsTotal.xlsx"))



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
                     (TrialOne_manRef_HR_19_19==T|
                        TrialOne_manRef_Hosp_19_19==T)|
                     (TrialOne_manRef_HR_20_20==T|
                        TrialOne_manRef_Hosp_20_20==T)|
                     (TrialOne_manRef_HR_21_21==T |
                        TrialOne_manRef_Hosp_21_21==T)|
                     (TrialOne_manRef_HR_22_22==T|
                        TrialOne_manRef_Hosp_22_22==T)|
                     (TrialOne_manRef_HR_23_23==T|
                        TrialOne_manRef_Hosp_23_23==T)), 
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

prelimAtt <- ar[ident_dhis2_booking==1,.(N=.N,
                       bookedb414=sum(bookgestagedays_cats=="(0,104]",
                                      na.rm = T),
                       ANC15_17Opps=sum(Opp_1,na.rm=T),
                       ANC15_17=sum(Succ_1, na.rm=T),
                       ANC15_17FALSE=sum(Succ_1==F, na.rm=T),
                       booked1515=sum(bookgestagedays_cats=="(104,125]",
                                      na.rm = T),
                       ANC18_22Opps=sum(Opp_2, na.rm=T),
                       ANC18_22=sum(Succ_2, na.rm=T),
                       ANC18_22FALSE=sum(Succ_2==F, na.rm=T),
                       booked1822=sum(bookgestagedays_cats=="(125,160]",
                                      na.rm = T),
                       booked2323=sum(bookgestagedays_cats=="(160,167]",
                                      na.rm = T),
                       ANC2428Opps=sum(!is.na(Opp_3), na.rm=T),
                       ANC24_28TRUE=sum(Succ_3, na.rm=T),
                       ANC24_28FALSE=sum(Succ_3==F, na.rm=T),
                       booked2428=sum(bookgestagedays_cats=="(167,202]",
                                      na.rm = T),
                       booked2930=sum(bookgestagedays_cats=="(202,216]", 
                                      na.rm = T),
                       ANC31_33Opps=sum(Opp_4, na.rm=T),
                       ANC31_33=sum(Succ_4, na.rm=T),
                       ANC31_33FALSE=sum(Succ_4==F, na.rm=T),
                       Booked31_33=sum(bookgestagedays_cats=="(216,237]",
                                       na.rm = T),
                       Booked34_34=sum(bookgestagedays_cats=="(237,244]", 
                                       na.rm = T),
                       ANC3537Opps=sum(Opp_5, na.rm=T),
                       ANC3537=sum(Succ_5, na.rm=T),
                       Booked35_37=sum(bookgestagedays_cats=="(244,265]",
                                       na.rm = T)),
                    
                    keyby=.(bookyear)]

openxlsx::write.xlsx(prelimAtt,file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         "2020",
                                         sprintf("%s_Attendance.xlsx",
                                                 lubridate::today()))) 



att <- ar[ident_dhis2_booking==T,.(N=.N,
             ANC15_17Opps=sum(Opp_1,na.rm=T),
             Attendance_15_17=sum(Succ_1, na.rm=T),
             booked1822=sum(bookgestagedays_cats=="(125,160]",
                            na.rm = T),
             booked2323=sum(bookgestagedays_cats=="(160,167]",
                            na.rm = T),
             Attendance_15_22=sum(Succ_1==T &
                                    Succ_2==T, na.rm=T),
             booked2323=sum(bookgestagedays_cats=="(160,167]",
                            na.rm = T),
             booked2428=sum(bookgestagedays_cats=="(167,202]",
                            na.rm = T),
             Attendance_15_28=sum(Succ_1==T &
                                    Succ_2==T &
                                    Succ_3==T, na.rm=T),
             booked2930=sum(bookgestagedays_cats=="(202,216]", 
                            na.rm = T),
             Booked31_33=sum(bookgestagedays_cats=="(216,237]",
                             na.rm = T),
             Attendance_15_33=sum(Succ_1==T &
                                    Succ_2==T &
                                    Succ_3==T &
                                    Succ_4==T, na.rm=T),
             Booked34_34=sum(bookgestagedays_cats=="(237,244]", 
                             na.rm = T),
             ANC3537Opps=sum(Opp_5, na.rm=T),
             Attendance_15_37=sum(Succ_1==T &
                                    Succ_2==T, 
                                    Succ_3==T &
                                    Succ_4==T &
                                    Succ_5==T, na.rm=T)),
          
          keyby=.(bookyear)]


openxlsx::write.xlsx(att,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               "2020",
                               "AllVisitsAttendance.xlsx"))



####################
# Hypertension
####################

prelimHTN <- ar[ident_dhis2_booking==T,
                .(N=.N,
                  "16 Week Visit"=sum(Succ_1==T, na.rm=T),
                  "BP_16"=sum(Succ_1==T &
                                 TrialOne_anbpsyst_present_15_17==T &
                                 TrialOne_anbpdiast_present_15_17==T, na.rm=T),
                  "20 Week Visit"=sum(Succ_2==T, na.rm=T),
                  "BP_20"=sum(Succ_2==T &
                                   TrialOne_anbpsyst_present_18_22==T &
                                   TrialOne_anbpdiast_present_18_22==T, na.rm=T),
                  "24_28 Week Visit"=sum(Succ_3==T, na.rm=T),
                  "BP_24_28"=sum(Succ_3==T &
                                 TrialOne_anbpsyst_present_24_28==T &
                                   TrialOne_anbpdiast_present_24_28==T,na.rm=T),
                  "32 Week Visit"=sum(Succ_4==T, na.rm=T),
                  "BP_32"=sum(Succ_4==T &
                              TrialOne_anbpsyst_present_24_28==T &
                              TrialOne_anbpdiast_present_24_28==T, na.rm=T),
                  
                  "36 Week Visit"=sum(Succ_5==T, na.rm=T),
                  "Bp_36 weeks"=sum(Succ_5 &
                                      TrialOne_anbpsyst_present_35_37==T &
                                      TrialOne_anbpdiast_present_35_37==T, na.rm=T)
                  ),
                
                keyby=.(bookyear)]

openxlsx::write.xlsx(prelimHTN,file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         "2020",
                                         sprintf("%s_BpOntime.xlsx",
                                                 lubridate::today()))) 



##########
# Anemia
##########

########## Anemia ########## 
# Define opportunities at 3 different cut off points

## booked before 24
ar[,Opportunity_anemia_screening_1:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]",
                                   "(125,160]",
                                   "(160,167]"),
       Opportunity_anemia_screening_1:=1]

xtabs(~ar$Opportunity_anemia_screening_1, addNA=T)


## booked 24 or has visit 
ar[,Opportunity_anemia_screening_2:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(167,202]")| 
         TrialOne_anvisitnew_24_28==T,
       Opportunity_anemia_screening_2:=1]

xtabs(~ar$Opportunity_anemia_screening_2, addNA=T)

# booked 29-34 weeks or has visit
ar[,Opportunity_anemia_screening_3:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]",
                                   "(237,244]"),
       Opportunity_anemia_screening_3:=1]

xtabs(~ar$Opportunity_anemia_screening_3, addNA=T)


## booked or visit at 35-37 weeks
ar[,Opportunity_anemia_screening_4:=as.numeric(NA)]
ar[bookgestagedays_cats %in% c("(244,265]") |
         TrialOne_anvisitnew_35_37==T, 
       Opportunity_anemia_screening_4:=1]

xtabs(~ar$Opportunity_anemia_screening_4, addNA=T)



## severe anemia at booking and at any other visit after that
ar[,Opportunity_anemia_screening_5:=as.numeric(NA)]
ar[TrialOne_labhb_anemia_sev_00_14==T|
         TrialOne_labhb_anemia_sev_15_17==T|
         TrialOne_labhb_anemia_sev_18_22==T|
         TrialOne_labhb_anemia_sev_23_23==T,Opportunity_anemia_screening_5:=1]

xtabs(~ar$Opportunity_anemia_screening_5, addNA=T)



# ADJUSTING OPPORTUNITIES FOR THOSE WHO HAVE BEEN REFERRED
## Before 24 weeks

#variable for man sev anemia anytime before 24 weeks
ar[,manhbsev:=(TrialOne_manhb_00_00 |
                     TrialOne_manhb_01_01 |
                     TrialOne_manhb_02_02 |
                     TrialOne_manhb_03_03 |
                     TrialOne_manhb_04_04 |
                     TrialOne_manhb_05_05 |
                     TrialOne_manhb_06_06 |
                     TrialOne_manhb_07_07 |
                     TrialOne_manhb_08_08 |
                     TrialOne_manhb_09_09 |
                     TrialOne_manhb_10_10 |
                     TrialOne_manhb_11_11 |
                     TrialOne_manhb_12_12 |
                     TrialOne_manhb_13_13 |
                     TrialOne_manhb_14_14 |
                     TrialOne_manhb_15_15 |
                     TrialOne_manhb_16_16 |
                     TrialOne_manhb_17_17 |
                     TrialOne_manhb_18_18 |
                     TrialOne_manhb_19_19 |
                     TrialOne_manhb_20_20 |
                     TrialOne_manhb_21_21 |
                     TrialOne_manhb_22_22 |
                     TrialOne_manhb_23_23)]
xtabs(~ar$manhbsev, addNA=T)


ar[,RefHr:=as.logical(NA)]
ar[Opportunity_anemia_screening_1==1, RefHr:=FALSE]
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
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
       RefHr:=TRUE]
xtabs(~ar$RefHr, addNA=T)

## At 24-28 weeks
ar[Opportunity_anemia_screening_2==1 &
         (TrialOne_anvisitnew_24_24==T & 
            (RefHr==T))|
         (TrialOne_anvisitnew_25_25==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T)), 
       Opportunity_anemia_screening_2:=Opportunity_anemia_screening_2-1]

xtabs(~ar$Opportunity_anemia_screening_2, addNA=T)

# 35-37 weeks
ar[Opportunity_anemia_screening_4==1 &
         (TrialOne_anvisitnew_35_35==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T|
               TrialOne_manRef_HR_31_31==T|
               TrialOne_manRef_HR_32_32==T|
               TrialOne_manRef_HR_33_33==T|
               TrialOne_manRef_HR_34_34==T))|
         (TrialOne_anvisitnew_36_36==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T|
               TrialOne_manRef_HR_31_31==T|
               TrialOne_manRef_HR_32_32==T|
               TrialOne_manRef_HR_33_33==T|
               TrialOne_manRef_HR_34_34==T|
               TrialOne_manRef_HR_35_35==T))|
         (TrialOne_anvisitnew_37_37==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T|
               TrialOne_manRef_HR_28_28==T|
               TrialOne_manRef_HR_29_29==T|
               TrialOne_manRef_HR_30_30==T|
               TrialOne_manRef_HR_31_31==T|
               TrialOne_manRef_HR_32_32==T|
               TrialOne_manRef_HR_33_33==T|
               TrialOne_manRef_HR_34_34==T|
               TrialOne_manRef_HR_35_35==T|
               TrialOne_manRef_HR_36_36==T)), 
       Opportunity_anemia_screening_4:=Opportunity_anemia_screening_4-1]
xtabs(~ar$Opportunity_anemia_screening_4, addNA=T)

#define different time cats for success
ar[, HbonTime_1a:= as.logical(NA)]
ar[Opportunity_anemia_screening_1==1, HbonTime_1a:=FALSE]

ar[, HbonTime_1b:= as.logical(NA)]
ar[Opportunity_anemia_screening_1==1 & 
         booklabhb<7 & booklabhb>=2,HbonTime_1b:=FALSE]


ar[, HbonTime_1c:= as.logical(NA)]
ar[Opportunity_anemia_screening_1==1 &
         booklabhb>=7 & booklabhb<11,HbonTime_1c:=FALSE ]



# Hbontime_2
ar[,HbonTime_2a:= as.logical(NA)]
ar[Opportunity_anemia_screening_2==1, HbonTime_2a:=FALSE]

ar[, HbonTime_2b:= as.logical(NA)]
ar[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_sev_24_28==T, HbonTime_2b:=FALSE]

ar[,HbonTime_2c:= as.logical(NA)]
ar[Opportunity_anemia_screening_2==1 & 
         TrialOne_labhb_anemia_mild_mod_24_28==T, HbonTime_2c:=FALSE]

# 29-34 weeks
# Hbontime_3
ar[, HbonTime_3a:= as.logical(NA)]
ar[Opportunity_anemia_screening_3==1 & 
         (!is.na(booklabhb)), HbonTime_3a:=FALSE]

ar[, HbonTime_3b:= as.logical(NA)]
ar[Opportunity_anemia_screening_3==1 &
         (booklabhb<7 & booklabhb>2), HbonTime_3b:=FALSE]

ar[, HbonTime_3c:= as.logical(NA)]
ar[Opportunity_anemia_screening_3==1 &
         (booklabhb<11 & booklabhb>=7), HbonTime_3c:=FALSE]

ar[, HbonTime_4a:= as.logical(NA)]
ar[Opportunity_anemia_screening_4==1, HbonTime_4a:=FALSE]

ar[, HbonTime_4b:= as.logical(NA)]
ar[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_sev_35_37==T,HbonTime_4b:=FALSE]

ar[, HbonTime_4c:= as.logical(NA)]
ar[Opportunity_anemia_screening_4==1 &
         TrialOne_labhb_anemia_mild_mod_35_37==T, HbonTime_4c:=FALSE]

ar[, HbonTime_5:= as.logical(NA)]
ar[Opportunity_anemia_screening_5==1, HbonTime_5:=FALSE]

ar[, HbonTime_6:= as.logical(NA)]
ar[Opportunity_anemia_screening_6==1, HbonTime_6:=FALSE]



#hb on time 1, 2, 3, vars
#Screen at bookings before 24 weeks??
#check booklabhb values if normal etc

# booked before 24 weeks
ar[HbonTime_1a==F & booklabhb>=11 & 
         booklabhb<=18, HbonTime_1a:=TRUE]
xtabs(~ar$HbonTime_1a, addNA=T)

ar[HbonTime_1b==F & 
         manhbsev==T,HbonTime_1b:=TRUE]
xtabs(~ar$HbonTime_1b, addNA=T)


ar[HbonTime_1c==F & 
         (TrialOne_manhb_mildmodhbret_00_00==T|
            TrialOne_manhb_mildmodhbret_01_01==T|
            TrialOne_manhb_mildmodhbret_02_02==T|
            TrialOne_manhb_mildmodhbret_03_03==T|
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
            TrialOne_manhb_mildmodhbret_23_23==T),HbonTime_1c:=TRUE]

xtabs(~ar$HbonTime_1c, addNA=T)

#24-28 screenings
ar[HbonTime_2a==F & 
         TrialOne_labhb_normal_24_28==T, HbonTime_2a:=TRUE]

ar[HbonTime_2b==F & 
         TrialOne_manhb_24_24==T|
         TrialOne_manhb_25_25==T|
         TrialOne_manhb_26_26==T|
         TrialOne_manhb_27_27==T|
         TrialOne_manhb_28_28==T, HbonTime_2b:=TRUE]

ar[HbonTime_2c==F & 
         TrialOne_manhb_mildmodhbret_24_24==T|
         TrialOne_manhb_mildmodhbret_25_25==T|
         TrialOne_manhb_mildmodhbret_26_26==T|
         TrialOne_manhb_mildmodhbret_27_27==T|
         TrialOne_manhb_mildmodhbret_28_28==T, HbonTime_2c:=TRUE]

#booked 29-30, 31-33, 34
ar[HbonTime_3a==F & Opportunity_anemia_screening_3==1 &
         (booklabhb<=18 & booklabhb>11), HbonTime_3a:=TRUE]


ar[HbonTime_3c==1 & 
         (TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T|
            TrialOne_manhb_mildmodhbret_31_31==T|
            TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T), 
       HbonTime_3c:=TRUE]

ar[HbonTime_3b==F & 
         (TrialOne_manhb_29_29==T|
            TrialOne_manhb_30_30==T|
            TrialOne_manhb_31_31==T|
            TrialOne_manhb_32_32==T|
            TrialOne_manhb_33_33==T|
            TrialOne_manhb_34_34==T), 
       HbonTime_3b:=TRUE]


# 35-37 screenings
ar[HbonTime_4a==F & 
         TrialOne_labhb_normal_35_37==T, HbonTime_4a:=TRUE]

ar[HbonTime_4b==F & 
         TrialOne_manhb_35_35==T|
         TrialOne_manhb_36_36==T|
         TrialOne_manhb_37_37==T, HbonTime_4b:=TRUE]

ar[HbonTime_4c==F &
         TrialOne_manhb_mildmodhbret_35_35==T|
         TrialOne_manhb_mildmodhbret_36_36==T|
         TrialOne_manhb_mildmodhbret_37_37==T, HbonTime_4c:=TRUE]

# severe anemia outside of time windows
ar[HbonTime_5==F & 
         (TrialOne_manhb_00_00==T|
            TrialOne_manhb_01_01==T|
            TrialOne_manhb_02_02==T|
            TrialOne_manhb_03_03==T|
            TrialOne_manhb_04_04==T|
            TrialOne_manhb_05_05==T|
            TrialOne_manhb_06_06==T|
            TrialOne_manhb_07_07==T|
            TrialOne_manhb_08_08==T|
            TrialOne_manhb_09_09==T|
            TrialOne_manhb_10_10==T|
            TrialOne_manhb_11_11==T|
            TrialOne_manhb_12_12==T|
            TrialOne_manhb_13_13==T|
            TrialOne_manhb_14_14==T|
            TrialOne_manhb_15_15==T|
            TrialOne_manhb_16_16==T|
            TrialOne_manhb_17_17==T|
            TrialOne_manhb_18_18==T|
            TrialOne_manhb_19_19==T|
            TrialOne_manhb_20_20==T|
            TrialOne_manhb_21_21==T|
            TrialOne_manhb_22_22==T|
            TrialOne_manhb_23_23==T|
            TrialOne_manhb_29_29==T|
            TrialOne_manhb_30_30==T|
            TrialOne_manhb_31_31==T|
            TrialOne_manhb_32_32==T|
            TrialOne_manhb_33_33==T|
            TrialOne_manhb_34_34==T),HbonTime_5:=TRUE]

## mild mod anemia
# if all of this is true and none of the other succcess (HBontime is true), then this #should be true. need to run all of the other success to get to that point first and #then calculate this
ar[,Opportunity_anemia_screening_6:=as.logical(FALSE)]
ar[(TrialOne_labhb_anemia_mild_mod_00_14==T|
          TrialOne_labhb_anemia_mild_mod_15_17==T|
          TrialOne_labhb_anemia_mild_mod_18_22==T|
          TrialOne_labhb_anemia_mild_mod_23_23==T|
          TrialOne_labhb_anemia_mild_mod_29_30==T|
          TrialOne_labhb_anemia_mild_mod_34_34==T) &
         (HbonTime_1a==F &
            HbonTime_1b==F &
            HbonTime_1c==F &
            HbonTime_2a==F &
            HbonTime_2b==F &
            HbonTime_2c==F &
            HbonTime_3a==F &
            HbonTime_3b==F &
            HbonTime_3c==F &
            HbonTime_4a==F &
            HbonTime_4b==F &
            HbonTime_4c==F &
            HbonTime_5==F),
       Opportunity_anemia_screening_6:=TRUE]

xtabs(~ar$Opportunity_anemia_screening_6, addNA=T)


#mild/mod anem retest
ar[HbonTime_6==F &
         (TrialOne_manhb_mildmodhbret_00_00==T|
            TrialOne_manhb_mildmodhbret_01_01==T|
            TrialOne_manhb_mildmodhbret_02_02==T|
            TrialOne_manhb_mildmodhbret_03_03==T|
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
            TrialOne_manhb_mildmodhbret_20_20==T|
            TrialOne_manhb_mildmodhbret_21_21==T|
            TrialOne_manhb_mildmodhbret_22_22==T|
            TrialOne_manhb_mildmodhbret_23_23==T|
            TrialOne_manhb_mildmodhbret_29_29==T|
            TrialOne_manhb_mildmodhbret_30_30==T|
            TrialOne_manhb_mildmodhbret_31_31==T|
            TrialOne_manhb_mildmodhbret_32_32==T|
            TrialOne_manhb_mildmodhbret_33_33==T|
            TrialOne_manhb_mildmodhbret_34_34==T),
       HbonTime_6:=TRUE]


prelimHB <- ar[ident_dhis2_booking==1,.(N=.N,
                      Opportun_1=sum(Opportunity_anemia_screening_1, na.rm=T),
                      Success_1a=sum(HbonTime_1a, na.rm=T),
                      Success_1aFalse=sum(HbonTime_1a==FALSE, na.rm=T),
                      Success_1b=sum(HbonTime_1b, na.rm=T),
                      Success_1bFalse=sum(HbonTime_1b==FALSE, na.rm=T),
                      Success_1c=sum(HbonTime_1c, na.rm=T),
                      Success_1cFalse=sum(HbonTime_1c==FALSE, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2a=sum(HbonTime_2a, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2b=sum(HbonTime_2b, na.rm=T),
                      Success_2bFalse=sum(HbonTime_2b==F, na.rm=T),
                      Opportun_2=sum(Opportunity_anemia_screening_2, na.rm=T),
                      Success_2c=sum(HbonTime_2c, na.rm=T),
                      Success_2cFalse=sum(HbonTime_2c==F, na.rm=T),
                      Opportun_3=sum(Opportunity_anemia_screening_3, na.rm=T),
                      Success_3a=sum(HbonTime_3a, na.rm=T),
                      Success_3b=sum(HbonTime_3b, na.rm=T),
                      Success_3bFales=sum(HbonTime_3b==FALSE, na.rm=T),
                      Success_3c=sum(HbonTime_3c, na.rm=T),
                      Sucess_3cFalse=sum(HbonTime_3c==F, na.rm=T),
                      Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                      Success_4a=sum(HbonTime_4a, na.rm=T),
                      Opportun_4=sum(Opportunity_anemia_screening_4, na.rm=T),
                      Success_4b=sum(HbonTime_4b, na.rm=T),
                      Screening4bF=sum(HbonTime_4b==F, na.rm=T),
                      Success_4c=sum(HbonTime_4c, na.rm=T),
                      Screening4cF=sum(HbonTime_4c==F, na.rm=T),
                      Opportun_5=sum(Opportunity_anemia_screening_5, na.rm=T),
                      Success_5=sum(HbonTime_5, na.rm=T),
                      success_5F=sum(HbonTime_5==FALSE, na.rm=T),
                      Opportun_6=sum(Opportunity_anemia_screening_6, na.rm=T),
                      Success_6=sum(HbonTime_6, na.rm=T),
                      success_6F=sum(HbonTime_6==F, na.rm=T)),
                   keyby=.(bookyear)]

openxlsx::write.xlsx(prelimHB,file.path(FOLDER_DATA_RESULTS,
                                        "annual reports",
                                        "2020",
                                        sprintf("%s_Hb.xlsx",
                                                lubridate::today()))) 




###########
# GDM
###########


###Redefining opportinites
ar[,Opportunity_GDM_screening_1:=as.numeric(NA)]
ar[,Opportunity_GDM_screening_2:=as.numeric(NA)]
ar[,Opportunity_GDM_screening_3:=as.numeric(NA)]
ar[,Opportunity_GDM_screening_4:=as.numeric(NA)]
#ar[,Opportunity_GDM_Screening_5:=as.numeric(NA)]

# before 24
ar[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]", 
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_1:=1]
#24-28
ar[bookgestagedays_cats %in% c("(167,202]")|
         TrialOne_anvisitnew_24_28==T,Opportunity_GDM_screening_2:=1]
# after 28
ar[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]", 
                                   "(237,244]",
                                   "(244,265]"), Opportunity_GDM_screening_3:=1]

# high rbs anywhere outside of the 24-28
ar[(TrialOne_labbloodglu_high_00_14==T|
          TrialOne_labbloodglu_high_15_17==T|
          TrialOne_labbloodglu_high_18_22==T|
          TrialOne_labbloodglu_high_23_23==T|
          TrialOne_labbloodglu_high_29_30==T|
          TrialOne_labbloodglu_high_31_33==T|
          TrialOne_labbloodglu_high_34_34==T|
          TrialOne_labbloodglu_high_35_37==T), Opportunity_GDM_screening_4:=1]

xtabs(~ar$Opportunity_GDM_screening_1, addNA=T)
xtabs(~ar$Opportunity_GDM_screening_2, addNA=T)
xtabs(~ar$Opportunity_GDM_screening_3, addNA=T)
xtabs(~ar$Opportunity_GDM_screening_4, addNA=T)




## Remove opportunities for people who were referred to HR or Hosp
#refHRHospmanRBG_1 rename to RefHr
ar[,RefHr:=as.logical(NA)]
ar[Opportunity_GDM_screening_1==1, RefHr:=FALSE]
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
          TrialOne_manRef_HR_14_14==T|
          TrialOne_manRef_HR_15_15==T|
          TrialOne_manRef_HR_16_16==T|
          TrialOne_manRef_HR_17_17==T|
          TrialOne_manRef_HR_18_18==T|
          TrialOne_manRef_HR_19_19==T|
          TrialOne_manRef_HR_20_20==T|
          TrialOne_manRef_HR_21_21==T|
          TrialOne_manRef_HR_22_22==T|
          TrialOne_manRef_HR_23_23==T),
       RefHr:=TRUE]
xtabs(~ar$RefHr, addNA=T)

#refHrHosp_2 rename to refHr_2
ar[,refHr_2:=(
  TrialOne_refHR_29_29==T|
    TrialOne_refHR_30_30==T|
    TrialOne_refHR_31_31==T|
    TrialOne_refHR_32_32==T|
    TrialOne_refHR_33_33==T|
    TrialOne_refHR_34_34==T|
    TrialOne_refHR_35_35==T|
    TrialOne_refHR_36_36==T|
    TrialOne_refHR_35_37==T)]


ar[Opportunity_GDM_screening_2==1 &
         (TrialOne_anvisitnew_24_24==T & 
            (RefHr==T))|
         (TrialOne_anvisitnew_25_25==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T))|
         (TrialOne_anvisitnew_26_26==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T))|
         (TrialOne_anvisitnew_27_27==T & 
            (RefHr==T|TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T))|
         (TrialOne_anvisitnew_28_28==T & 
            (RefHr==T|
               TrialOne_manRef_HR_24_24==T|
               TrialOne_manRef_HR_25_25==T|
               TrialOne_manRef_HR_26_26==T|
               TrialOne_manRef_HR_27_27==T)), 
       Opportunity_GDM_screening_2:=Opportunity_GDM_screening_2-1]

# checks
xtabs(~ar$Opportunity_GDM_screening_2, addNA=T)

#Screening before 24 weeks: Creating one var for 3 possibilities
ar[,screenb424:=as.logical(NA)]
ar[bookgestagedays_cats %in% c("(0,104]","(104,125]","(125,160]","(160,167]"),
       screenb424:=F]
ar[screenb424==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklaburglu) | !is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenb424:=T]
xtabs(~ar$screenb424, addNA=T)

scrb424 <- ar[,.(A=sum(ident_dhis2_control==T),
                     B=sum(ident_dhis2_control==F)),
                  keyby=.(screenb424)]

##Defining Successes 
ar[,GDMscreeningontime_1A:=as.logical(NA)]
ar[,GDMscreeningontime_1B:=as.logical(NA)]
ar[,GDMscreeningontime_1C:=as.logical(NA)]
ar[screenb424==F, 
       GDMscreeningontime_1:=FALSE]
ar[screenb424==T, 
       GDMscreeningontime_1:=TRUE]

xtabs(~ar$GDMscreeningontime_1, addNA=T)


ar[,GDMscreeningontime_1A:=as.logical(NA)]
ar[Opportunity_GDM_screening_1==1 & 
         booklaburglu=="NEG", 
       GDMscreeningontime_1A:=TRUE]

ar[,GDMscreeningontime_1B:=as.logical(NA)]
ar[Opportunity_GDM_screening_1==1 &
         booklaburglu=="POS" & 
         !is.na(booklabbloodglu), GDMscreeningontime_1B:=TRUE]

ar[,GDMscreeningontime_1C:=as.logical(NA)]
ar[booklabbloodglu_high==T &
         !is.na(booklabbloodglu) &
         RefHr==T, GDMscreeningontime_1C:=TRUE]



#24-28 weeks
ar[,GDMscreeningontime_2:=as.logical(NA)]
ar[Opportunity_GDM_screening_2==1 &
         (TrialOne_labbloodglu_exists_24_24==F &
            TrialOne_labbloodglu_exists_25_25==F &
            TrialOne_labbloodglu_exists_26_26==F &
            TrialOne_labbloodglu_exists_27_27==F &
            TrialOne_labbloodglu_exists_28_28==F) &
         (TrialOne_labfastbloodglu_exists_24_24==F &
            TrialOne_labfastbloodglu_exists_25_25==F &
            TrialOne_labfastbloodglu_exists_26_26==F &
            TrialOne_labfastbloodglu_exists_27_27==F &
            TrialOne_labfastbloodglu_exists_28_28==F), GDMscreeningontime_2:=F]
ar[Opportunity_GDM_screening_2==1 & 
         (TrialOne_labbloodglu_exists_24_24==T|
            TrialOne_labbloodglu_exists_25_25==T|
            TrialOne_labbloodglu_exists_26_26==T|
            TrialOne_labbloodglu_exists_27_27==T|
            TrialOne_labbloodglu_exists_28_28==T) &
         (TrialOne_labbloodglu_high_24_24==F|
            TrialOne_labbloodglu_high_25_25==F|
            TrialOne_labbloodglu_high_26_26==F|
            TrialOne_labbloodglu_high_27_27==F|
            TrialOne_labbloodglu_high_28_28==F)|
         (TrialOne_labfastbloodglu_exists_24_24==T|
            TrialOne_labfastbloodglu_exists_25_25==T|
            TrialOne_labfastbloodglu_exists_26_26==T|
            TrialOne_labfastbloodglu_exists_27_27==T|
            TrialOne_labfastbloodglu_exists_28_28==T),GDMscreeningontime_2:=TRUE]
xtabs(~ar$GDMscreeningontime_2, addNA=T)


#Screening after 28 weeks: Creating one var for 3 possibilities
ar[,screenafter28:=as.logical(NA)]
ar[bookgestagedays_cats %in% c("(202,216]","(216,237]","(237,244]","(244,265]"),
       screenafter28:=F]
ar[screenafter28==F &
         (booklabbloodglu_high==F | is.na(booklabbloodglu_high)) &
         (!is.na(booklabbloodglu)|!is.na(booklabfastbloodglu)),
       screenafter28:=T]
xtabs(~ar$screenafter28, addNA=T)

##Defining Success
ar[,GDMscreeningontime_3:=as.logical(NA)]
ar[screenafter28==F, 
       GDMscreeningontime_3:=FALSE]
ar[screenafter28==T,GDMscreeningontime_3:=TRUE]
xtabs(~ar$GDMscreeningontime_3, addNA=T)

#management fo high RBG outside of time windows
ar[, GDMscreeningontime_4:=as.logical(NA)]
ar[Opportunity_GDM_screening_4==1, GDMscreeningontime_4:= FALSE]
ar[GDMscreeningontime_4==F & 
         (RefHr==T|refHr_2==T),GDMscreeningontime_4:=TRUE]


prelimGDM <- ar[ident_dhis2_booking==1,.(N=.N,
                       Opportun_1=sum(Opportunity_GDM_screening_1==T, na.rm=T),
                       Success_1A=sum(GDMscreeningontime_1A==T, na.rm=T),
                       Success_1B=sum(GDMscreeningontime_1B==T, na.rm=T),
                       Success_1C=sum(GDMscreeningontime_1C==T, na.rm=T),
                       Screenb424=sum(screenb424==T, na.rm=T),
                       Screenb424False=sum(screenb424==F, na.rm=T),
                       Opportun_2=sum(Opportunity_GDM_screening_2==T, na.rm=T),
                       Success_2=sum(GDMscreeningontime_2==T, na.rm=T),
                       Opportun_3=sum(Opportunity_GDM_screening_3==T, na.rm=T),
                       Success_3=sum(GDMscreeningontime_3==T, na.rm=T),
                       screenafter28=sum(screenafter28==T, na.rm=T),
                       screenafter28False=sum(screenafter28==F, na.rm=T),
                       screenbtwn=sum(GDMscreeningontime_4==T, na.rm=T),
                       screenbtwnFalse=sum(GDMscreeningontime_4==F, na.rm=T),
                       Opportun_4=sum(Opportunity_GDM_screening_4==T, na.rm=T),
                       Succ_4=sum(GDMscreeningontime_4, na.rm=T)),
                    keyby=.(bookyear)]



openxlsx::write.xlsx(prelimGDM,file.path(FOLDER_DATA_RESULTS,
                                         "annual reports",
                                         "2020",
                                         sprintf("%s_GDM.xlsx",
                                                 lubridate::today()))) 




# ultrasound

ar[,useventsx:=0]

vars <- names(d)[stringr::str_detect(names(d),"^usevent_")]

for (i in vars){
  
  ar[!is.na(get(i)),useventsx:=useventsx+1]
}
xtabs(~ar$useventsx, addNA=T)

Usnums <- ar[ident_dhis2_booking==T,
                 .(N=.N,
                   mean=mean(useventsx, na.rm=T),
                   min=min(useventsx),
                   max=max(useventsx)),
                 keyby=.(bookyear)]

openxlsx::write.xlsx(Usnums,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               "2020",
                               "usnums.xlsx"))




UsnumsbookgA <- ar[ident_dhis2_booking==T,
                 .(N=.N,
                   mean=mean(useventsx, na.rm=T),
                   min=min(useventsx),
                   max=max(useventsx)),
                 keyby=.(bookyear, bookgestagedays_cats)]


openxlsx::write.xlsx(UsnumsbookgA,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               "2020",
                               "usnumsbyGA.xlsx"))




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
                               "2020",
                               "usOntime.xlsx"))






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
                    keyby=.(bookyear, bookgestagedays_cats)]

openxlsx::write.xlsx(usontimeGAcats,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               "2020",
                               "usOntimeBookGAcats.xlsx"))




nums <- ar[,.("Total Number of women"=.N,
              "Booked for ANC"=sum(ident_dhis2_booking==T, na.rm=T),
              "Booked ANC and no PPC"=sum(ident_dhis2_booking==1 &
                                            is.na(ident_dhis2_ppc)),
              "Booked for ANC and PPC"=sum(ident_dhis2_booking==1 &
                                             ident_dhis2_ppc==T, na.rm=T),
              "PPC only"=sum(ident_dhis2_booking==0 &
                               ident_dhis2_ppc==T, na.rm=T),
              "CPO and Booking"=sum(ident_dhis2_cpo==T &
                                      ident_dhis2_booking==T, na.rm=T),
              "CPO"=sum(ident_dhis2_cpo==T, na.rm=T),
              "CPO PPC and Booking"=sum(ident_dhis2_booking==T &
                                          ident_dhis2_cpo==T & 
                                          ident_dhis2_ppc==T, na.rm=T)),
           keyby=.(bookyear)]



openxlsx::write.xlsx(nums,
                     file.path(FOLDER_DATA_RESULTS,
                               "annual reports",
                               "2020",
                               "NumVisits.xlsx"))



#ar[,ussc_1:=sum(us exists 00-14)]
# 1st 2nd and 3rd trimester USs
## overall
## fgr screening
## referral
# care at delivery
## cs births
## availability of data from cpo and nbc


vars <- stringr::str_subset(names(ar),"^cpopregoutcome_")


vars <- stringr::str_subset(names(ar),"^cpomodeofdeliv_")



# quality of ppc
## how many visits
### how many return from anc and how many dont
### cs based on robson guideines

ppc <- ar[ident_dhis2_ppc=T]

ppc[,ppcbookdate:=ppcdate_1]
ppc[,ppcbookyear:=stringr::str_extract(ppcdate_1,"[0-9][0-9][0-9][0-9]")]
ppc[,ppcbookyear:= as.numeric(ppcbookyear)]

ppctab <- ppc[]

## first and second visits
## days after delivery
#Erase Everything and go home