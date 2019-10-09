###### SETUP STARTS ######
###Jzoor Conference for Buthaian###

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

d <- LoadDataFileFromNetworkWB()

###ANC stuff###
td <- d[ident_dhis2_booking==1]

td[,angestage_0:=bookgestage]
td[,andate_0:=bookdate]
td[,anevent_0:=bookevent]



#anc visits
a1 <- stringr::str_subset(names(td),"^angestage_")
a2 <- stringr::str_subset(names(td),"^andate_")
a3 <- stringr::str_subset(names(td),"^anevent_")
a4 <-stringr::str_subset(names(td),"^anbpsyst_")
a5 <-stringr::str_subset(names(td),"^anbpdiast_")


# attempt to make a long lab dataset
l1a <- stringr::str_subset(names(td),"^labgestage_")
l1b <- stringr::str_subset(names(td),"^labdate_")
l2 <- stringr::str_subset(names(td),"^labogct_")
l3 <- stringr::str_subset(names(td),"^labhb_")
l4 <- stringr::str_subset(names(td),"^labbloodglu_")
l5 <- stringr::str_subset(names(td),"^laburglu_")
l6 <- stringr::str_subset(names(td),"^labfastbloodglu_")

#ultrasound data sets
us1 <- stringr::str_subset(names(td),"^usgestage_")
us2 <- stringr::str_subset(names(td),"^usdate_")
us3 <- stringr::str_subset(names(td),"^uspres_")
us4 <- stringr::str_subset(names(td),"^usevent_")


#risks
r1a <- stringr::str_subset(names(td),"^riskgestage_")
r1b <- stringr::str_subset(names(td),"^riskdate_")
r2 <- stringr::str_subset(names(td),"^risktype_")

m1 <- stringr::str_subset(names(td),"^mandate_")
m2 <- stringr::str_subset(names(td),"^mantypey_")
m3 <- stringr::str_subset(names(td),"^mangestage_")
m4 <- stringr::str_subset(names(td),"^manperf_")



#blood hb
td[, bookhb:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labhb_1) &
     labhb_1>0,
   bookhb:= TRUE]

td[, book_anemia:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labhb_1) &
     labhb_1>0 & labhb_1<11,
   book_anemia:= TRUE]
# 
# # doing RISKS FOR ANEMIA
# td[book_anemia==T, book_anemia_risk := FALSE]
# # we need to cycle through all the risktypes
# # therefore we need to figure out HOW MANY risk types there are first
# numbers <- stringr::str_subset(names(td),"^risktype_")
# numbers <- stringr::str_remove(numbers,"risktype_")
# for(i in numbers){
#   risktype <- sprintf("risktype_%s",i)
#   riskdate <- sprintf("riskdate_%s",i)
#   
#   td[!is.na(book_anemia_risk) &
#        get(risktype) %in% c(
#          "MildAnemia",
#          "ModerateAnemia",
#          "SevereAnemia",
#          "HGBNotImproved"
#          ) &
#        lubridate::`%within%`(
#          get(riskdate), 
#          lubridate::interval(labdate_1,labdate_1+13)
#          ),
#      book_anemia_risk:= TRUE]
# }
# xtabs(~td$book_anemia_risk)

#bloodglu
td[, bookgluc:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labbloodglu_1) &
     labbloodglu_1>0,
   bookgluc:= TRUE]



# alternative method: td[,bookgluc:=as.numeric(difftime(labdate_1     #,bookdate, units=days))<14 &!is.na(labbloodglu_1) &
# labbloodglu_1>0]

td[, bookglucurine:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(laburglu_1) &
     laburglu_1>0,
   bookglucurine:= TRUE]
#if bookglucurine_NEG is false, then at booking the value is true. Did it this way so 
#we dont have to make another variable for the positive one
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(laburglu_1) &
     laburglu_1=="NEG",
   bookglucurine_NEG:= TRUE]



widelab <- td[,c("bookevent",
                 "bookgestage",
                 "bookorgname",
                 "bookorgdistrict",
                 "bookyear",
                 "bookhb",
                 "bookbpsyst",
                 "bookbpdiast",
                 "bookbpsyst",
                 "bookbpdiast",
                 "bookgluc",
                 "bookglucurine",
                 "book_anemia",
                 "bookglucurine_NEG",
                 a1,
                 a2,
                 a3,
                 a4,
                 a5,
                 l1a,
                 l1b,
                 l2,
                 l3,
                 l4,
                 l5,
                 l6,
                 r1a,
                 r1b,
                 r2,
                 m1,
                 m2,
                 m3,
                 m4,
                 us1,
                 us2,
                 us3,
                 us4),with=F]


#add labfasting blood glucose variable down here
longlab <- melt(widelab, id.vars = c("bookevent",
                                     "bookgestage",
                                     "bookorgname",
                                     "bookorgdistrict",
                                     "bookyear",
                                     "bookhb",
                                     "bookbpsyst",
                                     "bookbpdiast",
                                     "bookgluc",
                                     "bookglucurine",
                                     "book_anemia",
                                     "bookglucurine_NEG"),
                measure = patterns(
                  "^angestage_",
                  "^andate_",
                  "^anevent_",
                  "^anbpsyst_",
                  "^anbpdiast_",
                  "^labgestage_",
                  "^labdate_",
                  "^labogct_",
                  "^labhb_",
                  "^labbloodglu_",
                  "^laburglu_",
                  "^labfastbloodglu_",
                  "^usgestage_",
                  "^usdate_",
                  "^uspres_",
                  "^usevent_",
                  "^riskgestage_",
                  "^riskdate_",
                  "^risktype_",
                  "^mandate_",
                  "^mantypey_",
                  "^mangestage_",
                  "^manperf_"
                  
                ),
                value.name=c(
                  "angestage",
                  "andate",
                  "anevent",
                  "anbpsyst",
                  "anbpdiast",
                  "labgestage",
                  "labdate",
                  "labogct",
                  "labhb",
                  "labbloodglu",
                  "laburglu",
                  "labfastbloodglu",
                  "usgestage",
                  "usdate",
                  "uspres",
                  "usevent",
                  "riskgestage",
                  "riskdate",
                  "risktype",
                  "mandate",
                  "mantypey",
                  "mangestage",
                  "manperf"
                ))

#since labgestage are in whole numbers we dont have to worry about the decimal places
#if we had deciimals that we hve to take into consideration, we would have to either put a round paranthesis or use the round function around labgestage
longlab[,angestagecat:=fancycut::fancycut(x=angestage,
                                          "0-7"='[0,7]',
                                          "8-12"='[8,12]',
                                          "13-14"='[13,14]',
                                          "15-17"='[15,17]',
                                          "18-22"='[18,22]',
                                          "23-23"='[23,23]',
                                          "24-28"='[24,28]',
                                          "29-30"='[29,30]',
                                          "31-33"='[31,33]',
                                          "34-38"='[34,38]',
                                          "39-99"='[39,99]'
)]
xtabs(~longlab$angestagecat)


longlab[,labgestagecat:=fancycut::fancycut(x=labgestage,
                                           "0-7"='[0,7]',
                                           "8-12"='[8,12]',
                                           "13-14"='[13,14]',
                                           "15-17"='[15,17]',
                                           "18-22"='[18,22]',
                                           "23-23"='[23,23]',
                                           "24-28"='[24,28]',
                                           "29-30"='[29,30]',
                                           "31-33"='[31,33]',
                                           "34-38"='[34,38]',
                                           "39-99"='[39,99]'
                                      )]
xtabs(~longlab$labgestagecat)
#after this step we see that there are alot that are missing, so we want to check the 
#original labgestage to make sure that we arent losing them when we make our cats

xtabs(~longlab[is.na(labgestagecat)]$labgestage)
#table of extent>0 was the result so most likely they dont have the actual gest age

#here we are seeing how many arent missing of the ones we had before 
#since the sum is 0, we know that they really dont exist so our code is correct
sum(!is.na(longlab[is.na(labgestagecat)]$labgestage))


longlab[,bookgestagecat:=fancycut::fancycut(x=bookgestage,
                                            "0-7"='[0,7]',
                                            "8-12"='[8,12]',
                                            "13-14"='[13,14]',
                                            "15-17"='[15,17]',
                                            "18-22"='[18,22]',
                                            "23-23"='[23,23]',
                                            "24-28"='[24,28]',
                                            "29-30"='[29,30]',
                                            "31-33"='[31,33]',
                                            "34-38"='[34,38]',
                                            "39-99"='[39,99]'
)]

longlab[,usgestagecat:=fancycut::fancycut(x=usgestage,
                                          "0-7"='[0,7]',
                                          "8-12"='[8,12]',
                                          "13-14"='[13,14]',
                                          "15-17"='[15,17]',
                                          "18-22"='[18,22]',
                                          "23-23"='[23,23]',
                                          "24-28"='[24,28]',
                                          "29-30"='[29,30]',
                                          "31-33"='[31,33]',
                                          "34-38"='[34,38]',
                                          "39-99"='[39,99]'
)]
sum(!is.na(longlab[is.na(usgestagecat)]$usgestage))


longlab[,riskgestagecat:=fancycut::fancycut(x=riskgestage,
                                            "0-7"='[0,7]',
                                            "8-12"='[8,12]',
                                            "13-14"='[13,14]',
                                            "15-17"='[15,17]',
                                            "18-22"='[18,22]',
                                            "23-23"='[23,23]',
                                            "24-28"='[24,28]',
                                            "29-30"='[29,30]',
                                            "31-33"='[31,33]',
                                            "34-38"='[34,38]',
                                            "39-99"='[39,99]'
)]

longlab[,mangestagecat:=fancycut::fancycut(x=mangestage,
                                           "0-7"='[0,7]',
                                           "8-12"='[8,12]',
                                           "13-14"='[13,14]',
                                           "15-17"='[15,17]',
                                           "18-22"='[18,22]',
                                           "23-23"='[23,23]',
                                           "24-28"='[24,28]',
                                           "29-30"='[29,30]',
                                           "31-33"='[31,33]',
                                           "34-38"='[34,38]',
                                           "39-99"='[39,99]'
)]


sum(!is.na(longlab[is.na(bookgestagecat)]$bookgestage))




# fancycut(
#   x = -10:10,
#   Zero = 0,
#   Small = '[0,2)',
#   Medium = '[2,5]',
#   Large = '(5,10]'
#)

# lab risk

# declare all the people this is valid for
longlab[book_anemia==T,book_anemia_risk:=FALSE]

####### CREATING CLEAN DEFINITIONS FOR EACH ROW
longlab[,has_anvisit:= !is.na(anevent) & angestage>0]
longlab[,has_labhb:=!is.na(labhb) & labhb>0]
longlab[,has_labanemia:=!is.na(labhb) & labhb>0 & labhb<11]
longlab[,has_mildanemia:=!is.na(labhb) & labhb>9 & labhb<11]
longlab[,has_moderateanemia:=!is.na(labhb) & labhb>7 & labhb<9]
longlab[,has_severeanemia:=!is.na(labhb)&(labhb<=7)&labhb>0]
longlab[,has_noanemia:= !is.na(labhb) &
                        labhb>=11]


##Gestational Hypertension
longlab[,has_bp:=!is.na(anbpsyst) & 
                          anbpsyst>0 |
                          anbpdiast>0]
longlab[,has_bpnormal:=!is.na(has_bp) &
                        anbpsyst<=139 |
                        anbpdiast<90]

longlab[,has_mildGHT:= angestage>=20 & 
                                   anbpsyst>=140 &
                                   anbpsyst<150 |
                                   anbpdiast>=90 &
                                   anbpdiast<100]

longlab[,has_modGHT:= angestage>=20 &
                                   anbpsyst>=150 &
                                   anbpsyst<159 |
                                   anbpdiast>=100 &
                                   anbpdiast<109]

longlab[,has_sevGHT:= angestage>=20 &
                                  anbpsyst>=160 |
                                  anbpdiast>=110]

longlab[,has_chronicHT:=(angestage<20) &
                        (anbpsyst>140)]

longlab[, has_bpelevatedany:= anbpsyst>140 |
                              anbpdiast>100]



# we need to figure out the first gestational age with anemia
# so we can use this variable with the risk data
longlab[,labgestage_first_anemia:=NULL]
longlab[has_labanemia==TRUE,
        labgestage_first_anemia:=min(labgestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(labgestage_first_anemia),labgestage_first_anemia:=NA]
longlab[,labgestage_first_anemia:=mean(labgestage_first_anemia,na.rm=T),
        by=bookevent]
longlab[!is.na(labgestage_first_anemia), has_labanemia_risk:=FALSE]
longlab[!is.na(has_labanemia_risk==TRUE) &
          risktype %in% c(
            "MildAnemia",
            "ModerateAnemia",
            "SevereAnemia",
            "HGBNotImproved"
          ) &
          riskgestage >= labgestage_first_anemia, has_labanemia_risk:=TRUE]

longlab[,has_labanemia_risk_man:=!is.na(has_labanemia_risk) & 
          mantypey%in%c("MildAnemiaTreatmentFollowup",
                        "MildAneTreatment",
                        "ModAnemiaTreatmentFollowup",
                        "ModAneTreatment",
                        "SevAne"
                        
          ) & 
          manperf==1]

####variables for diffent kinds of hypertension
###Making variables for management process, only refer to hospital for mod and sevHT,
###chronic HT will be treated differently
###mildGHT
longlab[has_bp==TRUE &
          has_mildGHT==TRUE,
        angestage_first_mildGHT:=min(angestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(angestage_first_mildGHT),angestage_first_mildGHT:=NA]
longlab[,angestage_first_mildGHT:=mean(angestage_first_mildGHT,na.rm=T),
        by=bookevent]

longlab[,has_mildGHT_man:=!is.na(has_mildGHT) & 
          mantypey%in%c("UrineProteinScreening",
                        "SecondBPMeasurement",
                        "GHTLab",
                        "UrineAnalysisPositiveUS",
                        "MildHypertension"
          ) & 
          manperf==1]



###modGHT
longlab[has_bp==TRUE &
          has_modGHT==TRUE,
        angestage_first_modGHT:=min(angestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(angestage_first_modGHT),angestage_first_modGHT:=NA]
longlab[,angestage_first_modGHT:=mean(angestage_first_modGHT,na.rm=T),
        by=bookevent]

longlab[,has_modGHT_man:=!is.na(has_modGHT) & 
          mantypey%in%c("UrineProteinScreening",
                        "GHTUS",
                        "GHTLab",
                        "UrineAnalysisPositiveUS",
                        "ModerateHypertension",
                        "ModerateGHTHosp",
                        "GHTSymptomsHosp"
                        
          ) & 
          manperf==1]

##sevGHT
longlab[has_bp==TRUE &
          has_sevGHT==TRUE,
        angestage_first_sevGHT:=min(angestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(angestage_first_sevGHT),angestage_first_sevGHT:=NA]
longlab[,angestage_first_sevGHT:=mean(angestage_first_sevGHT,na.rm=T),
        by=bookevent]

longlab[,has_sevGHT_man:=!is.na(has_sevGHT) & 
          mantypey%in%c("GHTSymptomsHosp",
                        "SevereHypertension",
                        "GHTLab",
                        "GHTUS"
          ) & 
          manperf==1]


##chronicHT
longlab[has_bp==TRUE &
          has_chronicHT==TRUE,
        angestage_first_chronicHT:=min(angestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(angestage_first_chronicHT),angestage_first_chronicHT:=NA]
longlab[,angestage_first_chronicHT:=mean(angestage_first_mildGHT,na.rm=T),
        by=bookevent]

##Any high blood pressure
longlab[has_bpelevatedany==TRUE,
        angestage_first_has_bpelevatedany:=min(angestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(angestage_first_has_bpelevatedany),angestage_first_has_bpelevatedany:=NA]
longlab[,angestage_first_has_bpelevatedany:=mean(angestage_first_has_bpelevatedany,na.rm=T),
        by=bookevent]

###Defining our variables_GDM
longlab[,has_laburglu:=!is.na(laburglu)]
longlab[,has_laburglu_neg:= !is.na(has_laburglu) & laburglu=="NEG"]
longlab[,has_laburglu_pos:= !is.na(has_laburglu) & laburglu=="POS"]
longlab[,has_labbloodglu:=!is.na(has_laburglu) & !is.na(labbloodglu) & labbloodglu>0]
longlab[,has_labbloodglu_105_139:=!is.na(has_labbloodglu) & 
          labbloodglu>=105 & 
          labbloodglu<140]
longlab[,has_labbloodglu_140plus:=!is.na(has_labbloodglu) & labbloodglu>=140]

longlab[,has_laburglu_pos_rbs140plus:=(has_laburglu_neg==FALSE) &
          (has_labbloodglu_140plus==TRUE)]

longlab[,has_labogctany:=!is.na(labogct) & labogct>0 ]

longlab[,has_labogct:= (has_labbloodglu_105_139==T) & !is.na(labogct) & labogct>0 ]
longlab[,has_labogct_140plus:=!is.na(has_labogct) & labogct>=140]

longlab[,has_labfastbloodglu:=!is.na(labfastbloodglu) & labfastbloodglu>0]
longlab[,has_labfastbloodglu_95_125:=!is.na(labfastbloodglu) & 
                                    labfastbloodglu>=95 & 
                                    labfastbloodglu<126]
longlab[,has_labfastbloodglu_126plus:=!is.na(labfastbloodglu) & labfastbloodglu>=140]


####diabetes risk and management
longlab[,has_labDM:=(has_labogct_140plus==TRUE) |
          (has_labbloodglu_140plus==TRUE) |
          (has_laburglu_pos_rbs140plus==TRUE)]

# START HERE we need to figure out the first gestational age with anemia
# so we can use this variable with the risk data
longlab[,labgestage_first_DM:=NULL]
longlab[has_labDM==TRUE,
        labgestage_first_DM:=min(labgestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(labgestage_first_DM),labgestage_first_DM:=NA]
longlab[,labgestage_first_DM:=mean(labgestage_first_DM,na.rm=T),
        by=bookevent]
longlab[!is.na(labgestage_first_DM), has_labDM_risk:=FALSE]
longlab[!is.na(has_labDM_risk) &
          risktype %in% c(
            "GDM",
            "LikelyGDM",
            "LDM"
          ) &
          riskgestage >= labgestage_first_DM, has_labDM_risk:=TRUE]

longlab[,has_labDM_risk_man:=!is.na(has_labDM_risk) & 
          mantypey%in%c("DiabetesCondSpec",
                        "DiabetesDC",
                        "DiabetesHR",
                        "DiabetesRoutineBST",
                        "DiabetesUrineStickPositiveBST",
                        "GTTLikelyGDM",
                        "LikelyDiabetes",
                        "LikelyGDMLab",
                        "ScreeningGDM",
                        "ScreeningUrineGlucose"
                        
          ) &
          manperf==1]

length(unique(longlab$bookevent))

#creating variables for ultrasound
longlab[,has_us:=!is.na(usevent) & !is.na(usdate)]


# aggregate this down to each woman
each_woman <- longlab[,.(
  has_anvisit=max(has_anvisit,na.rm=TRUE),
  has_labhb=max(has_labhb,na.rm=TRUE),
  has_noanemia= max(has_noanemia, na.rm=TRUE),
  has_labanemia=max(has_labanemia,na.rm=TRUE),
  has_labanemia_risk=max(has_labanemia_risk,na.rm=TRUE),
  has_labanemia_risk_man=max(has_labanemia_risk_man,na.rm=TRUE),
  has_labanemia_risk_man2=max(has_labanemia_risk_man,na.rm=TRUE),
  has_severeanemia=max(has_severeanemia, na.rm=TRUE),
  has_mildanemia=max(has_mildanemia, na.rm=TRUE),
  has_moderateanemia=max(has_moderateanemia, na.rm=TRUE),
  has_bp=max(has_bp, na.rm=TRUE),
  has_bpnormal=max(has_bpnormal, na.rm=TRUE),
  has_mildGHT=max(has_mildGHT, na.rm=TRUE),
  has_modGHT=max(has_modGHT, na.rm=TRUE),
  has_sevGHT=max(has_sevGHT, na.rm=TRUE),
  has_chronicHT=max(has_chronicHT, na.rm=TRUE),
  has_modGHT_man=max(has_modGHT_man, na.rm=TRUE),
  has_mildGHT_man=max(has_mildGHT_man, na.rm=TRUE),
  has_sevGHT_man=max(has_sevGHT_man, na.rm=TRUE),
  has_bpelevatedany=max(has_bpelevatedany, na.rm=TRUE),
  has_laburglu_pos=max(has_laburglu_pos, na.rm=TRUE),
  has_labbloodglu=max(has_labbloodglu,na.rm=TRUE),
  has_labogct=max(has_labogct,na.rm=TRUE),
  has_labfastbloodglu=max(has_labfastbloodglu,na.rm=TRUE),
  has_labfastbloodglu_95_125=max(has_labfastbloodglu_95_125,na.rm=TRUE),
  has_labfastbloodglu_126plus=max(has_labfastbloodglu_126plus,na.rm=TRUE),
  has_laburglu_neg=max(has_laburglu_neg,na.rm=TRUE),
  has_labbloodglu_105_139=max(has_labbloodglu_105_139,na.rm=TRUE),
  has_labogctany=max(has_labogctany, na.rm=TRUE),
  has_labogct_140plus=max(has_labogct_140plus,na.rm=TRUE),
  has_labDM_risk=max(has_labDM_risk,na.rm=TRUE),
  has_labDM_risk_man=max(has_labDM_risk_man,na.rm=TRUE),
  has_us=max(has_us, na.rm=TRUE)
  
  
),keyby=.(
  bookorgname,
  bookorgdistrict,
  bookyear,
  bookevent,
  bookgestagecat,
  bookhb,
  bookbpsyst,
  bookbpdiast,
  angestagecat,
  labgestagecat,
  usgestagecat,
  bookgluc,
  bookglucurine,
  riskgestagecat,
  mangestagecat
)]
length(unique(each_woman$bookevent))

#getting rid of the infinites
vars <- names(each_woman)
for (v in vars){
  each_woman[is.infinite(get(v)), (v):= NA]
}

sum(each_woman$has_labhb,na.rm=T)
sum(each_woman$has_us,na.rm=T)



vars <- c("0-7",
          "8-12",
          "13-14",
          "15-17",
          "18-22",
          "23-23",
          "24-28",
          "29-30",
          "31-33",
          "34-38",
          "39-99")

# expand.grid multipies all of the columns together
# to create all possible combinations
# below this, we have a double loop. The reason that we have
# a double loop is because we want to cycle through all combinations
# of two variables. Instead of |creating it as we go", we can create
# ALL COMBINATIONS of the two variables before hand. This means that we
# just cycle through this instead, which only requires ONE LOOP and therefore
# simplifies everything
overview <- expand.grid(
  v=c(
    "has_anvisit",
    "has_labhb",
    "has_noanemia",
    "has_labanemia", 
    "has_mildanemia",
    "has_moderateanemia",
    "has_severeanemia",
    "has_labogct",
    "has_labfastbloodglu",
    "has_labbloodglu",
    "has_labanemia_risk",
    "has_labanemia_risk_man",
    "has_bp",
    "has_bpnormal",
    "has_mildGHT",
    "has_modGHT",
    "has_sevGHT",
    "has_chronicHT",
    "has_modGHT_man",
    "has_mildGHT_man",
    "has_sevGHT_man",
    "has_bpelevatedany",
    "has_labbloodglu",
    "has_laburglu_neg",
    "has_laburglu_pos",
    "has_labfastbloodglu",
    "has_labfastbloodglu_126plus",
    "has_labogctany",
    "has_laburglu_neg",
    "has_labfastbloodglu_95_125",
    "has_labbloodglu_105_139",
    "has_labogct_140plus",
    "has_labDM_risk",
    "has_labDM_risk_man",
    "has_us"),
  
  i=1:length(vars),
  stringsAsFactors=F
)
# overview is now a data.frame of all combinations of the above two variables
# we use stringsAsFactors=F to make sure that all strings are STRINGS
# (otherwise they will be factors)
# setDT turns it into data.table
setDT(overview)
overview[,ages:=vars[i]]
overview[,gestage_var:="labgestagecat"]
overview[v %in% c(
  "has_anvisit",
  "has_bp"
),gestage_var:="angestagecat"]
overview[v %in% c(
  "has_us"
  
),gestage_var:="usgestagecat"]
overview[v %in% c(
  "has_labanemia_risk",
  "has_labDM_risk"
),gestage_var:="riskgestagecat"]

overview[v %in% c(
  "has_labanemia_risk_man",
  "has_labDM_risk_man"
),gestage_var:="mangestagecat"]


# new variable to create
overview[,has_x:=stringr::str_replace_all(sprintf("%s_%s",v,ages),"-","_")]
overview

# this here is a single loop over all combinations of v and i
# this could replace the double loop below
for(j in 1:nrow(overview)){
  has_x = overview$has_x[j]
  gestage = overview$gestage_var[j]
  i = overview$i[j]
  v = overview$v[j]
  
  #dt[(get, ():=get()]
  #this is the format we use when we want to "get"    
  #something or as assigning it into a variable (assignment
  #). we use get either in row selection or on the right 
  #side of assigning something to look inside 
  
  each_woman[bookgestagecat %in% vars[1:i], 
             (has_x):=FALSE]
  each_woman[bookgestagecat %in% vars[1:i] &
               get(gestage) %in% vars[i] &
               get(v)==1,
             (has_x):=TRUE]
}

sum(each_woman$has_labanemia_0_7,na.rm=T)



each_woman

unique(each_woman$bookyear)

#max vs sum
#can have multiple 0s but can have multiple ones
#because 
#when you take the maximimum of nothing is negative infinity so get warning message.

#would do this way if we want to aggregate manually but can use the 
#lapply with .SD column function becuase its quicker
# each_woman[,
#            .(
#               has_labhb_0_7= max(has_labhb_0_7, na.rm=T),
#               has_labhb_8_12= max(has_labhb_8_12, na.rm=T),
#               has_labhb_13_14= max(has_labhb_13_14, na.rm=T)
#               ),
#            keyby=.(
#                   bookevent,
#                   bookgestagecat
#                   )
#            
#            ]


# mtcars[order(gear, cyl), lapply(.SD, mean), by = .(gear, cyl), 
#.SDcols = cols_chosen]

each_woman <- each_woman[,
                         lapply(.SD, max, na.rm=T),
                         
                         keyby=.(
                           bookyear,
                           bookorgdistrict,
                           bookorgname,
                           bookevent,
                           bookgestagecat,
                           bookhb,
                           bookbpsyst,
                           bookbpdiast,
                           bookgluc,
                           bookglucurine
                         ),
                         .SDcols=c("has_anvisit_0_7",
                                   "has_anvisit_8_12",
                                   "has_anvisit_13_14",
                                   "has_anvisit_15_17",
                                   "has_anvisit_18_22",
                                   "has_anvisit_23_23",
                                   "has_anvisit_24_28",
                                   "has_anvisit_29_30",
                                   "has_anvisit_31_33",
                                   "has_anvisit_34_38",
                                   "has_anvisit_39_99",
                                   
                                   "has_labhb_0_7",
                                   "has_labhb_8_12",
                                   "has_labhb_13_14",
                                   "has_labhb_15_17",
                                   "has_labhb_18_22",
                                   "has_labhb_23_23",
                                   "has_labhb_24_28",
                                   "has_labhb_29_30",
                                   "has_labhb_31_33",
                                   "has_labhb_34_38",
                                   "has_labhb_39_99",
                                   
                                   "has_noanemia_0_7",
                                   "has_noanemia_8_12",
                                   "has_noanemia_13_14",
                                   "has_noanemia_15_17",
                                   "has_noanemia_18_22",
                                   "has_noanemia_23_23",
                                   "has_noanemia_24_28",
                                   "has_noanemia_29_30",
                                   "has_noanemia_31_33",
                                   "has_noanemia_34_38",
                                   "has_noanemia_39_99",
                                   
                                   
                                   
                                   "has_mildanemia_0_7",
                                   "has_mildanemia_8_12",
                                   "has_mildanemia_13_14",
                                   "has_mildanemia_15_17",
                                   "has_mildanemia_18_22",
                                   "has_mildanemia_23_23",
                                   "has_mildanemia_24_28",
                                   "has_mildanemia_29_30",
                                   "has_mildanemia_31_33",
                                   "has_mildanemia_34_38",
                                   "has_mildanemia_39_99",
                                   "has_moderateanemia_0_7",
                                   "has_moderateanemia_8_12",
                                   "has_moderateanemia_13_14",
                                   "has_moderateanemia_15_17",
                                   "has_moderateanemia_18_22",
                                   "has_moderateanemia_23_23",
                                   "has_moderateanemia_24_28",
                                   "has_moderateanemia_29_30",
                                   "has_moderateanemia_31_33",
                                   "has_moderateanemia_34_38",
                                   "has_severeanemia_0_7",
                                   "has_severeanemia_8_12",
                                   "has_severeanemia_13_14",
                                   "has_severeanemia_15_17",
                                   "has_severeanemia_18_22",
                                   "has_severeanemia_23_23",
                                   "has_severeanemia_24_28",
                                   "has_severeanemia_29_30",
                                   "has_severeanemia_31_33",
                                   "has_severeanemia_34_38",
                                   "has_labanemia_0_7",
                                   "has_labanemia_8_12",
                                   "has_labanemia_13_14",
                                   "has_labanemia_15_17",
                                   "has_labanemia_18_22",
                                   "has_labanemia_24_28",
                                   "has_labanemia_29_30",
                                   "has_labanemia_31_33",
                                   "has_labanemia_34_38",
                                   "has_labanemia_risk_24_28",
                                   "has_labanemia_risk_31_33",
                                   "has_labanemia_risk_34_38",
                                   "has_labanemia_risk_man_24_28",
                                   "has_labanemia_risk_man_31_33",
                                   "has_labanemia_risk_man_34_38",
                                   
                                   "has_bp_0_7",
                                   "has_bp_8_12",
                                   "has_bp_13_14",
                                   "has_bp_15_17",
                                   "has_bp_18_22",
                                   "has_bp_23_23",
                                   "has_bp_24_28",
                                   "has_bp_29_30",
                                   "has_bp_31_33",
                                   "has_bp_34_38",
                                   
                                   "has_bpnormal_0_7",
                                   "has_bpnormal_8_12",
                                   "has_bpnormal_13_14",
                                   "has_bpnormal_15_17",
                                   "has_bpnormal_18_22",
                                   "has_bpnormal_23_23",
                                   "has_bpnormal_24_28",
                                   "has_bpnormal_29_30",
                                   "has_bpnormal_31_33",
                                   "has_bpnormal_34_38",
                                   
                                   "has_bpelevatedany_0_7",
                                   "has_bpelevatedany_8_12",
                                   "has_bpelevatedany_13_14",
                                   "has_bpelevatedany_15_17",
                                   "has_bpelevatedany_18_22",
                                   "has_bpelevatedany_23_23",
                                   "has_bpelevatedany_24_28",
                                   "has_bpelevatedany_29_30",
                                   "has_bpelevatedany_31_33",
                                   "has_bpelevatedany_34_38",
                                   
                                   
                                   "has_mildGHT_0_7",
                                   "has_mildGHT_8_12",
                                   "has_mildGHT_13_14",
                                   "has_mildGHT_15_17",
                                   "has_mildGHT_18_22",
                                   "has_mildGHT_23_23",
                                   "has_mildGHT_24_28",
                                   "has_mildGHT_29_30",
                                   "has_mildGHT_31_33",
                                   "has_mildGHT_34_38",
                                   "has_modGHT_0_7",
                                   "has_modGHT_8_12",
                                   "has_modGHT_13_14",
                                   "has_modGHT_15_17",
                                   "has_modGHT_18_22",
                                   "has_modGHT_23_23",
                                   "has_modGHT_24_28",
                                   "has_modGHT_29_30",
                                   "has_modGHT_31_33",
                                   "has_modGHT_34_38",
                                   "has_sevGHT_0_7",
                                   "has_sevGHT_8_12",
                                   "has_sevGHT_13_14",
                                   "has_sevGHT_15_17",
                                   "has_sevGHT_18_22",
                                   "has_sevGHT_23_23",
                                   "has_sevGHT_24_28",
                                   "has_sevGHT_29_30",
                                   "has_sevGHT_31_33",
                                   "has_sevGHT_34_38",
                                   "has_mildGHT_man_0_7",
                                   "has_mildGHT_man_8_12",
                                   "has_mildGHT_man_13_14",
                                   "has_mildGHT_man_15_17",
                                   "has_mildGHT_man_18_22",
                                   "has_mildGHT_man_23_23",
                                   "has_mildGHT_man_24_28",
                                   "has_mildGHT_man_29_30",
                                   "has_mildGHT_man_31_33",
                                   "has_mildGHT_man_34_38",
                                   "has_modGHT_man_0_7",
                                   "has_modGHT_man_8_12",
                                   "has_modGHT_man_13_14",
                                   "has_modGHT_man_15_17",
                                   "has_modGHT_man_18_22",
                                   "has_modGHT_man_23_23",
                                   "has_modGHT_man_24_28",
                                   "has_modGHT_man_29_30",
                                   "has_modGHT_man_31_33",
                                   "has_modGHT_man_34_38",
                                   "has_sevGHT_man_0_7",
                                   "has_sevGHT_man_8_12",
                                   "has_sevGHT_man_13_14",
                                   "has_sevGHT_man_15_17",
                                   "has_sevGHT_man_18_22",
                                   "has_sevGHT_man_23_23",
                                   "has_sevGHT_man_24_28",
                                   "has_sevGHT_man_29_30",
                                   "has_sevGHT_man_31_33",
                                   "has_sevGHT_man_34_38",
                                   "has_chronicHT_0_7",
                                   "has_chronicHT_8_12",
                                   "has_chronicHT_13_14",
                                   "has_chronicHT_15_17",
                                   "has_chronicHT_18_22",
                                   
                                   
                                   "has_laburglu_pos_0_7",
                                   "has_laburglu_pos_8_12",
                                   "has_laburglu_pos_13_14",
                                   "has_laburglu_pos_15_17",
                                   "has_laburglu_pos_18_22",
                                   "has_laburglu_pos_23_23",
                                   "has_laburglu_pos_24_28",
                                   "has_laburglu_pos_29_30",
                                   "has_laburglu_pos_31_33",
                                   "has_laburglu_pos_34_38",
                                   "has_labfastbloodglu_0_7",
                                   "has_labfastbloodglu_8_12",
                                   "has_labfastbloodglu_13_14",
                                   "has_labfastbloodglu_15_17",
                                   "has_labfastbloodglu_18_22",
                                   "has_labfastbloodglu_23_23",
                                   "has_labfastbloodglu_24_28",
                                   "has_labfastbloodglu_29_30",
                                   "has_labfastbloodglu_31_33",
                                   "has_labfastbloodglu_34_38",
                                   "has_labfastbloodglu_39_99",
                                   "has_labfastbloodglu_95_125_0_7",
                                   "has_labfastbloodglu_95_125_8_12",
                                   "has_labfastbloodglu_95_125_13_14",
                                   "has_labfastbloodglu_95_125_15_17",
                                   "has_labfastbloodglu_95_125_18_22",
                                   "has_labfastbloodglu_95_125_23_23",
                                   "has_labfastbloodglu_95_125_24_28",
                                   "has_labfastbloodglu_95_125_29_30",
                                   "has_labfastbloodglu_95_125_31_33",
                                   "has_labfastbloodglu_95_125_34_38",
                                   "has_labfastbloodglu_95_125_39_99",
                                   "has_labfastbloodglu_126plus_0_7",
                                   "has_labfastbloodglu_126plus_8_12",
                                   "has_labfastbloodglu_126plus_13_14",
                                   "has_labfastbloodglu_126plus_15_17",
                                   "has_labfastbloodglu_126plus_18_22",
                                   "has_labfastbloodglu_126plus_23_23",
                                   "has_labfastbloodglu_126plus_24_28",
                                   "has_labfastbloodglu_126plus_29_30",
                                   "has_labfastbloodglu_126plus_31_33",
                                   "has_labfastbloodglu_126plus_34_38",
                                   "has_labbloodglu_105_139_0_7",
                                   "has_labbloodglu_105_139_8_12",
                                   "has_labbloodglu_105_139_13_14",
                                   "has_labbloodglu_105_139_15_17",
                                   "has_labbloodglu_105_139_18_22",
                                   "has_labbloodglu_105_139_23_23",
                                   "has_labbloodglu_105_139_24_28",
                                   "has_labbloodglu_105_139_29_30",
                                   "has_labbloodglu_105_139_31_33",
                                   "has_labbloodglu_105_139_34_38",
                                   "has_labbloodglu_0_7",
                                   "has_labbloodglu_8_12",
                                   "has_labbloodglu_13_14",
                                   "has_labbloodglu_15_17",
                                   "has_labbloodglu_18_22",
                                   "has_labbloodglu_23_23",
                                   "has_labbloodglu_24_28",
                                   "has_labbloodglu_29_30",
                                   "has_labbloodglu_31_33",
                                   "has_labbloodglu_34_38",
                                   "has_labbloodglu_39_99",
                                   
                                   "has_labogctany_0_7",
                                   "has_labogctany_8_12",
                                   "has_labogctany_13_14",
                                   "has_labogctany_15_17",
                                   "has_labogctany_18_22",
                                   "has_labogctany_23_23",
                                   "has_labogctany_24_28",
                                   "has_labogctany_29_30",
                                   "has_labogctany_31_33",
                                   "has_labogctany_34_38",
                                   "has_labogctany_39_99",
                                   
                                   "has_labogct_24_28",
                                   "has_labogct_29_30",
                                   "has_labogct_31_33",
                                   "has_labogct_34_38",
                                   "has_labogct_39_99",
                                   "has_laburglu_neg_0_7",
                                   "has_laburglu_neg_8_12",
                                   "has_laburglu_neg_13_14",
                                   "has_laburglu_neg_13_14",
                                   "has_laburglu_neg_15_17",
                                   "has_laburglu_neg_18_22",
                                   "has_laburglu_neg_23_23",
                                   "has_laburglu_neg_24_28",
                                   "has_laburglu_neg_29_30",
                                   "has_laburglu_neg_31_33",
                                   "has_laburglu_neg_34_38",
                                   "has_laburglu_neg_39_99",
                                   "has_labogct_24_28",
                                   "has_labogct_29_30", 
                                   "has_labogct_31_33", 
                                   "has_labogct_34_38",
                                   "has_labogct_140plus_29_30",
                                   "has_labogct_140plus_31_33",
                                   "has_labogct_140plus_34_38",
                                   "has_labogct_140plus_39_99",
                                   "has_labDM_risk_0_7",
                                   "has_labDM_risk_8_12",
                                   "has_labDM_risk_13_14",
                                   "has_labDM_risk_15_17",
                                   "has_labDM_risk_18_22",
                                   "has_labDM_risk_23_23",
                                   "has_labDM_risk_24_28",
                                   "has_labDM_risk_29_30",
                                   "has_labDM_risk_31_33",
                                   "has_labDM_risk_34_38",
                                   "has_labDM_risk_39_99",
                                   "has_labDM_risk_man_0_7",
                                   "has_labDM_risk_man_8_12",
                                   "has_labDM_risk_man_13_14",
                                   "has_labDM_risk_man_15_17",
                                   "has_labDM_risk_man_18_22",
                                   "has_labDM_risk_man_23_23",
                                   "has_labDM_risk_man_24_28",
                                   "has_labDM_risk_man_29_30",
                                   "has_labDM_risk_man_31_33",
                                   "has_labDM_risk_man_34_38",
                                   "has_labDM_risk_man_39_99",
                                   
                                   "has_us_0_7",
                                   "has_us_8_12",
                                   "has_us_13_14",
                                   "has_us_15_17",
                                   "has_us_18_22",
                                   "has_us_23_23",
                                   "has_us_24_28",
                                   "has_us_29_30",
                                   "has_us_31_33",
                                   "has_us_34_38",
                                   "has_us_39_99"
                                   
                                   
                         )
                         
                         ]



#getting rid of the infinites
vars <- names(each_woman)

for (v in vars){
  each_woman[is.infinite(get(v)), (v):= NA]
}

#checking the information we have
longlab[bookevent=="zzghXNIsTfM"]

# vars1 <- c(1,1,0,1)
# vars2 <- c(1,0,1,0)
# 
# sum(vars1,vars2)
# sum(vars1|vars2)
# sum(vars1 & vars2)
  
each_woman[,has_bookevent:=!is.na(bookevent)]
#Make tables to make graphs from them
#Booked by gestational age category
##trimesters

td[,bookTrimestert:=fancycut::fancycut(x=bookgestage,
                                            "1st"='[0,12]',
                                            "2nd"='[13,26]',
                                            "3rd"='[27,42]'
                          )]


unique(td$bookTrimestert)

sd<- td[bookyear>=2017 &
          ident_dhis2_booking==1, 
                              c("bookTrimestert"
  
                            )]

#sd[,id:=1:.N]

pd <- melt.data.table(sd, id.vars="bookTrimestert")

levels(pd$bookTrimestert)
levels(pd$bookTrimestert) <- c("1st",
                               "2nd",
                               "3rd",
                               "Unknown due \n to no LMP")

uglytable <- pd[,.(
  N=.N),
  keyby=.(
    bookTrimestert
    
  )]


p <- ggplot(uglytable, aes(x= bookTrimestert, y=N, fill=bookTrimestert))
p <- p + geom_col(position="dodge", alpha=0.75)
p <- p + scale_fill_brewer("Booking by Trimester", palette="Set1")
#p <- p + scale_fill_discrete(name="Percents" ,labels=round((uglytable$percentage),digits=2))
p <- p + scale_x_discrete("Trimester")
p <- p + scale_y_continuous("Number of Women")
p <- p + labs(title="Booking by Trimester",
              subtitle = "January 2017-July 2019")
p <- p + geom_text(aes(label = N), 
                   size=6.5,
                   vjust = -0.5,
                   position=position_dodge(width=1))
p <- p + theme(text = element_text(size=44))
p <- p + theme_gray(22)
p 
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  sprintf("%s_BookingbyGestAge.png", lubridate::today())), 
  plot = p, width = 297, height = 210, units = "mm")

#attendance
uglytable <- each_woman[bookyear>=2017,
                        .(
                          "Booked"=.N,
                          "Booked_Before 16 weeks"=sum(has_anvisit_0_7==1|
                                                has_anvisit_8_12==1|
                                                has_anvisit_13_14==1,na.rm=TRUE),
                          "16 Week Visit"= sum(has_anvisit_15_17==1, na.rm=TRUE),
                          "2"= sum(has_anvisit_15_17 &                                                                           has_anvisit_18_22,  na.rm=TRUE),
                          "3"= sum(has_anvisit_15_17 &                                                                             has_anvisit_18_22 &
                                     has_anvisit_24_28,na.rm=TRUE),                                                     "4"= sum(has_anvisit_15_17 &                                                                            has_anvisit_18_22 &
                                  has_anvisit_24_28 &
                                  has_anvisit_31_33,na.rm=TRUE),                                                                                         
                          "5"= sum( has_anvisit_15_17 &                                                                           has_anvisit_18_22 &
                                      has_anvisit_24_28 &
                                      has_anvisit_31_33 &
                                      has_anvisit_34_38,na.rm=TRUE),
                          "18_22wksonly"=sum(has_anvisit_18_22, na.rm=TRUE),
                          "24_28 weeksonly"=sum(has_anvisit_24_28, na.rm=TRUE),
                          "31_33wksonly"=sum(has_anvisit_31_33, na.rm=TRUE),
                          "34_38wkonly"=sum(has_anvisit_34_38, na.rm=TRUE)
                          
                          
                        ),
                        keyby=
                          .(
                            bookyear
                          )]


openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_Attendance Table.xlsx",lubridate::today())))



#attendance table for graphs
uglytable <- each_woman[bookyear>=2017,
                        .(
                          "Before 16 weeks"=sum(has_anvisit_0_7==1,
                                                has_anvisit_8_12==1,
                                                has_anvisit_13_14==1, na.rm=TRUE),
                          "16 Week Visit"= sum(has_anvisit_15_17==1, na.rm=TRUE),
                          "2"= sum(has_anvisit_15_17 &                                                                           has_anvisit_18_22,  na.rm=TRUE),
                          "3"= sum(has_anvisit_15_17 &                                                                             has_anvisit_18_22 &
                                     has_anvisit_24_28,na.rm=TRUE),                                                     "4"= sum(has_anvisit_15_17 &                                                                            has_anvisit_18_22 &
                                    has_anvisit_24_28 &
                                    has_anvisit_31_33,na.rm=TRUE),                                                                                         
                          "5"= sum( has_anvisit_15_17 &                                                                           has_anvisit_18_22 &
                                    has_anvisit_24_28 &
                                    has_anvisit_31_33 &
                                    has_anvisit_34_38,na.rm=TRUE),
                          "18_22wksonly"=sum(has_anvisit_18_22, na.rm=TRUE),
                          "24_28 weeksonly"=sum(has_anvisit_24_28, na.rm=TRUE),
                          "31_33wksonly"=sum(has_anvisit_31_33, na.rm=TRUE),
                          "34_38wkonly"=sum(has_anvisit_34_38, na.rm=TRUE)
                          
                          
                        ),
                        keyby=
                          .(
                            bookyear
                          )]


openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_Attendance Table.xlsx",lubridate::today())))

plotdata <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Before 16 weeks",
    "16 Week Visit", 
    "2",
    "3",
    "4",
    "5"
    
    
  )
)

pd <- plotdata[,.(
  total = sum(value)
),
keyby=.(variable)
]

#maxYVAL <- max(uglytable$Booked)
labelAdjust <- maxYVAL*0.01

q <- ggplot(pd, aes(x=variable, 
                    y=total,
                    label=total,
                    fill=as.factor(variable)))

q <- q + geom_col()

q <- q + labs(title="Attendance of Essential ANC Visits ",
              subtitle = "January 2017 to July 2019") +
               xlab("ANC Visits Attended") +
               ylab("Women")
#centers title 
q <- q + theme(plot.title = element_text(size=18, hjust = 0.5),
               plot.subtitle = element_text(size=16, hjust = 0.5))
q <- q + theme(text = element_text(size=16))
#q <- q + labs(fill="Gestational Age at Visit")
q <- q + scale_fill_brewer( palette="Spectral",
                            name="Attendance of Essential Visits up Until", 
                             labels=c("Women Registered before 16 Weeks",
                                                     "15-17 Weeks",
                                                     "18-22 Weeks",
                                                     "24-28 Weeks",
                                                     "31-33 Weeks",
                                                     "34-38 Weeks"))
q <- q + geom_col(alpha=0.75)

q <- q + geom_text(vjust=0)
q

q <- q + theme(plot.title = element_text(size = 20, face = "bold"),
               axis.text.x = element_blank(),
               legend.title=element_text(size=10), 
               legend.text=element_text(size=9))
# q <- q + theme(axis.text.x = element_text(angle = 90,
#                                            hjust = 1,
#                                           vjust = 0.5))

q
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  sprintf("%s_Essential ANC Visits.png", lubridate::today())), 
  plot = q, width = 297, height = 210, units = "mm")



#LabHB_screenings and types of anemia
uglytable <- each_woman[,
                        .(
                          Booked=.N,
                          Screened_for_Anemia_at_Booking=sum(bookhb, na.rm=T),
                          No_anemia_total= sum( 
                                                has_noanemia_0_7|
                                                has_noanemia_8_12|
                                                has_noanemia_13_14|
                                                has_noanemia_15_17|
                                                has_noanemia_18_22|
                                                has_noanemia_23_23|
                                                has_noanemia_24_28|
                                                has_noanemia_29_30|
                                                has_noanemia_31_33, na.rm=TRUE),
                          no_labanemia_total= sum( has_labanemia_0_7==0 |
                                                     has_labanemia_8_12==0 |
                                                     has_labanemia_13_14==0 |
                                                     has_labanemia_15_17==0 |
                                                     has_labanemia_18_22==0 |
                                                     has_labanemia_24_28==0 |
                                                     has_labanemia_29_30==0 |
                                                     has_labanemia_31_33==0, na.rm=TRUE),
                            
                          Mild_anemia_total=sum( has_mildanemia_0_7|
                                                  has_mildanemia_13_14|
                                                  has_mildanemia_15_17|
                                                  has_mildanemia_18_22|
                                                  has_mildanemia_23_23|
                                                  has_mildanemia_24_28|
                                                  has_mildanemia_29_30|
                                                  has_mildanemia_31_33|
                                                  has_mildanemia_34_38, na.rm=TRUE),
                          
                          Moderate_anemia_total=sum(  has_moderateanemia_0_7|
                                                      has_moderateanemia_13_14|
                                                      has_moderateanemia_15_17|
                                                      has_moderateanemia_18_22|
                                                      has_moderateanemia_23_23|
                                                      has_moderateanemia_24_28|
                                                      has_moderateanemia_29_30|
                                                      has_moderateanemia_31_33|
                                                      has_moderateanemia_34_38, na.rm=TRUE),
                         Severe_anemia_total=sum(  has_severeanemia_0_7 |
                                                      has_severeanemia_13_14|
                                                      has_severeanemia_15_17|
                                                      has_severeanemia_18_22|
                                                      has_severeanemia_23_23|
                                                      has_severeanemia_24_28|
                                                      has_severeanemia_29_30|
                                                      has_severeanemia_31_33|
                                                      has_severeanemia_34_38, na.rm=TRUE),
                        Screened_1st_Trimester=sum( has_labhb_0_7 |
                                                    has_labhb_8_12, na.rm=TRUE),
                         
                        Screened_2nd_Trimester=sum( has_labhb_13_14 |
                                                    has_labhb_15_17 |
                                                    has_labhb_18_22 |
                                                    has_labhb_23_23 |
                                                    has_labhb_24_28,na.rm=TRUE),
                        Screened_3rd_Trimester= sum(has_labhb_29_30 |
                                                    has_labhb_31_33 |
                                                    has_anvisit_34_38, na.rm=TRUE),
                        No_anemia_1st_Trimester=sum(has_noanemia_0_7 |
                                                       has_noanemia_8_12, 
                                                        na.rm=TRUE),
                        No_anemia_2nd_Trimester=sum( has_noanemia_13_14 |
                                                          has_noanemia_15_17 |
                                                          has_noanemia_18_22 |
                                                          has_noanemia_23_23 |
                                                          has_noanemia_24_28 , na.rm=TRUE),
                        No_anemia_3rd_Trimester=sum(  has_noanemia_29_30 |
                                                          has_noanemia_31_33 |
                                                          has_noanemia_34_38, 
                                                        na.rm=TRUE),

                         Mild_anemia_1st_Trimester=sum(  has_mildanemia_0_7 |
                                                         has_mildanemia_8_12, 
                                                         na.rm=TRUE),
                         Mild_anemia_2nd_Trimester=sum(  has_mildanemia_13_14 |
                                                         has_mildanemia_15_17 |
                                                         has_mildanemia_18_22 |
                                                         has_mildanemia_23_23 |
                                                         has_mildanemia_24_28 , na.rm=TRUE),
                         Mild_anemia_3rd_Trimester=sum(  has_mildanemia_29_30 |
                                                         has_mildanemia_31_33 |
                                                         has_mildanemia_34_38, 
                                                         na.rm=TRUE),
                         Moderate_anemia_1st_Trimester=sum(has_moderateanemia_0_7 |
                                                         has_moderateanemia_8_12, 
                                                         na.rm=TRUE ),
                         Moderate_anemia_2nd_Trimester=sum(has_moderateanemia_13_14 |
                                                         has_moderateanemia_15_17 |
                                                         has_moderateanemia_18_22 |
                                                         has_moderateanemia_23_23 |
                                                         has_moderateanemia_24_28 ,
                                                         na.rm=TRUE),
                         Moderate_anemia_3rd_Trimester=sum(has_moderateanemia_29_30 |
                                                           has_moderateanemia_31_33 |
                                                           has_moderateanemia_34_38, 
                                                           na.rm=TRUE),
                         Severe_anemia_1st_Trimester=sum(has_severeanemia_0_7 |
                                                         has_severeanemia_8_12, na.rm=TRUE),
                        Severe_anemia_2nd_Trimester=sum(has_severeanemia_13_14 |
                                                        has_severeanemia_15_17 |
                                                        has_severeanemia_18_22 |
                                                        has_severeanemia_23_23 |
                                                        has_severeanemia_24_28 , 
                                                        na.rm=TRUE),
                        Severe_anemia_24_28_weeks=sum( has_severeanemia_24_28==1 , 
                                                        na.rm=TRUE),
                        
                        
                         Severe_anemia_3rd_Trimester=sum(has_severeanemia_29_30 |
                                                         has_severeanemia_31_33 |
                                                         has_severeanemia_34_38, 
                                                         na.rm=TRUE),
                        Mild_anemia_24_28_weeks=sum(has_mildanemia_24_28, na.rm=TRUE),
                        Moderate_anemia_24_28_weeks=sum(has_moderateanemia_24_28, na.rm=TRUE),
                        Severe_anemia_24_28_weeks=sum(has_severeanemia_24_28, 
                                                   na.rm=TRUE),
                        Mild_anemia_36_weeks=sum(has_mildanemia_34_38, na.rm=TRUE),
                        Moderate_anemia_36_weeks=sum(has_moderateanemia_34_38, na.rm=TRUE),
                        Severe_anemia_36_weeks=sum(has_severeanemia_34_38, 
                                                        na.rm=TRUE)
                        
                         
   
                          
                        ),
                        keyby=
                          .(
                            bookyear
                          )]


openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_Anemia Table.xlsx",lubridate::today())))


pd <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Booked", 
    "No_anemia_total",
    "Mild_anemia_total",
    "Moderate_anemia_total",
    "Severe_anemia_total"
  )
)

#pd[,denom:=NULL]
   
pd[variable=="Booked", denom:=value]

pd[,denom:=as.integer(mean(denom, na.rm=T)), by=bookyear]
##or
#pd[,denom2:=mean(denom, na.rm=T), by=bookyear]
#pd[,denom2:=NULL]
#the first line pulls the denominator into a new column
#the second line distributes the new column downwards

pd

pd[,perc:= round(100*value/denom, digits=2)]                      
#factoring and renaming variables in order to make them look nice and easier when we make the graphs
pd[bookyear%in%c(2017,2018,2019), pretty_variable:= factor(variable, 
                                                           levels=c("Booked", 
                                                 "No_anemia_total",
                                                 "Mild_anemia_total",
                                                 "Moderate_anemia_total",
                                                 "Severe_anemia_total"),
                                       labels=c("Booked", 
                                                "Not Anemic",
                                                "Mild Anemia", 
                                                "Moderate Anemia",
                                                "Severe Anemia"))]

maxYVAL <- max(uglytable$Booked)
labelAdjust <- maxYVAL*0.01

pd_labels <- pd[bookyear>="2017",.(
  total = sum(value)
),
keyby=.(variable)
]
# pd_labels[variable=="Booked", denom:=total]
# 
# pd_labels[,denom:=as.integer(mean(denom, na.rm=T))]
# pd_labels[,perc:= round(100*total/denom, digits=1)]

q <- q + geom_text(data=pd_labels, mapping=aes(text=total))

q <- ggplot(pd_labels, aes(x=variable, 
                    y=total,
                    label=total,
                    fill=variable))
q <- q + geom_col()

q <- q + labs(title="Anemia From the MCH eRegistry",
              subtitle ="January 2017-July 2019") +
              xlab("")+
              ylab("Women")

q <- q + theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks = element_blank())


q <- q + scale_fill_brewer( palette="Set2")
q <- q + geom_col(alpha=0.75)

q <- q + geom_text(vjust=0)
q
 
q <- q + theme(plot.title = element_text(size = 16, face = "bold"),
               plot.subtitle = element_text(size =12, face = "italic"),
               legend.text=element_text(size=9),
               text = element_text(size=9))


q
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  "Anemia.png"), 
  plot = q, width = 297, height = 210, units = "mm")


###Run original uglytable here first so that we can get the correct numbers
###HB screenings


pd <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Screened_1st_Trimester",
    "Screened_2nd_Trimester",
    "Screened_3rd_Trimester"
  )
)

pd[bookyear=="2018", pretty_variable:= factor(variable, levels=c( 
                                                                 "Screened_1st_Trimester",
                                                                 "Screened_2nd_Trimester",
                                                                 "Screened_3rd_Trimester"),
                                              labels=c(
                                                       "1st",
                                                       "2nd", 
                                                       "3rd"))]


pd_labels <- pd[bookyear>="2017",.(
  total = sum(value)
),
keyby=.(variable)
]


 

q <- ggplot(pd_labels, aes(x=variable, 
                                      y=total,
                                      label=total,
                                      fill=as.factor(variable)))
q <- q + geom_col()

q <- q + labs(title="Anemia Screening",
              subtitle ="By Trimester") +
              xlab("")+
             ylab("Women")

q <- q + theme(axis.title.x = element_blank(),
               legend.title = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks = element_blank())


q <- q + scale_fill_brewer( palette="Set2")
q <- q + geom_col(alpha=0.75)

q <- q + geom_text(vjust=0)
q

q <- q + theme(plot.title = element_text(size = 16, face = "bold"),
               plot.subtitle = element_text(size =12, face = "italic"),
               legend.text=element_text(size=9),
               text = element_text(size=9))


q
ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  sprintf("%s_Anemia_Screening_by_Trimester.png", lubridate::today())), 
  plot = q, width = 297, height = 210, units = "mm")





#HTN
uglytable <- each_woman[,
                        .(
                          Booked=.N,
                          Screened_at_Booking=sum(!is.na(bookbpsyst), na.rm=T),
                          BPTotal=sum( has_bp_0_7|
                                        has_bp_8_12|
                                        has_bp_13_14|
                                        has_bp_15_17|
                                        has_bp_18_22|
                                        has_bp_23_23|
                                        has_bp_24_28|
                                        has_bp_29_30|
                                        has_bp_31_33|
                                        has_bp_34_38, na.rm=TRUE ),
                          
                          NormalBPTotal=sum(has_bpnormal_0_7|
                                            has_bpnormal_8_12|
                                            has_bpnormal_13_14|
                                            has_bpnormal_15_17|
                                            has_bpnormal_18_22|
                                            has_bpnormal_23_23|
                                            has_bpnormal_24_28|
                                            has_bpnormal_29_30|
                                            has_bpnormal_31_33|
                                            has_bpnormal_34_38, na.rm=TRUE ),
                       
                          
                          ElevatedbpanyTotal= sum(has_bpelevatedany_0_7|
                                                  has_bpelevatedany_8_12|
                                                  has_bpelevatedany_13_14|
                                                  has_bpelevatedany_15_17|
                                                  has_bpelevatedany_18_22|
                                                  has_bpelevatedany_23_23|
                                                  has_bpelevatedany_24_28|
                                                  has_bpelevatedany_29_30|
                                                  has_bpelevatedany_31_33|
                                                  has_bpelevatedany_34_38, na.rm=TRUE ),
                          
                          Mild_GHT=sum( has_mildGHT_18_22|
                                                    has_mildGHT_23_23|
                                                    has_mildGHT_24_28|
                                                    has_mildGHT_29_30|
                                                    has_mildGHT_31_33|
                                                    has_mildGHT_34_38 , na.rm=TRUE),
                          Mild_GHT_18_22=sum(has_mildGHT_18_22, na.rm=TRUE),
                          Mild_GHT_24_28=sum(has_mildGHT_24_28, na.rm=TRUE),
                          Mild_GHT_34_38=sum(has_mildGHT_34_38, na.rm=TRUE),
                          
                          Mod_GHT_18_22=sum(has_moderateanemia_18_22, na.rm=TRUE),
                          Mod_GHT_24_28=sum(has_moderateanemia_24_28, na.rm=TRUE),
                          Mod_GHT_34_38=sum(has_moderateanemia_34_38, na.rm=TRUE),
                          
                          Moderate_GHT=sum( has_modGHT_18_22|
                                               has_modGHT_23_23|
                                               has_modGHT_24_28|
                                               has_modGHT_29_30|
                                               has_modGHT_31_33|
                                               has_modGHT_34_38 , na.rm=TRUE),
                          
                          Severe_GHT=sum( has_sevGHT_18_22|
                                            has_sevGHT_23_23|
                                            has_sevGHT_24_28|
                                            has_sevGHT_29_30|
                                            has_sevGHT_31_33|
                                            has_sevGHT_34_38, na.rm=TRUE),
                          
                          Screened_1st_Trimester=sum(has_bp_0_7,
                                                       has_bp_8_12, na.rm=TRUE),
                          
                          Screened_2nd_Trimester=sum(has_bp_13_14,
                                                        has_bp_15_17,
                                                        has_bp_18_22,
                                                        has_bp_23_23,na.rm=TRUE),
                          Screened_3rd_Trimester= sum( has_bp_24_28,
                                                       has_bp_29_30,
                                                        has_bp_31_33,
                                                        has_bp_34_38, na.rm=TRUE),
                          
                         NormalBp_1st_Trimester=sum(has_bpnormal_0_7|
                                                     has_bpnormal_8_12, na.rm=TRUE),
                          
                          NormalBp_2nd_Trimester=sum(has_bpnormal_13_14|
                                                     has_bpnormal_15_17|
                                                     has_bpnormal_18_22|
                                                     has_bpnormal_23_23,na.rm=TRUE),
                          NormalBp_3rd_Trimester= sum( has_bpnormal_24_28|
                                                       has_bpnormal_29_30|
                                                       has_bpnormal_31_33|
                                                       has_bpnormal_34_38, na.rm=TRUE),
                          
                          Chronic_Hypertension=sum(has_chronicHT_0_7|
                                                       has_chronicHT_8_12,
                                                       has_chronicHT_13_14,
                                                       has_chronicHT_15_17,
                                                       has_chronicHT_18_22, 
                                                          na.rm=TRUE),
                          
                         Mild_GHT_2nd_Trimester=sum(has_mildGHT_13_14|
                                                            has_mildGHT_15_17|
                                                            has_mildGHT_18_22|
                                                            has_mildGHT_23_23|
                                                            has_mildGHT_24_28 , na.rm=TRUE),
                         
                          Mild_GHT_3rd_Trimester=sum(has_mildGHT_29_30|
                                                            has_mildGHT_31_33|
                                                            has_mildGHT_34_38, 
                                                          na.rm=TRUE),
                         
                         
                          Moderate_GHT_2nd_Trimester=sum(has_modGHT_13_14|
                                                           has_modGHT_15_17|
                                                           has_modGHT_18_22|
                                                           has_modGHT_23_23|
                                                           has_modGHT_24_28 ,
                                                            na.rm=TRUE),
                          Moderate_GHT_3rd_Trimester=sum(has_modGHT_29_30|
                                                              has_modGHT_31_33|
                                                              has_modGHT_34_38, 
                                                            na.rm=TRUE),
        
                          Severe_GHT_2nd_Trimester=sum(has_sevGHT_13_14|
                                                            has_sevGHT_15_17|
                                                            has_sevGHT_18_22|
                                                            has_sevGHT_23_23|
                                                            has_sevGHT_24_28 , 
                                                          na.rm=TRUE),
                          Severe_GHT_3rd_Trimester=sum(has_sevGHT_29_30|
                                                            has_sevGHT_31_33|
                                                            has_sevGHT_34_38, 
                                                          na.rm=TRUE)
                          
                          
                          
                        ),
                        keyby=
                          .(
                            bookyear
                          )]

openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_GHT Table.xlsx", lubridate::today())))


###Screening of HTN
plotdata <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Num_women_booked", 
    "Screened_at_Booking",
    "Screened_1st_Trimester",
    "Screened_2nd_Trimester",
    "Screened_3rd_Trimester"
  )
)

q <- ggplot(plotdata[bookyear=="2018"], aes(x=variable, y=value))
q <- q + geom_col()
q


###HTN screenings
plotdata <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Num_women_booked", 
    "Screened_2nd_Trimester",
    "Mild_GHT_2nd_Trimester",
    "Moderate_GHT_2nd_Trimester",
    "Mild_GHT_3rd_Trimester",
    "Moderate_GHT_3rd_Trimester"
    
  )
)

q <- ggplot(plotdata[bookyear=="2018"], aes(x=variable, y=value))
q <- q + geom_col()
q

###Types of HTN
plotdata <- melt.data.table(
  uglytable,
  id.vars="bookyear", 
  measure.vars=c(
    "Num_women_booked", 
    "Screened_at_Booking",
    "Chronic_Hypertension",
    "Mild_GHT",
    "Moderate_GHT",
    "Severe_GHT"
  )
)

# q <- ggplot(plotdata[bookyear=="2018"], aes(x=variable, y=value))
# q <- q + geom_col()
# q





#OGCT
#add labfastbloddglu_126_plus in here somewhere
#DOUBLE CHECK these for commas...should be | ?
#has_labogct_0_7 means that the woman has an ogct value btwn 105 and 139
#has_labogctany_0_7 means that she has any ogctlab value
uglytable <- each_woman[,
                        .(
                          Num_women=.N,
                          Screened_Urine_glucose_at_booking=sum(bookglucurine, na.rm=TRUE),
                          Screened_random_blood_glucose_Total= sum(has_labbloodglu_0_7,
                                                             has_labbloodglu_8_12,
                                                             has_labbloodglu_13_14,
                                                             has_labbloodglu_15_17,
                                                             has_labbloodglu_18_22,
                                                             has_labbloodglu_23_23,
                                                             has_labbloodglu_24_28,
                                                             has_labbloodglu_29_30,
                                                             has_labbloodglu_31_33,
                                                             has_labbloodglu_34_38,
                                                             na.rm=TRUE),
                          
                          Screened_fasting_blood_glucose_Total= sum(has_labfastbloodglu_0_7,
                                                                   has_labfastbloodglu_8_12,
                                                                   has_labfastbloodglu_13_14,
                                                                   has_labfastbloodglu_15_17,
                                                                   has_labfastbloodglu_18_22,
                                                                   has_labfastbloodglu_23_23,
                                                                   has_labfastbloodglu_24_28,
                                                                   has_labfastbloodglu_29_30,
                                                                   has_labfastbloodglu_31_33,
                                                                   has_labfastbloodglu_34_38,
                                                                   na.rm=TRUE),
                     Screened_random_blood_glucose_before_24wks= sum(has_labbloodglu_0_7,
                                                                   has_labbloodglu_8_12,
                                                                   has_labbloodglu_13_14,
                                                                   has_labbloodglu_15_17,
                                                                   has_labbloodglu_18_22,
                                                                   has_labbloodglu_23_23,
                                                                   na.rm=TRUE),
                    Screened_random_blood_glucose_before_24wks= sum(has_labbloodglu_0_7,
                                                                   has_labfastbloodglu_8_12,
                                                                   has_labfastbloodglu_13_14,
                                                                   has_labfastbloodglu_15_17,
                                                                   has_labfastbloodglu_18_22,
                                                                   has_labfastbloodglu_23_23,
                                                                     na.rm=TRUE),
                    Screened_RBS_sixmos= sum(has_labfastbloodglu_24_28, na.rm=TRUE), 
                    Screened_FBS_sixmos= sum(has_labbloodglu_24_28, na.rm=TRUE), 
                     Positive_LaburGlu_Totals= sum(has_laburglu_pos_0_7,
                                                     has_laburglu_pos_8_12,
                                                     has_laburglu_pos_13_14,
                                                     has_laburglu_pos_15_17,
                                                     has_laburglu_pos_18_22,
                                                     has_laburglu_pos_23_23,
                                                     has_laburglu_pos_24_28,
                                                     has_laburglu_pos_29_30,
                                                     has_laburglu_pos_31_33,
                                                     has_laburglu_pos_34_38, na.rm=T),
                    
                    
                     Positive_LaburGlu_before_24weeks= sum(has_laburglu_pos_0_7,
                                                   has_laburglu_pos_8_12,
                                                   has_laburglu_pos_13_14,
                                                   has_laburglu_pos_15_17,
                                                   has_laburglu_pos_18_22,
                                                   has_laburglu_pos_23_23, na.rm=T),
           
                    
                     Screened_OGCT_with105to139valueAfter24wl= sum(has_labogct_24_28,
                                         has_labogct_29_30,
                                         has_labogct_31_33,
                                         has_labogct_34_38, na.rm=TRUE),
                    
                    FBS_likely_GDM=sum(has_labfastbloodglu_126plus_24_28,
                                       has_labfastbloodglu_126plus_29_30,
                                       has_labfastbloodglu_126plus_31_33,
                                       has_labfastbloodglu_126plus_34_38, na.rm=TRUE),
             
                      OGCT_LikelyDM=sum(has_labogct_24_28,
                                            has_labogct_140plus_29_30,
                                            has_labogct_140plus_31_33,
                                            has_labogct_140plus_34_38, na.rm=TRUE),
                    
                    Has_OGCTany_before24weeks=sum(has_labogctany_15_17,
                                                  has_labogctany_18_22,
                                                  has_labogctany_23_23,na.rm=TRUE),
                    
                    Has_OGCTany_after24weeks=sum(has_labogctany_24_28,
                                    has_labogctany_29_30,
                                    has_labogctany_31_33,
                                    has_labogctany_34_38,na.rm=TRUE)
       
                          
                        ),
                        keyby=
                          .(
                            bookyear
                          )]

openxlsx::write.xlsx(uglytable,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_GDM Table.xlsx",lubridate::today())))


####ANC, PPC, NBC bookings and visits

cat("\nNumANC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_an==T])

cat("\nNumANCvisits\n")
vars <- c(
  "bookevent",
  names(d)[stringr::str_detect(names(d),"^anevent_[0-9]*")]
)
d[,anevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), anevent_x:=anevent_x + 1]
}

#sum(d[ident_dhis2_control==F& bookyear>=2017]$anevent_x,na.rm=T)

#NBC
cat("\nNumNBC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_nbc==T& bookyear==2018])
cat("\nNumNBCvisits\n")
vars <- names(d)[stringr::str_detect(names(d),"^nbcevent_[0-9]*")]
d[,nbcevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), nbcevent_x:=nbcevent_x + 1]
}

#sum(d[ident_dhis2_control==F & bookyear==2018]$nbcevent_x,na.rm=T)

#PPC
cat("\nNumPPC\n")
nrow(d[ident_dhis2_control==F &ident_dhis2_ppc==T& bookyear==2018])
cat("\nNumPPCvisits\n")
vars <- names(d)[stringr::str_detect(names(d),"^ppcevent_[0-9]*")]
d[,ppcevent_x:=0]

for(i in vars){
  d[!is.na(get(i)), ppcevent_x:=ppcevent_x + 1]
}

#sum(d[ident_dhis2_control==F & bookyear==2018]$ppcevent_x,na.rm=T)

tab <- d[ident_dhis2_control==FALSE,
                      .(
                         Bookings=sum(ident_dhis2_booking==TRUE, na.rm=TRUE),
                         ANCVisits= sum(anevent_x, na.rm=TRUE),
                         PPCVisits=sum(ppcevent_x, na.rm=TRUE)
                         
                        ),keyby=.(bookyear)
         
         
         ]

openxlsx::write.xlsx(tab,
                     file.path(FOLDER_DATA_RESULTS,
                               "buthaina",
                               sprintf("%s_NumANCPPCVisits.xlsx",lubridate::today())))


