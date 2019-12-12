###Trial 1 Outcomes###

 # defining dataset
smallD <- smallD <- d[bookdate >= "2017-01-15"&
                        bookdate<="2017-09-15" &
                        ident_TRIAL_1==T,]

#changing lastusedd to days
#TO DO: make sure we get the 87 women who are missing an lmp or bookdate
smallD[,usedddays:=lastusedd - as.difftime(280, unit="days")]

smallD[,USorLMPdate:=booklmp]
smallD[!is.na(usedddays),USorLMPdate:=usedddays]
#smallD[is.na(USorLMPdate), USorLMPdate:=]

bookgestagearms<-smallD[,.(ArmA=sum(prettyExposure=="Trial Arm A", na.rm=TRUE),
                          ArmB=sum(prettyExposure=="Trial Arm B", na.rm=TRUE)),
                          
                          keyby=.(bookgestagedays)]

openxlsx::write.xlsx(bookgestagearms, 
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "demographics_and_history",
                       sprintf("%s_bookgestagearms.xlsx",lubridate::today())))


#recalculating bookgestational age into days
smallD[,bookgestagedays:=round(as.numeric(difftime(bookdate,USorLMPdate, units="days")))]

#recalculating angestational age into days
nam <- names(smallD)[stringr::str_detect(names(smallD),"^andate_[0-9]*$")]
num <- stringr::str_replace(nam,"andate_","")
for(i in num){
  print(i)
  smallD[,(sprintf("angestagedays_%s",i)):=round(as.numeric(difftime(
    get(sprintf("andate_%s", i)),
    USorLMPdate,
    units="days")),digits=1)]
}

# TO DO: get distributions of gestational ages for an, lab, us, etc.

#recalculating labgestational age into days
nam <- names(smallD)[stringr::str_detect(names(smallD),"^labdate_[0-9]*$")]
num <- stringr::str_replace(nam,"labdate_","")
for(i in num){
  print(i)
  smallD[,(sprintf("labgestagedays_%s",i)):=round(as.numeric(difftime(
    get(sprintf("labdate_%s", i)),
    USorLMPdate,
    units="days")),digits=1)]
}

#recalculating usgestational age into days
nam <- names(smallD)[stringr::str_detect(names(smallD),"^usdate_[0-9]*$")]
num <- stringr::str_replace(nam,"usdate_","")
for(i in num){
  print(i)
  smallD[,(sprintf("usgestagedays_%s",i)):=round(as.numeric(difftime(
    get(sprintf("usdate_%s", i)),
    USorLMPdate,
    units="days")),digits=1)]
}


#recalculating mangestational age into days
nam <- names(smallD)[stringr::str_detect(names(smallD),"^mandate_[0-9]*$")]
num <- stringr::str_replace(nam,"mandate_","")
for(i in num){
  print(i)
  smallD[,(sprintf("mangestagedays_%s",i)):=round(as.numeric(difftime(
    get(sprintf("mandate_%s", i)),
    USorLMPdate,
    units="days")),digits=1)]
}


#recalculating riskgestational age into days
nam <- names(smallD)[stringr::str_detect(names(smallD),"^riskdate_[0-9]*$")]
num <- stringr::str_replace(nam,"riskdate_","")
for(i in num){
  print(i)
  smallD[,(sprintf("riskgestagedays_%s",i)):=round(as.numeric(difftime(
    get(sprintf("riskdate_%s", i)),
    USorLMPdate,
    units="days")),digits=1)]
}

##making categories

  #booking
  #TO DO: check lower cut off
smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                       breaks=c(-500,0,14,104,119,
                                125,154,167, 196,
                                216,231,244,259),
                       include.lowest=T)]

#15 weeks=105 days
#17 weeks= 119 days
#18 weeks= 126 days
#22 weeks=154 days
#24 weeks= 168 days
#28 weeks=196 days
#31 weeks=217 days
#33 weeks= 231 days
#35 weeks= 245 days
#37 weeks= 259 days

#gestationalage at delivery
nrow(smallD[!is.na(merged_gestagedeliv)])
nrow(smallD[merged_abortion==T])

missinggA<-smallD[is.na(mahima_hospenteredgestage_1),
          
        c("bookdate",
          "booklmp",
          "merged_datedeliv",
          "merged_gestagedeliv",
          "cpodate_1",
          "cpopregoutcome_1", 
          "prettyExposure",
          "USorLMPdate",
          "mahima_gestageatbirthwk_1")]

table<- missinggA[,.(
                      BookdateNa=sum(is.na(bookdate)),
                      MahimagestagebirthNA=sum(is.na(mahima_gestageatbirthwk_1)),
                      USorLMPdateNa=sum(is.na(USorLMPdate)),
                      DoBNa=sum(is.na(merged_datedeliv)),
                     gAdelivNa=sum(is.na(merged_gestagedeliv)),
                      BooklmpNA=sum(is.na(booklmp)),
                      CPOdateNA=sum(is.na(cpodate_1)),
                      CPOpregNA=sum(is.na(cpopregoutcome_1))),
                  keyby=.(prettyExposure)]

#TO DO: figure out why some are missing gAs-either not calculated, not entered, etc
smallD[,USorLMPdateCombogAdays:= as.numeric(difftime(
                                          mahima_dateofbirth_1,
                                          USorLMPdate,
                                          units ="days"))]

smallD[,gAatBirth_cats:=cut((USorLMPdateCombogAdays),
                                  breaks=c(-1000,0,14,104,119,
                                           125,154,167, 196,
                                           216,231,244,259,266,280,294,314,500000),
                                  include.lowest=T)]

#added 37-38, 38-40,40-42, above 45 weeks
#38 weeks=266 days
#40 weeks=280 days
#42 weeks=294 days
#45 weeks=315 days

ANCVisitVariables <- function(smallD,weeks,variableOfInterestName,variableOfInterestPattern,zeroIsMissing=FALSE){
  smallD[,angestagedays_0:=bookgestagedays]
  smallD[,anbpdiast_0:=bookbpdiast]
  smallD[,anbpsyst_0:=bookbpsyst]
  # MAKE BOOK VISIT FOR ANEMIA
  
  # pull out a list of all of the angestage variables
  listOfGestAgeVars <- names(smallD)[stringr::str_detect(names(smallD),"^angestagedays")]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, "angestagedays",variableOfInterestPattern)

  
  for(i in 1:length(weeks)){
    # name of new variable
    var <- sprintf("custo_%s_%s",variableOfInterestName,names(weeks)[i])
    # initialize all as FALSE if has booking variable
    smallD[!is.na(ident_dhis2_booking),(var):=FALSE]
    
    # loop through the "gestage"/"bp" variables
    for(j in 1:length(listOfGestAgeVars)){
      gestageVar <- listOfGestAgeVars[j]
      interestVar <- listOfInterestVars[j]
      if(zeroIsMissing){
        smallD[!is.na(get(var)) & get(gestageVar) %in% weeks[[i]] & !is.na(get(interestVar)) & get(interestVar)!=0, (var):=TRUE]
      } else {
        smallD[!is.na(get(var)) & get(gestageVar) %in% weeks[[i]] & !is.na(get(interestVar)), (var):=TRUE]
      }
    }
  }
  smallD[,angestagedays_0:=NULL]
  smallD[,anbpdiast_0:=NULL]
  smallD[,anbpsyst_0:=NULL]
 
}

#this one just makes the variables
# it doesnt do any analyses at all
ANCvisitIndicators <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  smallD <- d[ident_TRIAL_1==TRUE,]
  

  # pull out the first booking date, and use it as angestage_0
  # gen angestage_0 = bookgestage
  weeks <- list(
    "<0_<0"=c(-500,0),
    "00_02"=c(0:14),
    "03_14"=c(15:104),
    "15_17"=c(105:119),
    "17-18"=c(120:125),
    "18_22"=c(126:154),
    "23_23"=c(155:167),
    "24_28"=c(168:196),
    "29_30"=c(197:210),
    "31_33"=c(217:231),
    "34_34"=c(232:244),
    "35_37"=c(245:259)
  )

  ANCvisitIndicators(d=smallD,
                          weeks=weeks,
                          variableOfInterestName="anvisit",
                          variableOfInterestPattern="angestagedays")
  
  ANCvisitIndicators(d=smallD,
                          weeks=weeks,
                          variableOfInterestName="anbpsyst",
                          variableOfInterestPattern="anbpsyst",
                          zeroIsMissing=TRUE)
  
  ANCvisitIndicators(d=smallD,
                          weeks=weeks,
                          variableOfInterestName="anbpdiast",
                          variableOfInterestPattern="anbpdiast",
                          zeroIsMissing=TRUE)
  
 
  d[!is.na(ident_dhis2_booking),custo_bookgestagedayscat:="WAITING TO BE ASSIGNED"]
  #d[!is.na(custo_bookgestagecat) & bookgestage %in% c(0:14),custo_bookgestagecat:="0-14"]
  #d[!is.na(custo_bookgestagecat) & bookgestage %in% c(15:17),custo_bookgestagecat:="15-17"]
  
  for(i in 1:length(weeks)){
    d[!is.na(custo_bookgestagedayscat) & bookgestagedays %in% weeks[[i]],custo_bookgestagedayscat:=names(weeks)[[i]]]
  }
  
  xtabs(~d$custo_bookgestagedayscat)
  

  
  d[custo_bookgestagedayscat %in% c("00_07","08_12","13_14"),custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat %in% c("00_07","08_12","13_14") & 
      custo_anvisit_15_17==TRUE & 
      custo_anvisit_18_22==TRUE & 
      custo_anvisit_24_28==TRUE & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="15_17",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="15_17" & 
      custo_anvisit_18_22==TRUE & 
      custo_anvisit_24_28==TRUE & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="18_22",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="18_22" & 
      custo_anvisit_24_28==TRUE & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="23_23",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="23_23" & 
      custo_anvisit_24_28==TRUE & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="24_28",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="24_28" & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="29_30",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="29_30" & 
      custo_anvisit_31_33==TRUE & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="31_33",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="31_33" & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="34_38",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="34_38" & 
      custo_anvisit_34_38==TRUE,
    custo_anvisit_timely_by_bookgestage:=TRUE]
  
  ##### BLOOD PRESURE
  d[custo_bookgestagecat %in% c("00_07","08_12","13_14"),custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat %in% c("00_07","08_12","13_14") & 
      (custo_anbpdiast_00_07==TRUE | custo_anbpdiast_08_12==TRUE | custo_anbpdiast_13_14==TRUE) & 
      custo_anbpdiast_15_17==TRUE & 
      custo_anbpdiast_18_22==TRUE & 
      custo_anbpdiast_24_28==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="15_17",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="15_17" & 
      custo_anbpdiast_15_17==TRUE & 
      custo_anbpdiast_18_22==TRUE & 
      custo_anbpdiast_24_28==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="18_22",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="18_22" & 
      custo_anbpdiast_18_22==TRUE & 
      custo_anbpdiast_24_28==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="23_23",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="23_23" & 
      custo_anbpdiast_23_23==TRUE & 
      custo_anbpdiast_24_28==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="24_28",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="24_28" & 
      custo_anbpdiast_24_28==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="29_30",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="29_30" & 
      custo_anbpdiast_29_30==TRUE & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="31_33",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="31_33" & 
      custo_anbpdiast_31_33==TRUE & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
  d[custo_bookgestagecat=="34_38",custo_anbpdiast_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="34_38" & 
      custo_anbpdiast_34_38==TRUE,
    custo_anbpdiast_timely_by_bookgestage:=TRUE]
  
}

sevHTN <- smallD[,.(sevSystolHTNatBirth=sum((as.numeric(merged_bpsyst))>=160, na.rm=TRUE),
                  sevDiastHTNatBirth=sum((as.numeric(merged_bpdiast))>=110, na.rm=TRUE))]

#TO DO: fix blood pressure, merged_bpsyst and merged_bpdiast
#To Do: fix the anc visit stuff





