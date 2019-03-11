ChiSqTestForMonthlyAvicennaMatching <- function(
  trialArmA,
  trialArmB,
  is_Avicenna_abb_amd_A,
  is_Avicenna_abb_amd_B
){
  
  x <- matrix(c(
    sum(trialArmA)-sum(is_Avicenna_abb_amd_A),
    sum(trialArmB)-sum(is_Avicenna_abb_amd_B),
    sum(is_Avicenna_abb_amd_A),
    sum(is_Avicenna_abb_amd_B)
  ),ncol=2)
  
  returnValue <- 99
  try(returnValue <- chisq.test(x)$p.value, TRUE)
  
  return(returnValue)
  
}

BASE_LINE_STATISTICAL_ANALYSIS <- function(d){

  bookings <- d[bookdate>=as.Date("2017-01-15") & bookdate<=as.Date("2017-09-15"),
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  
  print(bookings)
  bookings[,cohort:="Bookings"]
  print(bookings)

  
  Trial1 <- d[ident_TRIAL_1==TRUE,
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  Trial1[,cohort:="Trial 1"]
  print(Trial1)    
  
  EPD <- d[ident_TRIAL_1==TRUE & 
         isExpectedToHaveDelivered==TRUE,
              .(
                numWomen=.N
              ),
              by=.(
                ident_dhis2_control
              )]
  EPD[,cohort:="Expected to be delivered"]
  print(EPD)    
  
  
  MATCH <- d[ident_TRIAL_1==TRUE & 
             isExpectedToHaveDelivered==TRUE &
               !is.na(ident_avic_any),
           .(
             numWomen=.N
           ),
           by=.(
             ident_dhis2_control
           )]
  MATCH[,cohort:="Matched with Avicenna"]
  print(MATCH)  
  
  
  MATCH_gov <- d[ident_TRIAL_1==TRUE & 
                       isExpectedToHaveDelivered==TRUE &
                        is.na(ident_avic_any) &
                        is.na(ident_hbo) &
                       !is.na(ident_dhis2_dhis2hbo),
                     .(
                       numWomen=.N
                     ),
                     by=.(
                       ident_dhis2_control
                     )]
  MATCH_gov[,cohort:="Matched with Governmental"]
  print(MATCH_gov)
  
  MATCH_priv <- d[ident_TRIAL_1==TRUE & 
                   isExpectedToHaveDelivered==TRUE &
                   is.na(ident_avic_any) &
                   is.na(ident_dhis2_dhis2hbo) &
                   !is.na(ident_hbo),
                 .(
                   numWomen=.N
                 ),
                 by=.(
                   ident_dhis2_control
                 )]
  MATCH_priv[,cohort:="Matched with Private"]
  print(MATCH_priv)
  
  MATCH_excel <- d[ident_TRIAL_1==TRUE & 
                    isExpectedToHaveDelivered==TRUE &
                    is.na(ident_avic_any) &
                    is.na(ident_dhis2_dhis2hbo) &
                    is.na(ident_hbo) &
                    !is.na(ident_paperhbo),
                  .(
                    numWomen=.N
                  ),
                  by=.(
                    ident_dhis2_control
                  )]
  MATCH_excel[,cohort:="Matched with Excel"]
  print(MATCH_excel)
  
  
  
plotData <- rbind(bookings,
                    Trial1,
                    EPD,
                    MATCH,
                    MATCH_gov,
                    MATCH_priv,
                    MATCH_excel
                  )
  
  print(plotData)
  
  # turn cohort into a factor, with levels in the right order
  plotData[,cohort:=factor(cohort,
                           levels=c(
                             "Bookings",
                             "Trial 1",
                             "Expected to be delivered",
                             "Matched with Avicenna",
                             "Matched with Governmental",
                             "Matched with Private",
                             "Matched with Excel"
                           ))]
  
  plotData[,intOrControl:="Intervention"]
  plotData[ident_dhis2_control==TRUE,intOrControl:="Control"]
  
  print(plotData)
  
  setorder(plotData,cohort,-intOrControl)
  plotData[,cumulative_y:=cumsum(numWomen)-numWomen/2,by=cohort]
  
  #making the cohort names into seperate lines so when they are printed on the graph 
  #they will look nice
  levels(plotData$cohort) <- gsub(" ", "\n", levels(plotData$cohort))
  
  #need to fix label inside the the added matched cases
  
p <- ggplot(plotData)
p <- p + geom_bar(mapping=aes(x=cohort,y=numWomen,fill=intOrControl),stat="identity",colour="black")
p <- p + geom_text(mapping=aes(x=cohort,y=cumulative_y,label=numWomen))
p <- p + theme_gray(base_size = 16)
p <- p + scale_fill_brewer("",palette="BuPu")
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Number of women in 2017")
p <- p + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                          size=5, angle=0))
p <- p + labs(title="Trial 1")
p <- p + labs(caption=sprintf("Date of data extraction: %s",CLINIC_CONTROL_DATE))
p
ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "random",
  "baselinecohort.png"),
       plot=p,
       height=210,
       width=297,
       units="mm")
  
  
  ######## 

####Denominators
#stringr::str_subset(names(d),"expecte")

d[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),formatC(lubridate::month(bookdate),width=2,flag="0"))]

res <- d[,.(
  total=.N,
  mahimasClinics=sum(ident_TRIAL_1,na.rm=T),
  missing_ident_expected_delivered=sum(ident_TRIAL_1==T &
                                        (ident_expected_delivered==F|
                                           is.na(ident_expected_delivered))),
                                       
  mahimasClinicsAndExpctdtoHaveDeliv=sum(ident_dhis2_booking==1 &
                                           ident_TRIAL_1 & 
                                           ident_expected_delivered, na.rm=T),
  trialArmA=sum(ident_TRIAL_1==T &ident_dhis2_control==T, na.rm=T),
  trialArmB=sum(ident_TRIAL_1==T &ident_dhis2_control==F, na.rm=T),
  is_Avicenna_abb_amd=sum(
    ident_TRIAL_1==T & 
    ident_avic_abb==T & 
    ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb=sum( 
    ident_TRIAL_1==T &
    ident_avic_abb==T, na.rm=T),
  
  is_Avicenna_amd=sum( 
    ident_TRIAL_1==T &
    ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb_amd_A=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==T &
      ident_avic_abb==T & 
      ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb_amd_B=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==F &
      ident_avic_abb==T & 
      ident_avic_amd==T, na.rm=T),
  
  is_paperhbo_A=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==T &
      ident_paperhbo, na.rm=T),
  
  is_paperhbo_B=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==F &
      ident_paperhbo==T, na.rm=T),
  
  is_privateonsystem_A=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==F &
      ident_dhis2_dhis2hbo==T, na.rm=T),
  
  is_privateonsystem__B=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==T &
      ident_dhis2_dhis2hbo==T, na.rm=T),
  
  is_govtonsystem_A=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==T &
      ident_hbo==T, na.rm=T),
  
  is_govtonsystem_B=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==F &
      ident_hbo==F, na.rm=T)
  
  
  
  ),by=.(
  bookyearmonth
)]
setorder(res,bookyearmonth)
print(res)


res[,propAvicenna:=is_Avicenna_abb_amd/mahimasClinics]
res[,propAvicennaA:=is_Avicenna_abb_amd_A/trialArmA]
res[,propAvicennaB:=is_Avicenna_abb_amd_B/trialArmB]


res[,monthlyAviccenaPvalue:=
      ChiSqTestForMonthlyAvicennaMatching(
        trialArmA=trialArmA,
        trialArmB=trialArmB,
        is_Avicenna_abb_amd_A=is_Avicenna_abb_amd_A,
        is_Avicenna_abb_amd_B=is_Avicenna_abb_amd_B),
    by=bookyearmonth]

#### OVERALL PVALUE

# number control
sum(res$trialArmA)

# number intervention
sum(res$trialArmB)

# number control matched
sum(res$is_Avicenna_abb_amd_A)

# number intervention
sum(res$is_Avicenna_abb_amd_B)

x <- matrix(c(
  sum(res$trialArmA)-sum(res$is_Avicenna_abb_amd_A),
  sum(res$trialArmB)-sum(res$is_Avicenna_abb_amd_B),
  sum(res$is_Avicenna_abb_amd_A),
  sum(res$is_Avicenna_abb_amd_B)
),ncol=2)

(overallPvalue <- chisq.test(x)$p.value)
res[,totalAviccenaPvalue:=overallPvalue]


openxlsx::write.xlsx(res, file.path(FOLDER_DROPBOX_RESULTS,
                                    "mahima",
                                    "random",
                                    "DENOMINATORS_r.xlsx"))

}

IndicatorsOsloANCVisits <- function(d,weeks,variableOfInterestName,variableOfInterestPattern,zeroIsMissing=FALSE){
  d[,angestage_0:=bookgestage]
  d[,anbpdiast_0:=bookbpdiast]
  d[,anbpsyst_0:=bookbpsyst]
  
  # pull out a list of all of the angestage variables
  listOfGestAgeVars <- names(d)[stringr::str_detect(names(d),"^angestage")]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, "angestage",variableOfInterestPattern)
  
  
  
  #d[!is.na(ident_dhis2_booking),custo_anvist_15_17:=FALSE]
  #d[!is.na(custo_anvist_15_17) & angestage_0 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_1 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_2 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_3 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_4 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_5 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_6 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_7 %in% c(15:17),custo_anvist_15_17:=TRUE]
  #d[!is.na(custo_anvist_15_17) & angestage_8 %in% c(15:17),custo_anvist_15_17:=TRUE]
  # look below, this is the same as the loop underneath
  
  for(i in 1:length(weeks)){
    # name of new variable
    var <- sprintf("custo_%s_%s",variableOfInterestName,names(weeks)[i])
    # initialize all as FALSE if has booking variable
    d[!is.na(ident_dhis2_booking),(var):=FALSE]
    
    # loop through the "gestage"/"bp" variables
    for(j in 1:length(listOfGestAgeVars)){
      gestageVar <- listOfGestAgeVars[j]
      interestVar <- listOfInterestVars[j]
      if(zeroIsMissing){
        d[!is.na(get(var)) & get(gestageVar) %in% weeks[[i]] & !is.na(get(interestVar)) & get(interestVar)!=0, (var):=TRUE]
      } else {
        d[!is.na(get(var)) & get(gestageVar) %in% weeks[[i]] & !is.na(get(interestVar)), (var):=TRUE]
      }
    }
  }
  d[,angestage_0:=NULL]
  d[,anbpdiast_0:=NULL]
  d[,anbpsyst_0:=NULL]
}

#this one just makes the variables
# it doesnt do any analyses at all
IndicatorsOsloGenerate <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # pull out the first booking date, and use it as angestage_0
  # gen angestage_0 = bookgestage
  weeks <- list(
    "00_07"=c(0:7),
    "08_12"=c(8:12),
    "13_14"=c(13:14),
    "15_17"=c(15:17),
    "18_22"=c(18:22),
    "23_23"=c(23:23),
    "24_28"=c(24:28),
    "29_30"=c(29:30),
    "31_33"=c(31:33),
    "34_38"=c(34:38),
    "39_99"=c(39:99)
  )
  
  IndicatorsOsloANCVisits(d=d,
                          weeks=weeks,
                          variableOfInterestName="anvisit",
                          variableOfInterestPattern="angestage")
  
  IndicatorsOsloANCVisits(d=d,
                          weeks=weeks,
                          variableOfInterestName="anbpsyst",
                          variableOfInterestPattern="anbpsyst",
                          zeroIsMissing=TRUE)
  
  IndicatorsOsloANCVisits(d=d,
                          weeks=weeks,
                          variableOfInterestName="anbpdiast",
                          variableOfInterestPattern="anbpdiast",
                          zeroIsMissing=TRUE)
  
  
  # determine booking week group
  d[!is.na(ident_dhis2_booking),custo_bookgestagecat:="WAITING TO BE ASSIGNED"]
  #d[!is.na(custo_bookgestagecat) & bookgestage %in% c(0:14),custo_bookgestagecat:="0-14"]
  #d[!is.na(custo_bookgestagecat) & bookgestage %in% c(15:17),custo_bookgestagecat:="15-17"]
  
  for(i in 1:length(weeks)){
    d[!is.na(custo_bookgestagecat) & bookgestage %in% weeks[[i]],custo_bookgestagecat:=names(weeks)[[i]]]
  }
  
  xtabs(~d$custo_bookgestagecat)
  
  d[custo_bookgestagecat %in% c("00_07","08_12","13_14"),custo_anvisit_timely_by_bookgestage:=FALSE]
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
  for(i in c(
    "15_17",
    "18_22",
    "24_28",
    "31_33",
    "34_38"
  )){
    variableVisit <- sprintf("custo_anvisit_%s",i)
    variableDiast <- sprintf("custo_anbpdiast_%s",i)
    variableSyst <- sprintf("custo_anbpsyst_%s",i)
    
    variableResult <- sprintf("custo_anvisit_with_bp_%s",i)
    
    d[get(variableVisit)==TRUE,(variableResult):=FALSE]
    d[get(variableVisit)==TRUE & 
        get(variableDiast)==TRUE &
        get(variableSyst),(variableResult):=TRUE]
    
    print(xtabs(~d[[variableResult]]))
  }
  
}

IndicatorsOsloRandom <- function(d){
  resPalestine <- d[ident_expected_delivered==TRUE,.(
    numerator=sum(custo_anvisit_timely_by_bookgestage,na.rm=T),
    denominator=sum(!is.na(custo_anvisit_timely_by_bookgestage)),
    TOTALNUMOFPEOPLEINCAT=.N
  ),by=.(
    custo_bookgestagecat
  )]
  setorder(resPalestine,custo_bookgestagecat)
  
  resPalestine[,bookorgdistrict:="0Palestine"]
  
  resDistrict <- d[ident_expected_delivered==TRUE,.(
    numerator=sum(custo_anvisit_timely_by_bookgestage,na.rm=T),
    denominator=sum(!is.na(custo_anvisit_timely_by_bookgestage)),
    TOTALNUMOFPEOPLEINCAT=.N
  ),by=.(
    custo_bookgestagecat, bookorgdistrict
  )]
  
  # row bind (put the two data sets on top of each other)
  res <- rbind(resPalestine,resDistrict)
  
  
  
  setcolorder(res, c("bookorgdistrict",
                     "custo_bookgestagecat",
                     "numerator" ,
                     "denominator",
                     "TOTALNUMOFPEOPLEINCAT"))
  
  
  setorder(res,bookorgdistrict,custo_bookgestagecat)
  res <- res[!is.na(bookorgdistrict)]
  
  openxlsx::write.xlsx(res, 
                       file.path(FOLDER_DROPBOX_RESULTS,
                                 "mahima",
                                 "random",
                                 sprintf("%s_anvisit_timely_by_bookgestage.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
  # women who did booking anc and making ppc
  
  resPalestine <- d[ident_expected_delivered==TRUE,
                    .(
                      numerator=sum(ident_dhis2_ppc,na.rm=T),
                      denominator=sum(ident_dhis2_booking,na.rm=T)
                    ),by=
                      .(
                        
                      )]
  
  resPalestine
  
  
  resDistrict<- d[ident_expected_delivered==TRUE,
                  .(
                    numerator=sum(ident_dhis2_ppc,na.rm=T),
                    denominator=sum(ident_dhis2_booking,na.rm=T)
                    
                  ),by=
                    .(
                      bookorgdistrict                        
                    )]
  
  resDistrict
  resPalestine[,bookorgdistrict:="0Palestine"]
  res <- rbind(resPalestine,resDistrict)
  res
  
  openxlsx::write.xlsx(res, 
                       file.path(FOLDER_DROPBOX_RESULTS,
                                 "mahima",
                                 "random",
                                 sprintf("%s_ANC_with_PPC.xlsx",CLINIC_INTERVENTION_DATE)))
  
  openxlsx::write.xlsx(d[is.na(bookorgdistrict)], 
                       file.path(FOLDER_DATA_CLEAN,
                                 sprintf("%s_missing_bookorgdis.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
  
  
  
  names(d)[stringr::str_detect(names(d),"^cust")]
  
  
  #### Blood Pressure and ANC
  
  resPalestine <- d[ident_expected_delivered==TRUE,
                    .(
                      numerator_15_17=sum(custo_anvisit_with_bp_15_17,na.rm=T),
                      denominator_15_17=sum(!is.na(custo_anvisit_with_bp_15_17)),
                      
                      numerator_18_22=sum(custo_anvisit_with_bp_18_22,na.rm=T),
                      denominator_18_22=sum(!is.na(custo_anvisit_with_bp_18_22)),
                      
                      numerator_24_28=sum(custo_anvisit_with_bp_24_28,na.rm=T),
                      denominator_24_28=sum(!is.na(custo_anvisit_with_bp_24_28)),
                      
                      numerator_31_33=sum(custo_anvisit_with_bp_31_33,na.rm=T),
                      denominator_15_17=sum(!is.na(custo_anvisit_with_bp_31_33)),
                      
                      numerator_34_38=sum(custo_anvisit_with_bp_34_38,na.rm=T),
                      denominator_34_38=sum(!is.na(custo_anvisit_with_bp_34_38))
                    ),by=
                      .(
                        
                      )]
  
  resDistrict <- d[ident_expected_delivered==TRUE,
                   .(
                     numerator_15_17=sum(custo_anvisit_with_bp_15_17,na.rm=T),
                     denominator_15_17=sum(!is.na(custo_anvisit_with_bp_15_17)),
                     
                     numerator_18_22=sum(custo_anvisit_with_bp_18_22,na.rm=T),
                     denominator_18_22=sum(!is.na(custo_anvisit_with_bp_18_22)),
                     
                     numerator_24_28=sum(custo_anvisit_with_bp_24_28,na.rm=T),
                     denominator_24_28=sum(!is.na(custo_anvisit_with_bp_24_28)),
                     
                     numerator_31_33=sum(custo_anvisit_with_bp_31_33,na.rm=T),
                     denominator_15_17=sum(!is.na(custo_anvisit_with_bp_31_33)),
                     
                     numerator_34_38=sum(custo_anvisit_with_bp_34_38,na.rm=T),
                     denominator_34_38=sum(!is.na(custo_anvisit_with_bp_34_38))
                     
                   ),by=
                     .(
                       bookorgdistrict
                     )]
  
  resDistrict
  resPalestine[,bookorgdistrict:="0Palestine"]
  res <- rbind(resPalestine,resDistrict)
  res
  
  openxlsx::write.xlsx(res, 
                       file.path(FOLDER_DROPBOX_RESULTS,
                                 "mahima",
                                 "random",
                                 sprintf("%s_ANC_with_BP.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
  
  
}


Analyse_Mahima_Random <- function(d){
  BASE_LINE_STATISTICAL_ANALYSIS(d)
  IndicatorsOsloRandom(d)
}
