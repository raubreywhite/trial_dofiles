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
    "00_14"=c(0:14),
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
  
  d[custo_bookgestagecat=="00_14",custo_anvisit_timely_by_bookgestage:=FALSE]
  d[custo_bookgestagecat=="00_14" & 
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
  
  xtabs(~d$custo_anvisit_timely_by_bookgestage)
  
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
                                 "trial_1",
                                 "random_indicators",
                                 sprintf("%s_anvisit_timely_by_bookgestage.xlsx",CLINIC_INTERVENTION_DATE)))
  
  

  
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
                                 "indicators_for_mahima",
                                 sprintf("%s_ANC_with_PPC.xlsx",CLINIC_INTERVENTION_DATE)))
  
  openxlsx::write.xlsx(d[is.na(bookorgdistrict)], 
                       file.path(FOLDER_DATA_CLEAN,
                                 sprintf("%s_missing_bookorgdis.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
  
  
  
  names(d)[stringr::str_detect(names(d),"^cust")]
  
  
  #### BPand ANC
  
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
                                 "trial_1",
                                 "random_indicators",
                                 sprintf("%s_ANC_with_BP.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
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
    "indicators_for_mahima",
    "entered_and_calculated_gest_ages.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
}

IndicatorsOsloDemographics <- function(d){
  
  
  toAnalyse <- copy(d[ident_TRIAL_1==TRUE])
  toAnalyse[,bookorgdistrict:="Palestine"]
  toAnalyse[,bookorgdistricthashed:="Palestine"]
  toAnalyse <- rbind(toAnalyse,d[ident_TRIAL_1==TRUE])
  
  # we will just switch this one over
  toAnalyse[,aggregationVariable:=bookorgdistricthashed]
  # this is how to run a t-test
  # t.test(OUTCOME ~ GROUPINGVARIABLE, data=DATA)
  # res <- t.test(education~ident_dhis2_control,data=toAnalyse)
  
  ### YEARS OF EDUCATION
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(education~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable
  ]
  pvalueInfo[,pvalue:=sprintf("p=%s",formatC(pvalue,digits=2,format="f"))]
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable))
  p <- p + geom_boxplot(mapping=aes(y=education,fill=ident_dhis2_control))
  p <- p + geom_text(data=pvalueInfo,mapping=aes(y=0,label=pvalue))
  p <- p + scale_fill_brewer("Trial Arm",palette="Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Education differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "trial_1",
    "demographics",
    "education_years.png"),
    plot=p)
  
  ### AGE
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(age~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=age))
  p <- p+ geom_boxplot(mapping = aes(y=age, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "trial_1",
    "demographics",
    "age_years.png"),
    plot=p)
  
  
  ### AGE at marrige
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agemarriage~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agemarriage))
  p <- p+ geom_boxplot(mapping = aes(y=agemarriage, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at marriage differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "trial_1",
    "demographics",
    "agemarriage_years.png"),
    plot=p)
  
  
  ### AGE at first pregnancy
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agepregnancy~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agepregnancy))
  p <- p+ geom_boxplot(mapping = aes(y=agepregnancy, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at first pregnancy differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "trial_1",
    "demographics",
    "agepregnancy_years.png"),
    plot=p)
  
  #income
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(income~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=income))
  p <- p+ geom_boxplot(mapping = aes(y=income, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=-7000,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p+scale_y_continuous("Income")
  p <- p + labs(title="Income differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "trial_1",
    "demographics",
    "income_continuous.png"),
    plot=p)
  
  
  
  
  #d$agemarriage
  #d$age
  #d$agecat
  #d$agepregnancy
  #d$agepregnancycat
  #d$income
  #d$incomecat
  
  #plot(d$age~d$agepregnancy)
  
  
}

# this one makes nice excel tables containing
# the summary indicators (e.g. proportions)
IndicatorsOsloAnalyse <- function(d=NULL){
  #if(!exists("d")) d <- LoadDataFileFromNetwork()[ident_dhis2_booking==1]
  
  IndicatorsOsloRandom(d)
  IndicatorsOsloDemographics(d)
}





