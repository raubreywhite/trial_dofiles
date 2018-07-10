#this one just makes the variables
# it doesnt do any analyses at all
IndicatorsOsloGenerate <- function(d){
  # pull out the first booking date, and use it as angestage_0
  # gen angestage_0 = bookgestage
  d[,angestage_0:=bookgestage]
  
  # pull out a list of all of the angestage variables
  angestage <- names(d)[stringr::str_detect(names(d),"^angestage")]
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
  
  #d[!is.na(ident_dhis2_booking),cust_anvist_15_17:=FALSE]
  #d[!is.na(cust_anvist_15_17) & angestage_0 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_1 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_2 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_3 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_4 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_5 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_6 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_7 %in% c(15:17),cust_anvist_15_17:=TRUE]
  #d[!is.na(cust_anvist_15_17) & angestage_8 %in% c(15:17),cust_anvist_15_17:=TRUE]
  # look below, this is the same as the loop underneath
  
  for(i in 1:length(weeks)){
    # name of new variable
    var <- sprintf("cust_anvisit_%s",names(weeks)[i])
    # initialize all as FALSE if has booking variable
    d[!is.na(ident_dhis2_booking),(var):=FALSE]
    
    # loop through the gestage variables
    for(j in angestage){
      d[!is.na(get(var)) & get(j) %in% weeks[[i]], (var):=TRUE]
    }
  }
  
  sum(!is.na(d$ident_dhis2_booking))
  xtabs(~d$cust_anvisit_15_17)
  xtabs(~d$cust_anvisit_18_22)
  xtabs(~d$cust_anvisit_39_99)
  
  d[,angestage_0:=NULL]
  
  # determine booking week group
  d[!is.na(ident_dhis2_booking),cust_bookgestagecat:="WAITING TO BE ASSIGNED"]
  #d[!is.na(cust_bookgestagecat) & bookgestage %in% c(0:14),cust_bookgestagecat:="0-14"]
  #d[!is.na(cust_bookgestagecat) & bookgestage %in% c(15:17),cust_bookgestagecat:="15-17"]
  
  for(i in 1:length(weeks)){
    d[!is.na(cust_bookgestagecat) & bookgestage %in% weeks[[i]],cust_bookgestagecat:=names(weeks)[[i]]]
  }
  
  xtabs(~d$cust_bookgestagecat)
  
  d[cust_bookgestagecat=="0_14",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="0_14" & 
      cust_anvisit_15_17==TRUE & 
      cust_anvisit_18_22==TRUE & 
      cust_anvisit_24_28==TRUE & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="15_17",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="15_17" & 
      cust_anvisit_18_22==TRUE & 
      cust_anvisit_24_28==TRUE & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="18_22",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="18_22" & 
      cust_anvisit_24_28==TRUE & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="23_23",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="23_23" & 
      cust_anvisit_24_28==TRUE & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="24_28",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="24_28" & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="29_30",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="29_30" & 
      cust_anvisit_31_33==TRUE & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="31_33",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="31_33" & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  d[cust_bookgestagecat=="34_38",cust_anvisit_timely_by_bookgestage:=FALSE]
  d[cust_bookgestagecat=="34_38" & 
      cust_anvisit_34_38==TRUE,
    cust_anvisit_timely_by_bookgestage:=TRUE]
  
  xtabs(~d$cust_anvisit_timely_by_bookgestage)
  
}

# this one makes nice excel tables containing
# the summary indicators (e.g. proportions)
IndicatorsOsloAnalyse <- function(d){
  resPalestine <- d[ident_expected_delivered==TRUE,.(
    numerator=sum(cust_anvisit_timely_by_bookgestage,na.rm=T),
    denominator=sum(!is.na(cust_anvisit_timely_by_bookgestage)),
    TOTALNUMOFPEOPLEINCAT=.N
  ),by=.(
    cust_bookgestagecat
  )]
  setorder(resPalestine,cust_bookgestagecat)
  
  resPalestine[,bookorgdistrict:="0Palestine"]
  
  resDistrict <- d[ident_expected_delivered==TRUE,.(
    numerator=sum(cust_anvisit_timely_by_bookgestage,na.rm=T),
    denominator=sum(!is.na(cust_anvisit_timely_by_bookgestage)),
    TOTALNUMOFPEOPLEINCAT=.N
  ),by=.(
    cust_bookgestagecat, bookorgdistrict
  )]

  # row bind (put the two data sets on top of each other)
  res <- rbind(resPalestine,resDistrict)
  
  

  setcolorder(res, c("bookorgdistrict",
                     "cust_bookgestagecat",
                     "numerator" ,
                     "denominator"))
  
    
  setorder(res,bookorgdistrict,cust_bookgestagecat)
  res <- res[!is.na(bookorgdistrict)]
  
  openxlsx::write.xlsx(res, 
                       file.path(FOLDER_DROPBOX_RESULTS,
                                 sprintf("%s_Indicators_for_mahima.xlsx",CLINIC_INTERVENTION_DATE)))
  
  
}





