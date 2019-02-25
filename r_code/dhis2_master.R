CleanOrgName <- function(data,nameToReplace="bookorgname"){
  oldName <- nameToReplace
  newName <- sprintf("NEW_%s",nameToReplace)
  
  sData <- readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx")
  setDT(sData)
  setnames(sData,"bookorgname",oldName)
  setnames(sData,"NEW_bookorgname",newName)
  sData[is.na(get(newName)),(newName):=get(oldName)]
  
  toChangeToBool <- names(sData)[stringr::str_detect(names(sData),"^ident")]
  for(i in toChangeToBool) sData[[i]] <- !is.na(sData[[i]])
  
  missingNames <- data.table("bookorgname"=unique(data[[oldName]])[!unique(data[[oldName]]) %in% sData[[oldName]]])
  openxlsx::write.xlsx(missingNames,
                       sprintf("../data_raw/structural_data/to_be_processed_%s.xlsx",nameToReplace))
  
  
  sData <- sData[,c(oldName,newName),with=F]
  
  data[,(oldName):=unlist(ExtractOnlyEnglishLetters(get(oldName)))]
  
  nrow(data)
  data <- merge(data,sData,by=c(oldName),all.x=T)
  nrow(data)
  
  data[,(oldName):=get(newName)]
  data[,(newName):=NULL]
  
  return(data)
}

DHIS2_Master <- function(
  keepDoubleBookings=FALSE,
  includePPC=TRUE,
  minBookDate="2001-01-01",
  maxBookDate="2100-01-01",
  IS_GAZA=FALSE
  ){
  ####
  # DHIS2 BOOKING
  print("CLEANING DHIS2 BOOKING")
  if(keepDoubleBookings){
    data_DHIS2_Booking <-  DHIS2_BookingVisit(isControl=TRUE,keepDoubleBookings=TRUE)
  } else {
    data_DHIS2_Booking <- int <- DHIS2_BookingVisit(isControl=FALSE,IS_GAZA=IS_GAZA)
    if(!IS_GAZA){
      con <- DHIS2_BookingVisit(isControl=TRUE)
      
      names(con)[!names(con) %in% names(int)]
      names(int)[!names(int) %in% names(con)]
      con[,programinstancex:=NULL]
      con[,programinstancey:=NULL]
      
      data_DHIS2_Booking <- rbind(
        con,
        int, fill=T)
    }
  }
  # restrict bookdate
  data_DHIS2_Booking <- data_DHIS2_Booking[bookdate>=minBookDate & bookdate<=maxBookDate]
  
  # here we load in "bookorgname" and give a bunch of indicators
  # i.e. trial 1 indicators
  sData <- readxl::read_excel(file.path(FOLDER_DATA_RAW,"structural_data/bookorgname.xlsx"))
  setDT(sData)
  sData[is.na(NEW_bookorgname),NEW_bookorgname:=bookorgname]
  
  toChangeToBool <- names(sData)[stringr::str_detect(names(sData),"^ident")]
  for(i in toChangeToBool) sData[[i]] <- !is.na(sData[[i]])
  
  # create a new sdata just for PPC
  # so that we can get ppcorgdistrict
  pData <- sData[,c("bookorgname","bookorgdistrict")]
  setnames(pData,c("ppcorgname","ppcorgdistrict"))
  
  nData <- sData[,c("bookorgname","bookorgdistrict")]
  setnames(nData,c("nbcorgname","nbcorgdistrict"))
  
  # identify new bookorgnames!!
  unique(data_DHIS2_Booking$bookorgname)
  
  missingNames <- data.table("bookorgname"=unique(data_DHIS2_Booking$bookorgname)[!unique(data_DHIS2_Booking$bookorgname) %in% sData$bookorgname])
  openxlsx::write.xlsx(missingNames,file.path(FOLDER_DATA_RAW,"structural_data/to_be_processed_bookorgname.xlsx"))
  
  nrow(data_DHIS2_Booking)
  data_DHIS2_Booking <- merge(data_DHIS2_Booking,sData,by=c("bookorgname"),all.x=T)
  nrow(data_DHIS2_Booking)
  
  # changing the structural indicators to include additional data
  # e.g. trial 1 needs to also include dates
  data_DHIS2_Booking[,ident_TRIAL_1_clinics:=ident_TRIAL_1]
  
  # ident_TRIAL_1 is only for 2017-01-15 to 2017-09-15
  data_DHIS2_Booking[
    (bookdate<as.Date("2017-01-15") | bookdate>as.Date("2017-09-15")) &
      !is.na(ident_TRIAL_1),ident_TRIAL_1:=FALSE]
  
  data_DHIS2_Booking <- data_DHIS2_Booking[is.na(bookorgdistrict) | bookorgdistrict!="TEST"]
  
  ## structural indicators end
  
  data_DHIS2_Booking[,bookorgname:=NEW_bookorgname]
  data_DHIS2_Booking[,NEW_bookorgname:=NULL]
  
  # we create booknum based on motherIDNO, because women can have multiple uniqueIDs
  #setorder(data_DHIS2_Booking,uniqueid,bookdate)
  #data_DHIS2_Booking[,booknum:=1:.N,by=uniqueid]
  setorder(data_DHIS2_Booking,demoidnumber,bookdate)
  data_DHIS2_Booking[,booknum:=1:.N,by=demoidnumber]
  xtabs(~data_DHIS2_Booking$booknum)
  
  earlyData <- unique(data_DHIS2_Booking[,c("uniqueid","bookdate","booknum")])
  booklmp <- unique(data_DHIS2_Booking[,c("uniqueid","bookevent","booknum","booklmp")])
  data_ident_dhis2_booking <- unique(data_DHIS2_Booking[,c("uniqueid","ident_dhis2_booking")])
  
  ####
  # DHIS2 ANTENATAL
  print("CLEANING DHIS2 ANTENATAL")
  data_DHIS2_Antenatal <- int <- DHIS2_Antenatal(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  if(!IS_GAZA){
    con <- DHIS2_Antenatal(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
    data_DHIS2_Antenatal <- rbind(
      con,
      int)
  }
  nrow(data_DHIS2_Antenatal)
  
  ####
  # DHIS2 LAB
  print("CLEANING DHIS2 LAB")
  data_DHIS2_Lab <- int <- DHIS2_Lab(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  if(!IS_GAZA){
    con <- DHIS2_Lab(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
    data_DHIS2_Lab <- rbind(
      con,
      int)
  }
  nrow(data_DHIS2_Lab)
  
  ####
  # DHIS2 ULTRASOUND
  print("CLEANING DHIS2 ULTRASOUND")
  data_DHIS2_Ultrasound <- int <- DHIS2_Ultrasound(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  if(!IS_GAZA){
    con <- DHIS2_Ultrasound(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
    data_DHIS2_Ultrasound <- rbind(
      con,
      int)
  }
  nrow(data_DHIS2_Ultrasound)
  
  ####
  # DHIS2 RISK
  print("CLEANING DHIS2 RISK")
  data_DHIS2_RiskFactors <- DHIS2_RiskFactors(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_RiskFactors)
  
  ####
  # DHIS2 MANAGEMENTS
  print("CLEANING DHIS2 MANAGEMENT")
  data_DHIS2_Management <- int <- DHIS2_Management(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  if(!IS_GAZA){
    con <- DHIS2_Management(isControl=TRUE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
    data_DHIS2_Management <- rbind(
       con,
       int
    )
  }
  nrow(data_DHIS2_Management)
  
  ####
  # DHIS2 RISK
  print("CLEANING DHIS2 NNCRISK")
  data_DHIS2_NNCRiskFactors <- DHIS2_NNCRiskFactors(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_NNCRiskFactors)
  
  ####
  # DHIS2 MANAGEMENTS
  print("CLEANING DHIS2 NBMANAGEMENT")
  data_DHIS2_NBManagement <- DHIS2_NBManagement(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_NBManagement)
  
  ####
  # PREVIOUS PREGNANCIES
  print("CLEANING DHIS2 PREVIOUS PREGNANCY")
  data_DHIS2_PreviousPregnancies <- int <- DHIS2_PreviousPregnancies(isControl=FALSE, earlyData=earlyData, booklmp=booklmp, IS_GAZA=IS_GAZA)
  if(!IS_GAZA){
    con <- DHIS2_PreviousPregnancies(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
    data_DHIS2_PreviousPregnancies <- rbind(
      con,
      int)
  }
  nrow(data_DHIS2_PreviousPregnancies)
  
  ####
  # HOSPITAL BIRTH OUTCOMES
  print("CLEANING DHIS2 HOSPITAL BIRTH OUTCOMES")
  if(!IS_GAZA) data_DHIS2_HospitalBirthOutcomes <- DHIS2_HospitalBirthOutcomes(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  
  ####
  #
  print("CLINICAL CURRENT PREG OUTCOMES")
  data_DHIS2_CurrentPregnancyOutcomes <- DHIS2_CurrentPregnancyOutcomes(isControl=F,
                                                                        earlyData = earlyData,
                                                                        booklmp = booklmp,
                                                                        data_ident_dhis2_booking = data_ident_dhis2_booking,
                                                                        IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_CurrentPregnancyOutcomes)
  ####
  #
  print("CLINICAL CURRENT PREG OUTCOMES")
  data_DHIS2_PregnancyClosingNotes <- DHIS2_PregnancyClosingNotes(isControl=F,
                                                                        earlyData = earlyData,
                                                                        booklmp = booklmp,
                                                                  IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_PregnancyClosingNotes)
  ####
  #
  print("POSTPARTUM CARE")
  data_DHIS2_PostPartumCare <- DHIS2_DHIS2_PostPartumCare(isControl=F,
                                                          earlyData = earlyData,
                                                          booklmp = booklmp,
                                                          IS_GAZA=IS_GAZA)
  nrow(data_DHIS2_PostPartumCare)
  if(!keepDoubleBookings){
    nrow(data_DHIS2_PostPartumCare)
    data_DHIS2_PostPartumCare <- merge(x=data_DHIS2_PostPartumCare,y=pData,
                                       by="ppcorgname",all.x=T)
    nrow(data_DHIS2_PostPartumCare)
    sum(is.na(data_DHIS2_PostPartumCare$ppcorgdistrict))
    sum(is.na(data_DHIS2_PostPartumCare$ppcorgname))
    # print the ppcorgnames for people who are missing ppcorgdistrict
    # give me a vector
    unique(data_DHIS2_PostPartumCare[is.na(ppcorgdistrict)]$ppcorgname)
    # give it to me in 'data.table' form
    data_DHIS2_PostPartumCare[is.na(ppcorgdistrict),"ppcorgname"]
  }
  
  ####
  #
  print("NBC CARE")
  data_DHIS2_NewbornCare <- DHIS2_NewbornCare(isControl=F,
                                                          earlyData = earlyData,
                                                          booklmp = booklmp,
                                              IS_GAZA=IS_GAZA)
  if(!keepDoubleBookings & IS_GAZA==FALSE){
    nrow(data_DHIS2_NewbornCare)
    data_DHIS2_NewbornCare <- merge(x=data_DHIS2_NewbornCare,y=nData,
                                       by="nbcorgname",all.x=T)
  }
  
  
  
  ### 
  # start reshaping to wide
  # and merging
  print("RESHAPE TO WIDE AND MERGE DHIS2 ANTENATAL")
  d <- ReshapeToWideAndMerge(
    base=data_DHIS2_Booking,
    additional=data_DHIS2_Antenatal,
    valueVarsRegex="^an",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_an"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 LAB")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_Lab,
    valueVarsRegex="^lab",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_lab"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 ULTRASOUND")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_Ultrasound,
    valueVarsRegex="^us",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_us"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 RISK FACTORS")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_RiskFactors,
    valueVarsRegex="^risk",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_risk"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 MANAGEMENT")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_Management,
    valueVarsRegex="^man",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_man"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 NNCRISKFACTORS")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_NNCRiskFactors,
    valueVarsRegex="^nncrisk",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_nncrisk"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 MANAGEMENT")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_NBManagement,
    valueVarsRegex="^nbman",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_nbman"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 PREVIOUS PREGNANCIES")
  d <- ReshapeToWideAndMerge(
   base=d,
   additional=data_DHIS2_PreviousPregnancies,
   valueVarsRegex="^prev",
   dcastFormula="uniqueid~eventnum",
   mergeVars=c("uniqueid"),
   identName="ident_dhis2_prev"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
   
  if(!IS_GAZA){
    print("RESHAPE TO WIDE AND MERGE DHIS2 HOSPITAL BIRTH OUTCOMES")
    d <- ReshapeToWideAndMerge(
      base=d,
      additional=data_DHIS2_HospitalBirthOutcomes,
      valueVarsRegex="^dhis2hbo",
      dcastFormula="uniqueid+bookevent+booknum~eventnum",
      mergeVars=c("uniqueid","bookevent","booknum"),
      identName="ident_dhis2_dhis2hbo"
    )
    length(d$bookevent)
    length(unique(d$bookevent))
    nrow(data_DHIS2_Booking)
    nrow(d)
    ncol(d)
  }
  
  print("RESHAPE TO WIDE AND MERGE data_DHIS2_CurrentPregnancyOutcomes")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_CurrentPregnancyOutcomes,
    valueVarsRegex="^cpo",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_cpo"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE data_DHIS2_PregnancyClosingNotes")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_PregnancyClosingNotes,
    valueVarsRegex="^pcn",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_pcn"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE data_DHIS2_PostPartumCare")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_PostPartumCare,
    valueVarsRegex="^ppc",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_ppc"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE data_DHIS2_NewbornCare")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_NewbornCare,
    valueVarsRegex="^nbc",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_nbc"
  )
  length(d$bookevent)
  length(unique(d$bookevent))
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  
  
  
  
  d[,calc_expected_due_delivery:=booklmp+280]
  d[is.na(calc_expected_due_delivery),
    calc_expected_due_delivery:=usdate_1-usgestage_1*7+280]
  d[,isExpectedToHaveDelivered:=expecteddateofdelivery < min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)]
  xtabs(~d$isExpectedToHaveDelivered)
  
  setnames(d,"demoidnumber","motheridno")
  
  setorderv(d,cols=c("motheridno","bookdate"))
  d[,motheridbooknum:=1:.N,by=.(motheridno)]
  # the start date of matching with avicenna
  d[,motheridbook_earlyDate:=bookdate]
  # the end date of matching with avicenna
  d[,motheridbook_lateDate:=bookdate+31*10]
  # make sure that the "late date" doesnt overlap with a new pregnancy booking
  d[,temp:=shift(motheridbook_earlyDate,type = "lead"),by=.(motheridno)]
  d[motheridbook_lateDate>temp,motheridbook_lateDate:=temp-1]
  d[,temp:=NULL]
  
  d[,bookyearmonth:=YearMonth(bookdate)]
  d[,bookyear:=lubridate::year(bookdate)]
  d[,bookmonth:=lubridate::month(bookdate)]
  d[,bookmonth:=formatC(bookmonth,flag="0",width=2)]

  d[,bookorgdistricthashed:=openssl::md5(bookorgdistrict)]
  d[,bookorgdistricthashed:=stringr::str_sub(bookorgdistricthashed,1,6)]
  
  #####################
  #####################
  #####################
  
  #######
  
  if(!includePPC){
    d <- d[ident_dhis2_booking==1]
  }
  
  if(!keepDoubleBookings){
    #duplication problems///remove duplicated demographic sheets which deosent have booking or ppc or nbc
    
    duplicated_demographic_cases <- d[ident_dhis2_demo==1 & ident_dhis2_booking==0
        & is.na(ident_dhis2_ppc)& is.na(ident_dhis2_nbc)]
    
    
    openxlsx::write.xlsx(duplicated_demographic_cases,file.path( FOLDER_DATA_RESULTS,
                                       "duplicated demographic cases.xlsx"))
    
    
    d <- d[!(ident_dhis2_demo==1 & ident_dhis2_booking==0
               & is.na(ident_dhis2_ppc)& is.na(ident_dhis2_nbc))]
  }
 
  
  return(d)
}
