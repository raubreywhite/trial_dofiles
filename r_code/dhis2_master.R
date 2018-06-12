DHIS2_Master <- function(keepDoubleBookings=FALSE){
  ####
  # DHIS2 BOOKING
  print("CLEANING DHIS2 BOOKING")
  if(keepDoubleBookings){
    data_DHIS2_Booking <-  DHIS2_BookingVisit(isControl=TRUE,keepDoubleBookings=TRUE)
  } else {
    con <- DHIS2_BookingVisit(isControl=TRUE)
    int <- DHIS2_BookingVisit(isControl=FALSE)
    data_DHIS2_Booking <- rbind(
      con,
      int)
  }
  
  # here we load in "bookorgname" and give a bunch of indicators
  # i.e. trial 1 indicators
  sData <- readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx")
  setDT(sData)
  sData[is.na(NEW_bookorgname),NEW_bookorgname:=bookorgname]
  
  toChangeToBool <- names(sData)[stringr::str_detect(names(sData),"^ident")]
  for(i in toChangeToBool) sData[[i]] <- !is.na(sData[[i]])
  
  # identify new bookorgnames!!
  unique(data_DHIS2_Booking$bookorgname)
  
  missingNames <- data.table("bookorgname"=unique(data_DHIS2_Booking$bookorgname)[!unique(data_DHIS2_Booking$bookorgname) %in% sData$bookorgname])
  openxlsx::write.xlsx(missingNames,"../data_raw/structural_data/to_be_processed_bookorgname.xlsx")
  
  #
  
  nrow(data_DHIS2_Booking)
  data_DHIS2_Booking <- merge(data_DHIS2_Booking,sData,by=c("bookorgname"),all.x=T)
  nrow(data_DHIS2_Booking)
  
  # changing the structural indicators to include additional data
  # e.g. trial 1 needs to also include dates
  
  # ident_TRIAL_1 is only for 2017-01-15 to 2017-01-15
  data_DHIS2_Booking[
    (bookdate<as.Date("2017-01-15") | bookdate>as.Date("2018-01-15")) &
      !is.na(ident_TRIAL_1),ident_TRIAL_1:=FALSE]
  
  ## structural indicators end
  
  data_DHIS2_Booking[,bookorgname:=NEW_bookorgname]
  data_DHIS2_Booking[,NEW_bookorgname:=NULL]
  
  setorder(data_DHIS2_Booking,uniqueid,bookdate)
  data_DHIS2_Booking[,booknum:=1:.N,by=uniqueid]
  
  earlyData <- unique(data_DHIS2_Booking[,c("uniqueid","bookdate","booknum")])
  booklmp <- unique(data_DHIS2_Booking[,c("uniqueid","bookevent","booknum","booklmp")])
  
  ####
  # DHIS2 ANTENATAL
  print("CLEANING DHIS2 ANTENATAL")
  con <- DHIS2_Antenatal(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  int <- DHIS2_Antenatal(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  data_DHIS2_Antenatal <- rbind(
    con,
    int)
  
  ####
  # DHIS2 LAB
  print("CLEANING DHIS2 LAB")
  con <- DHIS2_Lab(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  int <- DHIS2_Lab(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  data_DHIS2_Lab <- rbind(
    con,
    int)
  
  ####
  # DHIS2 ULTRASOUND
  print("CLEANING DHIS2 ULTRASOUND")
  con <- DHIS2_Ultrasound(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  int <- DHIS2_Ultrasound(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  data_DHIS2_Ultrasound <- rbind(
    con,
    int)
  
  ####
  # DHIS2 RISK
  print("CLEANING DHIS2 RISK")
  data_DHIS2_RiskFactors <- DHIS2_RiskFactors(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  
  ####
  # DHIS2 MANAGEMENTS
  print("CLEANING DHIS2 MANAGEMENT")
  data_DHIS2_Management <- DHIS2_Management(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  
  ####
  # PREVIOUS PREGNANCIES
  print("CLEANING DHIS2 PREVIOUS PREGNANCY")
  con <- DHIS2_PreviousPregnancies(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  int <- DHIS2_PreviousPregnancies(isControl=FALSE, earlyData=earlyData, booklmp=booklmp)
  data_DHIS2_PreviousPregnancies <- rbind(
    con,
    int)
  
  ####
  # HOSPITAL BIRTH OUTCOMES
  print("CLEANING DHIS2 HOSPITAL BIRTH OUTCOMES")
  data_DHIS2_HospitalBirthOutcomes <- DHIS2_HospitalBirthOutcomes(isControl=TRUE, earlyData=earlyData, booklmp=booklmp)
  
  ####
  #
  print("CLINICAL CURRENT PREG OUTCOMES")
  data_DHSI2_CurrentPregnancyOutcomes <- DHIS2_CurrentPregnancyOutcomes(isControl=F,
                                                                        earlyData = earlyData,
                                                                        booklmp = booklmp)
  ####
  #
  print("CLINICAL CURRENT PREG OUTCOMES")
  data_DHIS2_PregnancyClosingNotes <- DHIS2_PregnancyClosingNotes(isControl=F,
                                                                        earlyData = earlyData,
                                                                        booklmp = booklmp)
  
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
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 HOSPITAL BIRTH OUTCOMES")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_HospitalBirthOutcomes,
    valueVarsRegex="^dhis2hbo",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_dhis2hbo"
  )
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE data_DHSI2_CurrentPregnancyOutcomes")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHSI2_CurrentPregnancyOutcomes,
    valueVarsRegex="^cpo",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum"),
    identName="ident_dhis2_cpo"
  )
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
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  d[,calc_expected_due_delivery:=booklmp+280]
  d[is.na(calc_expected_due_delivery),
    calc_expected_due_delivery:=usdate_1-usgestage_1*7+280]
  d[,isExpectedToHaveDelivered:=ifelse(calc_expected_due_delivery+14<as.Date(sprintf("%s-%s-01",MAX_YEAR,MAX_MONTH)),TRUE,FALSE)]
  xtabs(~d$isExpectedToHaveDelivered)
  
  setnames(d,"demoidnumber","motheridno")
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=1000))]
  }
  
  setorderv(d,cols=c("motheridno","bookdate"))
  d[,motheridbooknum:=1:.N,by=.(motheridno)]
  d[,motheridbook_earlyDate:=bookdate]
  d[,motheridbook_lateDate:=bookdate+31*10]
  # make sure that the "late date" doesnt overlap with a new pregnancy booking
  d[,temp:=shift(motheridbook_earlyDate,type = "lead"),by=.(motheridno)]
  d[motheridbook_lateDate>temp,motheridbook_lateDate:=temp-1]
  d[,temp:=NULL]
  
  #####################
  #####################
  #####################
  
  
  
  return(d)
}
