DHIS2_Master <- function(){
  
  ####
  # DHIS2 BOOKING
  print("CLEANING DHIS2 BOOKING")
  con <- DHIS2_BookingVisit(isControl=TRUE)
  int <- DHIS2_BookingVisit(isControl=FALSE)
  data_DHIS2_Booking <- rbind(
    con,
    int)
  
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
  
  ### 
  # start reshaping to wide
  # and merging
  print("RESHAPE TO WIDE AND MERGE DHIS2 ANTENATAL")
  d <- ReshapeToWideAndMerge(
    base=data_DHIS2_Booking,
    additional=data_DHIS2_Antenatal,
    valueVarsRegex="^an",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum")
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
    mergeVars=c("uniqueid","bookevent","booknum")
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
    mergeVars=c("uniqueid","bookevent","booknum")
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
    mergeVars=c("uniqueid","bookevent","booknum")
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
    mergeVars=c("uniqueid","bookevent","booknum")
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
    mergeVars=c("uniqueid")
  )
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  print("RESHAPE TO WIDE AND MERGE DHIS2 HOSPITAL BIRTH OUTCOMES")
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=data_DHIS2_HospitalBirthOutcomes,
    valueVarsRegex="^hbo",
    dcastFormula="uniqueid+bookevent+booknum~eventnum",
    mergeVars=c("uniqueid","bookevent","booknum")
  )
  nrow(data_DHIS2_Booking)
  nrow(d)
  ncol(d)
  
  d[,calc_expected_due_delivery:=booklmp+280]
  d[is.na(calc_expected_due_delivery),
    calc_expected_due_delivery:=usdate_1-usgestage_1*7+280]
  d[,isExpectedToHaveDelivered:=ifelse(calc_expected_due_delivery+14<as.Date(sprintf("%s-%s-01",MAX_YEAR,MAX_MONTH)),TRUE,FALSE)]
  xtabs(~d$isExpectedToHaveDelivered)
  
  return(d)
}