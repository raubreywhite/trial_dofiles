
SaveWomenWithAbortionsIn2017 <- function(d){
  
  sum(is.na(d$bookdate)) # we see here that no women are missing bookdate
  
  # women with dhis2 booking who booked in 2017
  women2017 <- d[ident_dhis2_booking==1 & 
                   bookdate>="2017-01-15" & 
                   bookdate<="2018-01-15"]
  
  #see how many bookings we have
  nrow(women2017)
  
  #interventino
  interventionWomen2017 <- women2017[ident_dhis2_control==0]
  nrow(interventionWomen2017)
  # interventino women with an abortion
  interventionWomen2017x <- interventionWomen2017[
    cpopregoutcome_1 %in% "ABO" |
    cpopregoutcome_2 %in% "ABO" |
    cpopregoutcome_3 %in% "ABO" |
    cpopregoutcome_4 %in% "ABO"
  ]
  nrow(interventionWomen2017x)
  print(nrow(interventionWomen2017x)/nrow(interventionWomen2017))
  mean(interventionWomen2017$cpopregoutcome_1=="ABO",na.rm=T)
  
  # control women
  controlWomen2017 <- women2017[ident_dhis2_control==1]
  nrow(controlWomen2017)
  
  controlWomen2017x <- controlWomen2017[
    dhis2hbopregoutcome_1 %in% c("ABO") |
    dhis2hbopregoutcome_2 %in% c("ABO")]
  nrow(controlWomen2017x)
  print(nrow(controlWomen2017x)/nrow(controlWomen2017))
  mean(controlWomen2017$dhis2hbopregoutcome_1=="ABO",na.rm=T)
  
  returnValue <- rbind(interventionWomen2017x,controlWomen2017x)
  returnValue <- returnValue[,
    c(
      "bookevent",
      "motheridno",
      "bookorgdistrict",
      "bookorgname",
      "bookdate",
      "firstname",
      "fathersname",
      "middlename",
      "familyname1",
      "familyname2",
      "husbandsname",
      "dob",
      "mobile",
      "booklmp",
      "village",
      "dhis2hbodateofdeliveryhospital_1",
      "dhis2hbogestagedeliv_1"
    )]
  
  warning("THIS IS ONLY CONTROLS, DO INERVENTIONS")
  
  openxlsx::write.xlsx(returnValue,
                       file.path(FOLDER_DATA_MBO,"abortions.xlsx"))
  saveRDS(returnValue,
                       file.path(FOLDER_DATA_MBO,"abortions.RDS"))
  
}
