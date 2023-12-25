CS_BabyBirth <- function(keepMotherID, files_to_be_read_in){
  d <- Get_CS_Data(folderName="Baby Birth", files_to_be_read_in=files_to_be_read_in,
                   ignoreAttributes = TRUE)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,babyrecorddatecreation:=Fix2DigitYear(Fix3LetterMonthToNumber(babyrecorddatecreation))]
  d[,babyrecorddatecreation:=as.Date(babyrecorddatecreation)]
  d[,date:=babyrecorddatecreation]
  d[,babypregnancynoofweeks:=as.numeric(stringr::str_extract(babypregnancynoofweeks,"^[0-9][0-9]"))]

  d[,year:=NULL]
  d[,month:=NULL]
  d[,day:=NULL]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  #d <- CSCreatePregnancyIDAndMakeItWide(d,tag="abb")
  
  return(d)
}