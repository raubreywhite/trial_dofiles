AVICENNA_BabyBirth <- function(keepMotherID){
  d <- Get_AVICENNA_Data(folderName="Baby Birth", ignoreAttributes = TRUE)
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
  
  d <- AVICENNACreatePregnancyIDAndMakeItWide(d,tag="abb")
  
  return(d)
}