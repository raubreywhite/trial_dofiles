CS_Observations <- function(keepMotherID, files_to_be_read_in){
  d <- Get_CS_Data(folderName="Observations",
                         takeFirstObs=TRUE,
                         dateName="dateprocess",
                         files_to_be_read_in = files_to_be_read_in,
                   ignoreAttributes = TRUE)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,dateprocess:=Fix2DigitYear(Fix3LetterMonthToNumber(dateprocess))]
  d[,dateprocess:=as.Date(dateprocess)]
  d[,date:=dateprocess]
  
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  #d <- CSCreatePregnancyIDAndMakeItWide(d,tag="aobs")
  
  return(d)
}