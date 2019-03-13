AVICENNA_Cause_Of_CS <- function(keepMotherID){
  d <- Get_AVICENNA_Data(folderName="Cause of CS",ignoreAttributes = T)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  d[,baradmissionid:=TRUE]
  
  d[,datecreated:=Fix2DigitYear(Fix3LetterMonthToNumber(datecreated))]
  d[,datecreated:=as.Date(datecreated)]
  d[,date:=datecreated]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=15))]
  }
  
  d <- AVICENNACreatePregnancyIDAndMakeItWide(d,tag="acs")
  
  return(d)
}