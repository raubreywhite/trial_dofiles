AVICENNA_Lab_Details <- function(keepMotherID){
  d <- Get_AVICENNA_Data(folderName="LAB")
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,testdate:=Fix2DigitYear(Fix3LetterMonthToNumber(testdate))]
  d[,testdate:=as.Date(testdate)]
  d[,date:=testdate]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  d <- AVICENNACreatePregnancyIDAndMakeItWide(d,tag="alab")
  
  return(d)
}