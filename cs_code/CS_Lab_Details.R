CS_Lab_Details <- function(keepMotherID, files_to_be_read_in){
  d <- Get_CS_Data(folderName="LAB",
                   files_to_be_read_in = files_to_be_read_in, ignoreAttributes = TRUE)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,testdate:=Fix2DigitYear(Fix3LetterMonthToNumber(testdate))]
  d[,testdate:=as.Date(testdate)]
  d[,date:=testdate]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  #d <- CSCreatePregnancyIDAndMakeItWide(d,tag="alab")
  
  return(d)
  

}