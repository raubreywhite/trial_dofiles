AVICENNA_Mothers_Details <- function(keepMotherID){
  d <- Get_AVICENNA_Data(folderName="Mothers Details")
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,babyrecorddatecreation:=Fix2DigitYear(Fix3LetterMonthToNumber(babyrecorddatecreation))]
  #d[,x:=Fix2DigitYear(babyrecorddatecreation)]
  d[,babyrecorddatecreation:=as.Date(babyrecorddatecreation)]
  d[,date:=babyrecorddatecreation]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  d <- AVICENNACreatePregnancyIDAndMakeItWide(d,tag="amd")
  d[,maxDate:=maxDate+10]
  
  return(d)
}