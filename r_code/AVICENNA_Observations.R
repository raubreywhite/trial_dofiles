AVICENNA_Observations <- function(keepMotherID){
  d <- Get_AVICENNA_Data(folderName="Observations",
                         takeFirstObs=TRUE,
                         dateName="dateprocess")[motheridno %in% keepMotherID]
  
  d[,dateprocess:=Fix2DigitYear(Fix3LetterMonthToNumber(dateprocess))]
  d[,dateprocess:=as.Date(dateprocess)]
  d[,date:=dateprocess]
  
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  d <- AVICENNACreatePregnancyIDAndMakeItWide(d,tag="aobs")
  
  return(d)
}