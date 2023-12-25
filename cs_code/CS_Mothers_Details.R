CS_Mothers_Details <- function(keepMotherID, files_to_be_read_in){
  d <- Get_CS_Data(folderName="Mothers Details", files_to_be_read_in = files_to_be_read_in)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  
  d[,babyrecorddatecreation:=Fix2DigitYear(Fix3LetterMonthToNumber(babyrecorddatecreation))]
  #d[,x:=Fix2DigitYear(babyrecorddatecreation)]
  d[,babyrecorddatecreation:=as.Date(babyrecorddatecreation)]
  d[,date:=babyrecorddatecreation]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=10))]
  }
  
  #d <- CSCreatePregnancyIDAndMakeItWide(d,tag="amd")
  #d[,maxDate:=maxDate+10]
  
  return(d)
}
