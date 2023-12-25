CS_Cause_Of_CS <- function(keepMotherID, files_to_be_read_in){
  d <- Get_CS_Data(folderName="Cause of CS", files_to_be_read_in = files_to_be_read_in, ignoreAttributes = TRUE)
  if(!is.null(keepMotherID)) d <- d[motheridno %in% keepMotherID]
  d[,baradmissionid:=TRUE]
  
  d[,datecreated:=Fix2DigitYear(Fix3LetterMonthToNumber(datecreated))]
  d[,datecreated:=as.Date(datecreated)]
  d[,date:=datecreated]
  
  if(.Platform$OS.type=="unix"){
    d[,motheridno:=as.character(rep(c(1:50000),length.out=.N,each=15))]
  }
  
  #d <- CSCreatePregnancyIDAndMakeItWide(d,tag="acs")
  
  return(d)
}