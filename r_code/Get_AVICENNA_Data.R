Get_AVICENNA_Data <- function(folderName){
  d <- vector("list",1000)
  i <- 1
  for(yearMonth in list.files(file.path(FOLDER_DATA_RAW,"avicenna"))){
    for(f in list.files(file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName))){
      d[[i]] <- readxl::read_excel(
        file.path(FOLDER_DATA_RAW,"avicenna",yearMonth,folderName,f)
      )
      i <- i + 1
    }
  }
  d <- MakeDataTableNamesLikeStata(rbindlist(d))
  ConvertAllFactorsToChar(d)
  return(d)
}