IdentifyArchivedData <- function(){
  f <- list.files(FOLDER_DATA_CLEAN,"^archive_[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  f <- rev(sort(f))
  f <- file.path(FOLDER_DATA_CLEAN,f)
  f
  return(f)
}