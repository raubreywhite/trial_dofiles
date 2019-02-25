
GetFoldersAndData <- function(d,location){
  if(!location %in% c("pal","wb","gaza")) stop("location is not 'pal', 'wb', or 'gaza'")
  folders <- list()
  if(location=="wb"){
    pd <- d[ident_gaza==F]
    folders[["dropbox"]] <- FOLDER_DROPBOX_RESULTS_WB
    folders[["results"]] <- FOLDER_DATA_RESULTS_WB
    folders[["data_raw"]] <- FOLDER_DATA_RAW_WB
  } else if(location=="gaza"){
    pd <- d[ident_gaza==T]
    folders[["dropbox"]] <- FOLDER_DROPBOX_RESULTS_GAZA
    folders[["results"]] <- FOLDER_DATA_RESULTS_GAZA
    folders[["data_raw"]] <- FOLDER_DATA_RAW_GAZA
  } else {
    pd <- copy(d)
    folders[["dropbox"]] <- FOLDER_DROPBOX_RESULTS_PAL
    folders[["results"]] <- FOLDER_DATA_RESULTS_PAL
  }
  return(list(
    pd=pd,
    folders=folders
  ))
}