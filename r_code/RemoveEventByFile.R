RemoveEventByFile <- function(d, filename){
  
  toRemove <- readxl::read_excel(file.path("..",
                                           "data_raw",
                                           "structural_data",
                                           filename))
  
  d <- d[!event %in% toRemove$Event,,]
  
  return(d)
}