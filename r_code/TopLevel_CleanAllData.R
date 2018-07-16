CleanAllData <- function(){
  timeStart <- Sys.time()
  
  # CLEAN DHIS
  dhis=suppressWarnings(DHIS2_Master())
  #dhis <- dhis[ident_dhis2_booking==1]
  keepMotherID <- unique(dhis$motheridno)
  
  # CLEAN AVICENNA
  avicenna=AVICENNA_Master(keepMotherID=keepMotherID, includeObs = FALSE)
  
  # MERGE DHIS TO AVICENNA
  nrow(dhis)
  d <- MergeDHISToAVICENNA(
    dhis=dhis,
    avicenna=avicenna)
  nrow(d)
  
  d[,avicennanum:=NULL]
  d[,minDate:=NULL]
  
  # delete the datasets "dhis" and "avicenna" to make space in the memory
  rm("dhis", envir=.GlobalEnv)
  rm("avicenna", envir=.GlobalEnv)
  
  # MERGE DATAFILE WITH HBO
  nrow(d)
  d <- MergeDHISToAVICENNA(
    dhis=d,
    avicenna=HBO_Master())
  nrow(d)
  
  # CALC INDICATORS OLSO
  IndicatorsOsloGenerate(d)
  
  timeEndAnalysis <- Sys.time()
  
  # garbage cleaning
  gc()
  
  return(d)
}