CleanAllData <- function(includePPC=TRUE){
  timeStart <- Sys.time()
  
  # CLEAN DHIS
  dhis=suppressWarnings(DHIS2_Master())
  if(!includePPC){
    dhis <- dhis[ident_dhis2_booking==1]
  }
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
  
  # this variable says where the baby info gets matched from
  d[,matching:=as.character(NA)]
  d[ident_avic_any==TRUE & is.na(matching),matching:="Avicenna"]
  d[ident_hbo==TRUE & is.na(matching),matching:="Governmental"]
  d[ident_dhis2_dhis2hbo==TRUE & is.na(matching),matching:="Private"]
  d[is.na(matching),matching:="Not"]
  
  # CLEAN DIFFERENT FILES CONSISTENTLY (e.g. gestational age)
  CleaningDifferentFilesConsistently(d)
  
  # CALC INDICATORS OLSO
  IndicatorsOsloGenerate(d)
  
  timeEndAnalysis <- Sys.time()
  
  # garbage cleaning
  gc()
  
  return(d)
}