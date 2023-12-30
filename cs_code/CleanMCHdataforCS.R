######################
CleanAllDataforCSStudy <- function(
  includePPC=TRUE,
  minBookDate="2001-01-01",
  maxBookDate="2100-01-01",
  delete=c(),
  IS_GAZA
){
  timeStart <- Sys.time()
  
  # CLEAN DHIS
  dhis=suppressWarnings(DHIS2_Master(
    keepDoubleBookings=FALSE,
    includePPC=includePPC,
    minBookDate=minBookDate,
    maxBookDate=maxBookDate,
    IS_GAZA=IS_GAZA
  ))
  
  if(length(delete)>0){
    for(i in delete){
      print(sprintf("Before deleting %s: %s colmns",i,ncol(dhis)))
      # delete all the variables named in 'delete' from dhis2
      vars <- names(dhis)[stringr::str_detect(names(dhis),i)]
      dhis[,(vars):=NULL]
      print(sprintf("After deleting %s: %s colmns",i,ncol(dhis)))
    }
  }
  
  if(IS_GAZA) return(dhis)
  
  keepMotherID <- unique(dhis$motheridno)
  
  # CLEAN AVICENNA
  cs=CS_Master(keepMotherID=keepMotherID, includeObs = TRUE)
  
  # MERGE DHIS TO AVICENNA
  nrow(dhis)
  d <- MergeDHISToAVICENNA(
    dhis=dhis,
    avicenna=cs)
  nrow(d)
  length(unique(d$bookevent))
  
  d[,avicennanum:=NULL]
  d[,minDate:=NULL]
  
  # delete the datasets "dhis" and "avicenna" to make space in the memory
  rm("dhis", envir=.GlobalEnv)
  rm("avicenna", envir=.GlobalEnv)
  
  print("****A")
  
  
  
  
  
  # CLEAN DIFFERENT FILES CONSISTENTLY (e.g. gestational age)
  #CleaningDifferentFilesConsistently(d)
  
  #Cleaning obvious mistakes
  FixObviousMistakes(d)
  
  # CREATING FURTHER VARIABLES
  print("****1")
  CreatingFurtherVariablesNormal(d)
  print("****2")
  CreatingFurtherVariablesMahima(d)
  print("****3")
  
  # CALC INDICATORS OLSO
 # IndicatorsOsloGenerate(d)
  
  timeEndAnalysis <- Sys.time()
  
  # garbage cleaning
  gc()
  
  return(d)
}




















