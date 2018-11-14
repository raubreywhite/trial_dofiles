CleanAllData <- function(
  includePPC=TRUE,
  minBookDate="2001-01-01",
  maxBookDate="2100-01-01",
  delete=c(),
  IS_GAZA
  ){
  timeStart <- Sys.time()
  
  # CLEAN DHIS
  dhis=suppressWarnings(DHIS2_Master(
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
  
  print("****A")
  
  # MERGE DATAFILE WITH HBO
  nrow(d)
  d <- MergeDHISToAVICENNA(
    dhis=d,
    avicenna=HBO_Master())
  nrow(d)
  
  print("****0")
  
  # this variable says where the baby info gets matched from
  d[,matching:=as.character(NA)]
  if("ident_avic_any" %in% names(d)) d[ident_avic_any==TRUE & is.na(matching),matching:="Avicenna"]
  if("ident_hbo" %in% names(d))  d[ident_hbo==TRUE & is.na(matching),matching:="Governmental"]
  if("ident_dhis2_dhis2hbo" %in% names(d))  d[ident_dhis2_dhis2hbo==TRUE & is.na(matching),matching:="Private"]
  d[is.na(matching),matching:="Not"]
  
  # CLEAN DIFFERENT FILES CONSISTENTLY (e.g. gestational age)
  #CleaningDifferentFilesConsistently(d)
  
  # CREATING FURTHER VARIABLES
  print("****1")
  CreatingFurtherVariablesNormal(d)
  print("****2")
  CreatingFurtherVariablesMahima(d)
  print("****3")
  CreatingFurtherVariablesPNIPH(d)
  print("****4")
  
  # CALC INDICATORS OLSO
  IndicatorsOsloGenerate(d)
  
  timeEndAnalysis <- Sys.time()
  
  # garbage cleaning
  gc()
  
  return(d)
}