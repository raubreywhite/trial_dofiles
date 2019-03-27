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
  avicenna=AVICENNA_Master(keepMotherID=keepMotherID, includeObs = TRUE)
  
  # MERGE DHIS TO AVICENNA
  nrow(dhis)
  d <- MergeDHISToAVICENNA(
    dhis=dhis,
    avicenna=avicenna)
  nrow(d)
  length(unique(d$bookevent))
  
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
  length(unique(d$bookevent))
  
  # MERGE DATAFILE WITH PAPERHBO
  paperhbo_search_for_bookevent(d)
  
  nrow(d)
  d <- ReshapeToWideAndMerge(
    base=d,
    additional=paperhbo(
      src="bookeventsfound",
      tagWithPaperHBO=TRUE),
    valueVarsRegex="^paperhbo",
    dcastFormula="motheridno+bookevent+booknum~eventnum",
    mergeVars=c("motheridno","bookevent","booknum"),
    identName="ident_paperhbo"
  )
  nrow(d)
  ncol(d)
  
  length(d$bookevent)
  length(unique(d$bookevent))
  
  print("****0")
  
  # this variable says where the baby info gets matched from
  d[,matching:=as.character(NA)]
  d[,matching_avicenna:=FALSE]
  d[,matching_gov:=FALSE]
  d[,matching_private:=FALSE]
  d[,matching_hbo:=FALSE]
  if("ident_avic_any" %in% names(d)){
    d[ident_avic_abb==TRUE & ident_avic_amd==TRUE & is.na(matching),matching:="Avicenna"]
    d[ident_avic_abb==TRUE & ident_avic_amd==TRUE, matching_avicenna:=TRUE]
  }
  if("ident_hbo" %in% names(d)){
    d[ident_hbo==TRUE & is.na(matching),matching:="Governmental"]
    d[ident_hbo==TRUE, matching_gov:=TRUE]
  }
  if("ident_dhis2_dhis2hbo" %in% names(d)){
    d[ident_dhis2_dhis2hbo==TRUE & is.na(matching),matching:="Private"]
    d[ident_dhis2_dhis2hbo==TRUE, matching_private:=TRUE]
  }
  if("ident_paperhbo" %in% names(d)){
    d[ident_paperhbo==TRUE & is.na(matching),matching:="PaperHBO"]
    d[ident_paperhbo==TRUE, matching_hbo:=TRUE]
  }
  d[is.na(matching),matching:="Not"]
  xtabs(~d$matching)
  
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
  IndicatorsOsloGenerate(d)

  # creating indicators for bad data who we cant really use
  d[bookdate<booklmp,ident_bad_bookdate_before_booklmp:=1]
  
  d[ident_bad_bookdate_before_booklmp==1, ident_bad_all:=1]
  
  timeEndAnalysis <- Sys.time()
  
  # garbage cleaning
  gc()
  
  return(d)
}