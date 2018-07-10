Analyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # mervett HBO_Completeness
  HBO_Completeness(d)

  
  BASE_LINE_STATISTICAL_ANALYSIS(d)
  IndicatorsOsloAnalyse(d)
  
  Analyse_BookingsByMonth(d)
  
  
  # tamara kappa/percent agreement values
  KappaValues()
}