Analyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  IndicatorsPNIPHDashboard(d)
  
  # mervett HBO_Completeness
  HBO_Completeness(d[ident_dhis2_booking==1])

  BASE_LINE_STATISTICAL_ANALYSIS(d[ident_dhis2_booking==1])
  IndicatorsOsloAnalyse(d[ident_dhis2_booking==1])
  
  Analyse_BookingsByMonth(d[ident_dhis2_booking==1])
  
  
  # tamara kappa/percent agreement values
  KappaValues()
}