GazaAnalyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  Analyse_BookingDescriptives(d[ident_dhis2_booking==1])
}

WBAnalyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  IndicatorsPNIPHDashboard(d)
  
  Analyse_HBO_Completeness(d[ident_dhis2_booking==1])
  Analyse_MissingBirthOutcomes(d)
  
  Analyse_Mahima_Random(d)
  Analyse_Mahima_Trial_1(d)
  
  Analyse_Medical_History_Trial_1(d)
  # KappaValues()
  
  
}