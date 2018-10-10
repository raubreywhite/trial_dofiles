Analyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  IndicatorsPNIPHDashboard(d)
  
  Analyse_HBO_Completeness(d[ident_dhis2_booking==1])
  Analyse_DataQuality(d)
  
  Analyse_Mahima_Random(d)
  Analyse_Mahima_Trial_1(d)
  #Analyse_Medical_History_Trial_1(d)

}