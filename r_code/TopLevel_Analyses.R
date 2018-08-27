Analyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  dir.create(FOLDER_DROPBOX_RESULTS)
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1","demographics"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"trial_1","random_indicators"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"booking_descriptives"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"indicators_for_mahima"))
  
  IndicatorsPNIPHDashboard(d)
  
  # mervett HBO_Completeness
  HBO_Completeness(d[ident_dhis2_booking==1])

  BASE_LINE_STATISTICAL_ANALYSIS(d[ident_dhis2_booking==1])
  IndicatorsOsloAnalyse(d[ident_dhis2_booking==1])
  
  Analyse_BookingDescriptives(d[ident_dhis2_booking==1])
  
  
  # tamara kappa/percent agreement values
  KappaValues()
}