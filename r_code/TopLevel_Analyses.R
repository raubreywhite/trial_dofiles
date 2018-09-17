Analyses <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  IndicatorsPNIPHDashboard(d)
  
  Analyse_HBO_Completeness(d[ident_dhis2_booking==1])
  Analyse_BookingDescriptives(d[ident_dhis2_booking==1])
  Analyse_DataQuality(d)
  
  Analyse_Mahima_Random(d)
  Analyse_Mahima_Trial_1(d)
  Analyse_Mahima_Demographics(d)
  
  dir.create(FOLDER_DROPBOX_RESULTS)
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"hbo_completeness"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"booking_descriptives"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"data_quality"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima"))
  
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","random"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","trial_1"))
  dir.create(file.path(FOLDER_DROPBOX_RESULTS,"mahima","demographics"))
  
  
  
  
  
  # mervett HBO_Completeness
  HBO_Completeness(d[ident_dhis2_booking==1])

  BASE_LINE_STATISTICAL_ANALYSIS(d[ident_dhis2_booking==1])
  IndicatorsOsloAnalyse(d[ident_dhis2_booking==1])
  
  
  Analyze_Datacleaning(d)
  
  # tamara kappa/percent agreement values
  KappaValues()
}