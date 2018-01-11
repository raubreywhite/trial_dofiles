clear
capture set maxvar 30000

capture cd "X:\data processing\"
capture cd "z:\data processing\"

capture mkdir "~/My Documents/trial_temp_data/"

global CLINIC_INTERVENTION_DATE="2017-12-19"
global CLINIC_CONTROL_DATE="2017-12-19"

global MAX_YEAR=substr("$CLINIC_CONTROL_DATE",1,4)
global MAX_MONTH=substr("$CLINIC_CONTROL_DATE",6,2)

run "trial_dofiles/01data_format_clinics.do"
run "trial_dofiles/04data_format_and_merge_aviccena.do"
run "trial_dofiles/05data_indicators.do"
run "trial_dofiles/06data_anonymize_and_dropbox.do"
run "trial_dofiles/08clinic_analysis_missing_birth_outcome.do"


*run "trial_dofiles/07analysis_descriptives.do"
*
*run "trial_dofiles/08district_analysis_missing_birth_outcome.do"
*run "trial_dofiles/09_trial_select_and_anonymize.do" 
*run "trial_dofiles/10_read drop identifier do file for Mahima.do" 
