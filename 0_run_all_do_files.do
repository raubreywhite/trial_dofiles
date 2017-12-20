clear
capture set maxvar 30000

cd "X:\data processing\"
 
global CLINIC_INTERVENTION_DATE="2017-12-03"
global CLINIC_CONTROL_DATE="2017-12-03"

run "trial dofiles/01data_format_clinics.do"
run "trial dofiles/04data_format_and_merge_aviccena.do"
run "trial dofiles/05data_indicators.do"
run "trial dofiles/07analysis_descriptives.do"
run "trial dofiles/08clinic_analysis_missing_birth_outcome.do"
run "trial dofiles/08district_analysis_missing_birth_outcome.do"
run "trial dofiles/09_trial_select_and_anonymize.do" 
run "trial dofiles/10_read drop identifier do file for Mahima.do" 
