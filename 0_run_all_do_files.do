clear
capture set maxvar 30000

capture cd "X:\data processing\"
capture cd "z:\data processing\"

run "trial_dofiles/00x_date.do"

capture mkdir "~/My Documents/trial_temp_data/"


run "trial_dofiles/01data_format_clinics.do"
run "trial_dofiles/04data_format_and_merge_aviccena.do"
run "trial_dofiles/05data_format_and_merge_birth_outcomes.do"
run "trial_dofiles/06data_indicators.do"
run "trial_dofiles/07data_anonymize_and_dropbox.do"
run "trial_dofiles/08clinic_analysis_missing_birth_outcome.do"

