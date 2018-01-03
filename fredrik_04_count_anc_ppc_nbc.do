capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

import delimited "data_raw/e.reg-intervention/2017-12-03/Clinical Postpartum care.csv", varnames(1) encoding("UTF-8") clear
capture ren v2 programstageinstance
ren event cpcevent
ren programstageinstance uniqueid
gen is_clinical_postpartum_care=1

codebook uniqueid
codebook cpcevent

keep uniqueid is_clinical_postpartum_care
duplicates drop

tempfile cpc
save `cpc'

import delimited "data_raw/e.reg-intervention/2017-12-03/Clinical Newborn care.csv", varnames(1) encoding("UTF-8") clear
capture ren v2 programstageinstance
ren event cncevent
ren programstageinstance uniqueid
gen is_clinical_newborn_care=1

codebook uniqueid
codebook cncevent

keep uniqueid is_clinical_newborn_care
duplicates drop

tempfile cnc
save `cnc'



use "data_clean/with_indicators.dta", clear

keep if isTrial1Intervention==1

count
joinby uniqueid using `cpc', unm(b)
count

tab _merge
//pause on
//pause

keep if _merge==1 | _merge==3
drop _merge

count
joinby uniqueid using `cnc', unm(b)
count

tab _merge
//pause on
//pause

keep if _merge==1 | _merge==3
drop _merge



tab is_clinical_newborn_care
tab is_clinical_postpartum_care

generate keep_is_follow_up=0
replace keep_is_follow_up=1 if is_clinical_newborn_care==1 & is_clinical_postpartum_care==1
