
//select 2017 data

cd "X:\data processing\"

use "data_clean/IDENT_aviccena_merged_clinical.dta", clear

/*
keep bookorgunit
duplicates drop
browse
*/
** KEEP ALL PEOPLE WHO GET BOOKED IN AFTER 15/1/2017
keep if newbookdatecorrect>=mdy(1,15,2017)

drop if bookorgunit=="(HR - Bethlehem MCH )امومة بيت لحم حمل خطر"
drop if bookorgunit=="(HR - Hewarah)عيادة حوارة حمل خطر"
drop if bookorgunit=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
drop if bookorgunit=="(HR - Bedia) بديا حمل خطر"
drop if bookorgunit=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
drop if bookorgunit=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
drop if bookorgunit=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
drop if bookorgunit=="(  - Central Health Directorate Salfit) صحة سلفيت"
drop if bookorgunit=="( - Jenin Central clinic) عيادة جنين المركزية"

tab is_aviccena is_clinical_int if expected_due_delivery <= mdy(8,1,2017), miss

save "data_temp/aviccena_merged_clinical_restricted.dta", replace

// GEN UNIQUE ANON ID
use "data_temp/aviccena_merged_clinical_restricted.dta", clear
sort uniqueid
egen anon_id=group(uniqueid)
keep uniqueid anon_id
save "data_clean/key.dta", replace

// GVE EVERYONE ANON ID
use "data_temp/aviccena_merged_clinical_restricted.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/aviccena_merged_clinical_restricted.dta", replace


use "data_temp/IDENTIFIABLE int Clinical Lab results.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/IDENTIFIABLE int Clinical Lab results.dta", replace

use "data_temp/IDENTIFIABLE int clinical ultrasounds expected due date.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/aviccena_merged_clinical_restricted.dta", replace

use "data_temp/IDENTIFIABLE int Clinical ANCRisks.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/IDENTIFIABLE int Clinical ANCRisks.dta", replace

use "data_temp/IDENTIFIABLE int Clinical ANCManagements.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/IDENTIFIABLE int Clinical ANCManagements.dta", replace

use "data_temp/IDENTIFIABLE int Clinical Previous pregnancies.dta", clear
count
joinby uniqueid using "data_clean/key.dta", unm(b)
count
tab _merge
keep if _merge==3
drop _merge
save "data_clean/IDENTIFIABLE int Clinical Previous pregnancies.dta", replace


