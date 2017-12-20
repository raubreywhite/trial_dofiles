clear
capture set maxvar 30000
//select 2017 data


import delimited "X:\data processing\data_raw\2017-e.reg\2017-11-5 Clinical set\Clinical Postpartum care.csv", varnames(1) encoding(utf8) clear 
rename v2 uniqueid
save "X:\data processing\data_temp\int_ Clinical Postpartum.dta", replace
 

import delimited "X:\data processing\data_raw\2017-e.reg\2017-11-5 Clinical set\Clinical Newborn care.csv", varnames(1) encoding(utf8) clear 
rename v2 uniqueid
save "X:\data processing\data_temp\int_clinical newborncare.dta", replace

cd "X:\data processing\"

use "data_clean/with_indicators.dta", clear
/// use "data_clean/IDENT_aviccena_merged_clinical.dta", clear

keep if isExpectedToHaveDelivered==1

