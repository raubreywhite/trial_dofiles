capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

use "data_clean/with_indicators.dta", clear

keep if isTrial1Intervention==1

set seed 4
gen randomNumber=runiform()
sort randomNumber
keep if _n<=10

capture export excel using "results/fredrik/choose_10_cases_randomly.xlsx", firstrow(var) replace

****added capture infront of line 16 because kept giving an error message that observations must be between 1-1048576, 26/12/2017******


/*
egen meanage_clinic= mean(age), by(demoorgunit)
egen meanincome_clinic= mean(income), by(demoorgunit)
egen meanbookingdate_clinic= mean(bookingdate), by(demoorgunit)

keep meanage_clinic meanincome_clinic meanbookingdate_clinic demoorgunit
duplicages drop
*/





