capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"
use "data_clean/with_indicators.dta", clear

tab isTrial1Control isTrial1Intervention, mis

tab isTrial1Control is_aviccena, mis

tab isTrial1Intervention is_aviccena, mis

keep if isTrial1Intervention==1

gen number_pregnancies=1

capture drop screened_diab_hypt_anem
gen screened_diab_hypt_anem=0 if ///
	!missing(D1_numerator) & ///
	!missing(H1_numerator) & ///
	!missing(A1_numerator)
replace screened_diab_hypt_anem=1 if ///
	D1_numerator==1 & ///
	H1_numerator==1 & ///
	A1_numerator==1 

	
preserve
tempfile temp
save `temp'
replace demoorgname="   AA-Palestine"
append using `temp'

collapse ///
	(mean) age income bookgestage perc_screened_diab_hypt_anem=screened_diab_hypt_anem ///
	(sum) number_pregnancies num_screened_diab_hypt_anem=screened_diab_hypt_anem ///
	A*numerator* A*denominator* ///
	D*numerator* D*denominator* ///
	H*numerator* H*denominator* ///
	F*numerator* F*denominator* ///
	M*numerator* M*denominator* ///
	(count) denom_screened_diab_hypt_anem=screened_diab_hypt_anem, ///
	by( demoorgname) fast
	
	
replace perc_screened_diab_hypt_anem=perc_screened_diab_hypt_anem*100

 

local order_that_i_like="demoorgname number_pregnancies age income bookgestage perc_screened_diab_hypt_anem num_screened_diab_hypt_anem denom_screened_diab_hypt_anem "
order `order_that_i_like'

	/*
pause on
pause
*/

foreach N of varlist *numerator* {
	di "`N'"
	local D=subinstr("`N'","numerator","denominator",.)
	di "`D'"
	local P=subinstr("`N'","numerator","perc",.)
	
	gen `P' = (`N'/`D')*100
	
	local order_that_i_like="`order_that_i_like' `N' `D' `P'"
}
display "`order_that_i_like'"
order `order_that_i_like'
	
export excel using "results/fredrik/primary_outcomes_by_clinic_1.xlsx", firstrow(var) replace
restore


/*
egen meanage_clinic= mean(age), by(demoorgunit)
egen meanincome_clinic= mean(income), by(demoorgunit)
egen meanbookingdate_clinic= mean(bookingdate), by(demoorgunit)

keep meanage_clinic meanincome_clinic meanbookingdate_clinic demoorgunit
duplicages drop
*/





