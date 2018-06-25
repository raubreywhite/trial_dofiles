capture set maxvar 30000

global DATE : di %td_CY-N-D  date("$S_DATE", "DMY")
di "$DATE"
capture mkdir "~/eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results/$DATE/"

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

local index=1
forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture use "data_clean/with_indicators_`year'-`month'.dta", clear
		// return of coomand//
		if(_rc!=0){
			continue
		}

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

			
		tempfile temp
		save `temp'
		replace demoorgname="   AA-Palestine"
		append using `temp'

		keep number_pregnancies age income bookgestage screened_diab_hypt_anem A*numerator* A*denominator* ///
			D*numerator* D*denominator* ///
			H*numerator* H*denominator* ///
			F*numerator* F*denominator* ///
			M*numerator* M*denominator* ///
			demoorgname
			
		drop if demoorgname=="Beit Jala Hospital"
		drop if demoorgname=="Bethlehem"
		drop if demoorgname=="Hebron (Al Khalil)"
		drop if demoorgname=="Palestinian Medical Complex"
		///drop if demoorgname==""

		
		// if the first (month-year) dataset, then just save
		if(`index'==1){ 
			save "~/My Documents/trial_temp_data/temp_fredrik_01.dta", replace
		}
		else{ 
			// if the 2nd, 3rd, 4th, .... (month-year) dataset, then append then save
			append using "~/My Documents/trial_temp_data/temp_fredrik_01.dta"
			save "~/My Documents/trial_temp_data/temp_fredrik_01.dta", replace
		}
		local index=`index'+1
	}
}

	
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
	
export excel using "~/eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results/$DATE/primary_outcomes_by_clinic_1.xlsx", firstrow(var) replace


/*
egen meanage_clinic= mean(age), by(demoorgunit)
egen meanincome_clinic= mean(income), by(demoorgunit)
egen meanbookingdate_clinic= mean(bookingdate), by(demoorgunit)

keep meanage_clinic meanincome_clinic meanbookingdate_clinic demoorgunit
duplicages drop
*/





