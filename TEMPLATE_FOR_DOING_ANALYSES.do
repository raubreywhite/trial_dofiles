capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

// NOTE: YOU NEED TO DEFINE A $MAX_YEAR
// YOU CAN DO THIS BY:
// global MAX_YEAR=2018
// BUT IT WILL ALSO BE AUTOMATICALLY DEFINED IN
// 0_run_all.do (if you run this first)

local index=1
forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture use "data_clean/with_indicators_`year'-`month'.dta", clear
		if(_rc!=0){
			continue
		}
		
		// keep the kind of people that you want
		keep if isTrial1Intervention==1
		
		// keep the variables that you want
		keep bookorgname
		
		if(`index'==1){ 
			save "~/My Documents/trial_temp_data/temp_file.dta", replace
		}
		else{ 
			// if the 2nd, 3rd, 4th, .... (month-year) dataset, then append then save
			append using "~/My Documents/trial_temp_data/temp_file.dta"
			save "~/My Documents/trial_temp_data/temp_file.dta", replace
		}
		local index=`index'+1
	}
}

// analyses start here

