capture set maxvar 30000

capture cd "Z:\data processing\"
capture cd "X:\data processing\"

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
	
		use "data_clean/with_indicators_`year'-`month'.dta", clear

		
		*Drop all identifier variables (except uniqueid)
		drop trackedentity dummy idtype MotherID firstname datecreated ///
		fathername middlename familyname1 familyname2 husbandname street village city ///
		camp mobile phone email consang dob income education agemarriage agepregnancy ///
		members age

		*Drop all identifier variables
		drop bookdate booklong booklat bookorgname bookorgcode bookorgunit bookidnumber

		// drop some dates
		drop booklmp bookdatelastbirth dateupdated newbookdatecorrect ///
		 expecteddateofdelivery newdobcorrect age1 avgincome newbooklmpcorrect
		 
		 ren isTrial1Control is_trial_arm_a
		 ren isTrial1Intervention is_trial_arm_b
		 
		save "~/Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_with_indicators_`year'-`month'.dta", replace
	}
}

 