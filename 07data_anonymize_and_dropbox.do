capture set maxvar 30000

capture cd "Z:\data processing\"
capture cd "X:\data processing\"
run "trial_dofiles/00x_date.do"

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
	
		capture use "data_clean/with_indicators_`year'-`month'.dta", clear
		if(_rc!=0){
			continue
		}
		
		*Drop all identifier variables (except uniqueid)
		drop trackedentity dummy idtype MotherID firstname datecreated ///
		fathername middlename familyname1 familyname2 husbandname street village city ///
		camp mobile phone email consang dob income education agemarriage agepregnancy ///
		members age 

		*Drop all identifier variables
		drop bookdate booklong booklat bookorgname bookorgcode bookorgunit bookidnumber ///
		demoorgname 
		//get rid of demoorgunit too???
		
		*Drop hbo identifiable informaion
		hbolatitude1 hboorganisationunit1 hbocon_woman_first_name1 hbocon_woman_family_name1 hbocon_husband_name1 ///
		hbocon_woman_address1 hboorganisationunitname2 hbocon_woman_first_name2 hbocon_woman_family_name2 hbocon_husband_name2 ///
		hboorganisationunitname3 hboorganisationunit3 hbocon_woman_first_name3 hbocon_woman_family_name3 hbocon_husband_name3 ///
		hbo_organisationunitname hbo_dataextractorusername hbo_alternateidentificationnum hbo_firstname hbo_fathersname ///
		hbo_husbandsfamilyname hbo_husbandsname hbo_middlename hbo_womanfamilyname hbo_village hbo_city hbo_dateofbirth ///
		hbo_mobilenumber hbo_educationinyears hbo_ageatmarriage hbo_ageatfirstpregnancy hbo_monthlyhouseholdincomeils ///
		hbo_numberofmembersinhousehold hbo_organisationunitname1 hbo_organisationunitname1 hbo_organisationunitname2

		// drop some dates
		drop booklmp bookdatelastbirth dateupdated newbookdatecorrect  ///
		 expecteddateofdelivery newdobcorrect age1 avgincome newbooklmpcorrect
		 
		 ren isTrial1Control is_trial_arm_a
		 ren isTrial1Intervention is_trial_arm_b
		 
		save "~/eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_with_indicators_`year'-`month'.dta", replace
	}
}

 