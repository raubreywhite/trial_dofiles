
capture cd "Z:\data processing\"
capture cd "X:\data processing\"

***************
**************
*** Mothers Details
**************

do "trial_dofiles/04data_format_and_merge_aviccena/motherdetails.do"

***************
**************
*** CAUSE OF CS
**************

do "trial_dofiles/04data_format_and_merge_aviccena/causecs.do"

***************
**************
*** DOCTORS NOTES
**************
do "trial_dofiles/04data_format_and_merge_aviccena/docnotes.do"

***************
**************
*** NURSES NOTES
**************
do "trial_dofiles/04data_format_and_merge_aviccena/nursenotes.do"

***************
**************
*** LAB DETAILS
**************
do "trial_dofiles/04data_format_and_merge_aviccena/lab.do"

***************
**************
*** OBSERVATIONS?BLOOD PRESSURE
**************
do "trial_dofiles/04data_format_and_merge_aviccena/bloodpressure.do"

***************
**************
*** Baby Birth
**************
do "trial_dofiles/04data_format_and_merge_aviccena/babybirth.do"

*****************
*****************
*****************
*****************
*****************
*****************
*****************
*****************
***************** MERGING EVERYTHING THAT ISNT BABY BIRTH
*****************
*****************
*****************
*****************
*****************
*****************
*****************

use "~/My Documents/trial_temp_data/motherdetails.dta", clear
keep MotherIDNO md_date md_pregID
reshape wide md_date, i(MotherIDNO) j(md_pregID)
save "~/My Documents/trial_temp_data/motherdetails_key.dta", replace

use "data_temp/bp.dta", clear
keep MotherIDNO obs_date obs_pregID
reshape wide obs_date, i(MotherIDNO) j(obs_pregID)
save "~/My Documents/trial_temp_data/bp_key.dta", replace

use "~/My Documents/trial_temp_data/lab.dta", clear
keep MotherIDNO lab_date lab_pregID
reshape wide lab_date, i(MotherIDNO) j(lab_pregID)
save "~/My Documents/trial_temp_data/lab_key.dta", replace

use "~/My Documents/trial_temp_data/docnotes.dta", clear
keep MotherIDNO dn_date dn_pregID
reshape wide dn_date, i(MotherIDNO) j(dn_pregID)
save "~/My Documents/trial_temp_data/docnoes_key.dta", replace

use "~/My Documents/trial_temp_data/causecs.dta", clear
keep MotherIDNO cs_date cs_pregID
reshape wide cs_date, i(MotherIDNO) j(cs_pregID)
save "~/My Documents/trial_temp_data/causecs_key.dta", replace

use "~/My Documents/trial_temp_data/bb.dta", clear
keep MotherIDNO bb_date bb_pregID
reshape wide bb_date, i(MotherIDNO) j(bb_pregID)
save "~/My Documents/trial_temp_data/bb_key.dta", replace

******
** MERGING IN KEY VARIABLES
use "~/My Documents/trial_temp_data/motherdetails_key.dta", clear
count
joinby MotherIDNO using "~/My Documents/trial_temp_data/bp_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "~/My Documents/trial_temp_data/lab_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "~/My Documents/trial_temp_data/docnoes_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "~/My Documents/trial_temp_data/causecs_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "~/My Documents/trial_temp_data/bb_key.dta", unm(b)
count
tab _merge
drop _merge


/// 
foreach X of varlist md_date* {
	di "`X'"
	local XVAL=subinstr("`X'","md_date","",.)
	foreach Q in "obs" "lab" "dn" "cs" "bb" {
		capture drop `Q'_pregID`XVAL'
		gen `Q'_pregID`XVAL'=.
		capture drop temp
		gen temp=100
		foreach Y of varlist `Q'_date* {
			local YVAL=subinstr("`Y'","`Q'_date","",.)
			di "`XVAL'"
			di "`YVAL'"
			replace `Q'_pregID`XVAL'=`YVAL' if abs(`Y'-`X')<temp & !missing(`X') & !missing(`Y')
			replace temp=abs(`Y'-`X') if abs(`Y'-`X')<temp & !missing(`X') & !missing(`Y')
		}
	}
}

keep MotherIDNO *pregID*
reshape long obs_pregID lab_pregID dn_pregID cs_pregID bb_pregID, i(MotherIDNO) j(md_pregID)

** MERGE BACK INTO ORIGINAL STUFF
count
joinby MotherIDNO md_pregID using "~/My Documents/trial_temp_data/motherdetails.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO obs_pregID using "~/My Documents/trial_temp_data/bp.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO lab_pregID using "~/My Documents/trial_temp_data/lab.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO dn_pregID using "~/My Documents/trial_temp_data/docnotes.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO cs_pregID using "~/My Documents/trial_temp_data/causecs.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO bb_pregID using "~/My Documents/trial_temp_data/bb.dta", unm(b)
count

tab _merge
drop _merge

destring MotherIDNO, replace

gen is_aviccena=1
drop if missing(md_date)


gen isExpectedToHaveDelivered=0
replace isExpectedToHaveDelivered=1 if expected_due_delivery<mdy($MAX_MONTH,1,$MAX_YEAR)
replace isExpectedToHaveDelivered=. if missing(expected_due_delivery)

save "~/My Documents/trial_temp_data/aviccena.dta", replace

*****************************
*****************************
*****************************
*****************************
*****************************
*****************************
***** MERGE STARTS HERE

use "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1.dta", clear
keep MotherIDNO newbookdatecorrect booking_number 
reshape wide newbookdatecorrect, i(MotherIDNO) j(booking_number)
save "~/My Documents/trial_temp_data/clinic_key.dta", replace

use "~/My Documents/trial_temp_data/aviccena.dta", clear
keep MotherIDNO md_date md_pregID
reshape wide md_date, i(MotherIDNO) j(md_pregID)
save "~/My Documents/trial_temp_data/aviccena_key.dta", replace

** MERGING IN KEY VARIABLES
use "~/My Documents/trial_temp_data/clinic_key.dta", clear
count
joinby MotherIDNO using "~/My Documents/trial_temp_data/aviccena_key.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
drop _merge
count

// FINDING OUT WHICH AVICCENA DATA IS AROUDN 10 MONTHS OR LESS FROM BOOKING DATE
foreach X of varlist newbookdatecorrect* {
	di "`X'"
	local XVAL=subinstr("`X'","newbookdatecorrect","",.)
	foreach Q in "md" {
		capture drop `Q'_pregID`XVAL'
		gen `Q'_pregID`XVAL'=.
		capture drop temp
		gen temp=31*10 // 10 months
		foreach Y of varlist `Q'_date* {
			local YVAL=subinstr("`Y'","`Q'_date","",.)
			di "`XVAL'"
			di "`YVAL'"
			replace `Q'_pregID`XVAL'=`YVAL' if `Y'>`X' & (`Y'-`X')<temp & !missing(`X') & !missing(`Y')
			replace temp=(`Y'-`X') if `Y'>`X' & (`Y'-`X')<temp & !missing(`X') & !missing(`Y')
		}
	}
}

keep MotherIDNO *pregID* newbookdatecorrect*
reshape long md_pregID newbookdatecorrect, i(MotherIDNO) j(booking_number)

drop if missing(newbookdatecorrect)
drop newbookdatecorrect

save "~/My Documents/trial_temp_data/merge_key_data_to_avicenna.dta", replace

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {		
		use "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1_`year'-`month'.dta", clear
		
		di "***`month'"
		count
		joinby MotherIDNO booking_number using "~/My Documents/trial_temp_data/merge_key_data_to_avicenna.dta", unm(none)
		joinby MotherIDNO md_pregID using "~/My Documents/trial_temp_data/aviccena.dta", unm(master)
		count
		
		save "~/My Documents/trial_temp_data/IDENT_aviccena_merged_clinical_`year'-`month'.dta", replace
	}
}


