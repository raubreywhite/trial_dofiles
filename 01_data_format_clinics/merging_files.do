use "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG ANC demographics and booking.dta", clear

count
joinby uniqueid bookevent using "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG clinical ultrasounds expected due date.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge

drop _merge

gen expected_due_delivery=newbooklmpcorrect+280
format %td expected_due_delivery
replace expected_due_delivery=us_expected_due_date if missing(expected_due_delivery)
codebook expected_due_delivery

ren demoidnumber MotherIDNO
gen is_clinical_int=1

*****
count
joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge

drop _merge

*****
count
joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Lab results.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge

drop _merge

*****
count
joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Ultrasound results.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge

drop _merge

*****
if($IS_CONTROL==0){
	count
	joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical ANCRisks.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge

	*****
	count
	joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical ANCManagements.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge
}
*****
count
joinby uniqueid using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge

drop _merge

if($IS_CONTROL==1){
	count
	joinby uniqueid bookevent using "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Hospital Birth Outcome.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge
}

if($IS_CONTROL==0){
	gen isTrial1Intervention=1
}
else {
	gen isTrial1Control=1
	gen isPretrial=1 if newbookdatecorrect < mdy(1,15,2017)
	capture tostring bookexamhead, replace
	capture tostring bookexamheadabn, replace
	capture tostring bookexamheart, replace
	capture tostring bookexamheartabn, replace
	capture tostring bookexamlung, replace
	capture tostring bookexamlungabn, replace
	capture tostring bookexamabd, replace
	capture tostring bookexamabdabn , replace
	capture tostring bookexamlimb , replace
	capture tostring bookexamlimbabn, replace
	capture tostring bookexambr , replace
	capture tostring  bookexambreastabn , replace
	capture destring  bookidnumber , replace
	capture tostring  bookallerfood , replace
	capture tostring  bookallerdrug , replace
	capture tostring  bookallerdrugspec , replace
	capture tostring  bookallersev , replace
	capture tostring  bookallersevspec , replace
	capture tostring  bookhistotherch , replace
	capture tostring  bookfetalmove , replace
	capture tostring  bookintendbirth , replace
	capture tostring  bookdatelastbirth , replace
	capture tostring bookhistbloodspec , replace
	capture tostring  bookrefchronic , replace
	capture tostring  bookvisitspec , replace
	capture tostring  bookmedpres , replace
	capture tostring  mobile, replace
	capture destring  phone, replace
	capture tostring  consang , replace
	capture destring  anidnumber1 , replace
	capture tostring  anallerfood1 , replace
	capture tostring anallerdrug1 , replace
	capture tostring anallerdrugspec1, replace
	capture tostring  anallersev1 , replace	
	capture tostring anallersevspec1 , replace
	capture tostring anfetalmove1 , replace
	capture tostring  anhistbloodspec1 , replace
	capture tostring anrefchronic1 , replace
}

save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG.dta", replace
