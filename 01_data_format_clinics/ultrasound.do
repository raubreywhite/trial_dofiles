if($IS_CONTROL==1){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Ultrasound.csv", varnames(1) encoding("UTF-8") clear
	
	// GENERATING anc_gestationalageatvisitweeks
	gen uniqueid =v2
	count
	joinby uniqueid  using "~/My Documents/trial_temp_data/booklmp.dta", unm(b)
	drop bookevent
	count

	tab _merge
	drop if _merge==2
	drop _merge
	/*
	FIX DATE HERE
	*/
	capture drop temp
	gen temp=substr(eventdate,1,10)
	gen date = date(temp, "YMD")
	format %td date
	
	gen anc_gestationalageatvisitweeks = floor((date - newbooklmpcorrect)/7)
	drop newbooklmpcorrect uniqueid temp date
	replace anc_gestationalageatvisitweeks=. if anc_gestationalageatvisitweeks<2 | anc_gestationalageatvisitweeks>42
	// FINISHED GENERATING anc_gestationalageatvisitweeks
	
	gen identificationdocumentnumber=""
	gen usreason =.
	gen usperformanceofultrasound =.
	gen usperformedelsewhere =.
	gen usectopicpregnancy =.
	gen usmolarpregnancy =.
	gen usfetusdesignationformultiplepre =.
	gen  usplacentalocation =.
	gen usplacentapreviagrade=.
	gen  usplacentaorientation =.
	gen usfetalsex =.
	gen  usanomalies =.
	gen usanomaliesspecified =.
	gen  uspelvicmass =.
	gen usovariancysts=.
	gen usfibroids =.
	
}
else if($IS_CONTROL==0){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Ultrasound results.csv", varnames(1) encoding("UTF-8") clear
}

capture rename v2 programstageinstance

*Rename variables
rename (event programstageinstance programstage eventdate longitude latitude ///
organisationunitname organisationunitcode organisationunit ///
identificationdocumentnumber anc_gestationalageatvisitweeks usreason ///
usperformanceofultrasound usperformedelsewhere usectopicpregnancy ///
usmolarpregnancy ancusnumberoffetuses usfetusdesignationformultiplepre ///
usfetusheartactivity usplacentalocation usplacentapreviagrade ///
usplacentaorientation usfetalpresentation usfetalsex ///
usamnioticfluiddeepestpocket usamnioticfluidindexcm usamnioticfluidquantity ///
usgestationalsac usgestationalsacinmm usgestationalsacinweeks ///
uscrownrumplengthmm uscrownrumplengthweeks usbiparietaldiametermm ///
usbiparietaldiameterweeks usfemurlengthmm usfemurlengthweeks ///
usabdominalcircumferencemm usabdominalcircumferenceweeks ///
usestimatedgestationalageegaweek usestimatedgestationalageegadays ///
usestimatedfetalweightgm usultrasoundestimateddateofdeliv usanomalies ///
usanomaliesspecified uspelvicmass usovariancysts usfibroids ///
ussuspectedintrauterinegrowthres ussuspectedlargeforgestationalag ///
usrecommendationscomments) (usevent uniqueid usprogstage usdate uslong uslat ///
usorgname usorgcode usorgunit usidnumber usgestage usreason usplace ///
usplaceother usectopic usmolar usnumberfetus usmultifetdesignation ///
usfh usplacenplace usplacenprev usplacenor uspres ussex usamnideeppoc ///
usamniindex usamniquant usgestsac usgestsacmm usgestsacweek uscrlmm uscrlweeks ///
usbpdmm usbpdweeks usfemurmm usfemurweeks usacmm usacweeks usegaweeks ///
usegadays usefw usedd usanom usanomspec usmass usovcyst usfibroid usiugr ///
uslga uscomments)

*Label variables 
label variable usevent "System generated additional identifier"
label variable uniqueid "System generated identifier common to all datasets"
label variable usprogstage "Name of anc module"
label variable usdate "Date- ultrasound"
label variable uslong "Longitude coordinates of clinic- ultrasound"
label variable uslat "Latitude coordinates of clinic- ultrasound"
label variable usorgname "Name of clinic- ultrasound"
label variable usorgcode "System generated code of clinic"
label variable usorgunit "System generated additional code of clinic"
label variable usidnumber "Woman's ID number"
label variable usgestage "Gestational age at visit- ultrasound"
label variable usreason "Indication for ultrasound- ultrasound"
label variable usplace "Place where ultrasound is performed- ultrasound"
label variable usplaceother "Place where ultrasound is performed specified- ultrasound"
label variable usectopic "Ectopic pregnancy- ultrasound"
label variable usmolar "Molar pregnancy- ultrasound"
label variable usnumberfetus "Number of fetuses- ultrasound"
label variable usmultifetdesignation "Designation of fetus in multiple pregnancy- ultrasound"
label variable usfh "Fetal heart- ultrasound"
label variable usplacenplace "Placental location- ultrasound"
label variable usplacenprev "Placenta previa grade- ultrasound"
label variable usplacenor "Placenta orientation- ultrasound"
label variable uspres "Fetal presentation- ultrasound"
label variable ussex "Fetal sex- ultrasound"
label variable usamnideeppoc "Amniotic fluid deep pocket- ultrasound"
label variable usamniindex "Amniotic fluid index- ultrasound"
label variable usamniquant "Amniotic fluid quantity- ultrasound"
label variable usgestsac "Gestational sac- ultrasound"
label variable usgestsacmm "Gestational sac in millimeter- ultrasound"
label variable usgestsacweek "Gestational sac in weeks- ultrasound"
label variable uscrlmm "Crown rump length in millimeter- ultrasound"
label variable uscrlweeks "Crown rump length in weeks- ultrasound"
label variable usbpdmm "Bipareital diameter in millimeter- ultrasound"
label variable usbpdweeks "Bipareital diameter in weeks- ultrasound"
label variable usfemurmm "Femur length in millimeter- ultrasound"
label variable usfemurweeks "Femur length in weeks- ultrasound"
label variable usacmm "Abdominal circumference in millimeter- ultrasound"
label variable usacweeks "Abdominal circumference in weeks- ultrasound"
label variable usegaweeks "Estimated gestational age in weeks- ultrasound"
label variable usegadays "Estimated gestational age in days- ultrasound"
label variable usefw "Estimated fetal weight- ultrasound"
label variable usedd "Expected date of delivery- ultrasound"
label variable usanom "Fetal anomalies- ultrasound"
label variable usanomspec "Fetal anomalies specified- ultrasound"
label variable usmass "Pregnancy with pelvic mass- ultrasound"
label variable usovcyst "Ovarian cyst- ultrasound"
label variable usfibroid "Fibroid uterus- ultrasound"
label variable usiugr "Suspected intrauterine growth restriction- ultrasound"
label variable uslga "Suspected large for gestational age- ultrasound"
label variable uscomments "Comments and recommendations- ultrasound"


count
joinby uniqueid using "~/My Documents/trial_temp_data/bookevent_ids.dta", unm(b)
count

tab _merge
drop if _merge==2 // dropping ids
drop _merge

foreach X in "usdate" {
	display "`X'"

	gen year=substr(`X',1,4) // extract first four characters
	gen month=substr(`X',6,2) // extract 6-7 characters
	gen day=substr(`X',9,2) // extract 9-10 characters
	destring year, replace
	destring month, replace
	destring day, replace
	gen new`X' = mdy(month, day, year)
	drop year month day 
	format new`X' %td
	summarize new`X'
}
drop usdate
ren newusdate usdate

count if usdate < newbookdatecorrect1
count if usdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

gen bookevent = bookevent1
foreach X of varlist bookevent* {
	if("`X'"=="bookevent"){
		continue
	}
	di "`X'"
	local XVAL=subinstr("`X'","bookevent","",.)
	replace bookevent = bookevent`XVAL' if usdate >= newbookdatecorrect`XVAL'
	drop bookevent`XVAL'
}

drop newbookdatecorrect*

// FIX THIS VARIABLE usedd
// change it from being a string into
// being a date
capture drop temp
gen temp=substr(usedd,1,10)
//drop usedd
replace temp=lower(temp)
capture drop date
gen date = date(temp, "YMD")
format %td date
//bro date usedd
drop temp
drop usedd
rename date usedd

save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical Ultrasound results.dta", replace

sort uniqueid bookevent usdate
//duplicates drop uniqueid bookevent andate, force
bysort uniqueid bookevent: gen eventnumber=_n

codebook uniqueid
reshape wide us*, i(uniqueid bookevent) j(eventnumber)
codebook uniqueid

save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Ultrasound results.dta", replace


use "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical Ultrasound results.dta", clear

keep uniqueid bookevent usgestage usdate
gen us_expected_due_date=usdate-usgestage*7+280
format %td us_expected_due_date

drop if missing(usgestage)
drop if usgestage==0
bysort uniqueid bookevent (usdate): egen latestDate=max(usdate)
keep if latestDate==usdate
drop latestDate

keep uniqueid bookevent us_expected_due_date

duplicates drop

collapse (mean) us_expected_due_date, by(uniqueid bookevent)
replace us_expected_due_date = round(us_expected_due_date)

save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG clinical ultrasounds expected due date.dta", replace
