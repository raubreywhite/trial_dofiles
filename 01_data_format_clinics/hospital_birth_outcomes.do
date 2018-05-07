
if($IS_CONTROL==1){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Hospital Birth Outcome.csv", varnames(1) encoding("UTF-8") clear

	ren ancpreviouspregnancyoutcome pregnancyoutcome // TODO these names seem bad, and should probably be changed
	drop if missing(pregnancyoutcome) & missing(con_woman_first_name) & missing(con_name_hosp_birth)
	
	capture ren v2 programstageinstance
	rename event hboevent
	rename programstageinstance uniqueid
	
	// fixing va names that are too long
	capture rename ancpreviouspregnancybirthweightg ancprevpregbirthweightg
	capture rename indicationforcsectionmentionedin indicforcsectionmentionedin
	capture rename ancdiastolicbloodpressuremmhg ancdiastbloodpressuremmhg
	
	foreach X of varlist programstage-usrecommendationscomments {
		ren `X' hbo`X'
	}
	
	//bro if uniqueid=="B9FWlWn5xrR"
	
	count
	joinby uniqueid using "~/My Documents/trial_temp_data/bookevent_ids.dta", unm(b)
	count

	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "hbodate" {
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
	drop hbodate
	ren newhbodate hbodate

	count if hbodate < newbookdatecorrect1
	count if hbodate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

	**
	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if hbodate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*
	gen is_hospital_birth_outcome=1
	save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Hospital Birth Outcome.dta", replace


	sort uniqueid hbodate
	duplicates drop uniqueid hbodate, force
	bysort uniqueid: gen eventnumber=_n
	
	// for women birth twice in the same year //

	codebook uniqueid
	reshape wide hbo*, i(uniqueid) j(eventnumber)
	codebook uniqueid

	order bookevent
	save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Hospital Birth Outcome.dta", replace
}
