if($IS_CONTROL==0){
	*Open ultrasound .dta file 
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANCManagements.csv", varnames(1) encoding("UTF-8") clear

	capture ren v2 programstageinstance

	*Rename variables
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber anc_gestationalageatvisitweeks ///
	managementdetails managementtype managementperformed createdeventidentifier) ///
	(manevent uniqueid manprogstage mandate manlong manlat manorgname manorgcode ///
	manorgunit manidnumber mangestage mandetail mantypex manperf mantypey)

	*Label variables 
	label variable manevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable manprogstage "Name of anc module"
	label variable mandate "Date- management"
	label variable manlong "Longitude coordinates of clinic- management"
	label variable manlat "Latitude coordinates of clinic- management"
	label variable manorgname "Name of clinic- management"
	label variable manorgcode "System generated code of clinic"
	label variable manorgunit "System generated additional code of clinic"
	label variable manidnumber "Woman's ID number"
	label variable mangestage "Gestational age- management"
	label variable mandetail "Details of treatment or referral- management"
	label variable mantypex "Type of management- management"
	label variable manperf "Whether management was performed or not"
	label variable mantypey "Type of management additional- management"


	count
	joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
	count

	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "mandate" {
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
	drop mandate
	ren newmandate mandate

	count if mandate < newbookdatecorrect1
	count if mandate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

	**
	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if mandate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*
	**

	save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical ANCManagements.dta", replace


	sort uniqueid bookevent mandate
	//duplicates drop uniqueid bookevent andate, force
	bysort uniqueid bookevent: gen eventnumber=_n

	codebook uniqueid
	reshape wide man*, i(uniqueid bookevent) j(eventnumber)
	codebook uniqueid

	save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical ANCManagements.dta", replace
}
