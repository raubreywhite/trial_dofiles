if($IS_CONTROL==0){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANCRisks.csv", varnames(1) encoding("UTF-8") clear

	capture ren v2 programstageinstance

	*Rename variables
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber ancrisktype riskdescription ///
	anc_gestationalageatvisitweeks createdeventidentifier) (riskevent uniqueid ///
	riskprogstage riskdate risklong risklat riskorgname riskorgcode riskorgunit ///
	riskidnumber risktype riskdesx riskgestage riskdesy)

	*Label variables 
	label variable riskevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable riskprogstage "Name of anc module"
	label variable riskdate "Date- risk detection"
	label variable risklong "Longitude coordinates of clinic- risk detection"
	label variable risklat "Latitude coordinates of clinic- risk detection"
	label variable riskorgname "Name of clinic- risk detection"
	label variable riskorgcode "System generated code of clinic"
	label variable riskorgunit "System generated additional code of clinic"
	label variable riskidnumber "Woman's ID number"
	label variable risktype "Type of risk identified- risk detection"
	label variable riskdesx "Description of risk- risk detection"
	label variable riskgestage "Gestational age- risk detection"
	label variable riskdesy "Description of risk identified additional- risk detection"

	count
	joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
	count

	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "riskdate" {
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
	drop riskdate
	ren newriskdate riskdate

	count if riskdate < newbookdatecorrect1
	count if riskdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

	**
	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if riskdate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*
	**

	save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical ANCRisks.dta", replace

	sort uniqueid bookevent riskdate
	//duplicates drop uniqueid bookevent riskdate, force
	bysort uniqueid bookevent: gen eventnumber=_n

	codebook uniqueid
	reshape wide risk*, i(uniqueid bookevent) j(eventnumber)
	codebook uniqueid

	save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical ANCRisks.dta", replace
}
