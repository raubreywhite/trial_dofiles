import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Lab results.csv", varnames(1) encoding("UTF-8") clear
*Open lab .dta file 
*Rename variables
if($IS_CONTROL==1){

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
	gen labwherewerethetestsperformed=.
	gen labtestsperformedatotherspecifie =.
	gen ancorppcvisit =.
	gen labcbcwhitebloodcells =.
	gen labcbcredbloodcellcount =.
	gen labanemiatreatmentresponse =.
	gen labcbcmeancorpuscularvolume =.
	gen  labcbcplatelets =.
	gen labsecondopiniononinitiatedtreat =.
	gen laboralglucosechallengetestogctm =.
	
}

ren v2 programstageinstance


rename (event programstageinstance programstage eventdate longitude latitude ///
organisationunitname organisationunitcode organisationunit ///
identificationdocumentnumber anc_gestationalageatvisitweeks ///
labwherewerethetestsperformed labtestsperformedatotherspecifie ///
ancorppcvisit labcbcwhitebloodcells labcbcredbloodcellcount ///
labcbchemoglobin labanemiatreatmentresponse labcbchematocrit ///
labcbcmeancorpuscularvolume labcbcplatelets labbloodgrouping ///
labrhdtyping labindirectcoombs laburinestickprotein laburinesticksugar ///
ancurineanalysisforurinarytracki labsecondopiniononinitiatedtreat ///
labrandombloodsugarmgdl labfastingbloodsugarmgdl ///
laboralglucosechallengetestogctm ancotherlabtest1 ///
anclabresultofotherlabtest1 ancotherlabtest2 anclabresultofotherlabtest2 ///
ancotherlabtest3 anclabresultofotherlabtest3) (labevent uniqueid labprogstage ///
labdate lablong lablat laborgname laborgcode laborgunit labidnumber ///
labgestage labplace labplacespec labanpp labwbc labrbc labhb labanemresp ///
labhct labmcv labplatelets labbloodgp labrh labict laburpro laburglu ///
laburuti laburutirep labbloodglu labfastbloodglu labogct labother1 labotherres1 ///
labother2 labotherres2 labother3 labotherres3)

*Label variables 
label variable labevent "System generated additional identifier"
label variable uniqueid "System generated identifier common to all datasets"
label variable labprogstage "Name of anc module"
label variable labdate "Date- lab test"
label variable lablong "Longitude coordinates of clinic- lab test"
label variable lablat "Latitude coordinates of clinic- lab test"
label variable laborgname "Name of clinic- lab test"
label variable laborgcode "System generated code of clinic"
label variable laborgunit "System generated additional code of clinic"
label variable labidnumber "Woman's ID number"
label variable labgestage "Gestational age at visit- lab test"
label variable labplace "Laboratory in which test is performed- lab test"
label variable labplacespec "Laboratory in which test is performed specified- lab test"
label variable labanpp "Does the lab test correspond to anc visit or ppc visit"
label variable labwbc "White blood cell count- lab test"
label variable labrbc "Red blood cell count- lab test"
label variable labhb "Hemoglobin- lab test"
label variable labanemresp "Hemoglobin to measure response to anemia treatment- lab test"
label variable labhct "Hematocrit- lab test"
label variable labmcv "Mean corpuscular volume- lab test"
label variable labplatelets "Platelet count- lab test"
label variable labbloodgp "Blood grouping- lab test"
label variable labrh "Rh typing- lab test"
label variable labict "Indirect Coomb's test- lab test"
label variable laburpro "Urine stick protein- lab test" 
label variable laburglu "Urine stick glucose- lab test"
label variable laburuti "Urine analysis for urinary tract infection- lab test"
label variable laburutirep "Urine analysis repeat for follow-up of urinary tract infection- lab test"
label variable labbloodglu "Random blood sugar- lab test"
label variable labfastbloodglu "Fasting blood sugar- lab test"
label variable labogct "Oral glucose challenge test- lab test"
label variable labother1 "Other lab test 1- lab test"
label variable labotherres1 "Result of other lab test 1- lab test"
label variable labother2 "Other lab test 2- lab test"
label variable labotherres2 "Result of other lab test 2- lab test"
label variable labother3 "Other lab test 3- lab test"
label variable labotherres3 "Result of other lab test 3- lab test"

count
joinby uniqueid using "~/My Documents/trial_temp_data/bookevent_ids.dta", unm(b)
count

tab _merge
drop if _merge==2 // dropping ids
drop _merge

foreach X in "labdate" {
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
drop labdate
ren newlabdate labdate

count if labdate < newbookdatecorrect1
count if labdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

**
gen bookevent = bookevent1
foreach X of varlist bookevent* {
	if("`X'"=="bookevent"){
		continue
	}
	di "`X'"
	local XVAL=subinstr("`X'","bookevent","",.)
	replace bookevent = bookevent`XVAL' if labdate >= newbookdatecorrect`XVAL'
	drop bookevent`XVAL'
}

drop newbookdatecorrect*
**

* LAB STUFF
save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical Lab results.dta", replace

sort uniqueid bookevent labdate
//duplicates drop uniqueid bookevent labdate, force
bysort uniqueid bookevent: gen eventnumber=_n

codebook uniqueid
if($IS_CONTROL==0){
	reshape wide lab*, i(uniqueid bookevent) j(eventnumber)
}
else {
	reshape wide lab* us*, i(uniqueid bookevent) j(eventnumber)
}
codebook uniqueid

save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Lab results.dta", replace
