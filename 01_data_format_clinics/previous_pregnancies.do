
*Open ultrasound .dta file 
if($IS_CONTROL==1){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Previous pregnancy table.csv", varnames(1) encoding("UTF-8") clear
	gen identificationdocumentnumber =""
}
else if($IS_CONTROL==0){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Previous pregnancies.csv", varnames(1) encoding("UTF-8") clear
}

capture ren v2 programstageinstance

*Open 'previous pregnancies' dataset
rename (event programstageinstance programstage eventdate longitude latitude ///
organisationunitname organisationunitcode organisationunit ///
identificationdocumentnumber ancpreviouspregnancygestation ///
ancpreviouspregnancyoutcome previousplaceofbirth ancmodeofpreviousdelivery ///
ancgenderofchildfrompreviou ancpreviouspregnancybirthweig ///
anchistoryofgestationaldiabe anchistoryofgestationalhyper ///
anchistoryofpreeclampsiainp anchistoryofeclampsiainprev ///
anchistoryofpuerperalsepsis anchistoryofantepartumhemorr ///
ancpostpartumhemorrhageinpre anchistoryofdvtinpreviousp ///
previouscomplicationsnone)(prevevent uniqueid prevprogstage prevdate prevlong ///
prevlat prevorgname prevorgcode prevorgunit previdnumber prevgestagebirth ///
prevoutcome prevbirthplace prevmodedelivery prevgender prevbirthweight ///
prevgdm prevhtn prevpreeclampsia preveclampsia prevpuersep prevaph ///
prevpph prevdvt prevnocompl)


*Label variables 
label variable prevevent "System generate additional identifier"
label variable uniqueid "System generated identifier common to all datasets"
label variable prevprogstage "Name of anc module"
label variable prevdate "Date- previous pregnancy"
label variable prevlong "Longitude coordinates of clinic- previous pregnancy"
label variable prevlat "Latitude coordinates of clinic- previous pregnancy"
label variable prevorgname "Name of clinic- previous pregnancy"
label variable prevorgcode "System generated code of clinic"
label variable prevorgunit "System generated additional code of clinic"
label variable previdnumber "Woman's ID number"
label variable prevgestagebirth "Gestational age at birth- previous pregnancy"
label variable prevoutcome "Outcome of pregnancy- previous pregnancy"
label variable prevbirthplace "Place of birth- previous pregnancy"
label variable prevmodedelivery "Mode of delivery- previous pregnancy"
label variable prevgender "Gender of baby- previous pregnancy"
label variable prevbirthweight "Birth weight- previous pregnancy"
label variable prevgdm "Gestational diabetes- previous pregnancy"
label variable prevhtn "Gestational hypertension- previous pregnancy"
label variable prevpreeclampsia "Preeclampsia- previous pregnancy"
label variable preveclampsia "Eclampsia- previous pregnancy"
label variable prevpuersep "Peurperal sepsis- previous pregnancy"
label variable prevaph "Antepartum hemorrhage- previous pregnancy"
label variable prevpph "Postpartum hemorrhage- previous pregnancy"
label variable prevdvt "Deep vein thrombosis- previous pregnancy"
label variable prevnocompl "No complications- previous pregnancy"


save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", replace


sort uniqueid prevdate
duplicates drop uniqueid prevdate, force
bysort uniqueid: gen eventnumber=_n

codebook uniqueid
reshape wide prev*, i(uniqueid) j(eventnumber)
codebook uniqueid

save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", replace
