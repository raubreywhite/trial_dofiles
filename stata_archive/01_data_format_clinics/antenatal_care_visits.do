if($IS_CONTROL==1){
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
	capture ren v2 programstageinstance
	sort programstageinstance eventdate
	bysort programstageinstance (eventdate): gen visitnum=_n
	keep if visitnum!=1
	drop visitnum
	
	gen identificationdocumentnumber=""
	
	gen ancallergiesdrugsandorseverefood=.
	gen ancpenicillinallergy=.
	gen anchistoryofothermedicineallergy=.
	gen ancothermedisineallergyspecified=.
	gen anchistroyofothersevereallergy=.
	gen ancothersevereallergyspecified=.
	gen anchistoryofchronichypertension=.
	gen anchistoryofotherchronicconditio=.
	gen anchistoryofblooddisorder =.
	gen ancfetalmovement=.
	gen  anchypothyreoidism =.
	//gen ancintendedplaceofbirth=.
	//gen ancdateoflastdelivery =.
	//gen ancrecommendedplaceofbirth =.
	//gen anctetanusboosterdose =.
	gen anchistoryofblooddisorderspecif =.
	gen ancfundalheightmeasurement =.
	gen ancedema =.
	//gen ancutifollowupscheduled =.
	gen ancreferralneededforotherchronic =.
	gen anchistoryofdeepveinthrombosisdv =.
	gen whichancvisitisthis=.
	gen  ancmedicineprescription =.
	//gen v81=.
	gen anchighriskdesignatedwoman =.
	gen anccounselingaboutironandfolicac =.
	
	//gen ancmchhandbookprovided=.
	gen ancpreviousepisodesofthrombosis=.
	gen anchistoryofklexaneprovidedtothe=.
	gen anccounselingaboutdangersignsdur=.
	gen anccounselingonnutrioninpregnanc=.
	gen anccounselingaboutbreastfeeding=.
	gen anccounselingaboutlaborsigns=.
	gen anccounselingaboutpkuscreening=.
	gen ancppcvisitundertakenbywhom=.
	gen ppcwasthisinformationfirstcollec =.
	
	
	gen ancsuspectedprematureruptureofme=.
	gen ancsuspectedpretermprematurerupt =.
	gen  anceclampticconvulsions =.
	gen ancvaginalbleeding =.
	gen anchypertensioncerebralorvisuals =.
	gen  anc_gestationalageatvisitweeks =.
	gen  ancotheridentifiedconditions =.
	gen anchistoryofdiabetesmellitusiori=.
	gen anchistoryofrenaldisease =.
	gen anchistoryofbronchialasthma=.
	gen anchistoryofepilepsy =.
	gen anchistoryofcardiacdisease =.
	gen anchistoryofmentaldisturbance =.
	gen ancreproductivetractinfectionrti =.
	gen v42=.
	gen  anchomevisitoratthehealthclinic =.
	
	// in intervention, this is just a booking variable
	drop ancvaccinatedforttaccordingtogui
	
}
else {
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Antenatal care visit.csv", varnames(1) encoding("UTF-8") clear
}

capture ren v2 programstageinstance

*Rename variables
rename (event programstageinstance programstage eventdate longitude latitude ///
organisationunitname organisationunitcode organisationunit ///
identificationdocumentnumber ancsuspectedprematureruptureofme ///
ancsuspectedpretermprematurerupt anceclampticconvulsions ///
ancvaginalbleeding ancfetalmovement ancbodyweightkg ///
ancdiastolicbloodpressuremmhg ancsystolicbloodpressuremmhg ///
ancedema ancfetalheartsoundfhs ancfundalheightmeasurement ///
anchypertensioncerebralorvisuals ancfetalpresentationcheckedbypal ///
whichancvisitisthis ancmedicineprescription anc_gestationalageatvisitweeks ///
anchighriskdesignatedwoman ancotheridentifiedconditions ///
anchistoryofchronichypertension anchistoryofdiabetesmellitusiori ///
anchistoryofrenaldisease anchypothyreoidism anchistoryofbronchialasthma ///
anchistoryofblooddisorder anchistoryofblooddisorderspecif ///
anchistoryofepilepsy anchistoryofcardiacdisease anchistoryofmentaldisturbance ///
ancreproductivetractinfectionrti anchistoryofdeepveinthrombosisdv ///
anchistoryofotherchronicconditio v42 ancreferralneededforotherchronic ///
ancallergiesdrugsandorseverefood anchistoryofothermedicineallergy ///
ancothermedisineallergyspecified anchistroyofothersevereallergy ///
ancothersevereallergyspecified ancpenicillinallergy ///
ancothermedicationsthewomaniscur anccounselingaboutironandfolicac ///
ancpreviousepisodesofthrombosis anchistoryofklexaneprovidedtothe ///
anccounselingaboutlaborsigns anccounselingaboutpkuscreening ///
anccounselingaboutdangersignsdur anccounselingonnutrioninpregnanc ///
anchomevisitoratthehealthclinic ppcwasthisinformationfirstcollec ///
ancppcvisitundertakenbywhom) (anevent uniqueid anprogstage andate ///
anlong anlat anorgname anorgcode anorgunit anidnumber anprom anpprom ///
aneclamp anvagbleed anfetalmove  anweight anbpdiast ///
anbpsyst anexamedema anexamfh anexamsfh anhisthtnsymp ///
anexampalp anvisitweeks anmedpres angestage anhighrisk anothercond ///
anhisthtn anhistdm anhistrd anhisthypothyr anhistasthma ///
anhistblood anhistbloodspec anhistepi anhistcardiac ///
anhistpsy anhistrti anhistdvt anhistchronic anhistchronicspec ///
anrefchronic anallerfood anallerdrug anallerdrugspec anallersev ///
anallersevspec anallerpen anothermed ancounsifa anhistthr anchistclex ///
ancounslabor ancounspku ancounsdanger ancounsnut anhomeorclinic anbackupfile ///
anseenby)

*Label variables 
label variable anevent "System generated additional identifier"
label variable uniqueid "System generated identifier common to all datasets" 
label variable anprogstage "Name of anc module"
label variable andate "Date- antenatal visit"
label variable anlong "Longitude coordinates of clinic- antenatal visit"
label variable anlat "Latitude coordinates of clinic- antenatal visit"
label variable anorgname "Name of clinic- antenatal visit"
label variable anorgcode "System generated code of clinic"
label variable anorgunit "System generated additional code of clinic"
label variable anidnumber "Woman's ID number"
label variable anprom "Premature rupture of membranes- antenatal visit"
label variable anpprom "Preterm premature rupture of membranes- antenatal visit"
label variable aneclamp "Eclampsia in current pregnancy- antenatal visit"
label variable anvagbleed "Vaginal bleeding in current pregnancy- antenatal visit"
label variable anfetalmove  "Fetal movements present or absent- antenatal visit"
label variable anweight "Woman's weight- antenatal visit"
label variable anbpdiast "Diastolic blood pressure- antenatal visit"
label variable anbpsyst "Systolic blood pressure- antenatal visit"
label variable anexamedema "Edema examination- antenatal visit"
label variable anexamfh "Fetal heart sounds examination- antenatal visit"
label variable anexamsfh "Symphysiofundal height measurement- antenatal visit"
label variable anhisthtnsymp "Woman complaints of cerebral or visual symptoms suggestive of high blood pressure- antenatal visit"
label variable anexampalp "Palpation to check for presentation- antenatal visit" 
label variable anvisitweeks "Week of the antenatal care visit- antenatal visit"
label variable anmedpres "Medicine prescription- booking visit"
label variable angestage "Gestational age at visit- antenatal visit"
label variable anhighrisk "High risk pregnancy or not- antenatal visit"
label variable anothercond "Other conditions identified- antenatal visit"
label variable anhisthtn "History of chronic hypertension- antenatal visit"
label variable anhistdm "History of diabetes mellitus- antenatal visit"
label variable anhistrd "History of renal disease- antenatal visit"
label variable anhisthypothyr "History of hypothryroidism- antenatal visit"
label variable anhistasthma "History of bronchial asthma- antenatal visit"
label variable anhistblood "History of blood disorders- antenatal visit"
label variable anhistbloodspec "History of blood disorders specified- antenatal visit"
label variable anhistepi "History of epilepsy- antenatal visit"
label variable anhistcardiac "History of cardiac disease- antenatal visit"
label variable anhistpsy "History of mental illness- antenatal visit"
label variable anhistrti "History of reproductive tract infections- antenatal visit"
label variable anhistdvt "History of deep vein thrombosis- antenatal visit"
label variable anhistchronic "History of chronic conditions- antenatal visit"
label variable anhistchronicspec "History of chronic conditions specified- antenatal visit"
label variable anrefchronic "Referral for chronic conditions- antenatal visit"
label variable anallerfood "Allergy to food- antenatal visit"
label variable anallerdrug "Allergy to other drugs- antenatal visit"
label variable anallerdrugspec "Allergy to other drug specified- antenatal visit"
label variable anallersev "Other severe allergy- antenatal visit"
label variable anallersevspec "Other severe allergy specified- antenatal visit"
label variable anallerpen "Allergy to penicillin- antenatal visit"
label variable anothermed "Other medications woman is taking- antenatal visit"
label variable ancounsifa "Counseling for iron and folic acid tablets- antenatal visit"
label variable anhistthr "History of episodes of thrombosis- antenatal visit"
label variable anchistclex "History of clexane use- antenatal visit"
label variable ancounslabor "Counseling for signs and symptoms of labor- antenatal visit"
label variable ancounspku "Counseling for PKU screening- antenatal visit"
label variable ancounsdanger "Counseling for danger signs- antenatal visit"
label variable ancounsnut "Counseling for nutrition and food intake- antenatal visit"
label variable anhomeorclinic "Home visit or clinic visit- antenatal visit"
label variable anbackupfile "Information first entered on paper and then into the eRegistry- antenatal visit"
label variable anseenby "Woman seen by- antenatal visit"
display "MAYBE WRONG 1"
count
joinby uniqueid using "~/My Documents/trial_temp_data/bookevent_ids.dta", unm(b)
count
display "MAYBE WRONG 2"
tab _merge
drop if _merge==2 // dropping ids
drop _merge

foreach X in "andate" {
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
display "MAYBE WRONG 3"
drop andate
ren newandate andate
display "MAYBE WRONG 4"
count if andate < newbookdatecorrect1
count if andate < newbookdatecorrect1 & !missing(newbookdatecorrect1)


**
gen bookevent = bookevent1
foreach X of varlist bookevent* {
	if("`X'"=="bookevent"){
		continue
	}
	di "`X'"
	local XVAL=subinstr("`X'","bookevent","",.)
	replace bookevent = bookevent`XVAL' if andate >= newbookdatecorrect`XVAL'
	drop bookevent`XVAL'
}

drop newbookdatecorrect*
**

count
joinby uniqueid bookevent using "~/My Documents/trial_temp_data/booklmp.dta", unm(b)
count

tab _merge
drop if _merge==2
drop _merge

gen angestationalage = andate - newbooklmpcorrect
drop newbooklmpcorrect


save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", replace

count

sort uniqueid bookevent andate
//duplicates drop uniqueid bookevent andate, force
bysort uniqueid bookevent: gen eventnumber=_n

ren anccounselingaboutbreastfeeding anccounselingaboutbf

 

codebook uniqueid
if($IS_CONTROL==1){
	ren con_anc_gestationaageatvisitweek con_anc_gestageatvisitweek
	ren con_anc_gestationaageatvisitsize con_anc_gestageatvisitsize
	reshape wide an* con* usrecommendationscomments, i(uniqueid bookevent) j(eventnumber)
}
else {
	reshape wide an*, i(uniqueid bookevent) j(eventnumber)
}
codebook uniqueid

save "~/My Documents/trial_temp_data/wide_IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", replace
count
