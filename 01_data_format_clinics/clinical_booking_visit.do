if($IS_CONTROL==1){

	/*
		Read in ANC followup
		Keep the first observation per pregnancy
	*/
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
	capture ren v2 programstageinstance
	sort programstageinstance eventdate
	bysort programstageinstance (eventdate): gen visitnum=_n
	keep if visitnum==1
	drop visitnum
	tempfile firstbookingvisit
	save `firstbookingvisit'
	
	/*
		Merge in the first ANC observation so that this file
		looks the same as the interventiond data
	*/
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Green File.csv", varnames(1) encoding("UTF-8") clear
	capture ren v2 programstageinstance
	
	count
	joinby programstageinstance using `firstbookingvisit', unm(b)
	count
	
	tab _merge
	keep if _merge==1 | _merge==3
	drop _merge
	
	
	
	
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
	gen ancintendedplaceofbirth=.
	gen ancdateoflastdelivery =.
	gen ancrecommendedplaceofbirth =.
	gen anctetanusboosterdose =.
	gen anchistoryofblooddisorderspecif =.
	gen ancfundalheightmeasurement =.
	gen ancedema =.
	gen ancutifollowupscheduled =.
	gen ancreferralneededforotherchronic =.
	gen anchistoryofdeepveinthrombosisdv =.
	gen whichancvisitisthis=.
	gen  ancmedicineprescription =.
	gen v81=.
	gen anchighriskdesignatedwoman =.
	gen anccounselingaboutironandfolicac =.
	
	gen ancmchhandbookprovided=.
	gen ancpreviousepisodesofthrombosis=.
	gen anchistoryofklexaneprovidedtothe=.
	gen anccounselingaboutdangersignsdur=.
	gen anccounselingonnutrioninpregnanc=.
	gen anccounselingaboutbreastfeeding=.
	gen anccounselingaboutlaborsigns=.
	gen anccounselingaboutpkuscreening=.
	gen ancppcvisitundertakenbywhom=.
	gen ppcwasthisinformationfirstcollec =.
	
	capture tostring anclmpstatus, replace
	capture tostring ancotherfamilyconcernspecified, replace
	capture tostring ancpallor, replace
	
}
else {
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Booking Visit.csv", varnames(1) encoding("UTF-8") clear
}

capture ren v2 programstageinstance 
// delete people with duplicate eventdates (these are obvious duplicates)
duplicates drop programstageinstance eventdate, force

// delete people with duplicae LMPs (again, duplicate pregnancies(
duplicates drop programstageinstance anclmpdate, force

duplicates tag programstageinstance, gen("dup")
//bro programstageinstance eventdate anclmpdate if dup>0



*Rename variables
rename (event programstageinstance programstage eventdate longitude latitude ///
organisationunitname organisationunitcode organisationunit ///
identificationdocumentnumber ancdiastolicbloodpressuremmhg ///
anceclampticconvulsions ancpallor anclmpdate ///
ancsuspectedpretermprematurerupt ancsuspectedprematureruptureofme ///
ancsystolicbloodpressuremmhg ancallergiesdrugsandorseverefood ///
ancpenicillinallergy anchistoryofothermedicineallergy ///
ancothermedisineallergyspecified anchistroyofothersevereallergy ///
ancothersevereallergyspecified anchistoryofchronichypertension ///
anchistoryofdiabetesmellitusiori anchistoryofrenaldisease ///
anchistoryofotherchronicconditio anchistoryofblooddisorder ///
ancbodyheightm ancbodyweightkg ancmedicalexaminationofbreastsan ///
anchistoryofbronchialasthma anchistoryofcardiacdisease ///
ancconsecutiveabortions anchistoryofepilepsy ancfamilyhistoryofblooddisease ///
ancfamilyhistoryofbronchialastma ancfamilyhistoryofcardiacdisease ///
ancfamilyhistoryofcongenitalanom ancfamilyhistoryofdiabetesmellit ///
ancfamilyhistoryofhypertension ancfamilyhistoryofinbornerrorofm ///
ancfamilyhistoryofrenaldisease ancfetalmovement anchypothyreoidism ///
ancintendedplaceofbirth ancdateoflastdelivery anchistoryofmentaldisturbance ///
anchistoryofgestationaldiabetesm anchistoryofanypreviousperinatal ///
ancreproductivetractinfectionrti anchistoryofpostpartumhemorrhage ///
ancrecommendedplaceofbirth anctetanusboosterdose ///
ancvaccinatedforttaccordingtogui anchistoryofuterineanomalyorinju ///
anchistoryofuterinesurgeryexclud ancvaginalbleeding ///
anchistoryofblooddisorderspecif anchistoryofcsections2 ///
ancfundalheightmeasurement ancmedicalexaminationofabdomen ///
ancmedicalexaminationofheart ancmedicalexaminationoflowerlimb ///
ancmedicalexaminationoflungs ancmedicalexaminationofheadandne ///
ancedema anclmpstatus ancinvitrofertilizationivf ///
ancinfertility1yearpriortocurren ancutifollowupscheduled ///
ancreferralneededforotherchronic anchistoryofeclampsiainanyprevio ///
anchistoryofdeepveinthrombosisdv anchypertensioncerebralorvisuals ///
ancfetalpresentationcheckedbypal ancfetalheartsoundfhs whichancvisitisthis ///
ancprimigravidapg ancmedicineprescription v81 ///
ancabnormalfindingsofheadandneck ///
ancabnormalfindingsofabdomenspec ancabnormalfindingsofbreastsando ///
ancabnormalfindingsoflowerlimbs ancabnormalfindingsoflungs ///
ancabnormalfindingsofheart ancothermedicationsthewomaniscur ///
anchistoryofmultiparity anchistoryofanypreviouspretermbi ///
anchistoryofdvtrelatedtoanypre anchistoryofantepartumhemorrhag ///
anchistoryofgestationalhypertent anchistoryofpreeclampsiainanypre ///
anchistoryofpuerperalsepsisinany anc_gestationalageatvisitweeks ///
anchighriskdesignatedwoman anchistoryofanycomplicatedcsecti ///
anccounselingaboutironandfolicac ancmchhandbookprovided ///
ancpreviousepisodesofthrombosis anchistoryofklexaneprovidedtothe ///
anccounselingaboutdangersignsdur anccounselingonnutrioninpregnanc ///
anccounselingaboutbreastfeeding anccounselingaboutlaborsigns ///
anccounselingaboutpkuscreening ancotherrelevantfamilyhistorycon ///
ancotherfamilyconcernspecified ancppcvisitundertakenbywhom ///
anchomevisitoratthehealthclinic ppcwasthisinformationfirstcollec) ///
(bookevent uniqueid bookprogstage bookdate booklong booklat bookorgname ///
bookorgcode bookorgunit bookidnumber bookbpdiast bookeclamp bookpallor ///
booklmp bookpprom bookprom bookbpsyst bookallerfood ///
bookallerpen bookallerdrug bookallerdrugspec bookallersev bookallersevspec ///
bookhisthtn bookhistdm bookhistrd bookhistotherch bookhistblood ///
bookheight bookweight bookexambr bookhistasthma ///
bookhistcard bookhistabort bookhistepi bookfamblood bookfamasthma ///
bookfamcardiac bookfamcong bookfamdm bookfamhtn bookfammeta bookfamrd ///
bookfetalmove bookhisthypothyr bookintendbirth bookdatelastbirth bookhistpsy ///
bookhistgdm bookhistperi bookhistrti bookhistpph bookrecobirth bookttdose ///
bookttguide bookhistute bookhistutesur bookvagbleed ///
bookhistbloodspec bookhistcs bookexamsfh bookexamabd bookexamheart ///
bookexamlimb bookexamlung bookexamhead bookexamedema booklmpknown bookhistivf ///
bookhistinfert bookutifollow bookrefchronic ///
bookhisteclamp bookhistdvt bookhisthtnsymp bookexampalp bookexamfh bookvisitspec ///
bookprimi bookmedpres bookhistotherchronic bookexamheadabn bookexamabdabn ///
bookexambreastabn bookexamlimbabn bookexamlungabn bookexamheartabn ///
bookhistmed bookparity bookhistpreterm ///
bookhistprevdvt bookhistaph bookhistghtn bookhistpreecl bookhistpuersep ///
bookgestage bookhighrisk bookhistcscompl bookcounsifa bookancbook ///
bookhistthrom bookhistclex bookcounsdanger ///
bookcounsnut bookcounsbf bookcounslabor bookcounspku bookcounsfamhist bookfamspec ///
bookseenby bookhomeorclinic bookbackupfile) 


*Label variables 
label variable bookevent "System generated additional identifier"
label variable uniqueid "System generated identifier common to all datasets"
label variable bookprogstage "Name of anc module" 
label variable bookdate "Date- booking visit"
label variable booklong "Longitude coordinates of clinic- booking visit"
label variable booklat "Latitude coordinates of clinic- booking visit"
label variable bookorgname "Name of clinic- booking visit"
label variable bookorgcode "System generated code of clinic"
label variable bookorgunit "System generated additional code of clinic"
label variable bookidnumber "Woman's ID number"
label variable bookbpdiast "Diastolic blood pressure- booking visit"
label variable bookeclamp "Eclampsia in current pregnancy- booking visit"
label variable bookpallor "Pallor on examination- booking visit"
label variable booklmp "Last menstrual period date"
label variable bookpprom "Preterm premature rupture of membranes in current pregnancy- booking visit"
label variable bookprom "Preterm rupture of membranes in current pregnancy- booking visit"
label variable bookbpsyst "Systolic blood pressure- booking visit"
label variable bookallerfood "Allergy to food- booking visit"
label variable bookallerpen "Allergy to penicillin- booking visit"
label variable bookallerdrug "Allergy to other drugs- booking visit"
label variable bookallerdrugspec "Allergy to other drug specified- booking visit"
label variable bookallersev "Other severe allergy- booking visit"
label variable bookallersevspec "Other severe allergy specified- booking visit"
label variable bookhisthtn "History of chronic hypertension- booking visit"
label variable bookhistdm "History of diabetes mellitus- booking visit" 
label variable bookhistrd "History of renal disease- booking visit"
label variable bookhistotherch "History of other chronic conditions- booking visit"
label variable bookhistblood "History of blood disorders- booking visit"
label variable bookheight "Height of woman- booking visit"
label variable bookweight "Weight of woman- booking visit"
label variable bookexambr "Examination of breast- booking visit"
label variable bookhistasthma "History of bronchial asthma- booking visit"
label variable bookhistcard "History of cardiac disease- booking visit"
label variable bookhistabort "History of recurrent abortions- booking visit"
label variable bookhistepi "History of epilepsy- booking visit"
label variable bookfamblood "Family history of blood disorder- booking visit"
label variable bookfamasthma "Family history of bronchial asthma- booking visit"
label variable bookfamcardiac "Family history of cardiac disease- booking visit"
label variable bookfamcong "Family history of congenital anomalies- booking visit"
label variable bookfamdm "Family history of diabetes mellitus- booking visit"
label variable bookfamhtn "Family history of hypertension- booking visit"
label variable bookfammeta "Family history of inborn errors of metabolism- booking visit"
label variable bookfamrd "Family history of renal disease- booking visit"
label variable bookfetalmove "Fetal movements present or absent- booking visit"
label variable bookhisthypothyr "History of hypothyroidism- booking visit"
label variable bookintendbirth "Intended place of delivery- booking visit"
label variable bookdatelastbirth "Most recent delivery date"
label variable bookhistpsy "History of mental illness- booking visit"
label variable bookhistgdm "History of gestational diabetes mellitus- booking visit"
label variable bookhistperi "History of previous perinatal mortality- booking visit"
label variable bookhistrti "History of reproductive tract infection- booking visit"
label variable bookhistpph "History of postpartum hemorrhage"
label variable bookrecobirth "Recommended place of delivery- booking visit"
label variable bookttdose "Tetanus toxoid booster dose- booking visit"
label variable bookttguide "Tetanus toxoid vaccinations given according to guidelines- booking visit"
label variable bookhistute "History of uterine anomaly or injury- booking visit"
label variable bookhistutesur "History of uterine surgery- booking visit"
label variable bookvagbleed "Vaginal bleeding- booking visit"
label variable bookhistbloodspec "History of blood disorder specified- booking visit"
label variable bookhistcs "History of Caesarean section- booking visit"
label variable bookexamsfh "Symphysiofundal height measurement- booking visit"
label variable bookexamabd "Abdomen examination- booking visit"
label variable bookexamheart "Heart examination- booking visit"
label variable bookexamlimb "Lower limb examination- booking visit"
label variable bookexamlung "Lung examination- booking visit"
label variable bookexamhead "Head and neck examination- booking visit"
label variable bookexamedema "Edema examination- booking visit"
label variable booklmpknown "Last menstrual period date known or not- booking visit"
label variable bookhistivf "Conception by in-vitro fertilization- booking visit"
label variable bookhistinfert "History of infertility- booking visit"
label variable bookutifollow "Follow-up scheduled for urinary tract infections- booking visit"
label variable bookrefchronic "Woman referred for chronic conditions- booking visit"
label variable bookhisteclamp "History of eclampsia in previous pregnancies- booking visit"
label variable bookhistdvt "History of deep vein thrombosis in previous pregnancies- booking visit"
label variable bookhisthtnsymp "Woman complaints of cerebral or visual symptoms suggestive of high blood pressure- booking visit"
label variable bookexampalp "Palpation to check for presentation- booking visit"
label variable bookexamfh "Fetal heart sound examination- booking visit"
label variable bookvisitspec "Name of the anc module- booking visit"
label variable bookprimi "Is this the first pregnancy- booking visit"
label variable bookmedpres "Medicine prescriptions- booking visit"
label variable bookhistotherchronic "History of other chronic conditions- booking visit"
label variable bookexamheadabn "Head and neck examination abnormal- booking visit"
label variable bookexamabdabn "Abdomen examination abnormal- booking visit"
label variable bookexambreastabn "Breast examination abnormal- booking visit"
label variable bookexamlimbabn "Lower limb examination abnormal- booking visit"
label variable bookexamlungabn "Lung examination abnormal- booking visit"
label variable bookexamheartabn "Heart examination abnormal- booking visit"
label variable bookhistmed "History of taking medications- booking visit"
label variable bookparity "Parity of the woman- booking visit"
label variable bookhistpreterm "History of preterm labor in previous pregnancies- booking visit"
label variable bookhistprevdvt "History of deep vein thrombosis in previous pregnancies- booking visit"
label variable bookhistaph "History of antepartum hemorrhage in previous pregnancies- booking visit"
label variable bookhistghtn "History of gestational hypertension in previous pregnancies- booking visit"
label variable bookhistpreecl "History of preeclampsia in previous pregnancies- booking visit"
label variable bookhistpuersep "History of peurperal sepsis in previous pregnancies- booking visit"
label variable bookgestage "Gestational age at visit- booking visit"
label variable bookhighrisk "High risk condition detected- booking visit"
label variable bookhistcscompl "History of complicated Caesarean sections- booking visit"
label variable bookcounsifa "Counseling for iron and folic acid tablets- booking visit"
label variable bookancbook "MCH handbook given- booking visit"
label variable bookhistthrom "History of episodes of thrombosis- booking visit"
label variable bookhistclex "History of clexane use- booking visit"
label variable bookcounsdanger "Counseling for danger signs- booking visit"
label variable bookcounsnut "Counseling for nutrition and food intake- booking visit"
label variable bookcounsbf "Counseling for breast feeding- booking visit"
label variable bookcounslabor "Counseling for signs and symptoms of labor- booking visit"
label variable bookcounspku "Counseling for PKU screening- booking visit"
label variable bookcounsfamhist "Counseling for family history of conditions- booking visit"
label variable bookfamspec "Family history of specific conditions- booking visit"
label variable bookseenby "Woman seen by- booking visit"
label variable bookhomeorclinic "Home visit or clinic visit- booking visit"
label variable bookbackupfile "Information first entered on paper and then into the eRegistry- booking visit"

// Fix wrong bookorgnames
replace bookorgname="(Al.Nasaria) النصارية" if bookorgname=="(Al-nsaryeh) النصاريه"

/*
*Create a small dataset with only uniqueid and date of booking visit
keep uniqueid bookdate

*Save as "int_anc booking visit_small dataset to get mother's age"
save "data_temp/int_anc booking visit_small.dta", replace

joinby uniqueid 
*/
*Open the saved dataset

count // how many observations do we have at the start
joinby uniqueid using "~/My Documents/trial_temp_data/$TAG Clinical Demographics.dta", unm(b)
count // how many obs do we have after merging
 
 
tab _merge // see where the obs came from
tab _merge, nol
//keep if _merge==3 // keep all people. people just in demographic file and not in green file are abortions
drop _merge

/*
	for the abortions, we now have "day of showing up and getting registered in demographic file"
	as "day of booking" (ie bookdate)
*/

gen is_demof_not_greenf_or_bookf=1 if missing(bookevent)
sort bookevent
gen temp=_n
tostring temp, replace force
replace temp="$TAG"+temp
replace bookdate=datecreated if missing(bookevent)
replace bookevent=temp if missing(bookevent)
drop temp

// drop women whose 2nd, 3rd, etc pregnancies have LMPs before the first pregnancy's booking date
sort uniqueid bookdate
//bysort uniqueid: gen pregnum=_n
forvalues X=1/10 {
	capture drop booking_number
	bysort uniqueid (bookdate): gen min_event_date=bookdate if _n==`X' // makes new variabe of woman's first event
	replace min_event_date=min_event_date[_n-1] if missing(min_event_date) // fills down empty spaces	

	bysort uniqueid (bookdate): gen booking_number=_n
	drop if booklmp < min_event_date & !missing(booklmp) & booking_number>`X'
	drop min_event_date
}

di 1
**Create a new variable for gestational age**

*Convert dates into days// Dates are already formatted right in these datasets//
foreach X in "bookdate" "booklmp" "dob" {
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
di 2
ren newdob newdobcorrect
ren newbookdate newbookdatecorrect
ren newbooklmp newbooklmpcorrect
di 
// create unique set of ids so tht we know if it is first booking or second booking
preserve
keep bookevent uniqueid newbookdate booking_number
reshape wide bookevent newbookdate, i(uniqueid) j(booking_number)
save "~/My Documents/trial_temp_data/bookevent_ids.dta", replace
count
count
count
count

restore



*Generate an intermediate variable- difference between lmp and date of visit
generate bookgestageinter= newbookdatecorrect-newbooklmpcorrect
//Gives gestational age in days//

*Calculate EDD based on LMP
generate expecteddateofdelivery = newbooklmpcorrect + 280

gen age1 = newbookdatecorrect  - newdobcorrect
summarize age1

gen age = floor(age1/365)
summarize age

gen agecat = .
summarize agecat

///missing values////

replace agecat = 1 if (age <=20)
replace agecat = 2 if (age >=21) & (age <=25)
replace agecat = 3 if (age >=26) & (age <=30)
replace agecat = 4 if (age >=31) & (age <=35)
replace agecat = 5 if (age >=36) & (age <=40)
replace agecat = 6 if (age >=41) & (age <=45)
replace agecat = 7 if (age >=46) & (age <=50)
replace agecat = 8 if (age >50)

count if missing(age)
count if missing(agecat)

*Describe data 
describe

save "~/My Documents/trial_temp_data/IDENTIFIABLE $TAG ANC demographics and booking.dta", replace
count
codebook uniqueid

keep uniqueid bookevent newbooklmpcorrect 
duplicates drop
save "~/My Documents/trial_temp_data/booklmp.dta",replace
