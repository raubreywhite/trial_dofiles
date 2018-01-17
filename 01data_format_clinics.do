clear
capture set maxvar 30000

*Open booking .dta file 
capture cd "Z:\data processing\"
capture cd "X:\data processing\"

**Intervention clinics data_comprehensive version


*General instructions before start
//Create folders called Datasets, DO files, output and resources 
//Move all datasets for STATA to 'datasets'
//Save all DO files under 'DO files'
//Save all new datasets after cleaning under 'output'
//Save all log files under 'output'


//Name all intervention clinics datasets as 'intervention_filename'
//Name all control clinics datasets as 'control_filename'
//Name all avicenna datasets as 'avicenna_filename'
//Name all combined datasets as 'combined_filename'

**Make sure there is only one sheet per excel file**


**Data processing logic
//1. Remove all identifiable data from the datasets
//2. Replace dates with gestational age
//3. Replace names of clinics with code
//4. Merge files on uniqueid
//5. Check if there are no identifable data left

forvalues IS_CONTROL=1(-1)0 {
	if(`IS_CONTROL'==0){
		global DATA_DATE="$CLINIC_INTERVENTION_DATE"
		global DATA_RAW="data_raw\e.reg-intervention/$DATA_DATE"
		global TAG="INT"
		global CLINICAL_OR_CONTROL="Clinical"
	} 
	else if(`IS_CONTROL'==1){
		global DATA_DATE="$CLINIC_CONTROL_DATE"
		global DATA_RAW="data_raw\e.reg-control/$DATA_DATE"
		global TAG="CON"
		global CLINICAL_OR_CONTROL="Control"
	}
	global IS_CONTROL=`IS_CONTROL'
	
	display "$TAG"


	******************
	*** DEMOGRAPHICS
	****************
	do "trial_dofiles/01_data_format_clinics/demographics.do"

	**************** 
	****CLINICAL BOOKING VISIT
	do "trial_dofiles/01_data_format_clinics/clinical_booking_visit.do"

	*******************
	**** ANTENATAL CARE VISITS
	*Open ANC visits .dta file 
	do "trial_dofiles/01_data_format_clinics/antenatal_care_visits.do"
	
	*******************
	*** LAB STUF
	*******************
	do "trial_dofiles/01_data_format_clinics/lab.do"

	*********************
	** ULTRASOUND STUFF
	*********************
	*Open ultrasound .dta file 
	do "trial_dofiles/01_data_format_clinics/ultrasound.do"
	
	************************
	**** RISK FACTORS
	do "trial_dofiles/01_data_format_clinics/riskfactors.do"

	*********************
	** MANAGEMENTS
	*********************
	do "trial_dofiles/01_data_format_clinics/managements.do"

	*********************
	** PREVIOUS PREGNANCIES
	*********************
	do "trial_dofiles/01_data_format_clinics/previous_pregnancies.do"
	
	*************************************
	*********** HOSPITAL BIRTH OUTCOMES
	do "trial_dofiles/01_data_format_clinics/hospital_birth_outcomes.do"
	
	*************************************
	*********** START MERGING TOGETHER IMPORTANT DATASETS
	do "trial_dofiles/01_data_format_clinics/merging_files.do"
	

}

use "~/My Documents/trial_temp_data/IDENTIFIABLE INT.dta", replace
append using "~/My Documents/trial_temp_data/IDENTIFIABLE CON.dta", force

/* TAMARA IF YOU DECIDE THAT YOU WANT TO PERMANENTLY
REMOVE A BOOKEVENT FROM THE DATASET, THEN PUT IT
IN HERE */

drop if bookevent=="HUxqueKIVVi"
drop if bookevent=="Jcjl6LXjVHG" // <- this is the woman that entered in wrong clinic
drop if bookevent=="GqRyqspR02f"
drop if bookevent=="mXStZu67x12"   
drop if bookevent=="aO6ZqOWIVY2"
drop if bookevent=="icfkS7FaDNg" // entered as a separte file even it exists in pretrial 
drop if bookevent=="B6sGTGIlvBP"

******** added these on december 24, 2017. these are doubles in demogrphic data******
drop if uniqueid=="Hrt6pxBniy9"
drop if uniqueid=="UZ53c6fKhTy"
drop if uniqueid=="ye9fvoVSalO"
drop if uniqueid=="McathozJMdD"
drop if uniqueid=="POisbVSl4J8"


/* TAMARA STOP if bookevent=="HERE */

/* TAMARA HERE YOU LABEL THE CLINICS */

gen is_high_risk_clinic=.
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Bethlehem MCH )امومة بيت لحم حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Hewarah)عيادة حوارة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Bedia) بديا حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(  - Central Health Directorate Salfit) صحة سلفيت"
replace is_high_risk_clinic=1 if bookorgunit=="( - Jenin Central clinic) عيادة جنين المركزية"


gen is_phase_1_clinic=.
gen is_phase_2_clinic=.

replace is_phase_1_clinic=1 if bookorgunit=="????"
replace is_phase_2_clinic=1 if bookorgunit=="????"

/* TAMARA STOP LABELLING CLINICS */

tab isTrial1Control
tab isTrial1Intervention

//
//PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS

replace dataextractorusername=subinstr(lower(dataextractorusername)," ","",.)

sort MotherIDNO bookdate
bro MotherIDNO bookdate dataextractorusername
gen temp_khadejeh=0
replace temp_khadejeh=1 if dataextractorusername=="khadejeh"
bysort MotherIDNO bookdate: egen has_khadejeh=max(temp_khadejeh)

drop if has_khadejeh==1 & dataextractorusername!="khadejeh"
drop temp_khadejeh has_khadejeh

//END OF PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS

capture drop dup
duplicates tag MotherIDNO newbookdatecorrect, gen("dup")
order MotherID uniqueid bookevent

export excel using "data_clean/duplicated_bookevent.xlsx" if dup>0, firstrow(var) replace
// WE DEFINITELY NEED TO FIX THIS
// HOPEFULLY TAMARA CAN IDENTIFY AND DELETE THE ONES THAT ARE BAD
duplicates drop MotherIDNO newbookdatecorrect, force
// FIX THIS ABOVE, WITH PROPER REMOVING OF DUPLICATES

// WE NOW 

// WEST BANK
// Some mothers have multiple unique IDs.
// I am overwritting multiple ids with the first ID seen
sort MotherIDNO newbookdatecorrect
bysort MotherIDNO: replace uniqueid=uniqueid[_n-1] if _n>1

// WILL NEED TO DO THIS FOR GAZA DATA AS WELL

// regenerate booking number, now that all mother ids corespond to one uniqueid
drop booking_number
bysort uniqueid (bookdate): gen booking_number=_n

// here we generate the monthly-year datasets
capture drop bookdate_month_year
generate bookdate_month_year = string(newbookdatecorrect, "%tdCCYY-NN")
capture drop temp_year
gen temp_year=string(newbookdatecorrect, "%tdCCYY")
destring temp_year, replace
sum temp_year
global MAX_YEAR=r(max)
drop temp_year
order bookdate_month_year
forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		preserve
		keep if bookdate_month_year=="`year'-`month'"
		capture save "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1_`year'-`month'.dta", replace
		restore
	}
}

save "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1.dta", replace
//use "data_temp/IDENTIFIABLE trial_1.dta", clear


// CREATE AN EXCEL FILE FOR DOUBLE ENTRY
use "~/My Documents/trial_temp_data/IDENTIFIABLE CON.dta", clear
codebook bookevent

duplicates tag MotherIDNO, gen("duplicated_motheridno")
keep if duplicated_motheridno==1
sort MotherIDNO bookdate dataextractorusername
order bookevent MotherIDNO bookdate dataextractorusername andate* usdate* labdate*

export excel using "data_clean/double_entered.xlsx", replace firstrow(var)


