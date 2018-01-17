
capture cd "Z:\data processing\"
capture cd "X:\data processing\"

// MOTHER ID
import excel using "data_raw/avicenna/ramallah_data/Mother Details 2015.xlsx", clear firstrow
save "~/My Documents/trial_temp_data/ramallah_motherdetails.dta", replace
import excel using "data_raw/avicenna/ramallah_data/Mother Details 2016.xlsx", clear firstrow
append using "~/My Documents/trial_temp_data/ramallah_motherdetails.dta"
codebook PATIENTID
duplicates drop MotherIDNO PATIENTID, force
save "~/My Documents/trial_temp_data/ramallah_motherdetails.dta", replace
count
keep MotherIDNO PATIENTID
count
save "~/My Documents/trial_temp_data/ramallah_idnumbers.dta", replace

// BP
import excel using "data_raw/avicenna/ramallah_data/BP 2015.xlsx", clear firstrow
save "~/My Documents/trial_temp_data/ramallah_BP.dta", replace
import excel using "data_raw/avicenna/ramallah_data/BP 2016.xlsx", clear firstrow
append using "~/My Documents/trial_temp_data/ramallah_BP.dta"
save "~/My Documents/trial_temp_data/ramallah_BP.dta", replace
count
joinby PATIENTID using "~/My Documents/trial_temp_data/ramallah_idnumbers.dta", unm(b)
count
tab _merge
drop if _merge==2
drop _merge
save "~/My Documents/trial_temp_data/ramallah_BP.dta", replace

// LAB
import excel using "data_raw/avicenna/ramallah_data/Lab Test 2015.xlsx", clear firstrow
save "~/My Documents/trial_temp_data/ramallah_lab.dta", replace
import excel using "data_raw/avicenna/ramallah_data/Lab Test 2016.xlsx", clear firstrow
append using "~/My Documents/trial_temp_data/ramallah_lab.dta"
save "~/My Documents/trial_temp_data/ramallah_lab.dta", replace
count
joinby PATIENTID using "~/My Documents/trial_temp_data/ramallah_idnumbers.dta", unm(b)
count
tab _merge
drop if _merge==2
drop _merge
save "~/My Documents/trial_temp_data/ramallah_lab.dta", replace

// PRESENTATION
import excel using "data_raw/avicenna/ramallah_data/PRESENTATION 2015.xls", clear firstrow
save "~/My Documents/trial_temp_data/ramallah_presentation.dta", replace
import excel using "data_raw/avicenna/ramallah_data/PRESENTATION 2016.xls", clear firstrow
append using "~/My Documents/trial_temp_data/ramallah_presentation.dta"
save "~/My Documents/trial_temp_data/ramallah_presentation.dta", replace
count
joinby PATIENTID using "~/My Documents/trial_temp_data/ramallah_idnumbers.dta", unm(b)
count
tab _merge
drop if _merge==2
drop _merge
save "~/My Documents/trial_temp_data/ramallah_presentation.dta", replace

// BABY PRESENTATION
capture local allfiles : dir "data_raw/avicenna/ramallah_data\BIRTH OUTCOMES 2015/" files "*.xlsx"
foreach file in `allfiles' { 
	display "`file'"
	import excel using "data_raw/avicenna/ramallah_data\BIRTH OUTCOMES 2015/`file'", clear firstrow
	count

	local newfile=subinstr("`file'","xlsx","dta",.)
	save "~/My Documents/trial_temp_data/`newfile'", replace
}

capture local allfiles : dir "data_raw/avicenna/ramallah_data\BIRTH OUTCOMES 2016/" files "*.xlsx"
foreach file in `allfiles' { 
	display "`file'"
	import excel using "data_raw/avicenna/ramallah_data\BIRTH OUTCOMES 2016/`file'", clear firstrow
	count

	local newfile=subinstr("`file'","xlsx","dta",.)
	save "~/My Documents/trial_temp_data/`newfile'", replace
}

capture mkdir "data_raw/avicenna/2014-00"
capture mkdir "data_raw/avicenna/2014-00/LAB"
capture mkdir "data_raw/avicenna/2014-00/Mothers Details"
capture mkdir "data_raw/avicenna/2014-00/Observations"
capture mkdir "data_raw/avicenna/2014-00/Baby Birth"


// LAB DATA
use "~/My Documents/trial_temp_data/ramallah_lab.dta", clear

gen MotherFullName=""
gen NAME=""
drop PATIENTID

order ///
	MotherFullName ///
	MotherIDNO ///
	TestName ///
	TestUnit ///
	TestMinVal ///
	TestMaxVal ///
	TestResult ///
	TestDate ///
	NAME
	
label var MotherFullName "Mother Full Name"
label var MotherIDNO "Mother ID NO"
label var TestName "Test Name"
label var TestUnit "Test Unit"
label var TestMinVal "Test Min Val"
label var TestMaxVal "Test Max Val"
label var TestResult "Test Result"
label var TestDate "Test Date"
label var NAME "NAME"

export excel using "data_raw/avicenna/2014-00/LAB/ramallah.xlsx", firstrow(varl) replace

// MOTHER ID DETAILS
use "~/My Documents/trial_temp_data/ramallah_motherdetails.dta", clear

drop PATIENTID
gen NAME=""
gen MIDDLENAME=""
gen SURNAME=""
gen FATHERNAME=""
gen BabyRecordDateCreation="" // TODO THIS IS IMPORTANT TO FIX

label var MotherIDNO "Mother ID NO"
label var MotherFullName "Mother Full Name"
label var MotherBirthdate "Mother Birthdate"
label var ADDRESS "ADDRESS"
label var CITYID "CITYID"
label var NAME "NAME"
label var MIDDLENAME "MIDDLENAME"
label var SURNAME "SURNAME"
label var FATHERNAME "FATHERNAME"
label var BabyRecordDateCreation "Baby Record date creation"

order MotherIDNO ///
	MotherFullName ///
	MotherBirthdate ///
	ADDRESS ///
	CITYID ///
	NAME ///
	MIDDLENAME ///
	SURNAME ///
	FATHERNAME ///
	BabyRecordDateCreation

export excel using "data_raw/avicenna/2014-00/Mothers Details/ramallah.xlsx", firstrow(varl) replace

// OBSERVATIONS/BLOOD PRESSURE
use "~/My Documents/trial_temp_data/ramallah_BP.dta", clear

drop PATIENTID
gen MotherFullName=""
gen NAME=""

label var MotherIDNO "Mother ID NO"
label var MotherFullName "Mother Full Name"
label var DATEPROCESS "DATEPROCESS"
label var SISTOLIC "SISTOLIC"
label var DIASTOLIC "DIASTOLIC"
label var NAME "NAME"


export excel using "data_raw/avicenna/2014-00/Observations/ramallah.xlsx", firstrow(varl) replace

// Baby births
// 2015 alive
use "~/My Documents/trial_temp_data/bw  alive female 2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Female"
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw  alive male 2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Male"
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace
//2016 alive
use "~/My Documents/trial_temp_data/bw  alive female 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Female"
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw  alive male 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Male"
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

//2015 stillborn
use "~/My Documents/trial_temp_data/bw stillborn 2015 female.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Female"
gen BabyBirthResult="Stillborn"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw stillborn 2015 male.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Male"
gen BabyBirthResult="Stillborn"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace
//2016 stillborn
use "~/My Documents/trial_temp_data/bw stillborn 2016 female.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Female"
gen BabyBirthResult="Stillborn"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw stillborn 2016 male.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender="Male"
gen BabyBirthResult="Stillborn"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

//2015 multiple
use "~/My Documents/trial_temp_data/bw multiple a 2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender=""
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw multiple b  2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender=""
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace


//2016 multiple
use "~/My Documents/trial_temp_data/bw multiple a 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender=""
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

use "~/My Documents/trial_temp_data/bw multiple b  2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyWeight
gen BabyGender=""
gen BabyBirthResult="Alive"
keep MotherIDNO BabyWeight BabyGender BabyBirthResult
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

replace BabyWeight=trim(BabyWeight) // get rid of white space on left and right
drop if missing(BabyWeight)
count
duplicates tag MotherIDNO year, gen("dup")
tab dup
drop if dup>0
drop dup
save "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", replace

/*	
use "~/My Documents/trial_temp_data/alive male or alive female 2015.dta", clear
append using "~/My Documents/trial_temp_data/alive male or alive female 2016.dta"
*/

use "~/My Documents/trial_temp_data/dob 2 nd stage 2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyRecordDateCreation
keep MotherIDNO BabyRecordDateCreation
gen year=2015
save "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", replace

use "~/My Documents/trial_temp_data/dob 3rd stage 015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyRecordDateCreation
keep MotherIDNO BabyRecordDateCreation
gen year=2015
append using "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", replace

use "~/My Documents/trial_temp_data/dob 2rd stage 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyRecordDateCreation
keep MotherIDNO BabyRecordDateCreation
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", replace

use "~/My Documents/trial_temp_data/dob 3rd stage 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyRecordDateCreation
keep MotherIDNO BabyRecordDateCreation
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", replace

replace BabyRecordDateCreation=trim(BabyRecordDateCreation)
drop if missing(BabyRecordDateCreation)
// TODO: FIX DATES, THESE ARE VERY WRONG
// oldest babies at the front
gsort MotherIDNO year -BabyRecordDateCreation
bysort MotherIDNO year: gen n=_n
keep if n==1
drop n
drop if missing(MotherIDNO)
save "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", replace

// mode of delivery

use "~/My Documents/trial_temp_data/mode of delivery_2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren ANSWERTEXT BabyBirthType
keep MotherIDNO BabyBirthType
gen year=2015
save "~/My Documents/trial_temp_data/ramallah_baby_birth_mode.dta", replace

use "~/My Documents/trial_temp_data/mode of delivery 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren ANSWERTEXT BabyBirthType
keep MotherIDNO BabyBirthType
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth_mode.dta"

replace BabyBirthType=trim(BabyBirthType)
drop if missing(BabyBirthType)

gen is_cs=0
replace is_cs=1 if BabyBirthType=="B"
replace is_cs=1 if BabyBirthType=="Breech"
replace is_cs=1 if BabyBirthType=="CS"

collapse (max) is_cs, by(MotherIDNO year)
gen BabyBirthType="Normal Birth" if is_cs==0
replace BabyBirthType="Cesarean Birth" if is_cs==1

drop is_cs
save "~/My Documents/trial_temp_data/ramallah_baby_birth_mode.dta", replace

// apgar 10min

use "~/My Documents/trial_temp_data/q28 apgar_2015.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyAPGAR10MINScore
keep MotherIDNO BabyAPGAR10MINScore
gen year=2015
save "~/My Documents/trial_temp_data/ramallah_baby_birth_apgar.dta", replace

use "~/My Documents/trial_temp_data/q28  apgar 2016.dta", clear
ren IDENTIFICATIONNO MotherIDNO
ren DATATEXT BabyAPGAR10MINScore
keep MotherIDNO BabyAPGAR10MINScore
gen year=2016
append using "~/My Documents/trial_temp_data/ramallah_baby_birth_apgar.dta"
save "~/My Documents/trial_temp_data/ramallah_baby_birth_apgar.dta", replace

// MERGING IT ALL IN TO GET BABY BIRTHDS
use "~/My Documents/trial_temp_data/ramallah_baby_birth.dta", clear

count
joinby MotherIDNO year using "~/My Documents/trial_temp_data/ramallah_baby_birth_date.dta", unm(b)
count

tab _merge
drop if _merge==2
drop _merge
count

count
joinby MotherIDNO year using "~/My Documents/trial_temp_data/ramallah_baby_birth_mode.dta", unm(b)
count

tab _merge
drop if _merge==2
drop _merge
count

/*
TODO: FIX APGAR SCORES
count
joinby MotherIDNO year using "~/My Documents/trial_temp_data/ramallah_baby_birth_apgar.dta", unm(b)
count

tab _merge
drop if _merge==2
drop _merge
count
*/

gen MotherFullName=""
gen BabyBirthMark=""
gen BabyBirthComment=""
gen BabyBirthDate=""
gen BabyHeight=""
gen BabyAPGAR1=""
gen BabyAPGAR5=""
gen BabyAPGAR10=""
gen BabyPregWks=""
gen BabyAnusStatus=""
gen NAME=""


label var MotherIDNO "Mother ID NO"
label var MotherFullName "Mother Full Name"
label var BabyBirthMark "Baby Birth Mark"
label var BabyBirthComment "Baby Birth Comment"
label var BabyBirthDate "Baby Birthdate"
label var BabyWeight "Baby Weight"
label var BabyHeight "Baby Height" 
label var BabyAPGAR1 "Baby APGAR 1 MIN Score"
label var BabyAPGAR5 "Baby APGAR 5 MIN Score" 
label var BabyAPGAR10 "Baby APGAR 10 MIN Score" 
label var BabyPregWks "Baby Pregnancy no of weeks"
label var BabyGender "Baby Gender" 
label var BabyAnusStatus "Baby Anus status"
label var BabyRecordDateCreation "Baby Record date creation"
label var BabyBirthResult "Baby Birth Result" 
label var BabyBirthType "Baby Birth Type"
label var NAME "NAME"

export excel using "data_raw/avicenna/2014-00/Baby Birth/ramallah.xlsx", firstrow(varl) replace








