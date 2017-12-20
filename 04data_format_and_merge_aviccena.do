 cd "X:\data processing\"

***************
**************
*** Mothers Details
**************

local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Mothers Details\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Mothers Details/`file'", clear firstrow

			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/motherdetails.dta"
			}
			save "data_temp/motherdetails.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}
count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(BabyRecorddatecreation,1,9)
drop BabyRecorddatecreation
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

duplicates drop MotherIDNO date, force
ren date md_date
ren pregID md_pregID

save "data_temp/motherdetails.dta", replace



***************
**************
*** CAUSE OF CS
**************
local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Cause of CS\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Cause of CS/`file'", clear firstrow

			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/causecs.dta"
			}
			save "data_temp/causecs.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}

count 
codebook MotherIDNO

replace DATATEXT=upper(DATATEXT)

count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(DATECREATED,1,9)
drop DATECREATED
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

drop BARADMISSION STATUS NAME MotherFullName
duplicates drop MotherIDNO date, force
ren date cs_date
ren pregID cs_pregID
codebook MotherIDNO

ren DATATEXT cs_commenttext

save "data_temp/causecs.dta", replace


***************
**************
*** DOCTORS NOTES
**************
local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Doctor Notes\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Doctor Notes/`file'", clear firstrow

			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/docnotes.dta"
			}
			save "data_temp/docnotes.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}

count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(DATECREATED,1,9)
drop DATECREATED
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

sort MotherIDNO
bysort MotherIDNO pregID: gen id = _n
reshape wide COMMENTTEXT, i(MotherIDNO pregID date) j(id)

gen doc_commenttext=""
foreach X of varlist COMMENT* {
	replace doc_commenttext=doc_commenttext+", "+`X' if !missing(`X')
}
drop COMMENT*
ren date dn_date
ren pregID dn_pregID
duplicates drop MotherIDNO dn_date, force

save "data_temp/docnotes.dta", replace


***************
**************
*** NURSES NOTES
**************
/*
local allfiles : dir "data_raw\avicenna\avicenna data_2017\Nurse Notes\" files "*.xlsx"
local index=1
foreach file in `allfiles' { 
	display "`file'"
	import excel using "data_raw\avicenna\avicenna data_2017\Nurse Notes/`file'", clear firstrow

	if(`index'>1){
		append using "data_temp/nursenotes.dta"
	}
	save "data_temp/nursenotes.dta", replace
	
	local index=`index'+1

}

count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(DATECREATED,1,9)
drop DATECREATED
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

sort MotherIDNO
bysort MotherIDNO pregID: gen id = _n
reshape wide NOTE, i(MotherIDNO pregID date) j(id)

gen nurse_commenttext=""
foreach X of varlist NOTE* {
	replace nurse_commenttext=nurse_commenttext+", "+`X' if !missing(`X')
}
drop NOTE*
ren date dn_date
duplicates drop MotherIDNO dn_date, force

save "data_temp/nursenotes.dta", replace
*/

***************
**************
*** LAB DETAILS
**************
local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\LAB\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\LAB/`file'", clear firstrow

			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/lab.dta"
			}
			save "data_temp/lab.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}


count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(TestDate,1,9)
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

drop TestName TestUnit TestMinVal TestMaxVal MotherFullName NAME 
ren TestResult Hb_val
ren TestDate Hb_date
sort MotherIDNO pregID Hb_date
bysort MotherIDNO pregID: gen id = _n
reshape wide Hb_val Hb_date, i(MotherIDNO pregID date) j(id)

ren date lab_date
ren pregID lab_pregID

save "data_temp/lab.dta", replace

***************
**************
*** OBSERVATIONS
**************
local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Observations\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Observations/`file'", clear firstrow
			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/bp.dta"
			}
			save "data_temp/bp.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}

replace DATEPROCESS=subinstr(DATEPROCESS,"-16","-17",.)  if MotherIDNO=="403446479"
replace DATEPROCESS=subinstr(DATEPROCESS,"-16","-17",.)  if MotherIDNO=="851478529"

count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(DATEPROCESS,1,9)
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

drop MotherFullName NAME 
ren DATEPROCESS bp_date
ren SISTOLIC bp_sist_val
ren DIASTOLIC bp_dias_val

duplicates drop

sort MotherIDNO pregID bp_date
bysort MotherIDNO pregID: gen id = _n
tab id
reshape wide bp_date bp_sist_val bp_dias_val, i(MotherIDNO pregID date) j(id)

ren date obs_date
ren pregID obs_pregID

// dropping obviously wrong data from 2008
drop if obs_date < mdy(12,31,2008)

save "data_temp/bp.dta", replace

***************
**************
*** Baby Birth
**************
local index=1
forvalues year=2017/2030 {
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Baby Birth\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Baby Birth/`file'", clear firstrow
			
			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "data_temp/bb.dta"
			}
			save "data_temp/bb.dta", replace
			
			local index=`index'+1
			/* do your stuff here */
		}
	}
}

drop BabyBirthdate BabyGender BabyAnusstatus BabyBirthComment BabyBirthMark MotherFullName NAME

count if missing(MotherIDNO)
drop if missing(MotherIDNO)
gen temp=substr(BabyRecorddatecreation,1,9)
replace temp=lower(temp)
gen date = date(temp, "DM20Y")
drop if missing(date)
format %td date
drop temp

sort MotherIDNO date
gen pregID=0
bysort MotherIDNO: gen datedif=date-date[_n-1]
replace datedif=0 if missing(datedif)

* identify when new pregnancies start
replace pregID=1 if datedif>100
* propogate increased pregID down throughout the women via cumulative sum
bysort MotherIDNO (date): replace pregID=sum(pregID)
replace pregID=pregID+1

bysort MotherIDNO pregID: egen temp=max(date)
replace date=temp
drop temp datedif

destring BabyWeight, replace force
replace BabyWeight=BabyWeight*1000 if BabyWeight<20

duplicates drop MotherIDNO pregID BabyWeight, force

sort MotherIDNO pregID BabyRecorddatecreation
drop BabyRecorddatecreation
bysort MotherIDNO pregID: gen id = _n
tab id
reshape wide BabyWeight BabyHeight BabyAPGAR1MINScore BabyAPGAR5MINScore BabyAPGAR10MINScore BabyPregnancynoofweeks BabyBirthResult BabyBirthType, i(MotherIDNO pregID date) j(id)

ren date bb_date
ren pregID bb_pregID

save "data_temp/bb.dta", replace

*****************
*****************
*****************
*****************
*****************
*****************
*****************
*****************
***************** MERGING EVERYTHING THAT ISNT BABY BIRTH
*****************
*****************
*****************
*****************
*****************
*****************
*****************

use "data_temp/motherdetails.dta", clear
keep MotherIDNO md_date md_pregID
reshape wide md_date, i(MotherIDNO) j(md_pregID)
save "data_temp/motherdetails_key.dta", replace

use "data_temp/bp.dta", clear
keep MotherIDNO obs_date obs_pregID
reshape wide obs_date, i(MotherIDNO) j(obs_pregID)
save "data_temp/bp_key.dta", replace

use "data_temp/lab.dta", clear
keep MotherIDNO lab_date lab_pregID
reshape wide lab_date, i(MotherIDNO) j(lab_pregID)
save "data_temp/lab_key.dta", replace

use "data_temp/docnotes.dta", clear
keep MotherIDNO dn_date dn_pregID
reshape wide dn_date, i(MotherIDNO) j(dn_pregID)
save "data_temp/docnoes_key.dta", replace

use "data_temp/causecs.dta", clear
keep MotherIDNO cs_date cs_pregID
reshape wide cs_date, i(MotherIDNO) j(cs_pregID)
save "data_temp/causecs_key.dta", replace

use "data_temp/bb.dta", clear
keep MotherIDNO bb_date bb_pregID
reshape wide bb_date, i(MotherIDNO) j(bb_pregID)
save "data_temp/bb_key.dta", replace

** MERGING IN KEY VARIABLES
use "data_temp/motherdetails_key.dta", clear
count
joinby MotherIDNO using "data_temp/bp_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "data_temp/lab_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "data_temp/docnoes_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "data_temp/causecs_key.dta", unm(b)
count
tab _merge
drop _merge

count
joinby MotherIDNO using "data_temp/bb_key.dta", unm(b)
count
tab _merge
drop _merge


/// 
foreach X of varlist md_date* {
	di "`X'"
	local XVAL=subinstr("`X'","md_date","",.)
	foreach Q in "obs" "lab" "dn" "cs" "bb" {
		capture drop `Q'_pregID`XVAL'
		gen `Q'_pregID`XVAL'=.
		capture drop temp
		gen temp=100
		foreach Y of varlist `Q'_date* {
			local YVAL=subinstr("`Y'","`Q'_date","",.)
			di "`XVAL'"
			di "`YVAL'"
			replace `Q'_pregID`XVAL'=`YVAL' if abs(`Y'-`X')<temp & !missing(`X') & !missing(`Y')
			replace temp=abs(`Y'-`X') if abs(`Y'-`X')<temp & !missing(`X') & !missing(`Y')
		}
	}
}

keep MotherIDNO *pregID*
reshape long obs_pregID lab_pregID dn_pregID cs_pregID bb_pregID, i(MotherIDNO) j(md_pregID)

** MERGE BACK INTO ORIGINAL STUFF
count
joinby MotherIDNO md_pregID using "data_temp/motherdetails.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO obs_pregID using "data_temp/bp.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO lab_pregID using "data_temp/lab.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO dn_pregID using "data_temp/docnotes.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO cs_pregID using "data_temp/causecs.dta", unm(b)
count

tab _merge
drop _merge

count
joinby MotherIDNO bb_pregID using "data_temp/bb.dta", unm(b)
count

tab _merge
drop _merge

destring MotherIDNO, replace

gen is_aviccena=1

save "data_temp/aviccena.dta", replace

*****************************
*****************************
*****************************
*****************************
*****************************
*****************************
***** MERGE STARTS HERE

use "data_temp/IDENTIFIABLE trial_1.dta", clear
keep MotherIDNO newbookdatecorrect booking_number 
reshape wide newbookdatecorrect, i(MotherIDNO) j(booking_number)
save "data_temp/clinic_key.dta", replace

use "data_temp/aviccena.dta", clear
keep MotherIDNO md_date md_pregID
reshape wide md_date, i(MotherIDNO) j(md_pregID)
save "data_temp/aviccena_key.dta", replace

** MERGING IN KEY VARIABLES
use "data_temp/clinic_key.dta", clear
count
joinby MotherIDNO using "data_temp/aviccena_key.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
drop _merge
count
// FINDING OUT WHICH AVICCENA DATA IS AROUDN 10 MONTHS OR LESS FROM BOOKING DATE
foreach X of varlist newbookdatecorrect* {
	di "`X'"
	local XVAL=subinstr("`X'","newbookdatecorrect","",.)
	foreach Q in "md" {
		capture drop `Q'_pregID`XVAL'
		gen `Q'_pregID`XVAL'=.
		capture drop temp
		gen temp=31*10 // 10 months
		foreach Y of varlist `Q'_date* {
			local YVAL=subinstr("`Y'","`Q'_date","",.)
			di "`XVAL'"
			di "`YVAL'"
			replace `Q'_pregID`XVAL'=`YVAL' if `Y'>`X' & (`Y'-`X')<temp & !missing(`X') & !missing(`Y')
			replace temp=(`Y'-`X') if `Y'>`X' & (`Y'-`X')<temp & !missing(`X') & !missing(`Y')
		}
	}
}

keep MotherIDNO *pregID*
reshape long md_pregID, i(MotherIDNO) j(booking_number)

** MERGE BACK INTO ORIGINAL STUFF
count
joinby MotherIDNO booking_number using "data_temp/IDENTIFIABLE trial_1.dta", unm(b)
count

tab _merge
keep if _merge==3
tab booking_number
drop _merge

count
joinby MotherIDNO md_pregID using "data_temp/aviccena.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
tab _merge
drop _merge



save "data_clean/IDENT_aviccena_merged_clinical.dta", replace



