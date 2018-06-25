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
