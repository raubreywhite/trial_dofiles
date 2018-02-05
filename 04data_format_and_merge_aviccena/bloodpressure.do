local index=1
forvalues year=2014/$MAX_YEAR {
	foreach month in "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture local allfiles : dir "data_raw\avicenna/`year'-`month'\Observations\" files "*.xlsx"
		if(_rc!=0){
			continue
		}
		foreach file in `allfiles' { 
			display "`file'"
			import excel using "data_raw\avicenna/`year'-`month'\Observations/`file'", clear firstrow
			
			tostring MotherFullName ///
				NAME ///
				, replace
				
			count
			if(r(N)==0){
				continue
			}
			if(`index'>1){
				append using "~/My Documents/trial_temp_data/bp.dta"
			}
			save "~/My Documents/trial_temp_data/bp.dta", replace
			
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

save "~/My Documents/trial_temp_data/bp.dta", replace
