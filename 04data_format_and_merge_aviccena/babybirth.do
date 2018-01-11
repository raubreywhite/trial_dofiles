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
				append using "~/My Documents/trial_temp_data/bb.dta"
			}
			save "~/My Documents/trial_temp_data/bb.dta", replace
			
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

save "~/My Documents/trial_temp_data/bb.dta", replace
