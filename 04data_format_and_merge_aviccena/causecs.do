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
				append using "~/My Documents/trial_temp_data/causecs.dta"
			}
			save "~/My Documents/trial_temp_data/causecs.dta", replace
			
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

save "~/My Documents/trial_temp_data/causecs.dta", replace
