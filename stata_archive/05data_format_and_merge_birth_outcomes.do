
capture cd "Z:\data processing\"
capture cd "X:\data processing\"
run "trial_dofiles/00x_date.do"

*HOSITAL BIRTH OUTCOMES
import delimited "data_raw\e.reg-control/$CLINIC_CONTROL_DATE/Hospital Birth Outcome Hospital Birth Outcome.csv", varnames(1) encoding("UTF-8") clear
capture ren v2 programstageinstance
ren programstageinstance uniqueid


// here we will delete purposefully the rows that are duplicated
//drop if event==""

order uniqueid


ren ancpreviouspregnancybirthweightg ancpreviouspregbirthwg
ren indicationforcsectionmentionedin indicforcsectionmentionedin
ren con_ancsystolicbloodpressuremmhg con_ancsystolicbpmmhg
ren con_ancdiastolicbloodpressuremmh con_ancdiastolicbpmmh

foreach X of varlist event-con_labcbchematocrit {
	ren `X' hbo_`X'
}

// we need to drop all women who are missing date of delivery
// because we cant do anything with them
capture mkdir "Results/hbo_missing_date_of_delivery/"
capture mkdir "Results/hbo_missing_date_of_delivery/$DATE"
capture export excel using "Results/hbo_missing_date_of_delivery/$DATE/missing_date_of_delivery.xlsx" if missing(hbo_dateofdeliveryhospital), replace firstrow(var)

drop if missing(hbo_dateofdeliveryhospital)

gen year=substr(hbo_dateofdeliveryhospital,1,4) // extract first four characters
gen month=substr(hbo_dateofdeliveryhospital,6,2) // extract 6-7 characters
gen day=substr(hbo_dateofdeliveryhospital,9,2) // extract 9-10 characters
destring year, replace
destring month, replace
destring day, replace
gen newhbo_dateofdeliveryhospital = mdy(month, day, year)
format newhbo_dateofdeliveryhospital %td

//bro newhbo_dateofdeliveryhospital hbo_dateofdeliveryhospital

drop hbo_dateofdeliveryhospital
ren newhbo_dateofdeliveryhospital hbo_dateofdeliveryhospital
order hbo_dateofdeliveryhospital

drop year month day

// DROP EMPTY/BAD HBOS HERE!!!!!!!!!!Mervett 
//fix these
drop if hbo_event=="NVmgZxLsHu6"
drop if hbo_event=="fSaVss6e5Mh"
drop if hbo_event=="GY4BJVPeB5T"
drop if hbo_event=="d3hiPFMbPLm"
drop if hbo_event=="Jqr6Efi4ixR"
drop if hbo_event=="m8RHo0dhCQb"
drop if hbo_event=="IforRFDYu7t"
drop if hbo_event=="R22PVjZlAc9"
drop if hbo_event=="XOOcU00Fwcg"
drop if hbo_event=="eC6FXll2qBh"
drop if hbo_event=="kMrlIYZbNBl"
drop if hbo_event=="JrrnXDq1PV8"
drop if hbo_event=="Qi2rMUpRQXZ"
drop if hbo_event=="RXmcm8wmjzC"
drop if hbo_event=="BuJWXGBkTC3"
drop if hbo_event=="GMuWsHZK0hu"
drop if hbo_event=="kpTgA8fSo3M"
drop if hbo_event=="LNcXFqo2dq2"
drop if hbo_event=="U4Ad8EXyhHk"
drop if hbo_event=="rHUm7L07uhO"

//drop multiple uniqueids for multiple hbo demographic entries
drop if uniqueid=="vtIQXW8R8vG"
drop if uniqueid=="djnoqjbpttn"
drop if uniqueid=="cwLuDFmnwec"
drop if uniqueid=="kGuieQmvsqA"
drop if uniqueid=="Pqg5LOSvDRg"
drop if uniqueid=="t9ykJgUnl1v"
drop if uniqueid=="nQKMYIO1WHM"
drop if uniqueid=="rv58Wb8kbJw"
drop if uniqueid=="vEHHZud5H2g"
drop if uniqueid=="PbmJnWsdzDd"
drop if uniqueid=="cmis6aTlwmL"
drop if uniqueid=="OFynjFuXz3y"
drop if uniqueid=="gQLnZw6ghQV"
drop if uniqueid=="eKtmBvarMcq"
drop if uniqueid=="UE4oDaGxHQQ"
drop if uniqueid=="WjCjKgWkFAn"
drop if uniqueid=="XvBg69JSVKf"
drop if uniqueid=="wMLLbibtGr3"
drop if uniqueid=="vtIQXW8R8vG"
drop if uniqueid=="djnoqjbpttn"
drop if uniqueid=="cwLuDFmnwec"
drop if uniqueid=="Pqg5LOSvDRg"
drop if uniqueid=="t9ykJgUnl1v"
drop if uniqueid=="PbmJnWsdzDd"
drop if uniqueid=="ArFey3KiITj"


//replace wrong id numbers here (make them the same as avicenna 
//FIX THIS!
//replace identificationdocumentnumber-control data="850616152" if identificationdocumentnumber-control data=="859616152"
































bysort uniqueid hbo_dateofdeliveryhospital: gen id = _n
tab id
reshape wide hbo_event-hbo_con_labcbchematocrit, i(uniqueid hbo_dateofdeliveryhospital) j(id)

save "~/My Documents/trial_temp_data/Hospital Birth Outcome Hospital Birth Outcome.dta", replace


* DEMOGRAPHICS
import delimited "data_raw\e.reg-control/$CLINIC_CONTROL_DATE/Hospital Demographics.csv", varnames(1) encoding("UTF-8") clear
ren instance uniqueid

// we see here that there is only
// one row per woman
codebook uniqueid

count
joinby uniqueid using "~/My Documents/trial_temp_data/Hospital Birth Outcome Hospital Birth Outcome.dta", unm(b)
count

tab _merge
drop _merge

drop if missing(hbo_dateofdeliveryhospital)

ren identificationdocumentnumbercont MotherIDNO
order MotherIDNO
drop if missing(MotherIDNO)

ren alternateidentificationnumber  alternateidentificationnum
ren husbandsfamilynameاسمعائلةالزوج husbandsfamilyname
ren firstnameالإسمالأول firstname
ren fathersnameاسمالأب fathersname
ren husbandsnameاسمالزوج husbandsname
ren middlenameاسمالجد middlename
ren womanfamilynameاسمعائلةالمرأة womanfamilyname

foreach X of varlist uniqueid-numberofmembersinhousehold {
	ren `X' hbo_`X'
}



bysort MotherIDNO (hbo_dateofdeliveryhospital): gen hbo_pregID=_n
order hbo_dateofdeliveryhospital
tab hbo_pregID

/*
bysort MotherIDNO: egen max_num_kids=max(hbo_pregID)
bro if max_num_kids==2
order hbo_dataextractorusername
*/

gen s=1
gen is_hbo=1

save "~/My Documents/trial_temp_data/Hospital Birth Outcome_ready_to_merge.dta", replace

*****************************
*****************************
*****************************
*****************************
*****************************
*****************************
***** MERGE STARTS HERE

use "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1.dta", clear
keep MotherIDNO newbookdatecorrect booking_number 
reshape wide newbookdatecorrect, i(MotherIDNO) j(booking_number)
save "~/My Documents/trial_temp_data/clinic_key.dta", replace

use "~/My Documents/trial_temp_data/Hospital Birth Outcome_ready_to_merge.dta", clear
keep MotherIDNO hbo_dateofdeliveryhospital hbo_pregID
ren hbo_dateofdeliveryhospital hbo_date
reshape wide hbo_date, i(MotherIDNO) j(hbo_pregID)
save "~/My Documents/trial_temp_data/hbo_key.dta", replace

** MERGING IN KEY VARIABLES
use "~/My Documents/trial_temp_data/clinic_key.dta", clear
count
joinby MotherIDNO using "~/My Documents/trial_temp_data/hbo_key.dta", unm(b)
count

tab _merge
keep if _merge==1 | _merge==3
drop _merge
count

// FINDING OUT WHICH AVICCENA DATA IS AROUDN 10 MONTHS OR LESS FROM BOOKING DATE
foreach X of varlist newbookdatecorrect* {
	di "`X'"
	local XVAL=subinstr("`X'","newbookdatecorrect","",.)
	foreach Q in "hbo" {
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
		
		// if multiple pregnancies match to one book date, only keep the latest
		forvalues Y=1/300 {
			local YNext=`Y'+1
			capture replace `Q'_pregID`Y'=. if `Q'_pregID`Y'==`Q'_pregID`YNext'
		}
	}
}

keep MotherIDNO *pregID* newbookdatecorrect*
reshape long hbo_pregID newbookdatecorrect, i(MotherIDNO) j(booking_number)

drop if missing(newbookdatecorrect)
drop newbookdatecorrect

save "~/My Documents/trial_temp_data/merge_key_data_to_hbo.dta", replace

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {		
		capture use "~/My Documents/trial_temp_data/IDENT_aviccena_merged_clinical_`year'-`month'.dta", clear
		if(_rc!=0){
			continue
		}
		
		di "***`month'"
		count
		capture drop _merge
		joinby MotherIDNO booking_number using "~/My Documents/trial_temp_data/merge_key_data_to_hbo.dta", unm(none)
		capture drop _merge
		joinby MotherIDNO hbo_pregID using "~/My Documents/trial_temp_data/Hospital Birth Outcome_ready_to_merge.dta", unm(master)
		capture drop _merge
		count
		
		save "~/My Documents/trial_temp_data/IDENT_aviccena_hbo_merged_clinical_`year'-`month'.dta", replace
	}
}


