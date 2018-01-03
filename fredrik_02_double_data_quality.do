capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

use "data_temp/IDENTIFIABLE CON.dta", clear

capture drop dup
duplicates tag MotherIDNO bookdate, gen("dup")
tab dup
keep if dup>0
drop dup

replace dataextractorusername="_NAJ" if dataextractorusername=="Najah"
replace dataextractorusername="_KHA" if dataextractorusername=="khadejeh"
replace dataextractorusername="_KHA" if dataextractorusername=="Khadejeh"
replace dataextractorusername="_OMA" if dataextractorusername=="Omar"
*************Added Omar as a data extractor 26/12/2017 and rplace Khadejeh with _KHA**************

local longvariables=""
foreach X of varlist * {
	if("`X'"=="MotherIDNO"){
		continue
	}
	if("`X'"=="dataextractorusername"){
		continue
	}
	if(strlen("`X'")>25){
		local Y1=substr("`X'",1,10)
		local Y2=substr("`X'",strlen("`X'")-15,strlen("`X'"))
		local Y="`Y1'`Y2'"
		ren `X' `Y'
		di "`X' - `Y'"
		local X="`Y'"
	}
	local longvariables="`longvariables' `X'"
	
	// turn strings to groups if needed
	capture confirm string var `X'
	if _rc==0 {
		egen temp=group(`X')
		drop `X'
		ren temp `X'
	}
}
di "`longvariables'"

//keep MotherIDNO dataextractorusername bookgestage age income

capture drop reshape wide `longvariables', i(MotherIDNO) j(dataextractorusername) string

foreach X of varlist * {
	capture replace `X'=-9999 if missing(`X')
}

set obs 10000
gen var=""
gen agreement=.
local index=1

// MERVETT AND TAMARA YOU CAN CHANGE *_NAJ TO 
// THE VARIABLES THAT YOU ARE INTERESTED IN
// TO MAKE IT RUN QUICKER. JUST REMEMBER,
// THAT YOU ONLY NEED TO PUT IN THE _NAJ VARIABLE
// e.g. 
// foreach X of varlist agemarriagecat_NAJ agepregnancycat_NAJ educationcat_NAJ 
{
foreach X of varlist *_NAJ {
	local Y=subinstr("`X'","_NAJ","_KHA","_OMA".)
	capture kap `X' `Y'
	if(_rc==0){
		replace var="`X'" in `index'
		replace agreement=r(prop_o) in `index'
		local index=`index'+1
	}
}

keep var agreement
drop if missing(var)

sort agreement

export excel using "results/fredrik/double_data_quality_agreement_proportion.xlsx", firstrow(var) replace


/*
// THIS STUFF WAS FOR THE MAIN DO FILE
replace dataextractorusername=subinstr(lower(dataextractorusername)," ","",.)

sort MotherIDNO bookdate
bro MotherIDNO bookdate dataextractorusername
gen temp_khadejeh=0
replace temp_khadejeh=1 if dataextractorusername=="khadejeh"
bysort MotherIDNO bookdate: egen has_khadejeh=max(temp_khadejeh)

drop if has_khadejeh==1 & dataextractorusername!="khadejeh"
drop dup temp_khadejeh has_khadejeh

keep if has_khadejeh==0 | (has_khadejeh==1 & dataextractorusernam=="khadejeh")
*/ 
