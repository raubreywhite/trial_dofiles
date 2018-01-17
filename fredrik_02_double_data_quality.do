capture set maxvar 30000

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

use "~/My Documents/trial_temp_data/IDENTIFIABLE CON.dta", clear 

/// this data base will be updated at the end of 01dofile///


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
capture drop dup
duplicates tag MotherIDNO bookdate, gen("dup")
tab dup
keep if dup>0
drop dup

replace dataextractorusername="_NAJ" if dataextractorusername=="Najah"
replace dataextractorusername="_KHA" if dataextractorusername=="khadejeh"
replace dataextractorusername="_KHA" if dataextractorusername=="Khadejeh"
replace dataextractorusername="_KHA" if dataextractorusername=="KHADEJEH"
replace dataextractorusername="_OMA" if dataextractorusername=="Omar"

drop if dataextractorusername=="Ibtesam"
drop if dataextractorusername=="159711"

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

tab dataextractorusername

//keep MotherIDNO dataextractorusername bookgestage age income

duplicates tag MotherIDNO dataextractorusername, gen("dup")
tab dup
order MotherIDNO dataextractorusername bookid unique
//bro if dup==1
drop if dup==1
drop dup

reshape wide `longvariables', i(MotherIDNO) j(dataextractorusername) string

foreach X of varlist * {
	capture replace `X'=-9999 if missing(`X')
}

set obs 10000
gen var=""
gen agreement_NAJ_KHA=.
gen agreement_NAJ_OMR=.
gen agreement_KHA_OMR=.
local index=1

// MERVETT AND TAMARA YOU CAN CHANGE *_NAJ TO 
// THE VARIABLES THAT YOU ARE INTERESTED IN
// TO MAKE IT RUN QUICKER. JUST REMEMBER,
// THAT YOU ONLY NEED TO PUT IN THE _NAJ VARIABLE
// e.g. 
// foreach X of varlist agemarriagecat_NAJ agepregnancycat_NAJ educationcat_NAJ 

foreach X of varlist *_NAJ {
	local Y=subinstr("`X'","_NAJ","_KHA",.)
	capture kap `X' `Y'
	if(_rc==0){
		replace var="`X'" in `index'
		replace agreement_NAJ_KHA=r(prop_o) in `index'
	}
	local Y=subinstr("`X'","_NAJ","_OMR",.)
	capture kap `X' `Y'
	if(_rc==0){
		replace var="`X'" in `index'
		replace agreement_NAJ_OMR=r(prop_o) in `index'
	}
	
	local X1=subinstr("`X'","_NAJ","_KHA",.)
	capture kap `X1' `Y'
	if(_rc==0){
		replace var="`X'" in `index'
		replace agreement_KHA_OMR=r(prop_o) in `index'
	}
	
	local index=`index'+1
}

keep var agreement*
drop if missing(var)

sort agreement_NAJ_KHA

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
