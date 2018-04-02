clear
capture set maxvar 30000

capture noisily cd "C:\Users\Tamara_Awwad\eRegistry CRCT Dropbox\Data management eRegQual\Results_From_PNIPH"
capture noisily cd "~/My Documents/trial_temp_data/"

capture noisily mkdir "trial_temp_data/"
capture noisily mkdir "trial_temp_data/data_raw\"

// DHIS2
// DEMOGRAPHICS
capture noisily mkdir "trial_temp_data/data_raw\e.reg-intervention"
capture noisily mkdir "trial_temp_data/data_raw\e.reg-intervention\2018-03-04"

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Demographics.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist identificationdocumentnumber-numberofmembersinhousehold {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Demographics.csv", replace

********************
capture noisily mkdir "trial_temp_data/data_raw\e.reg-control\"
capture noisily mkdir "trial_temp_data/data_raw\e.reg-control\2018-03-18"

import delimited "X:\data processing\data_raw\e.reg-control\2018-03-18/Control Demographics.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist identificationdocumentnumber-numberofmembersinhousehold {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control Demographics.csv",replace

**************** 
****CLINICAL BOOKING VISIT

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Booking Visit.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunit-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Booking Visit.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control ANC Follow up sheet.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control ANC Green File.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control ANC Green File.csv", replace

**************** 
**** ANTENATAL CARE VISITS

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Antenatal care visit.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunit-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Antenatal care visit.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control ANC Follow up sheet.csv", replace

*******************
*** LAB STUFF

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Lab results.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Lab results.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control Lab results.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control Lab results.csv", replace

*******************
** ULTRASOUND STUFF

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Ultrasound results.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Ultrasound results.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control Ultrasound.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control Ultrasound.csv", replace

************************
**** RISK FACTORS

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical ANCRisks.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical ANCRisks.csv", replace

*********************
** MANAGEMENTS

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical ANCManagements.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical ANCManagements.csv", replace

*********************
** PREVIOUS PREGNANCIES

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Previous pregnancies.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Previous pregnancies.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Control Previous pregnancy table.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Control Previous pregnancy table.csv", replace

*************************************
*********** HOSPITAL BIRTH OUTCOMES

import delimited "X:\data processing\data_raw\e.reg-intervention/2018-03-04/Clinical Previous pregnancies.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist organisationunitname-identificationdocumentnumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-intervention\2018-03-04/Clinical Previous pregnancies.csv", replace


*************************************
*********** AVICCENA

**************
*** Mothers Details
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\"
capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10"

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Mothers Details"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Mothers Details\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO-FATHERNAME {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Mothers Details\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** CAUSE OF CS
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Cause of CS"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Cause of CS\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO-NAME {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Cause of CS\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** DOCTORS NOTES
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Doctor Notes"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Doctor Notes\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Doctor Notes\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** NURSES NOTES
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Nurse Notes"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Nurse Notes\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Nurse Notes\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** LAB DETAILS
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\LAB"

import excel using "X:\data processing\data_raw\avicenna/2017-10\LAB\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherFullName-MotherIDNO NAME {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\LAB\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** OBSERVATIONS?BLOOD PRESSURE
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Observations"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Observations\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO-MotherFullName NAME {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Observations\Oct 2017.xlsx", firstrow(varl) replace

***************
**************
*** Baby Birth
**************

capture noisily mkdir "trial_temp_data/data_raw\avicenna\2017-10\Baby Birth"

import excel using "X:\data processing\data_raw\avicenna/2017-10\Baby Birth\Oct 2017.xlsx", firstr clear
foreach X of varlist MotherIDNO-MotherFullName NAME {
	capture replace `X'="9"
	capture replace `X'=9
}
export excel using "trial_temp_data/data_raw\avicenna\2017-10\Baby Birth\Oct 2017.xlsx", firstrow(varl) replace

*************************************
*********** BIRTH OUTCOMES

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Hospital Birth Outcome Hospital Birth Outcome.csv", varnames(1) encoding("UTF-8") clear
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Hospital Birth Outcome Hospital Birth Outcome.csv", replace

import delimited "X:\data processing\data_raw\e.reg-control/2018-03-18/Hospital Demographics.csv", varnames(1) encoding("UTF-8") clear
foreach X of varlist identificationdocumenttype-mobilenumber {
	capture replace `X'="9"
	capture replace `X'=9
}
export delimited "trial_temp_data/data_raw\e.reg-control\2018-03-18/Hospital Demographics.csv", replace












