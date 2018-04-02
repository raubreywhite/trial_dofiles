clear
capture set maxvar 30000

capture cd "X:\data processing\"
capture cd "Z:\data processing\"

run "trial_dofiles/00x_date.do"

capture mkdir "~/My Documents/trial_temp_data/"

import delimited "data_raw\e.reg-intervention/2018-03-04/Clinical Demographics.csv", varnames(1) encoding("UTF-8") clear
keep organisationunitname
duplicates drop
save "~/My Documents/trial_temp_data/temp.dta", replace
import delimited "data_raw/e.reg-control/2018-03-18/Control Demographics.csv", varnames(1) encoding("UTF-8") clear
keep organisationunitname
duplicates drop
append using "~/My Documents/trial_temp_data/temp.dta"
duplicates drop

gen good_organisationunitname = "" 
gen length = length(organisationunitname) 
su length, meanonly 

forval i = 1/`r(max)' { 
     local char substr(organisationunitname, `i', 1) 
     local OK inrange(`char', "a", "z") | inrange(`char', "A", "Z")
     replace good_organisationunitname = good_organisationunitname + `char' if `OK' 
}

replace good_organisationunitname=lower(good_organisationunitname)
sort good_organisationunitname
keep good_organisationunitname
duplicates drop

ren good_organisationunitname organisationunitname

export excel using "X:\data processing\data_raw\structural_data\organisationunitname_manual_cleaning.xlsx", firstrow(var)
