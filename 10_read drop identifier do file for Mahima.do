*Open booking .dta file 
cd "X:\data processing\"
global DATA_DATE="2017-12-03"

*****************************************
*****************************************
*****************************************
****************** ANONYMIZING FOR MAHIMA
*****************************************
*****************************************
*****************************************

*****************************************
************ ANC DEMOGRAPHICS AND BOOKING

use "data_clean/$DATA_DATE/IDENTIFIABLE int ANC demographics and booking.dta", clear

** KEEP ALL PEOPLE WHO GET BOOKED IN AFTER 15/1/2017
keep if newbookdatecorrect>=mdy(1,15,2017)

*Drop all identifier variables (except uniqueid)
drop trackedentity dummy idtype demoidnumber firstname datecreated ///
fathername middlename familyname1 familyname2 husbandname street village city ///
camp mobile phone email consang dob income education agemarriage agepregnancy ///
members age

*Drop all identifier variables
drop bookdate booklong booklat bookorgname bookorgcode bookorgunit bookidnumber

// drop some dates
drop booklmp bookdatelastbirth dateupdated newbookdatecorrect ///
 expecteddateofdelivery newdobcorrect age1 avgincome newbooklmpcorrect

*Save file as "ANC demographics_anonymized"
save "data_clean/$DATA_DATE/ANONYMIZED int ANC demographics and booking.dta", replace

*****************************************
************ Clinical Antenatal care visit
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical Antenatal care visit.dta", clear
drop andate anlong anlat anorgname anorgcode anorgunit anidnumber
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical Antenatal care visit.dta", replace

*****************************************
*********** LAB
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical Lab results.dta", clear
drop labdate lablong lablat laborgname laborgcode laborgunit labidnumber
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical Lab results.dta", replace

******************************************
*********** ULTRASOUND
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical Ultrasound results.dta", clear
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical Ultrasound results.dta", replace

******************************************8
************* RISK
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical ANCRisks.dta", replace
//drop ID dates?????
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical ANCRisks.dta", replace

******************************************
************* MANAGEMENT
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical ANCManagements.dta", clear
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical ANCManagements.dta", replace

*****************************************
*************** PREVIOUS PREGNANCIES
use "data_clean/$DATA_DATE/IDENTIFIABLE int Clinical Previous pregnancies.dta", clear
drop prevevent prevlong prevlat prevorgname prevorgcode prevorgunit previdnumber
save "data_clean/$DATA_DATE/ANONYMIZED int Clinical Previous pregnancies.dta", replace


****DO WE NEED THE DATES OF DELIVERIES?****
**USE THIS DATASET TO CALCULATE GRAVIDA, PARA, ABORTIONS, LIVING CHILDREN IN CONTROL AND INTERVENTION DATASETS**



*Info 1

**Combine booking visits with risks 
**Combine booking visits dataset with anc visits- number of women that have no anc visits (after booking visit)
**Combine anc visits with risks 

*Info 2

**Anc visits dataset- convert dates to gestational ages and then classify 
**generate new variable
**Flatten the dataset (after dropping all other variables) and get gestational ages at visits 

*Info 3

**Tabulate doubleentry variable for uniqueid for lab
**Count the '1's to get the denominator 
