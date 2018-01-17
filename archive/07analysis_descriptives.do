
//select 2017 data

cd "X:\data processing\"

use "data_clean/with_indicators.dta", clear

keep if isExpectedToHaveDelivered==1

collapse (sum) ///
	A*numerator* A*denominator* ///
	M*numerator* M*denominator* ///
	D*numerator* D*denominator* ///
	H*numerator* H*denominator* ///
	F*numerator* F*denominator*

gen id=_n
reshape long A M D H F, i(id) j(information) string

ren (A M D H F) (nA nM nD nH nF)
drop id
gen id=_n
reshape long n, i(id information) j(letter) string

drop if missing(n)

split information, p("_")
ren information1 letter_num
ren information2 numerator_denom
ren information3 week_start
ren information4 week_end

keep n letter letter_num numerator_denom week_start week_end

reshape wide n, i(letter letter_num week_start week_end) j(numerator_denom) string

export excel using "results/crude_indicators.xlsx", replace firstrow(var)

exit

gen id=_n


gen zip = regexs(0) if(regexm(address, "[0-9][0-9][0-9][0-9][0-9]"))


capture log close
log using "results/descriptives.txt", replace text
************************
***** DEMOGRAPHICS
************************
count
tostring MotherIDNO, replace force
count if missing(MotherIDNO)
count if !missing(MotherIDNO) & (strlen(MotherIDNO)<8 | strlen(MotherIDNO)>=10)

count if missing(mobile)
count if !missing(mobile) & strlen(mobile)!=10

count if missing(income)
count if !missing(income) & income < 0 
sum income

count if missing(members)
count if !missing(members) & (members < 0 | members > 15)
sum members

************************
***** BOOKING
************************

count if missing(bookgestage)
count if !missing(bookgestage)
count if !missing(bookgestage) & bookgestage <0
count if !missing(bookgestage) & bookgestage == 0
count if !missing(bookgestage) & bookgestage <= 12
count if !missing(bookgestage) & bookgestage > 12 & bookgestage <= 16
count if !missing(bookgestage) & bookgestage > 16 & bookgestage <= 22
count if !missing(bookgestage) & bookgestage > 22 & bookgestage <= 24
count if !missing(bookgestage) & bookgestage > 24 & bookgestage <= 32
count if !missing(bookgestage) & bookgestage > 33 & bookgestage <= 36
count if !missing(bookgestage) & bookgestage > 36

count if missing(bookheight)
count if !missing(bookheight) & bookheight==0
count if !missing(bookheight) & (bookheight <= 0 | bookheight > 2)
sum bookheight

count if missing(bookweight)
count if !missing(bookweight) & bookweight==0
count if !missing(bookweight) & (bookweight <= 10 | bookweight > 200)
sum bookweight

count if missing(bookbpdiast)
count if !missing(bookbpdiast) & bookbpdiast==0
count if !missing(bookbpdiast) & bookbpdiast>0
sum bookbpdiast

count if missing(bookbpsyst)
count if !missing(bookbpsyst) & bookbpsyst==0
count if !missing(bookbpsyst) & bookbpsyst>0
sum bookbpsyst
use "data_temp\IDENTIFIABLE int Clinical ANCRisks.dta" ,clear 

tab risktype

use "data_clean/IDENT_aviccena_merged_clinical.dta", clear
keep uniqueid bookgestage

joinby uniqueid using "data_temp\IDENTIFIABLE int Clinical Lab results.dta" ,unm(b)
tab _merge
tab _merge, nol
drop if _merge==2

generate is_booking=0 
replace is_booking= 1 if bookgestage==labgestage & !missing(labhb) & labhb!=0
tab is_booking
collapse (max)is_booking, by(uniqueid)
codebook uniqueid
tab is_booking




use "data_clean/IDENT_aviccena_merged_clinical.dta", clear
keep uniqueid bookgestage expected_due_delivery 

joinby uniqueid using "data_temp\IDENTIFIABLE int Clinical Antenatal care visit.dta" ,unm(b)
tab _merge
tab _merge, nol
drop if _merge==2

keep if expected_due_delivery<mdy(8,1,2017)

generate is_angestage1_12=0 
replace is_angestage1_12= 1 if !missing(angestage) & angestage>=1 & angestage<=12

generate is_angestage15_17=0 
replace is_angestage15_17= 1 if !missing(angestage) & angestage>=15 & angestage<=17

generate is_angestage18_22=0 
replace is_angestage18_22= 1 if !missing(angestage) & angestage>=18 & angestage<=22

generate is_angestage24_28=0 
replace is_angestage24_28= 1 if !missing(angestage) & angestage>=24 & angestage<=28

generate is_angestage31_33=0 
replace is_angestage31_33= 1 if !missing(angestage) & angestage>=31 & angestage<=33

generate is_angestage36_45=0 
replace is_angestage36_45= 1 if !missing(angestage) & angestage>=36 & angestage<=45

collapse (max)is_angestage1_12 is_angestage15_17 is_angestage18_22 ///
	is_angestage24_28 is_angestage31_33 is_angestage36_45 , by(uniqueid)
	
tab1 is_angestage1_12 is_angestage15_17 is_angestage18_22 is_angestage24_28 is_angestage31_33 is_angestage36_45



use "data_clean/IDENT_aviccena_merged_clinical.dta", clear
keep uniqueid bookgestage expected_due_delivery 

joinby uniqueid using "data_temp\IDENTIFIABLE int Clinical ANCManagements.dta" ,unm(b)
tab _merge
tab _merge, nol
drop if _merge==2

tab mantype1 
tab mantype2


log close






