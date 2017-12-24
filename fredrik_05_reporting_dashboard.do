 capture set maxvar 30000
//select 2017 data

global DATE : di %td_CY-N-D  date("$S_DATE", "DMY")
di "$DATE"
capture mkdir "~/Dropbox/Data management eRegQual/Results_From_PNIPH/Results/$DATE/"


cd "X:\data processing\"
use "data_clean/with_indicators.dta", clear

capture log close
log using "~/Dropbox/Data management eRegQual/Results_From_PNIPH/Results/$DATE/reporting_dashboard.txt", replace text


******US Routine ANC Final***************

forvalues x=1/58 {
tostring usdate`x', generate (usdate_string`x')
}

tab usreason1

capture drop usreason_routine
gen usreason_routine=0

forvalues x=1/58 {
          replace usreason_routine`x'=usreason_routine`x'+1 if  usreason`x'==1 & ///
		  usorgname`x'=="( - Sartah) سرطه" & ///
		  usdate_string`x'>=mdy(9,01,2017) & usdate_string`x'<mdy(10,01,2017)
		  }
		  
		  tab usreason_routine
		  

		  


******Amniotic fluid Normal*************


generate usdate_string= date(usdate, "YMDhms")
tab usdate_string
format usdate_string %td

tab usamniquant1

capture drop usamniquant
gen usamniquant=0

forvalues x=1/58 {
          replace usamniquant=usamniquant+1 if  usamniquant=="NORMAL" & ///
		  usorgname`x'=="( - Qarawah) قراوه" & ///
		  usdate`x'>=mdy(9,1,2017) & usdate`x'<mdy(10,1,2017)
		  }
		  
		  tab usamniquant




******Placenta Normal***************

tab usplacenor47 
tab usplacenplace48 
tab usplacenprev48

capture drop usplacenplace
gen usplacenplace=0

forvalues x=1/58 {
          replace usplacenplace=usplacenplace+1 if  usplacenplace=="NOTPREVIA" & ///
		  usorgname`x'=="( - Sartah) سرطه" & ///
		  usdate`x'>=mdy(9,1,2017) & usdate`x'<mdy(10,1,2017)
		  }
		  
		  tab usplacenplace
		
		************************
di "2=doctor"
di "3=nurse"

count if ///
	bookorgname=="( - Qarawah) قراوه" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)

count if ///
	bookorgname=="( - Qerah) قيره" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)

count if bookseenby==2 & ///
	bookorgname=="( - Qarawah) قراوه" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)

count if bookseenby==3 & ///
	bookorgname=="( - Qerah) قيره" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)

	*********************
	
capture drop ancseenby_visit
gen ancseenby_visit=0

forvalues x=1/21 {
          replace ancseenby_visit=ancseenby_visit+1 if ///
		  anorgname`x'=="( - Qerah) قيره" & ///
		  andate`x'>=mdy(9,1,2017) & andate`x'<mdy(10,1,2017)
		  }
		  tab ancseenby_visit
		  	
	
capture drop ancseenby_doctor
gen ancseenby_doctor=0

forvalues x=1/21 {
          replace ancseenby_doctor=ancseenby_doctor+1 if anseenby`x'==2 & ///
		  anorgname`x'=="( - Qerah) قيره" & ///
		  andate`x'>=mdy(9,1,2017) & andate`x'<mdy(10,1,2017)
		  }
		  tab ancseenby_doctor
		  *******************
		  
capture drop ancseenby_visit
gen ancseenby_visit=0

forvalues x=1/21 {
          replace ancseenby_visit=ancseenby_visit+1 if ///
		  anorgname`x'=="( - Al-zbabdeh) الزبابده" & ///
		  andate`x'>=mdy(8,1,2017) & andate`x'<mdy(9,01,2017)
		  }
		  tab ancseenby_visit		  
		  
capture drop ancseenby_nurse
gen ancseenby_nurse=0


forvalues x=1/21 {
          replace ancseenby_nurse=ancseenby_nurse+1 if anseenby`x'==3 & ///
		  anorgname`x'=="( - Al-zbabdeh) الزبابده" & ///
		  andate`x'>=mdy(8,1,2017) & andate`x'<mdy(9,01,2017)
		  }
		  
          tab ancseenby_nurse
		  ****************
		  
capture drop mantypeHR 
gen mantypeHR=0
forvalues x=1/100 {

capture replace mantypeHR=1 if mantypex`x'=="RefHighRisk" & ///
        bookorgname=="(  - Central Health Directorate Salfit) صحة سلفيت" & ///
		mandate`x'>=mdy(8,1,2017) & mandate`x'<mdy(9,1,2017)
		}
tab  mantypeHR

		  

capture drop mantypeFA 
gen mantypeFA=0
forvalues x=1/100 {

capture replace mantypeFA=1 if mantypex`x'=="FolicSupplements" & ///
        bookorgname=="(  - Central Health Directorate Salfit) صحة سلفيت" & ///
		mandate`x'>=mdy(9,1,2017) & mandate`x'<mdy(10,31,2017)
		}
tab  mantypeFA
		  

capture drop mantypeanemia
gen mantypeanemia=0
forvalues x=1/100 {

capture replace mantypeanemia=1 if mantypex`x'=="AnemiaTreatment" & ///
        bookorgname=="( - Qarawah) قراوه" & ///
		mandate`x'>=mdy(9,1,2017) & mandate`x'<mdy(10,31,2017)
		}
tab  mantypeanemia
		  



forvalues x=1/21 {
          replace ancseenby_visit=ancseenby_visit+1 if ///
		  anorgname`x'=="(  - Central Health Directorate Salfit) صحة سلفيت" & ///
		  andate`x'>=mdy(9,1,2017) & andate`x'<mdy(10,1,2017)
		  }
		  tab ancseenby_visit
		  			  

*****breast exams**********
          *****missing br exams*****
replace bookexambr=trim(bookexambr)
replace bookexambr="" if bookexambr=="."
tab bookexambr, miss
count if ///
	missing(bookexambr) & ///
	bookorgname=="( - Qerah) قيره" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)
	
	****count number who had br exams****
	
	count if ///
	(bookexambr=="ABNORMAL" | bookexambr=="NORMAL")& ///
	bookorgname=="( - Qerah) قيره" & ///
	newbookdatecorrect>=mdy(9,1,2017) & newbookdatecorrect<mdy(10,1,2017)
	
************* December 21. 2017*********************
	
*******Women giving birth in a hospital*******
  
		******from current pregnancy******
import delimited "X:\data processing\data_raw\e.reg-intervention\2017-12-19/Clinical Current pregnancy outcome.csv", ///
	varnames(1) encoding("UTF-8") clear
	
	tab previousplaceofbirth
	
generate eventdate_string= date(eventdate, "YMDhms")
tab eventdate_string
format eventdate_string %td
		
	count if ///
		organisationunitname=="(  - Central Health Directorate Salfit) صحة سلفيت" & ///
	    (previousplaceofbirth=="GOV" | previousplaceofbirth=="PH" | previousplaceofbirth=="NGO" | previousplaceofbirth=="UNRWA") & ///
		eventdate_string>=mdy(9,1,2017) & eventdate_string<mdy(10,1,2017)	
		
	
********Women who delivered by CS*************
 
		******from current pregnancy******
generate eventdate_string= date(eventdate, "YMDhms")
tab eventdate_string
format eventdate_string %td

 tab ancmodeofpreviousdelivery
		
count if ///
		organisationunitname=="( - Qarawah) قراوه"& ///
	    (ancmodeofpreviousdelivery=="Caesarian section") & ///
		eventdate_string>=mdy(9,1,2017) & eventdate_string<mdy(10,1,2017)
	
count if ///
		organisationunitname=="( - Qarawah) قراوه" & ///
	    (ancpreviouspregnancyoutcome=="LIVE") & ///
		ancgenderofchildfrompreviouspreg=="FEMALE" & ///
		eventdate_string>=mdy(9,1,2017) & eventdate_string<mdy(10,1,2017)	
		
		
count if ///
		organisationunitname=="( - Qarawah) قراوه" & ///
	    (ancpreviouspregnancyoutcome=="LIVE") & ///
		ancgenderofchildfrompreviouspreg=="MALE" & ///
		eventdate_string>=mdy(9,1,2017) & eventdate_string<mdy(10,1,2017)
		
import delimited "X:\data processing\data_raw\e.reg-intervention\2017-12-19/Clinical Postpartum care.csv", ///
	varnames(1) encoding("UTF-8") clear	
	
	
generate eventdate_string= date(eventdate, "YMDhms")
tab eventdate_string
format eventdate_string %td
		
		
count if ///
		organisationunitname=="( - Qarawah) قراوه" & ///
		(ppcbreastinspection=="ABNORMAL" | ppcbreastinspection=="NORMAL") & ///
		eventdate_string>=mdy(9,1,2017) & eventdate_string<mdy(10,1,2017)
		
import delimited "X:\data processing\data_raw\e.reg-intervention\2017-12-19/Clinical PPCRisks.csv", ///
	varnames(1) encoding("UTF-8") clear	
	
save "X:\data processing\data_temp\int_clinical ppcrisks.dta" ///
use "X:\data processing\data_temp\int_clinical ppcrisks.dta" ///


import delimited "X:\data processing\data_raw\e.reg-intervention\2017-12-19/Clinical PPC Managements.csv", ///
	varnames(1) encoding("UTF-8") clear	
	
save "X:\data processing\data_temp\int_clinical ppcmanagment.dta" ///
use "X:\data processing\data_temp\int_clinical ppcmanagment.dta" ///


use "X:\data processing\data_temp\int_ Clinical Postpartum.dta", clear 
 /// make it wide ///
 
 


********T-PPC Hb 11 and over************


*******T-PPC Iron and Folic Acid givn???********



******US Routine ANC Final***************



******Amniotic fluid Normal*************



******Placenta Normal***************
		
		  
log close





