clear 
capture set maxvar 30000
//select 2017 data

capture cd "Z:\data processing\"
capture cd "X:\data processing\"
run "trial_dofiles/00x_date.do"

import excel using "data_raw/bookorgnames.xlsx", clear firstrow
ren District district
save "~/My Documents/trial_temp_data/bookorgnames.dta", replace

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		capture use "data_clean/with_indicators_`year'-`month'.dta", clear
		if(_rc!=0){
			continue
		}

		keep if isExpectedToHaveDelivered==1
		keep if missing(is_aviccena) & missing(is_hospital_birth_outcome) & missing(is_hbo)

		drop if bookorgname=="(HR - Bethlehem MCH )امومة بيت لحم حمل خطر"
		drop if bookorgname=="(HR - Hewarah)عيادة حوارة حمل خطر"
		drop if bookorgname=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
		drop if bookorgname=="(HR - Bedia) بديا حمل خطر"
		drop if bookorgname=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
		drop if bookorgname=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
		drop if bookorgname=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
		drop if bookorgname=="(  - Central Health Directorate Salfit) صحة سلفيت"
		drop if bookorgname=="( - Jenin Central clinic) عيادة جنين المركزية"

		
	******Added these on 2/7/2018 to remove the trial HR clinics*****
		drop if bookorgname=="(HR - Al-krantena) الكرنتينا حمل خطر"
		drop if bookorgname=="(HR- Al-Eizariya ) رعاية الامومة والطفولة حمل خطر"
		drop if bookorgname=="(HR- Al-Janobeyeh) الجنوبية حمل خطر"
		drop if bookorgname=="(HR- Al-haram)  الحرم"
		drop if bookorgname=="(HR- Al-Haram Al-Ibrahemi)الحرم حمل خطر"
		drop if bookorgname=="(HR- Al-Ram) الرام حمل خطر"
		drop if bookorgname=="(HR- Halhul ) حلحول  حمل خطر"
		drop if bookorgname=="( HR - Ithna ) اذنا حمل خطر"
		drop if bookorgname=="(HR- Azzun) عزون حمل خطر"
		drop if bookorgname=="(HR- Bani Na'im) بني نعيم حمل خطر"
		drop if bookorgname=="(HR -Al manshar )  المنشر حمل خطر"
		drop if bookorgname=="(HR- Yata Health Directorate) يطا  حمل خطر"
		drop if bookorgname=="(HR- Sanniriya) سنيريا حمل خطر"
		
	
		tostring phone, replace
		replace mobile=phone  if missing(mobile)

		keep MotherIDNO ///
			bookorgname ///
			bookdate ///
			firstname ///
			fathername ///
			middlename ///
			familyname1 ///
			familyname2 ///
			husbandname ///
			dob ///
			mobile ///
			booklmp ///
			village ///
			bookintendbirth ///
			bookrecobirth ///
			expected_due_delivery
			
		generate book_ym = "`year'-`month'"
			
		if(`year'==2015 & `month'==1){ 
			save "~/My Documents/trial_temp_data/MBO_CLINIC.dta", replace
		}
		else{ 
			append using "~/My Documents/trial_temp_data/MBO_CLINIC.dta"
			save "~/My Documents/trial_temp_data/MBO_CLINIC.dta", replace
		}
		display "Month IS NOW FINISHED"

	}
	display "YEAR IS NOW FINISHED"
}

count
joinby bookorgname using "~/My Documents/trial_temp_data/bookorgnames.dta", unm(b)
tab _merge
drop _merge

gen MotherIDNO_in_AVICENNA=""
gen place_of_delivery=""
gen date_of_delivery=""
gen baby_live_still_abortion=""
gen gestage_at_delivery_or_abortion=.
gen contact_w_mother_to_fill_qs=""
gen reason_for_failed_contact=""
gen birth_weight=.
gen bp_at_admission_to_labor=""
gen hb_at_admission_to_labor=.
gen mode_of_delivery=""
gen presentation_at_delivery=""
gen indic_for_cesarian=""


label var MotherIDNO "رقم الهوية"
label var firstname "الاسم الاول"
label var fathername "اسم الاب"
label var middlename "اسم الجد"
label var familyname1 "اسم العائلة قبل الزواج"
label var familyname2 "اسم العائلة بعد الزواج"
label var husbandname "اسم الزوج"
label var mobile "رقم الهاتف"
label var dob "تاريخ ميلاد الام"
label var booklmp "اخر دورة شهرية"
label var place_of_delivery "مكان الولادة"
label var date_of_delivery "تاريخ الولادة"
label var baby_live_still_abortion "نتيجة الولادة (حي، اجهاض...)"
label var gestage_at_delivery_or_abortion "عمر الحمل عند الولادةاو انتهاء الحمل"
label var contact_w_mother_to_fill_qs "تم التواصل مع الام وتعبئة الاستبيان"
label var reason_for_failed_contact "سبب عدم التواصل"
label var birth_weight "وزن الطفل"
label var bp_at_admission_to_labor "نسبة الهيموغلوبين عند الولادة"
label var hb_at_admission_to_labor "ضغط الدم عند الولادة"
label var mode_of_delivery "mode of delivery"
label var presentation_at_delivery "presentation at delivery"
label var indic_for_cesarian "indication for cesarian"

generate expected_ym = string(expected_due_delivery, "%tdCCYY-NN")

/*
*****make sure the bookorgname is written the same in intervention and control*****
gen district=""
replace district="Hebron" if bookorgname=="(- Al manshar ) المنشر"
replace district="Jenin" if bookorgname=="( - Al-ramah) الرامه"
replace district="Hebron" if bookorgname=="(- Al Ramma )  الرامة"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Deir Jarir) دير جرير"
replace district="Ramallah&Al-bireh" if bookorgname=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Kharbatha Al-mesbah) خربثا المصباح"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Nea'leen) نعلين"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Beituniya) بيتونيا"
replace district="Nablus" if bookorgname=="( - Al-naqorah) الناقوره"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Al-janye ) الجانيه"
replace district="Bethlehem" if bookorgname=="( - Al-shwawreh) الشواوره"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Kharbatha Bani Hareth) خربثا بني حارث"
replace district="Salfit" if bookorgname=="( - Qarawah) قراوه"
replace district="Nablus" if bookorgname=="( - Talfeet) تلفيت"
replace district="Jenin" if bookorgname=="( - Al-zbabdeh) الزبابده"
replace district="Nablus" if bookorgname=="( - Bazria) بزاريا"
replace district="Nablus" if bookorgname=="( - Central MCH) الرعاية المركزية نابلس"
replace district="Jenin" if bookorgname=="( - Al-hashmeyeh) الهاشميه"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Ebween) عبوين"
replace district="Jenin" if bookorgname=="( - Aneen) عانين"
replace district="Jenin" if bookorgname=="( - Jenin Central clinic) عيادة جنين المركزية"
replace district="Nablus" if bookorgname=="( - Ras Al-een) راس العين"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Shuqba) عيادة شقبا"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Rammun) رمون"
replace district="Ramallah&Al-bireh" if bookorgname=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
replace district="Jenin" if bookorgname=="( - Al-Fandaqumiya) الفندقوميه"
replace district="Nablus" if bookorgname=="( - Salem) سالم"
replace district="Salfit" if bookorgname=="( - Sartah) سرطه"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Beit Sera)بيت سيرا"
replace district="Jenin" if bookorgname=="( - Raba) رابا"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Beit rema) بيت ريما"
replace district="Nablus" if bookorgname=="( - Awarta) عورتا"
replace district="Jenin" if bookorgname=="( - Meithalun) ميثلون"
replace district="Jenin" if bookorgname=="( - Mirka) مركه"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Al-Bireh MCH)  أمومة البيرة"
replace district="Nablus" if bookorgname=="( - Rojeeb) روجيب"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Ramallah New MCH) رام الله الجديدة"
replace district="Salfit" if bookorgname=="(  - Central Health Directorate Salfit) صحة سلفيت"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Safa) صفا"
replace district="Nablus" if bookorgname=="( - Bieta) بيتا"
replace district="Salfit" if bookorgname=="( - Mardah) مرده"
replace district="Hebron" if bookorgname=="(- Ein Sara) عين سارة"
replace district="Hebron" if bookorgname=="(- Aqaba) عقابا"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Beiteen) بيتين"
replace district="Salfit" if bookorgname=="( - Farkha) فرخه"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Qarawah Bani Zeid) قراوه بني زيد"
replace district="Jenin" if bookorgname=="( - Al-Taibeh) الطيبه"
replace district="Salfit" if bookorgname=="( - Az Zawiya) الزاوية"
replace district="Bethlehem" if bookorgname=="(HR - Bethlehem MCH )امومة بيت لحم حمل خطر"
replace district="Jenin" if bookorgname=="( - Kufr Raa'e) كفر راعي"
replace district="Nablus" if bookorgname=="(HR - Hewarah)عيادة حوارة حمل خطر"
replace district="Hebron" if bookorgname=="(- Tarqomia ) ترقوميا"
replace district="Bethlehem" if bookorgname=="( - Al-eabedieh) العبيديه"
replace district="Hebron" if bookorgname=="(HR- Al-Haram Al-Ibrahemi)الحرم حمل خطر"
replace district="Jenin" if bookorgname=="( - Kferet) كفيرت"
replace district="Jenin" if bookorgname=="( - Jalqamoos) جلقموس"
replace district="Nablus" if bookorgname=="( - Doma) دوما"
replace district="Nablus" if bookorgname=="( - Borqah) برقه"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Kufr Ein) كفر عين"
replace district="Salfit" if bookorgname=="(HR - Bedia) بديا حمل خطر"
replace district="Salfit" if bookorgname=="( - Kufr Al-deek) كفر الديك"
replace district="Salfit" if bookorgname=="( - Yasoof and Izkaka clinic) ياسوف و زكاكا"
replace district="Nablus" if bookorgname=="( - Qabalan) قبلان"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Abu Falah) ابو فلاح"
replace district="Jenin" if bookorgname=="( - AL-a'rakah) العرقه"
replace district="Nablus" if bookorgname=="( - Beit Eba) بيت ايبا"
replace district="Nablus" if bookorgname=="( - Tel) تل"
replace district="Jenin" if bookorgname=="( - Al-atarah) العطاره"
replace district="Salfit" if bookorgname=="( - Bedia) بديا"
replace district="Hebron" if bookorgname=="(- Taffuh ) تفوح"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Deir Amar) دير عمار"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Bettello) بيتتللو"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Al-tereh) الطيره"
replace district="Bethlehem" if bookorgname=="( - Al-ma'sarah) المعصره"
replace district="Salfit" if bookorgname=="( - Qerah) قيره"
replace district="Hebron" if bookorgname=="(- Beit kahel ) بيت كاحل"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Bill'in) بلعين"
replace district="Jenin" if bookorgname=="( - Marj Bani Amer/ Beit Qad) مرج بني عامر/بيت قاد"
replace district="Jenin" if bookorgname=="( - Misilyah) مسليه"
replace district="Salfit" if bookorgname=="( - Kefel Hares) كفل حارس"
replace district="Bethlehem" if bookorgname=="( - Al-khader) الخضر"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Mazari' al-Nubani) مزارع النوباني"
replace district="Tulkarm" if bookorgname=="(- Al-Janobeyeh) الجنوبية"
replace district="Hebron" if bookorgname=="(- Al-krantena) الكرنتينا"
replace district="Tulkarm" if bookorgname=="(- Qaffin) قفين"
replace district="Tulkarm" if bookorgname=="(- Deir al ghusun) دير الغصون"
replace district="North Hebron" if bookorgname=="(- Beit Ummar  ) بيت امر"
replace district="Hebron" if bookorgname=="(- Al-masharqah ) المشارقة"
replace district="Tulkarm" if bookorgname=="(- Shweikeh) شويكة"
replace district="Tubas" if bookorgname=="(- Tubas Central) طوباس المركزية"
replace district="Jenin" if bookorgname=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
replace district="Jenin" if bookorgname=="( - Silat adh Dhahr)  سيلة الظهر"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Ein Yabrud) عين يبرود"
replace district="Hebron" if bookorgname=="(- Ithna ) اذنا"
replace district="Tulkarm" if bookorgname=="(- Zeita) زيتا"
replace district="Bethlehem" if bookorgname=="( - Beit Sahour) بيت ساحور"
replace district="Hebron" if bookorgname=="(Al-Haram Al-Ibrahemi )  الحرم الابراهيمي"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Deir Netham) دير نظام"
replace district="Hebron" if bookorgname=="(- Wad Azeez ) واد عزيز"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Deir Al-sudan) دير السودان"
replace district="Jenin" if bookorgname=="( - Dier Abu Dea'f ) عيادة دير ابو ضعيف"
replace district="Nablus" if bookorgname=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
replace district="Tulkarm" if bookorgname=="(- Bal'a) بلعا"
replace district="Bethlehem" if bookorgname=="( - Wadi foqeen) واد فوقين"
replace district="Tulkarm" if bookorgname=="(- Seida) صيدا"
replace district="Ramallah&Al-bireh" if bookorgname=="( - Emsafa) إم صفا"
*/

****making district level stuff below******


global DATE : di %td_CY-N-D  date("$S_DATE", "DMY")
di "$DATE"
capture mkdir "Results/mbo_district/"
capture mkdir "Results/mbo_district/$DATE/"

capture mkdir "Results/mbo_district_by_bookingdate/"
capture mkdir "Results/mbo_district_by_bookingdate/$DATE/"




forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
	
		// by expected due date
		count if expected_ym=="`year'-`month'"
		local N=r(N)
		
		if(`N'>0){
			// if there is no one with an expected year_month due date, then skip to next loop cycle
			capture mkdir "Results/mbo_district/$DATE/`year'-`month'"
			
			levelsof district, local(levels) 
			foreach x of local levels {
				preserve
				display "`x'"
				keep if district== "`x'"
				keep if expected_ym=="`year'-`month'"
				
				count
				local N=r(N)
				if(`N'==0){
					restore
					// if there is no one with an expected year_month due date, then skip to next loop cycle
					continue
				}
				
				capture export excel using "Results/mbo_district/$DATE/`year'-`month'/`x' district.xlsx", replace firstrow(varl)
				restore 
			}
		}
		
		// by booking date
		count if book_ym=="`year'-`month'"
		local N=r(N)
		
		if(`N'>0){
			capture mkdir "Results/mbo_district_by_bookingdate/$DATE/`year'-`month'"
			
			levelsof district, local(levels) 
			foreach x of local levels {
				preserve
				display "`x'"
				keep if district== "`x'"
				keep if book_ym=="`year'-`month'"
				
				count
				local N=r(N)
				if(`N'==0){
					restore
					// if there is no one with an expected year_month due date, then skip to next loop cycle
					continue
				}
				
				capture export excel using "Results/mbo_district_by_bookingdate/$DATE/`year'-`month'/`x' district.xlsx", replace firstrow(varl)
				restore 
			}
		}
		
		
	}
}

****making clinical level stuff below******


global DATE : di %td_CY-N-D  date("$S_DATE", "DMY")
di "$DATE"
capture mkdir "Results/mbo_clinic/"
capture mkdir "Results/mbo_clinic/$DATE/"

capture mkdir "Results/mbo_clinic_by_bookingdate/"
capture mkdir "Results/mbo_clinic_by_bookingdate/$DATE/"



levelsof bookorgname, local(levels) 
 foreach l of local levels {
	display "`l'"
 }

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		count if expected_ym=="`year'-`month'"
		local N=r(N)
		
		if(`N'>0){
			// if there is no one with an expected year_month due date, then skip to next loop cycle
				
			capture mkdir "Results/mbo_clinic/$DATE/`year'-`month'"
			
			levelsof bookorgname, local(levels) 
			foreach x of local levels {
				preserve
				display "`x'"
				keep if bookorgname== "`x'"
				keep if expected_ym=="`year'-`month'"
				
				count
				local N=r(N)
				if(`N'==0){
					restore
					// if there is no one with an expected year_month due date, then skip to next loop cycle
					continue
				}
				
				// This fixes up clinic names with forward slashes in them (replaces them with underscores _)
				local newFileName=subinstr("`x'","/","_",.)
				capture export excel using "Results/mbo_clinic/$DATE/`year'-`month'/`newFileName' clinics.xlsx", replace firstrow(varl)
				restore 
			}
		}
		
		count if book_ym=="`year'-`month'"
		local N=r(N)
		
		if(`N'>0){
			capture mkdir "Results/mbo_clinic_by_bookingdate/$DATE/`year'-`month'"
			
			levelsof bookorgname, local(levels) 
			foreach x of local levels {
				preserve
				display "`x'"
				keep if bookorgname== "`x'"
				keep if book_ym=="`year'-`month'"
				
				count
				local N=r(N)
				if(`N'==0){
					restore
					// if there is no one with an expected year_month due date, then skip to next loop cycle
					continue
				}
				
				// This fixes up clinic names with forward slashes in them (replaces them with underscores _)
				local newFileName=subinstr("`x'","/","_",.)
				capture export excel using "Results/mbo_clinic_by_bookingdate/$DATE/`year'-`month'/`newFileName' clinics.xlsx", replace firstrow(varl)
				restore 
			}
		}
	}
}

/*

foreach x in "(- Al manshar ) المنشر" "( - Al-ramah) الرامه" "(- Al Ramma )  الرامة" "( - Deir Jarir) دير جرير" "( - Kharbatha Al-mesbah) خربثا المصباح" ///
 "(HR - Al-Bireh MCH) عيادة البيرة حمل خطر" "( - Nea'leen) نعلين" "( - Beituniya) بيتونيا" "( - Al-naqorah) الناقوره" {
*/ 
 
 