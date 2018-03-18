clear
capture set maxvar 30000

*Open booking .dta file 
capture cd "Z:\data processing\"
capture cd "X:\data processing\"
run "trial_dofiles/00x_date.do"

**Intervention clinics data_comprehensive version


*General instructions before start
//Create folders called Datasets, DO files, output and resources 
//Move all datasets for STATA to 'datasets'
//Save all DO files under 'DO files'
//Save all new datasets after cleaning under 'output'
//Save all log files under 'output'


//Name all intervention clinics datasets as 'intervention_filename'
//Name all control clinics datasets as 'control_filename'
//Name all avicenna datasets as 'avicenna_filename'
//Name all combined datasets as 'combined_filename'

**Make sure there is only one sheet per excel file**


**Data processing logic
//1. Remove all identifiable data from the datasets
//2. Replace dates with gestational age
//3. Replace names of clinics with code
//4. Merge files on uniqueid
//5. Check if there are no identifable data left

forvalues IS_CONTROL=1(-1)0 {
	if(`IS_CONTROL'==0){
		global DATA_DATE="$CLINIC_INTERVENTION_DATE"
		global DATA_RAW="data_raw\e.reg-intervention/$DATA_DATE"
		global TAG="INT"
		global CLINICAL_OR_CONTROL="Clinical"
	} 
	else if(`IS_CONTROL'==1){
		global DATA_DATE="$CLINIC_CONTROL_DATE"
		global DATA_RAW="data_raw\e.reg-control/$DATA_DATE"
		global TAG="CON"
		global CLINICAL_OR_CONTROL="Control"
	}
	global IS_CONTROL=`IS_CONTROL'
	
	display "$TAG"


	******************
	*** DEMOGRAPHICS
	****************
	do "trial_dofiles/01_data_format_clinics/demographics.do"

	**************** 
	****CLINICAL BOOKING VISIT
	do "trial_dofiles/01_data_format_clinics/clinical_booking_visit.do"

	*******************
	**** ANTENATAL CARE VISITS
	*Open ANC visits .dta file 
	do "trial_dofiles/01_data_format_clinics/antenatal_care_visits.do"
	
	*******************
	*** LAB STUF
	*******************
	do "trial_dofiles/01_data_format_clinics/lab.do"

	*********************
	** ULTRASOUND STUFF
	*********************
	*Open ultrasound .dta file 
	do "trial_dofiles/01_data_format_clinics/ultrasound.do"
	
	************************
	**** RISK FACTORS
	do "trial_dofiles/01_data_format_clinics/riskfactors.do"

	*********************
	** MANAGEMENTS
	*********************
	do "trial_dofiles/01_data_format_clinics/managements.do"

	*********************
	** PREVIOUS PREGNANCIES
	*********************
	do "trial_dofiles/01_data_format_clinics/previous_pregnancies.do"
	
	*************************************
	*********** HOSPITAL BIRTH OUTCOMES
	do "trial_dofiles/01_data_format_clinics/hospital_birth_outcomes.do"
	
	*************************************
	*********** START MERGING TOGETHER IMPORTANT DATASETS
	do "trial_dofiles/01_data_format_clinics/merging_files.do"
	

}

use "~/My Documents/trial_temp_data/IDENTIFIABLE INT.dta", replace
append using "~/My Documents/trial_temp_data/IDENTIFIABLE CON.dta", force

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
//
//drop if bookevent=="ylesMxciU5N"//
//drop if bookevent=="DiTKRZHakZ8"
//drop if bookevent=="YJDoJzSTR0D"
//drop if bookevent=="CON16"
//drop if bookevent=="CON71"
//drop if bookevent=="CON75"
//drop if bookevent=="CON3"	   
//drop if bookevent=="CON58"	   
//drop if bookevent=="CON76"	   
//drop if bookevent=="CON51"	   
//drop if bookevent=="CON55"	   
//drop if bookevent=="CON78"	 
//drop if bookevent=="CON12"
//drop if bookevent=="CON49"
//drop if bookevent=="CON68"
///
******** added these on december 24, 2017. these are doubles in demogrphic data******
//drop if uniqueid=="Hrt6pxBniy9"
//drop if uniqueid=="UZ53c6fKhTy"
//drop if uniqueid=="ye9fvoVSalO"
//drop if uniqueid=="McathozJMdD"
//drop if uniqueid=="POisbVSl4J8"
//drop if uniqueid=="S1e1Nf3AFMw"


//should we repeat unique id for some of the up bookevent
//drop if uniqueid=="cgYXclaHGlW"
//drop if uniqueid=="xLqF1c17PXQ"
//drop if uniqueid=="ioxriC3ra7h"
//drop if uniqueid=="LUPgDyX5eY3"
//drop if uniqueid=="MrMdXwanACS"
//drop if uniqueid=="TRrr3PICkHp"
//drop if uniqueid=="FlJuwf9RfJ1"
//drop if uniqueid=="BvTwv6VkqzO"
//drop if uniqueid=="w6hZnooFWPk"
//drop if uniqueid=="q05EAFKTmm5"
//drop if uniqueid=="B6sGTGIlvBP"
//drop if uniqueid=="FwRbP2RnGFf"
//drop if uniqueid=="LY28iEA0ctz"
//drop if uniqueid=="ijqGKzaQeKe"///




/* TAMARA STOP if bookevent=="HERE */

/* TAMARA HERE YOU LABEL THE CLINICS */
gen is_phase_1_clinic=.

gen is_phase_2_clinic=.

replace is_phase_1_clinic=1 if bookorgunit=="????"
///	/( - Kharbatha Al-mesbah) خربثا المصباح
//	( - Bieta) بيتا
//	( - Jenin Central clinic) عيادة جنين المركزية
//	(Oreef) عوريف
//	(HR - Bethlehem MCH )امومة بيت لحم حمل خطر
//	( - Kufr Al-deek) كفر الديك
//	( - Salem) سالم
//	(  - Central Health Directorate Salfit) صحة سلفيت
//	( - Qerah) قيره
//	(HR - Al-Bireh MCH) عيادة البيرة حمل خطر
//	( - Farkha) فرخه
//	( - Deir Jarir) دير جرير
//	( - Borqah) برقه
//	( - Al-atarah) العطاره
//	(HR - Hewarah)عيادة حوارة حمل خطر
//	( - Tel) تل
//	(Tura) طوره
//	( - Al-ma'sarah) المعصره
//	( - Qabalan) قبلان
//	( - Al-eabedieh) العبيديه
//	(Dier Al.Hatab) دير الحطب
//	(Beit rema) بيت ريما
//	(Fahmeh) فحمة
//	( - Ras Al-een) راس العين
//	( - Raba) رابا
//	(Qarawah Bani Zeid) قراوه بني زيد
//	(Al-Fandaqumiya) الفندقوميه
//	( - Mirka) مركه
//	(Al-Mazra' Al-gharbeieh) المزرعة الغربية
//	( - Central MCH) الرعاية المركزية نابلس
//	( - Meithalun) ميثلون
//	( - Bettello) بيتتللو
//	( - Deir Amar) دير عمار
//	(Az Zawiya) الزاوية
//	( - Marj Bani Amer/ Beit Qad) مرج بني عامر/بيت قاد
//	(Kafr Malik ) كفر مالك
//	( - Kefel Hares) كفل حارس
//	( - Misilyah) مسليه
//	(Deir Sharaf) دير شرف
//	( - Aneen) عانين
//	( - Kferet) كفيرت
//	(Shebteen) شبتين
//	( - Doma) دوما
//	(Biet Imreen) بيت امرين
//	( - Qarawah) قراوه
//	(Al-atarah) العطاره
//	( - Beit Sera)بيت سيرا
//	( - Sartah) سرطه
//	(Al-nsaryeh) النصاريه
//	(Al-jadedah) الجديده
//	( - Nea'leen) نعلين
//	(Awarta) عورتا
//	( - Al-shwawreh) الشواوره
//	( - Al-Bireh MCH)  أمومة البيرة
//	( - Jalqamoos) جلقموس
//	(Beit Fjar) بيت فجار
//	(Kober ) عيادة كوبر
//	( - Awarta) عورتا
//	( - Beit rema) بيت ريما
//	( - Ramallah New MCH) رام الله الجديدة
//	( - Beit Eba) بيت ايبا
//	(Bieta) بيتا
//	( - Silat adh Dhahr)  سيلة الظهر
//	( - Talfeet) تلفيت
//	( - Al-zbabdeh) الزبابده
//	(HR - Bedia) بديا حمل خطر
//	( - Al-hashmeyeh) الهاشميه
//	( - Mazari' al-Nubani) مزارع النوباني
//	(Deir Abu Mash'al) دير أبو مشعل
//	( - Rojeeb) روجيب
//	( - Al-khader) الخضر
//	( - Qarawah Bani Zeid) قراوه بني زيد
//	( - Deir Netham) دير نظام
//	( - Bill'in) بلعين
//	(Hosnan) حوسان
//	( - Shuqba) عيادة شقبا
//	( - Beit Sahour) بيت ساحور
//	(Barta'h) برطعه
//	( - Kufr Ein) كفر عين
//	(Anza) عنزا
//	( - Yasoof and Izkaka clinic) ياسوف و زكاكا
//	( - Beiteen) بيتين
//	( - Al-tereh) الطيره
//	(Dier Abu Dea'f ) عيادة دير ابو ضعيف
//	(Yetma) يتما
//	( - AL-a'rakah) العرقه
//	( - Mardah) مرده
//	( - Hewarah) عيادة حوارة
//	(Deir Istiya) دير ستيا
//	( - Ebween) عبوين
//	(Qusra) قصرة
//	(Asira al-Shamaliya') عصيره الشماليه
//	( - Al-naqorah) الناقوره
//	( - Kufr Raa'e) كفر راعي
//	"(Al-jalameh)	الجلمة"
//	( - Al-Fandaqumiya) الفندقوميه
//	(HR -Ramallah New MCH) رام الله الجديدة حمل خطر
//	(Deir Ballut) دير بلوط
//	( - Beituniya) بيتونيا
//	( - Kharbatha Bani Hareth) خربثا بني حارث
//	(Burqa) برقة
//	( - Bazria) بزاريا
//	(Kharbatha Al-mesbah) خربثا المصباح
//	(Azmoot) عزموط
//	( - Ein Yabrud) عين يبرود
//	(Beit Ur Altahta) بيت عور التحتا
//	(Harmalah) حرملة
//	"(Arorah)	عارورة"
//	(Bruqin) بروقين
//	(Haris) حارس
//	(Beit liqia) بيت لقيا
//	( - Safa) صفا
//	(Silat al-hartheyeh) سيلة الحارثية
//	(Rafat) رافات
//	( - Al-Taibeh) الطيبه
//	(Doma) دوما
//	(Al-tereh) الطيره
//	(Serees) سيريس
//	(Qusin )قوصين
//	(Jalbun) جلبون
//	( - Al-ramah) الرامه
//	( - Bedia) بديا
//	"(Al-mghayer)	المغير"
//	(Kafr Dan) كفردان
//	(Al-Midyah) المدية
//	(Rumaneh) رمانه
//	( - Abu Falah) ابو فلاح
//	(Sarra) صرة
//	(- Bethlehem MCH )  أمومة بيت لحم
//	(Selwad) سلواد
//	"(Etarah)	عطارة"
//	( - Rammun) رمون
//	(Enabous) عينابوس
//	(Deir Ibzee') دير ابزيع
//	(Tqoo') تقوع
//	(Dura al Qarea') دورا القرع
//	(Burkeen) عيادة برقين
//	( - Deir Al-sudan) دير السودان
//	(Faqqua') فقوعة
//	"(Qebiah)	قبية"
//	(Beit Jala) بيت جالا
//	(Al.Nasaria) النصارية
//	"(Deir qdees)	دير قديس"
//	(Deir Dibwan) دير دبوان
//	( - Wadi foqeen) واد فوقين
//	(Birzeit) بيرزيت
//	Bartaá (برطعة)
//	( - Emsafa) إم صفا
//	"(Masha)	مسحة"
//	(Kafr Kood) كفر قود
//	(Al-eabedieh) العبيديه
//	( - Dier Abu Dea'f ) عيادة دير ابو ضعيف
//	(HR - Central MCH)الرعاية المركزية نابلس حمل خطر
//	( - Al-janye ) الجانيه
//	(Bateer) بتير
//	( - Az Zawiya) الزاوية
//	( HR - Silat adh Dhahr) سيلة الظهر حمل خطر

//

replace is_phase_2_clinic=1 if bookorgunit=="????"
//	// (- Taffuh ) تفوح
//	(- Al manshar ) المنشر
//	(- Ein Sara) عين سارة
//	(- Qaffin) قفين
//	(- Al-Dhahiriya Hosp.for Safe Delivery ) مستشفى الظاهريه للطوارىء و الولاده الامنه
//	(- Al-Eizariya MCH) رعاية الامومة والطفولة
//	(- Al-krantena) الكرنتينا
//	(- Seida) صيدا
//	(- Deir al ghusun) دير الغصون
//	(- Kharas) خاراس
//	(- Beit 'Awwa ) بيت عوا
//	(Al-Haram Al-Ibrahemi )  الحرم الابراهيمي
//	(- Tubas Central) طوباس المركزية
//	(- Aqaba) عقابا
//	(- Al Ramma )  الرامة
//	(- Illar) علار
//	(- Kafr Al Labad) كفر اللبد
//	(Aqraba) عقربا
//	(- Beit Ummar  ) بيت امر
//	(- Al-masharqah ) المشارقة
//	( HR - Al-krantena) الكرنتينا حمل خطر
//	(- Al-Janobeyeh) الجنوبية
//	(- Bani Na'im ) بني نعيم
//	(- Bal'a) بلعا
//	(- Wad Azeez ) واد عزيز
//	(- Kafr Jammal ) كفر جمال
//	(- Sa'ir) سعير
//	(- Halhul) حلحول
//	(- Surif ) صوريف
//	(- Tarqomia ) ترقوميا
//	(- Sanniriya) سنيريا
//	(- Yata Health Directorate) يطا المركزية
//	(- Qalqilia Health Directorate)  قلقيلية المركزية
//	(- Ithna ) اذنا
//	(- Al-Rihiya) الريحية
//	(- Al Samu') السموع
//	(- Beit kahel ) بيت كاحل
//	(- Hajja) حجة
//	(- Roq'a) رقعة
//	(HR- Al-Eizariya ) رعاية الامومة والطفولة حمل خطر
//	(- Habla) حبلة
//	(- Roq'a Al Arus) رقعة العروس
//	( - Jericho MCH clinic) امومة رعاية اريحا
//	(- Dura Central) دورا المركزية
//	(- Al Gharbiya) الغربية
//	(- Al Ramadin ) الرماضين
//	(HR- Al-Janobeyeh) الجنوبية حمل خطر
//	(- Beit Ula ) بيت اولا
//	(- Tubas old ) طوباس الوسطى
//	(- Tayaseer) تياسير
//	(- Deir Samit East ) دير سامت
//	(- Shweikeh) شويكة
//	(- Qafan al Khamis) قفان خميس
//	( - Arabeyeh) عربيه
//	( - Al-Shuyukh ) الشيوخ
//	(- Azzun) عزون
//	(- Zeita) زيتا
//	(- Hizma) حزما
//	(HR- Al-haram)  الحرم
//	(HR- Al-Haram Al-Ibrahemi)الحرم حمل خطر
//	(HR- Al-Ram) الرام حمل خطر
//	(- Al-Ram) الرام
//	(- Al Taqwa) التقوى
//	( - Beit Lid) بيت ليد
//	(HR- Halhul ) حلحول  حمل خطر
//	( HR - Ithna ) اذنا حمل خطر
//	(- Al Karmil ) الكرمل
//	(- Al ghweeta) الغويطة
//	(HR- Azzun) عزون حمل خطر
//	(- Al Maqbeya) المقبية
//	(HR- Bani Na'im) بني نعيم حمل خطر
//	(- Al Jib) الجيب
//	(- Bir Nabala) بيرنبالا
//	(HR -Al manshar )  المنشر حمل خطر
//	(HR- Yata Health Directorate) يطا  حمل خطر
//	(HR- Sanniriya) سنيريا حمل خطر

gen is_high_risk_clinic=.
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Bethlehem MCH)امومة بيت لحم حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Hewarah)عيادة حوارة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Bedia) بديا حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
replace is_high_risk_clinic=1 if bookorgunit=="(  - Central Health Directorate Salfit) صحة سلفيت"
replace is_high_risk_clinic=1 if bookorgunit=="( - Jenin Central clinic) عيادة جنين المركزية"
////(HR - Bethlehem MCH )امومة بيت لحم حمل خطر/
//(HR - Al-Bireh MCH) عيادة البيرة حمل خطر/
//(HR - Hewarah)عيادة حوارة حمل خطر/
//(HR - Bedia) بديا حمل خطر/
// (HR - Al-krantena) الكرنتينا حمل خطر/
//(HR -Ramallah New MCH) رام الله الجديدة حمل خطر/
//(HR- Al-Eizariya ) رعاية الامومة والطفولة حمل خطر/
//(HR- Al-Janobeyeh) الجنوبية حمل خطر/
//(HR- Al-haram)  الحرم/
//(HR- Al-Haram Al-Ibrahemi)الحرم حمل خطر/
//(HR- Al-Ram) الرام حمل خطر/
//(HR- Halhul ) حلحول  حمل خطر/
//	( HR - Ithna ) اذنا حمل خطر/
//	(HR- Azzun) عزون حمل خطر/
//	(HR- Bani Na'im) بني نعيم حمل خطر/
//	(HR - Central MCH)الرعاية المركزية نابلس حمل خطر/
//	(HR -Al manshar )  المنشر حمل خطر/
//	(HR- Yata Health Directorate) يطا  حمل خطر/
//	( HR - Silat adh Dhahr) سيلة الظهر حمل خطر/
//	(HR- Sanniriya) سنيريا حمل خطر//

//
/* TAMARA STOP LABELLING CLINICS */

tab isTrial1Control
tab isTrial1Intervention

//
//PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS

replace dataextractorusername=subinstr(lower(dataextractorusername)," ","",.)

sort MotherIDNO bookdate
bro MotherIDNO bookdate dataextractorusername
gen temp_khadejeh=0
replace temp_khadejeh=1 if dataextractorusername=="khadejeh"
bysort MotherIDNO bookdate: egen has_khadejeh=max(temp_khadejeh)

drop if has_khadejeh==1 & dataextractorusername!="khadejeh"
drop temp_khadejeh has_khadejeh

//END OF PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS

capture drop dup
duplicates tag MotherIDNO newbookdatecorrect, gen("dup")
order MotherID uniqueid bookevent

export excel using "data_clean/duplicated_bookevent.xlsx" if dup>0, firstrow(var) replace
// WE DEFINITELY NEED TO FIX THIS
// HOPEFULLY TAMARA CAN IDENTIFY AND DELETE THE ONES THAT ARE BAD
duplicates drop MotherIDNO newbookdatecorrect, force
// FIX THIS ABOVE, WITH PROPER REMOVING OF DUPLICATES

// WE NOW 

// WEST BANK
// Some mothers have multiple unique IDs.
// I am overwritting multiple ids with the first ID seen
sort MotherIDNO newbookdatecorrect
bysort MotherIDNO: replace uniqueid=uniqueid[_n-1] if _n>1

// WILL NEED TO DO THIS FOR GAZA DATA AS WELL

// regenerate booking number, now that all mother ids corespond to one uniqueid
drop booking_number
bysort uniqueid (bookdate): gen booking_number=_n

// here we generate the monthly-year datasets
capture drop bookdate_month_year
generate bookdate_month_year = string(newbookdatecorrect, "%tdCCYY-NN")
capture drop temp_year
gen temp_year=string(newbookdatecorrect, "%tdCCYY")
destring temp_year, replace
sum temp_year
global MAX_YEAR=r(max)
drop temp_year
order bookdate_month_year
forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {
		preserve
		keep if bookdate_month_year=="`year'-`month'"
		capture save "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1_`year'-`month'.dta", replace
		restore
	}
}

save "~/My Documents/trial_temp_data/IDENTIFIABLE trial_1.dta", replace
//use "data_temp/IDENTIFIABLE trial_1.dta", clear



// CREATE AN EXCEL FILE FOR DOUBLE ENTRY
use "~/My Documents/trial_temp_data/IDENTIFIABLE CON.dta", clear
codebook bookevent

duplicates tag MotherIDNO, gen("duplicated_motheridno")
keep if duplicated_motheridno==1
sort MotherIDNO bookdate dataextractorusername
order bookevent MotherIDNO bookdate dataextractorusername andate* usdate* labdate*

count
joinby bookevent using "data_clean/previously_double_entered.dta", unm(b)
count

preserve
keep bookevent
save "data_clean/previously_double_entered.dta", replace
restore

tab _merge, nol
keep if _merge==1 // only keep the new double entered

count
local N=r(N)
if(`N'>0){
	// if there are actually new cases, then save them
	capture export excel using "data_clean/double_entered_$DATE.xlsx", firstrow(var)
}
