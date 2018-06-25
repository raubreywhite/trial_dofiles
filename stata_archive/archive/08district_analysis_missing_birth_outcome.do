capture set maxvar 30000
//select 2017 data

capture cd "X:\data processing\"
capture cd "Z:\data processing\"


use "data_clean/with_indicators.dta", clear
/// use "data_clean/IDENT_aviccena_merged_clinical.dta", clear



keep if isExpectedToHaveDelivered==1
keep if newbookdatecorrect>mdy(1,1,2017)

tab is_aviccena is_clinical_int, miss

keep if missing(is_aviccena)

drop if bookorgname=="(HR - Bethlehem MCH )امومة بيت لحم حمل خطر"
drop if bookorgname=="(HR - Hewarah)عيادة حوارة حمل خطر"
drop if bookorgname=="(HR - Al-Bireh MCH) عيادة البيرة حمل خطر"
drop if bookorgname=="(HR - Bedia) بديا حمل خطر"
drop if bookorgname=="(HR -Ramallah New MCH) رام الله الجديدة حمل خطر"
drop if bookorgname=="(HR - Central MCH)الرعاية المركزية نابلس حمل خطر"
drop if bookorgname=="( HR - Silat adh Dhahr) سيلة الظهر حمل خطر"
drop if bookorgname=="(  - Central Health Directorate Salfit) صحة سلفيت"
drop if bookorgname=="( - Jenin Central clinic) عيادة جنين المركزية"

tostring phone, replace

replace mobile=phone  if missing(mobile)
/*
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

keep MotherIDNO ///
district ///
bookorgname ///
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
bookrecobirth  


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


label var district "المحافظة"
label var bookorgname "العيادة"
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


sort bookorgname

export excel using "results/missing birth outcomes from intervention data/all clinics.xlsx", replace firstrow(varl)





foreach x in "Hebron" "Bethlehem" "Jenin" "Salfit" "Nablus" "Ramallah&Al-bireh" "Tulkarm" "North Hebron" "Tubas" {
preserve
	display "`x'"
	keep if district== "`x'"
capture export excel using "results/missing birth outcomes from intervention data/`x' clinics.xlsx", replace firstrow(varl)
restore 
}

foreach x in "( - Emsafa) إم صفا"  {
preserve
	display "`x'"
	keep if district== "`x'"
capture export excel using "results/missing birth outcomes from intervention data/`x' clinics.xlsx", replace firstrow(varl)
restore
 }




