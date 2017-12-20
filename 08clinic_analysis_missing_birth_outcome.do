clear 
capture set maxvar 30000
//select 2017 data

capture cd "Z:\data processing\"
capture cd "X:\data processing\"


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

keep MotherIDNO ///
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
village 

 


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


/*

foreach x in "(- Al manshar ) المنشر" "( - Al-ramah) الرامه" "(- Al Ramma )  الرامة" "( - Deir Jarir) دير جرير" "( - Kharbatha Al-mesbah) خربثا المصباح" ///
 "(HR - Al-Bireh MCH) عيادة البيرة حمل خطر" "( - Nea'leen) نعلين" "( - Beituniya) بيتونيا" "( - Al-naqorah) الناقوره" {
*/ 
 
 
 foreach x in "( - Al-ramah) الرامه" "(- Al manshar ) المنشر" "(- Al Ramma )  الرامة" "( - Deir Jarir) دير جرير" "( - Kharbatha Al-mesbah) خربثا المصباح" ///
 "( - Nea'leen) نعلين" "( - Beituniya) بيتونيا" "( - Al-naqorah) الناقوره" "( - Al-janye ) الجانيه" "( - Al-shwawreh) الشواوره" ///
 "( - Kharbatha Bani Hareth) خربثا بني حارث" "( - Qarawah) قراوه" "( - Talfeet) تلفيت" "( - Al-zbabdeh) الزبابده" "( - Bazria) بزاريا" ///
 "( - Central MCH) الرعاية المركزية نابلس" "( - Al-hashmeyeh) الهاشميه" "( - Ebween) عبوين" "( - Aneen) عانين" "( - Jenin Central clinic) عيادة جنين المركزية" ///
 "( - Ras Al-een) راس العين" "( - Shuqba) عيادة شقبا" "( - Rammun) رمون" "( - Al-Fandaqumiya) الفندقوميه" "( - Salem) سالم" "( - Sartah) سرطه" ///
 "( - Beit Sera)بيت سيرا" "( - Raba) رابا" "( - Beit rema) بيت ريما" "( - Awarta) عورتا" "( - Meithalun) ميثلون" "( - Mirka) مركه" ///
 "( - Al-Bireh MCH)  أمومة البيرة" "( - Rojeeb) روجيب" "( - Ramallah New MCH) رام الله الجديدة" "(  - Central Health Directorate Salfit) صحة سلفيت" ///
 "( - Safa) صفا" "( - Bieta) بيتا" "( - Mardah) مرده" "(- Ein Sara) عين سارة" "(- Aqaba) عقابا" "( - Beiteen) بيتين" "( - Farkha) فرخه" ///
 "( - Qarawah Bani Zeid) قراوه بني زيد" "( - Al-Taibeh) الطيبه" "( - Az Zawiya) الزاوية" "( - Kufr Raa'e) كفر راعي" "(- Tarqomia ) ترقوميا" ///
 "( - Al-eabedieh) العبيديه" "( - Kferet) كفيرت" "( - Jalqamoos) جلقموس" "( - Doma) دوما" "( - Borqah) برقه" "( - Kufr Ein) كفر عين" ///
 "( - Kufr Al-deek) كفر الديك" "( - Yasoof and Izkaka clinic) ياسوف و زكاكا" "( - Qabalan) قبلان" "( - Abu Falah) ابو فلاح" "( - AL-a'rakah) العرقه" ///
 "( - Beit Eba) بيت ايبا" "( - Tel) تل" "( - Al-atarah) العطاره" "( - Bedia) بديا" "(- Taffuh ) تفوح" "( - Deir Amar) دير عمار" "( - Bettello) بيتتللو" ///
 "( - Al-tereh) الطيره" "( - Al-ma'sarah) المعصره" "( - Qerah) قيره" "(- Beit kahel ) بيت كاحل" "( - Bill'in) بلعين" ///
 "( - Marj Bani Amer/ Beit Qad) مرج بني عامر/بيت قاد" "( - Misilyah) مسليه" "( - Kefel Hares) كفل حارس" "( - Al-khader) الخضر" ///
 "( - Mazari' al-Nubani) مزارع النوباني" "(- Al-Janobeyeh) الجنوبية" "(- Al-krantena) الكرنتينا" "(- Qaffin) قفين" "(- Deir al ghusun) دير الغصون" ///
 "(- Beit Ummar  ) بيت امر" "(- Al-masharqah ) المشارقة" "(- Shweikeh) شويكة" "(- Tubas Central) طوباس المركزية" "( - Silat adh Dhahr)  سيلة الظهر" ///
 "( - Ein Yabrud) عين يبرود" "(- Ithna ) اذنا" "(- Zeita) زيتا" "( - Beit Sahour) بيت ساحور" "(Al-Haram Al-Ibrahemi )  الحرم الابراهيمي" ///
 "( - Deir Netham) دير نظام" "(- Wad Azeez ) واد عزيز" "( - Deir Al-sudan) دير السودان" "( - Dier Abu Dea'f ) عيادة دير ابو ضعيف" "(- Bal'a) بلعا" ///
 "( - Wadi foqeen) واد فوقين" "(- Seida) صيدا" "( - Emsafa) إم صفا" ///
 {
preserve
	display "`x'"
	keep if bookorgname== "`x'"
 capture export excel using "results/missing birth outcomes from intervention data/`x' clinics.xlsx", replace firstrow(varl)
restore 

}

