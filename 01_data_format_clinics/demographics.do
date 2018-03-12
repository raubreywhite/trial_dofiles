
*Open demographics .dta file 
import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Demographics.csv", varnames(1) encoding("UTF-8") clear

// FOR CONTROL DATA
if($IS_CONTROL==1){
	//drop if created<"2017-10-02 01:00:00.000"
	gen streetname=""
	gen camp=""
	gen telephonenumber=""
	gen emailaddress=""
	gen consanguinityدرجةالقرابهبينالزوج=.
	
	/*
	identify duplicates
	*/
	duplicates tag identificationdocumentnumbercont, gen("dup")
	gen numberOfTimesInData=dup+1
	drop dup
	tab numberOfTimesInData
}

*Check dataset 

*List the variables
codebook

*Describe data 
describe


*Rename variables 
rename (instance created lastupdated organisationunit organisationunitname ///
trackedentity inactive identificationdocumenttype ///
identificationdocumentnumber firstnameالإسمالأول ///
fathersnameاسمالأب middlenameاسمالجد womanfamilynameاسمعائلةالمرأة ///
husbandsfamilynameاسمعائلةالزوج husbandsnameاسمالزوج ///
streetname village city camp mobilenumber telephonenumber ///
dateofbirth emailaddress ageatmarriage ageatfirstpregnancy ///
consanguinityدرجةالقرابهبينالزوج educationinyears ///
monthlyhouseholdincomeils numberofmembersinhousehold) (uniqueid datecreated ///
dateupdated demoorgunit demoorgname trackedentity dummy idtype demoidnumber firstname ///
fathername middlename familyname1 familyname2 husbandname street village city ///
camp mobile phone dob email agemarriage agepregnancy consang education ///
income members)

*Label variables 
label variable uniqueid "System generated identifier common to all datasets"
label variable datecreated "Date of opening file- demographics"
label variable dateupdated "Date of updating file- demographics"
label variable demoorgunit "System generated additional code of clinic"
label variable demoorgname "Name of clinic- demographics"
label variable trackedentity "Legacy variable in DHIS2- demographics"
label variable dummy "Legacy variable in DHIS2- demographics"
label variable idtype "Woman's identification document type- demographics"
label variable demoidnumber "Woman's ID number- demographics"
label variable firstname "Woman's first name- demographics"
label variable fathername "Woman's father's name- demographics"
label variable middlename "Woman's middle name- demographics"
label variable familyname1 "Woman's family name 1- demographics"
label variable familyname2 "Woman's family name 2- demographics"
label variable husbandname "Woman's husband's name- demographics"
label variable street "Address of woman street- demographics"
label variable village "Address of woman village- - demographics"
label variable city "Address of woman city- demographics"
label variable camp "Address of woman camp- demographics"
label variable mobile "Woman's mobile number- demographics"
label variable phone "Woman's phone number- demographics"
label variable dob "Woman's date of birth- demographics"
label variable email "Woman's email- demographics"
label variable agemarriage "Woman'a age at marriage- demographics"
label variable agepregnancy "Woman's age at first pregnancy- demographics"
label variable consang "Consanguinity in the woman's marriage- demographics"
label variable education "Woman's years of education- demographics"
label variable income "Woman's monthly household income- demographics"
label variable members "Number of members in the woman's household- demographics"

replace demoidnumber=-_n if missing(demoidnumber)
*Describe data
describe

// Fix wrong bookorgnames
replace demoorgname="(Al.Nasaria) النصارية" if demoorgname=="(Al-nsaryeh) النصاريه"
replace demoorgname="(Barta'h) برطعه" if demoorgname=="Bartaá (برطعة)" 
replace demoorgname="( - Abu Falah)ابو فلاح" if demoorgname=="(Abu Falah)ابو فلاح"
replace demoorgname="( - AL-a'rakah)العرقه" if demoorgname=="(AL-a'rakah)العرقه"
replace demoorgname="( - Al Ramma ) الرامة" if demoorgname=="(Al Ramma )الرامة" 
replace demoorgname="( - Al-Bireh MCH)  أمومة البيرة" if demoorgname=="(Al.Biereh) أمومة البيرة"
replace demoorgname="( - Al-Bireh MCH)  أمومة البيرة" if demoorgname=="(Al-Bireh MCH) عيادة البيرة"
replace demoorgname="( - Al-eabedieh) العبيديه " if demoorgname=="(Al-eabedieh) العبيديه"
replace demoorgname="(- Al-Janobeyeh) الجنوبية" if demoorgname=="(Al-Janobeyeh) الجنوبية"
replace demoorgname="( - Al-khader) الخضر" if demoorgname=="(Al-khader) الخضر"
replace demoorgname="( - Al-ma'sarah) المعصره" if demoorgname=="(Al-ma'sarah) المعصره"
replace demoorgname="( - Al-shwawreh) الشواوره" if demoorgname=="(Al-shwawreh) الشواوره"
replace demoorgname="( - Beit Eba) بيت ايبا" if demoorgname=="(Beit Eba) بيت ايبا"
replace demoorgname="( - Beiteen) بيتين" if demoorgname=="(Beiteen) بيتين"
replace demoorgname="( - Beituniya) بيتونيا" if demoorgname=="(Beituniya) بيتونيا"
replace demoorgname="( - Bieta) بيتا" if demoorgname=="(Bieta) بيتا"
replace demoorgname="(  - Central Health Directorate Salfit) صحة سلفيت" if demoorgname=="(Central Heath Directorate Salfit) مديرية صحة سلفيت"
replace demoorgname="( - Deir Al-sudan) دير السودان" if demoorgname=="(Deir Al-sudan) دير السودان"
replace demoorgname="( - Jenin Central clinic) عيادة جنين المركزية" if demoorgname=="(Jenin Main Center) عيادة جنين المركزية"
replace demoorgname="( - Kharbatha Al-mesbah) خربثا المصباح" if demoorgname=="(Kharbatha Al-mesbah) خربثا المصباح"
replace demoorgname="( - Kufr Raa'e) كفر راعي" if demoorgname=="(Kufr Raa'e) كفر راعي"
replace demoorgname="( - Meithalun) ميثلون" if demoorgname=="(Meithalun) ميثلون"
replace demoorgname="( - Central MCH) الرعاية المركزية نابلس" if demoorgname=="(Nablus MCH) الرعاية المركزية نابلس"
replace demoorgname="( - Nea'leen) نعلين" if demoorgname=="(Nea'leen) نعلين"
replace demoorgname="( - Qarawah Bani Zeid) قراوه بني زيد" if demoorgname=="(Qarawah Bani Zeid) قراوه بني زيد"
replace demoorgname="( - Ramallah New MCH) رام الله الجديدة" if demoorgname=="(Ramallah New MCH)  عيادة رام الله الجديدة"
replace demoorgname="( - Rammun) رمون" if demoorgname=="(Rammun) رمون"
replace demoorgname="( - Ras Al-een) راس العين" if demoorgname=="(Ras Al-een) راس العين"
replace demoorgname="( - Rojeeb) روجيب" if demoorgname=="(Rojeeb) روجيب"
replace demoorgname="( - Silat adh Dhahr)  سيلة الظهر" if demoorgname=="(Silat adh Dhahr)سيلة الظهر"


//
*Replacing MotherID numbers from clinics to the correct ones from Avicenna so that*
*we can possible match more to Avicenna //
// this variable is numeric//

replace demoidnumber=900466626 if demoidnumber==900466616
replace demoidnumber=901083521 if demoidnumber==901083527
replace demoidnumber=918567635 if demoidnumber==910567635
replace demoidnumber=946457471 if demoidnumber==946457421
replace demoidnumber=85367887	 if demoidnumber==853637882
replace demoidnumber=411113470 if demoidnumber==411113970
replace demoidnumber=850465480 if demoidnumber==850465485
replace demoidnumber=850672078 if demoidnumber==850872078
replace demoidnumber=853203562 if demoidnumber==853203560
replace demoidnumber=414406916 if demoidnumber==414406918
replace demoidnumber=950337778 if demoidnumber==950337378
replace demoidnumber=850347997 if demoidnumber==850437997
replace demoidnumber=860626250 if demoidnumber==850535360
replace demoidnumber=854992138 if demoidnumber==700177637
replace demoidnumber=852327122 if demoidnumber==852327022
replace demoidnumber=85075736 if demoidnumber==850757634
replace demoidnumber=402022209 if demoidnumber==402822209
replace demoidnumber=833009645 if demoidnumber==853009645
replace demoidnumber=8501170071 if demoidnumber==850470071
replace demoidnumber=41085636 if demoidnumber==410852636
replace demoidnumber=94659532 if demoidnumber==946595428
replace demoidnumber=85049343 if demoidnumber==854049343
replace demoidnumber=832760750 if demoidnumber==852760750
replace demoidnumber=851478925 if demoidnumber==852467737
replace demoidnumber=852637722 if demoidnumber==852637727
replace demoidnumber=853672633 if demoidnumber==853672632
replace demoidnumber=854682548 if demoidnumber==854692548
replace demoidnumber=859503836 if demoidnumber==854503836
replace demoidnumber=900386821 if demoidnumber==900386871
replace demoidnumber=907612384 if demoidnumber==907612394
replace demoidnumber=936248172 if demoidnumber==911666873
replace demoidnumber=911703678 if demoidnumber==911783678
replace demoidnumber=936118628 if demoidnumber==936118678
replace demoidnumber=936248172 if demoidnumber==936748177
replace demoidnumber=937217684 if demoidnumber==937717684
replace demoidnumber=937218224 if demoidnumber==937718724
replace demoidnumber=937219615 if demoidnumber==937719615
replace demoidnumber=937220928 if demoidnumber==937720928
replace demoidnumber=941282322 if demoidnumber==941287377
replace demoidnumber=949487713 if demoidnumber==949987713
replace demoidnumber=949996982 if demoidnumber==949996987
replace demoidnumber=957623532 if demoidnumber==907623532
replace demoidnumber=90719876 if demoidnumber==907149876
replace demoidnumber=401941862 if demoidnumber==401949862
replace demoidnumber=90636311 if demoidnumber==850695412
replace demoidnumber=90517294 if demoidnumber==905171294
replace demoidnumber=90719876 if demoidnumber==907149876
replace demoidnumber=401941862 if demoidnumber==401949862
replace demoidnumber=90636311 if demoidnumber==850695412
replace demoidnumber=90517294 if demoidnumber==905171294
replace demoidnumber=8540473687 if demoidnumber==850473687
replace demoidnumber=118315274 if demoidnumber==415045400
replace demoidnumber=9466413630 if demoidnumber==946641362
replace demoidnumber=95679766 if demoidnumber==956792766
replace demoidnumber=90027046 if demoidnumber==900027046
replace demoidnumber=85364084 if demoidnumber==853640845
replace demoidnumber=85343944 if demoidnumber==853193944
replace demoidnumber=950232530 if demoidnumber==950232330
replace demoidnumber=946641262 if demoidnumber==946641362
replace demoidnumber=900270046 if demoidnumber==900027046
replace demoidnumber=856781048 if demoidnumber==905611695
replace demoidnumber=854909954 if demoidnumber==854909959
replace demoidnumber=853686845 if demoidnumber==853686848
replace demoidnumber=401466397 if demoidnumber==401466347
replace demoidnumber=859023660 if demoidnumber==859023665
replace demoidnumber=949296568 if demoidnumber==949296768
replace demoidnumber=920574827 if demoidnumber==920674827
replace demoidnumber=851937998 if demoidnumber==851937995
replace demoidnumber=805234372 if demoidnumber==852343722
replace demoidnumber=949559782 if demoidnumber==949559728
replace demoidnumber=941121556 if demoidnumber==941121956
replace demoidnumber=859894150 if demoidnumber==859894156
replace demoidnumber=853929798 if demoidnumber==853629798
replace demoidnumber=900433801 if demoidnumber==900435801
replace demoidnumber=816198217 if demoidnumber==853690501
replace demoidnumber=850695412 if demoidnumber==90636311



			








/*
*Save dataset as "int_anc demographics_labelled"
save "data_temp/int_ demographics_labelled.dta", replace

*Open the last saved dataset
use "data_temp/int_ demographics_labelled.dta", replace

*???Create a dataset with only required dates??? ***Correlate with the booking visit dates***




*Create a dataset with missing ID numbers
**Create a subset of data only with missing id numbers 
summarize demoidnumber 

*Recode missing values with numeric
replace demoidnumber=9 if missing(demoidnumber)

*Create a subset of data by keeping only women with missing id numbers
keep if demoidnumber==9

*Save this dataset as 'int_anc demographics_missing id'
save "data_temp/int_anc demographics_missing id.dta", replace




*Open dataset ""ANC demographics_labelled" to continue with processing
use "data_temp/int_ demographics_labelled.dta", clear
*/

**ageatmarriage
generate agemarriagecat = .
summarize agemarriagecat

replace agemarriagecat = 1 if (agemarriage <= 20)
replace agemarriagecat = 2 if (agemarriage >=21) & (agemarriage <=25)
replace agemarriagecat = 3 if (agemarriage >=26) & (agemarriage <=30)
replace agemarriagecat = 4 if (agemarriage >=31) & (agemarriage <=35)
replace agemarriagecat = 5 if (agemarriage >=36) & (agemarriage <=40)
replace agemarriagecat = 6 if (agemarriage >40)

summarize agemarriagecat

**ageatfirstpregnancy
generate agepregnancycat = .
summarize agepregnancycat

replace agepregnancycat = 1 if (agepregnancy <= 20)
replace agepregnancycat = 2 if (agepregnancy >=21) & (agepregnancy <=25)
replace agepregnancycat = 3 if (agepregnancy >=26) & (agepregnancy <=30)
replace agepregnancycat = 4 if (agepregnancy >=31) & (agepregnancy <=35)
replace agepregnancycat = 5 if (agepregnancy >=36) & (agepregnancy <=40)
replace agepregnancycat = 6 if (agepregnancy >40)

summarize agepregnancycat

**mothers education
generate educationcat = .
summarize educationcat

replace educationcat = 1 if (education <10)
replace educationcat = 2 if (agepregnancy >=10) & (agepregnancy <=13)
replace educationcat = 3 if (agepregnancy >13)

summarize educationcat

**income 

**Generate average monthly household income
gen avgincome = income/members

generate incomecat = .
summarize incomecat

replace incomecat = 1 if (avgincome <=200)
replace incomecat = 2 if (avgincome >=201) & (income <=900)
replace incomecat = 3 if (avgincome >=901) & (income <=1824)
replace incomecat = 4 if (avgincome >=1825) & (income <=3054)
replace incomecat = 5 if (avgincome >3055)

summarize incomecat

save "~/My Documents/trial_temp_data/$TAG Clinical Demographics.dta", replace
