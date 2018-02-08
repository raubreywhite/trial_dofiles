
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
