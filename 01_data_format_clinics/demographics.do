
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

*Keep control clinics
keep if demoorgunit="Gw6kEZyjgRF"
keep if demoorgunit="AGwKNkyNvGs"
keep if demoorgunit="hZxZflEk1Vy"
keep if demoorgunit="OMaQqvPhgNk"
keep if demoorgunit="Z8T9JnYYVUr"
keep if demoorgunit="CpWJXZaUciZ"
keep if demoorgunit="CnoMThtOYPp"
keep if demoorgunit="ZQ2vTHw3IoR"
keep if demoorgunit="YczymSWFo2p"
keep if demoorgunit="RVOS83hcjpk"
keep if demoorgunit="jz9tilhys6j"
keep if demoorgunit="KkpfAbvipYP"
keep if demoorgunit="YhTHRTNE3l5"
keep if demoorgunit="PyYJN3GrAIh"
keep if demoorgunit="ggt1hp3vsTP"
keep if demoorgunit="RUFlAgnwlNN"
keep if demoorgunit="JLrVN9K7lG7"
keep if demoorgunit="VCir0FCET6w"
keep if demoorgunit="IjvdZImXzEV"
keep if demoorgunit="J8TXULepBEw"
keep if demoorgunit="udNO1wZWBzs"
keep if demoorgunit="GcxOwXIWxOk"
keep if demoorgunit="CH7ss241VCu"
keep if demoorgunit="uTFJpvH36Vm"
keep if demoorgunit="F2lCdelJGle"
keep if demoorgunit="s0Fi24L584j"
keep if demoorgunit="gAhaNv9x2OR"
keep if demoorgunit="WIo5lnio9WB"
keep if demoorgunit="TanbCARjnRM"
keep if demoorgunit="PCeBDgFjTDV"
keep if demoorgunit="QK22Yvc6zQZ"
keep if demoorgunit="LBpUEFWlMcC"
keep if demoorgunit="SMTeE9zov5n"
keep if demoorgunit="vRAKPYfJ8pJ"
keep if demoorgunit="FRxZxzWt3o1"
keep if demoorgunit="BD0qU2RKCl7"
keep if demoorgunit="SVuxZ4doPl9"
keep if demoorgunit="kmiIUyjfFVh"
keep if demoorgunit="S50gnBLhhHs"
keep if demoorgunit="ohrr7bsHAZM"
keep if demoorgunit="jyeScTunGnM"
keep if demoorgunit="scjtbWPOx24"
keep if demoorgunit="c46J9HAxyDI"
keep if demoorgunit="ZhZ6C3FCUqK"
keep if demoorgunit="HimVTpgpH8m"
keep if demoorgunit="nSUAqQfyg3d"
keep if demoorgunit="lBWbi9nCxuB"
keep if demoorgunit="JtLc6itCgi6"
keep if demoorgunit="Yl1WibsqFiN"
keep if demoorgunit="nfc2CWFI5dh"
keep if demoorgunit="amKUZoa0jyw"
keep if demoorgunit="Ma9N6vU7GxL"
keep if demoorgunit="jt15leini7x"
keep if demoorgunit="LuW0gdAOk0j"
keep if demoorgunit="T446PVxQ7p9"
keep if demoorgunit="RzNkAFY2oY4"
keep if demoorgunit="aj9PtSrqRsc"
keep if demoorgunit="o6u6LCy4z5w"
keep if demoorgunit="dpya0kM9tr7"
keep if demoorgunit="pZ4XYCrBd0q"
keep if demoorgunit="jljAZRCKDTx"
keep if demoorgunit="NIqt4tQRhsI"
keep if demoorgunit="FAyRvRvfeC0"
keep if demoorgunit="heps3KtrCY0"

*Keep intervention clinics
keep if demoorgunit="scGkzi4of4Y"
keep if demoorgunit="qxhrSxu5jmq"
keep if demoorgunit="nq4scxmppQj"
keep if demoorgunit="m9gKdscICth"
keep if demoorgunit="SUYnXoKu0TO"
keep if demoorgunit="VMpHY5Q6HOR"
keep if demoorgunit="UQbl9tpOseU"
keep if demoorgunit="d6bTxpbFpCU"
keep if demoorgunit="RdWzmCwGTzX"
keep if demoorgunit="OfQEt75Xj0a"
keep if demoorgunit="wlnti4Yu7nV"
keep if demoorgunit="zyNorfTNyYL"
keep if demoorgunit="Xdi5U1NywWj"
keep if demoorgunit="Uom6OZkXtDI"
keep if demoorgunit="NVyS3gF4KnB"
keep if demoorgunit="T0WHe6TvcvS"
keep if demoorgunit="n5wtbp3gC4c"
keep if demoorgunit="IxPMYvn60ML"
keep if demoorgunit="LoSmvZZOAdF"
keep if demoorgunit="YdohX5K9z6A"
keep if demoorgunit="hC2TRttDoMN"
keep if demoorgunit="Z986s36Xbbo"
keep if demoorgunit="pibddvIEJC6"
keep if demoorgunit="miHJR86HfwE"
keep if demoorgunit="DOh4zWfSuV3"
keep if demoorgunit="r89C6oohko0"
keep if demoorgunit="vmKlyAHs2NN"
keep if demoorgunit="G7BmuIb52Is"
keep if demoorgunit="YgjggVlyniX"
keep if demoorgunit="bEhBkyktHIE"
keep if demoorgunit="detrUDr34Z6"
keep if demoorgunit="hjzOvFOtTep"
keep if demoorgunit="U5djnsOO1dg"
keep if demoorgunit="ZCFP7LYroN0"
keep if demoorgunit="O34P8jjPoMR"
keep if demoorgunit="TNJpB7EdLgR"
keep if demoorgunit="BfOYqyWqOoK"
keep if demoorgunit="OepKbwiLkKL"
keep if demoorgunit="IUZdzzV5yl4"
keep if demoorgunit="BzCzBbjOOl5"
keep if demoorgunit="QU61nPCmcNK"
keep if demoorgunit="Se2dfC5qHxQ"
keep if demoorgunit="oO9KJDXz6WI"
keep if demoorgunit="ZPavrtXhD3r"
keep if demoorgunit="RDBEkXZtAxG"
keep if demoorgunit="S415vMprogg"
keep if demoorgunit="RUtearLgmnM"
keep if demoorgunit="kJQ8BWRYeau"
keep if demoorgunit="N5OeccPesTv"
keep if demoorgunit="h0zSAxXvP3B"
keep if demoorgunit="sqo6IOEPZ5J"
keep if demoorgunit="kfgtGEnmbUW"
keep if demoorgunit="QisbYa6sziB"
keep if demoorgunit="wKHSiylgvT0"
keep if demoorgunit="BCaaVmJQclX"
keep if demoorgunit="Er8UfD0DzFV"
keep if demoorgunit="AdEwTj99b2n"
keep if demoorgunit="KYj7qW1B9Y9"
keep if demoorgunit="h6jH9a9fV3f"
keep if demoorgunit="oaGatHJrxlr"
keep if demoorgunit="b6Elx5Od9yI"
keep if demoorgunit="j9lY05aAWrq"
keep if demoorgunit="pnaDp9ZYVje"
keep if demoorgunit="rbR33kIrRw2"
keep if demoorgunit="OFfYtYd1Q3D"
keep if demoorgunit="aGnKVidwoMx"
keep if demoorgunit="Yc2UiacRLEW"
keep if demoorgunit="KSfVKx6VI3M"



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
// replace demoorgname="(  - Central Health Directorate Salfit) صحة سلفيت" if demoorgname=="(Central Heath Directorate Salfit) مديرية صحة سلفيت"
replace demoorgname="( - Deir Al-sudan) دير السودان" if demoorgname=="(Deir Al-sudan) دير السودان"
replace demoorgname="( - Jenin Central clinic) عيادة جنين المركزية" if demoorgname=="(Jenin Main Center) عيادة جنين المركزية"
replace demoorgname="( - Kharbatha Al-mesbah) خربثا المصباح" if demoorgname=="(Kharbatha Al-mesbah) خربثا المصباح"
replace demoorgname="( - Kufr Raa'e) كفر راعي" if demoorgname=="(Kufr Raa'e) كفر راعي"
replace demoorgname="( - Meithalun) ميثلون" if demoorgname=="(Meithalun) ميثلون"
//replace demoorgname="( - Central MCH) الرعاية المركزية نابلس" if demoorgname=="(Nablus MCH) الرعاية المركزية نابلس"
replace demoorgname="( - Nea'leen) نعلين" if demoorgname=="(Nea'leen) نعلين"
replace demoorgname="( - Qarawah Bani Zeid) قراوه بني زيد" if demoorgname=="(Qarawah Bani Zeid) قراوه بني زيد"
replace demoorgname="( - Ramallah New MCH) رام الله الجديدة" if demoorgname=="(Ramallah New MCH)  عيادة رام الله الجديدة"
replace demoorgname="( - Rammun) رمون" if demoorgname=="(Rammun) رمون"
replace demoorgname="( - Ras Al-een) راس العين" if demoorgname=="(Ras Al-een) راس العين"
replace demoorgname="( - Rojeeb) روجيب" if demoorgname=="(Rojeeb) روجيب"
replace demoorgname="( - Silat adh Dhahr)  سيلة الظهر" if demoorgname=="(Silat adh Dhahr)سيلة الظهر"


//
*Replacing MotherID numbers from clinics to the correct ones from Avicenna so that*
*we can possible match more to Avicenna for pretrial//
// this variable is numeric//

replace demoidnumber=900466616 if demoidnumber==900466626
replace demoidnumber=901083527 if demoidnumber==901083521
replace demoidnumber=910567635 if demoidnumber==918567635
replace demoidnumber=946457421 if demoidnumber==946457471
replace demoidnumber=853637882 if demoidnumber==85367887
replace demoidnumber=411113970 if demoidnumber==411113470
replace demoidnumber=850465485 if demoidnumber==850465480
replace demoidnumber=850872078 if demoidnumber==850672078
replace demoidnumber=853203560 if demoidnumber==853203562
replace demoidnumber=414406918 if demoidnumber==414406916
replace demoidnumber=950337378 if demoidnumber==950337778
replace demoidnumber=850437997 if demoidnumber==850347997
replace demoidnumber=850535360 if demoidnumber==860626250
replace demoidnumber=700177637 if demoidnumber==854992138
replace demoidnumber=852327022 if demoidnumber==852327122
replace demoidnumber=850757634 if demoidnumber==85075736
replace demoidnumber=402822209 if demoidnumber==402022209
replace demoidnumber=853009645 if demoidnumber==833009645
replace demoidnumber=850470071 if demoidnumber==8501170071
replace demoidnumber=410852636 if demoidnumber==41085636
replace demoidnumber=946595428 if demoidnumber==94659532
replace demoidnumber=854049343 if demoidnumber==85049343
replace demoidnumber=852760750 if demoidnumber==832760750
replace demoidnumber=852467737 if demoidnumber==851478925
replace demoidnumber=852637727 if demoidnumber==852637722
replace demoidnumber=853672632 if demoidnumber==853672633
replace demoidnumber=854692548 if demoidnumber==854682548
replace demoidnumber=854503836 if demoidnumber==859503836
replace demoidnumber=900386871 if demoidnumber==900386821
replace demoidnumber=907612394 if demoidnumber==907612384
replace demoidnumber=911666873 if demoidnumber==936248172
replace demoidnumber=911783678 if demoidnumber==911703678
replace demoidnumber=936118678 if demoidnumber==936118628
replace demoidnumber=936748177 if demoidnumber==936248172
replace demoidnumber=937717684 if demoidnumber==937217684
replace demoidnumber=937718724 if demoidnumber==937218224
replace demoidnumber=937719615 if demoidnumber==937219615
replace demoidnumber=937720928 if demoidnumber==937220928
replace demoidnumber=941287377 if demoidnumber==941282322
replace demoidnumber=949987713 if demoidnumber==949487713
replace demoidnumber=949996987 if demoidnumber==949996982
replace demoidnumber=907623532 if demoidnumber==957623532
replace demoidnumber=907149876 if demoidnumber==90719876
replace demoidnumber=401949862 if demoidnumber==401941862
replace demoidnumber=850695412 if demoidnumber==90636311
replace demoidnumber=905171294 if demoidnumber==90517294
replace demoidnumber=907149876 if demoidnumber==90719876
replace demoidnumber=401949862 if demoidnumber==401941862
replace demoidnumber=850695412 if demoidnumber==90636311
replace demoidnumber=905171294 if demoidnumber==90517294
replace demoidnumber=850473687 if demoidnumber==8540473687
replace demoidnumber=415045400 if demoidnumber==118315274
replace demoidnumber=946641362 if demoidnumber==9466413630
replace demoidnumber=956792766 if demoidnumber==95679766
replace demoidnumber=900027046 if demoidnumber==90027046
replace demoidnumber=853640845 if demoidnumber==85364084
replace demoidnumber=853193944 if demoidnumber==85343944
replace demoidnumber=950232330 if demoidnumber==950232530
replace demoidnumber=946641362 if demoidnumber==946641262
replace demoidnumber=900027046 if demoidnumber==900270046
replace demoidnumber=905611695 if demoidnumber==856781048
replace demoidnumber=854909959 if demoidnumber==854909954
replace demoidnumber=853686848 if demoidnumber==853686845
replace demoidnumber=401466347 if demoidnumber==401466397
replace demoidnumber=859023665 if demoidnumber==859023660
replace demoidnumber=949296768 if demoidnumber==949296568
replace demoidnumber=920674827 if demoidnumber==920574827
replace demoidnumber=851937995 if demoidnumber==851937998
replace demoidnumber=852343722 if demoidnumber==805234372
replace demoidnumber=949559728 if demoidnumber==949559782
replace demoidnumber=941121956 if demoidnumber==941121556
replace demoidnumber=859894156 if demoidnumber==859894150
replace demoidnumber=853629798 if demoidnumber==853929798
replace demoidnumber=900435801 if demoidnumber==900433801
replace demoidnumber=853690501 if demoidnumber==816198217
replace demoidnumber=90636311 if demoidnumber==850695412


//replace demoidnumber=900466626 if demoidnumber==900466616
//replace demoidnumber=901083521 if demoidnumber==901083527
//replace demoidnumber=918567635 if demoidnumber==910567635
//replace demoidnumber=946457471 if demoidnumber==946457421
//replace demoidnumber=85367887	 if demoidnumber==853637882
//replace demoidnumber=411113470 if demoidnumber==411113970
//replace demoidnumber=850465480 if demoidnumber==850465485
//replace demoidnumber=850672078 if demoidnumber==850872078
//replace demoidnumber=853203562 if demoidnumber==853203560
//replace demoidnumber=414406916 if demoidnumber==414406918
//replace demoidnumber=950337778 if demoidnumber==950337378
//replace demoidnumber=850347997 if demoidnumber==850437997
//replace demoidnumber=860626250 if demoidnumber==850535360
//replace demoidnumber=854992138 if demoidnumber==700177637
//replace demoidnumber=852327122 if demoidnumber==852327022
//replace demoidnumber=85075736 if demoidnumber==850757634
//replace demoidnumber=402022209 if demoidnumber==402822209
//replace demoidnumber=833009645 if demoidnumber==853009645
//replace demoidnumber=8501170071 if demoidnumber==850470071
//replace demoidnumber=41085636 if demoidnumber==410852636
//replace demoidnumber=94659532 if demoidnumber==946595428
//replace demoidnumber=85049343 if demoidnumber==854049343
//replace demoidnumber=832760750 if demoidnumber==852760750
//replace demoidnumber=851478925 if demoidnumber==852467737
//replace demoidnumber=852637722 if demoidnumber==852637727
//replace demoidnumber=853672633 if demoidnumber==853672632
//replace demoidnumber=854682548 if demoidnumber==854692548
//replace demoidnumber=859503836 if demoidnumber==854503836
//replace demoidnumber=900386821 if demoidnumber==900386871
//replace demoidnumber=907612384 if demoidnumber==907612394
//replace demoidnumber=936248172 if demoidnumber==911666873
//replace demoidnumber=911703678 if demoidnumber==911783678
//replace demoidnumber=936118628 if demoidnumber==936118678
//replace demoidnumber=936248172 if demoidnumber==936748177
//replace demoidnumber=937217684 if demoidnumber==937717684
//replace demoidnumber=937218224 if demoidnumber==937718724
//replace demoidnumber=937219615 if demoidnumber==937719615
//replace demoidnumber=937220928 if demoidnumber==937720928
//replace demoidnumber=941282322 if demoidnumber==941287377
//replace demoidnumber=949487713 if demoidnumber==949987713
//replace demoidnumber=949996982 if demoidnumber==949996987
//replace demoidnumber=957623532 if demoidnumber==907623532
//replace demoidnumber=90719876 if demoidnumber==907149876
//replace demoidnumber=401941862 if demoidnumber==401949862
//replace demoidnumber=90636311 if demoidnumber==850695412
//replace demoidnumber=90517294 if demoidnumber==905171294
//replace demoidnumber=90719876 if demoidnumber==907149876
//replace demoidnumber=401941862 if demoidnumber==401949862
//replace demoidnumber=90636311 if demoidnumber==850695412
//replace demoidnumber=90517294 if demoidnumber==905171294
//replace demoidnumber=8540473687 if demoidnumber==850473687
//replace demoidnumber=118315274 if demoidnumber==415045400
//replace demoidnumber=9466413630 if demoidnumber==946641362
//replace demoidnumber=95679766 if demoidnumber==956792766
//replace demoidnumber=90027046 if demoidnumber==900027046
//replace demoidnumber=85364084 if demoidnumber==853640845
//replace demoidnumber=85343944 if demoidnumber==853193944
//replace demoidnumber=950232530 if demoidnumber==950232330
//replace demoidnumber=946641262 if demoidnumber==946641362
//replace demoidnumber=900270046 if demoidnumber==900027046
//replace demoidnumber=856781048 if demoidnumber==905611695
//replace demoidnumber=854909954 if demoidnumber==854909959
//replace demoidnumber=853686845 if demoidnumber==853686848
//replace demoidnumber=401466397 if demoidnumber==401466347
//replace demoidnumber=859023660 if demoidnumber==859023665
//replace demoidnumber=949296568 if demoidnumber==949296768
//replace demoidnumber=920574827 if demoidnumber==920674827
//replace demoidnumber=851937998 if demoidnumber==851937995
//replace demoidnumber=805234372 if demoidnumber==852343722
//replace demoidnumber=949559782 if demoidnumber==949559728
//replace demoidnumber=941121556 if demoidnumber==941121956
//replace demoidnumber=859894150 if demoidnumber==859894156
//replace demoidnumber=853929798 if demoidnumber==853629798
//replace demoidnumber=900433801 if demoidnumber==900435801
//replace demoidnumber=816198217 if demoidnumber==853690501
//replace demoidnumber=850695412 if demoidnumber==90636311



			








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
