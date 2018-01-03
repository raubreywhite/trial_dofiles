clear
capture set maxvar 30000

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
	
	display "$TAG"
	
	*Open booking .dta file 
	capture cd "Z:\data processing\"
	capture cd "X:\data processing\"

	if("$DATA_DATE"==""){
		error 100
	}
	*** create some folders
	capture mkdir "data_clean/$DATA_DATE"

	******************
	*** DEMOGRAPHICS
	****************

	*Open demographics .dta file 
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Demographics.csv", varnames(1) encoding("UTF-8") clear

	// FOR CONTROL DATA
	if(`IS_CONTROL'==1){
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

	save "data_temp/$TAG Clinical Demographics.dta", replace


	**************** 
	****CLINICAL BOOKING VISIT
	if(`IS_CONTROL'==1){
	
		/*
			Read in ANC followup
			Keep the first observation per pregnancy
		*/
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
		capture ren v2 programstageinstance
		sort programstageinstance eventdate
		bysort programstageinstance (eventdate): gen visitnum=_n
		keep if visitnum==1
		drop visitnum
		tempfile firstbookingvisit
		save `firstbookingvisit'
		
		/*
			Merge in the first ANC observation so that this file
			looks the same as the interventiond data
		*/
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Green File.csv", varnames(1) encoding("UTF-8") clear
		capture ren v2 programstageinstance
		
		count
		joinby programstageinstance using `firstbookingvisit', unm(b)
		count
		
		tab _merge
		keep if _merge==1 | _merge==3
		drop _merge
		
		
		
		
		gen identificationdocumentnumber=""
		gen ancallergiesdrugsandorseverefood=.
		gen ancpenicillinallergy=.
		gen anchistoryofothermedicineallergy=.
		gen ancothermedisineallergyspecified=.
		gen anchistroyofothersevereallergy=.
		gen ancothersevereallergyspecified=.
		gen anchistoryofchronichypertension=.
		gen anchistoryofotherchronicconditio=.
		gen anchistoryofblooddisorder =.
		gen ancfetalmovement=.
		gen  anchypothyreoidism =.
		gen ancintendedplaceofbirth=.
		gen ancdateoflastdelivery =.
		gen ancrecommendedplaceofbirth =.
		gen anctetanusboosterdose =.
		gen anchistoryofblooddisorderspecif =.
		gen ancfundalheightmeasurement =.
		gen ancedema =.
		gen ancutifollowupscheduled =.
		gen ancreferralneededforotherchronic =.
		gen anchistoryofdeepveinthrombosisdv =.
		gen whichancvisitisthis=.
		gen  ancmedicineprescription =.
		gen v81=.
		gen anchighriskdesignatedwoman =.
		gen anccounselingaboutironandfolicac =.
		
		gen ancmchhandbookprovided=.
		gen ancpreviousepisodesofthrombosis=.
		gen anchistoryofklexaneprovidedtothe=.
		gen anccounselingaboutdangersignsdur=.
		gen anccounselingonnutrioninpregnanc=.
		gen anccounselingaboutbreastfeeding=.
		gen anccounselingaboutlaborsigns=.
		gen anccounselingaboutpkuscreening=.
		gen ancppcvisitundertakenbywhom=.
		gen ppcwasthisinformationfirstcollec =.
		
		capture tostring anclmpstatus, replace
		capture tostring ancotherfamilyconcernspecified, replace
		capture tostring ancpallor, replace
		
	}
	else {
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Booking Visit.csv", varnames(1) encoding("UTF-8") clear
	}

	capture ren v2 programstageinstance 
	// delete people with duplicate eventdates (these are obvious duplicates)
	duplicates drop programstageinstance eventdate, force

	// delete people with duplicae LMPs (again, duplicate pregnancies(
	duplicates drop programstageinstance anclmpdate, force

	duplicates tag programstageinstance, gen("dup")
	//bro programstageinstance eventdate anclmpdate if dup>0

	

	*Rename variables
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber ancdiastolicbloodpressuremmhg ///
	anceclampticconvulsions ancpallor anclmpdate ///
	ancsuspectedpretermprematurerupt ancsuspectedprematureruptureofme ///
	ancsystolicbloodpressuremmhg ancallergiesdrugsandorseverefood ///
	ancpenicillinallergy anchistoryofothermedicineallergy ///
	ancothermedisineallergyspecified anchistroyofothersevereallergy ///
	ancothersevereallergyspecified anchistoryofchronichypertension ///
	anchistoryofdiabetesmellitusiori anchistoryofrenaldisease ///
	anchistoryofotherchronicconditio anchistoryofblooddisorder ///
	ancbodyheightm ancbodyweightkg ancmedicalexaminationofbreastsan ///
	anchistoryofbronchialasthma anchistoryofcardiacdisease ///
	ancconsecutiveabortions anchistoryofepilepsy ancfamilyhistoryofblooddisease ///
	ancfamilyhistoryofbronchialastma ancfamilyhistoryofcardiacdisease ///
	ancfamilyhistoryofcongenitalanom ancfamilyhistoryofdiabetesmellit ///
	ancfamilyhistoryofhypertension ancfamilyhistoryofinbornerrorofm ///
	ancfamilyhistoryofrenaldisease ancfetalmovement anchypothyreoidism ///
	ancintendedplaceofbirth ancdateoflastdelivery anchistoryofmentaldisturbance ///
	anchistoryofgestationaldiabetesm anchistoryofanypreviousperinatal ///
	ancreproductivetractinfectionrti anchistoryofpostpartumhemorrhage ///
	ancrecommendedplaceofbirth anctetanusboosterdose ///
	ancvaccinatedforttaccordingtogui anchistoryofuterineanomalyorinju ///
	anchistoryofuterinesurgeryexclud ancvaginalbleeding ///
	anchistoryofblooddisorderspecif anchistoryofcsections2 ///
	ancfundalheightmeasurement ancmedicalexaminationofabdomen ///
	ancmedicalexaminationofheart ancmedicalexaminationoflowerlimb ///
	ancmedicalexaminationoflungs ancmedicalexaminationofheadandne ///
	ancedema anclmpstatus ancinvitrofertilizationivf ///
	ancinfertility1yearpriortocurren ancutifollowupscheduled ///
	ancreferralneededforotherchronic anchistoryofeclampsiainanyprevio ///
	anchistoryofdeepveinthrombosisdv anchypertensioncerebralorvisuals ///
	ancfetalpresentationcheckedbypal ancfetalheartsoundfhs whichancvisitisthis ///
	ancprimigravidapg ancmedicineprescription v81 ///
	ancabnormalfindingsofheadandneck ///
	ancabnormalfindingsofabdomenspec ancabnormalfindingsofbreastsando ///
	ancabnormalfindingsoflowerlimbs ancabnormalfindingsoflungs ///
	ancabnormalfindingsofheart ancothermedicationsthewomaniscur ///
	anchistoryofmultiparity anchistoryofanypreviouspretermbi ///
	anchistoryofdvtrelatedtoanypre anchistoryofantepartumhemorrhag ///
	anchistoryofgestationalhypertent anchistoryofpreeclampsiainanypre ///
	anchistoryofpuerperalsepsisinany anc_gestationalageatvisitweeks ///
	anchighriskdesignatedwoman anchistoryofanycomplicatedcsecti ///
	anccounselingaboutironandfolicac ancmchhandbookprovided ///
	ancpreviousepisodesofthrombosis anchistoryofklexaneprovidedtothe ///
	anccounselingaboutdangersignsdur anccounselingonnutrioninpregnanc ///
	anccounselingaboutbreastfeeding anccounselingaboutlaborsigns ///
	anccounselingaboutpkuscreening ancotherrelevantfamilyhistorycon ///
	ancotherfamilyconcernspecified ancppcvisitundertakenbywhom ///
	anchomevisitoratthehealthclinic ppcwasthisinformationfirstcollec) ///
	(bookevent uniqueid bookprogstage bookdate booklong booklat bookorgname ///
	bookorgcode bookorgunit bookidnumber bookbpdiast bookeclamp bookpallor ///
	booklmp bookpprom bookprom bookbpsyst bookallerfood ///
	bookallerpen bookallerdrug bookallerdrugspec bookallersev bookallersevspec ///
	bookhisthtn bookhistdm bookhistrd bookhistotherch bookhistblood ///
	bookheight bookweight bookexambr bookhistasthma ///
	bookhistcard bookhistabort bookhistepi bookfamblood bookfamasthma ///
	bookfamcardiac bookfamcong bookfamdm bookfamhtn bookfammeta bookfamrd ///
	bookfetalmove bookhisthypothyr bookintendbirth bookdatelastbirth bookhistpsy ///
	bookhistgdm bookhistperi bookhistrti bookhistpph bookrecobirth bookttdose ///
	bookttguide bookhistute bookhistutesur bookvagbleed ///
	bookhistbloodspec bookhistcs bookexamsfh bookexamabd bookexamheart ///
	bookexamlimb bookexamlung bookexamhead bookexamedema booklmpknown bookhistivf ///
	bookhistinfert bookutifollow bookrefchronic ///
	bookhisteclamp bookhistdvt bookhisthtnsymp bookexampalp bookexamfh bookvisitspec ///
	bookprimi bookmedpres bookhistotherchronic bookexamheadabn bookexamabdabn ///
	bookexambreastabn bookexamlimbabn bookexamlungabn bookexamheartabn ///
	bookhistmed bookparity bookhistpreterm ///
	bookhistprevdvt bookhistaph bookhistghtn bookhistpreecl bookhistpuersep ///
	bookgestage bookhighrisk bookhistcscompl bookcounsifa bookancbook ///
	bookhistthrom bookhistclex bookcounsdanger ///
	bookcounsnut bookcounsbf bookcounslabor bookcounspku bookcounsfamhist bookfamspec ///
	bookseenby bookhomeorclinic bookbackupfile) 


	*Label variables 
	label variable bookevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable bookprogstage "Name of anc module" 
	label variable bookdate "Date- booking visit"
	label variable booklong "Longitude coordinates of clinic- booking visit"
	label variable booklat "Latitude coordinates of clinic- booking visit"
	label variable bookorgname "Name of clinic- booking visit"
	label variable bookorgcode "System generated code of clinic"
	label variable bookorgunit "System generated additional code of clinic"
	label variable bookidnumber "Woman's ID number"
	label variable bookbpdiast "Diastolic blood pressure- booking visit"
	label variable bookeclamp "Eclampsia in current pregnancy- booking visit"
	label variable bookpallor "Pallor on examination- booking visit"
	label variable booklmp "Last menstrual period date"
	label variable bookpprom "Preterm premature rupture of membranes in current pregnancy- booking visit"
	label variable bookprom "Preterm rupture of membranes in current pregnancy- booking visit"
	label variable bookbpsyst "Systolic blood pressure- booking visit"
	label variable bookallerfood "Allergy to food- booking visit"
	label variable bookallerpen "Allergy to penicillin- booking visit"
	label variable bookallerdrug "Allergy to other drugs- booking visit"
	label variable bookallerdrugspec "Allergy to other drug specified- booking visit"
	label variable bookallersev "Other severe allergy- booking visit"
	label variable bookallersevspec "Other severe allergy specified- booking visit"
	label variable bookhisthtn "History of chronic hypertension- booking visit"
	label variable bookhistdm "History of diabetes mellitus- booking visit" 
	label variable bookhistrd "History of renal disease- booking visit"
	label variable bookhistotherch "History of other chronic conditions- booking visit"
	label variable bookhistblood "History of blood disorders- booking visit"
	label variable bookheight "Height of woman- booking visit"
	label variable bookweight "Weight of woman- booking visit"
	label variable bookexambr "Examination of breast- booking visit"
	label variable bookhistasthma "History of bronchial asthma- booking visit"
	label variable bookhistcard "History of cardiac disease- booking visit"
	label variable bookhistabort "History of recurrent abortions- booking visit"
	label variable bookhistepi "History of epilepsy- booking visit"
	label variable bookfamblood "Family history of blood disorder- booking visit"
	label variable bookfamasthma "Family history of bronchial asthma- booking visit"
	label variable bookfamcardiac "Family history of cardiac disease- booking visit"
	label variable bookfamcong "Family history of congenital anomalies- booking visit"
	label variable bookfamdm "Family history of diabetes mellitus- booking visit"
	label variable bookfamhtn "Family history of hypertension- booking visit"
	label variable bookfammeta "Family history of inborn errors of metabolism- booking visit"
	label variable bookfamrd "Family history of renal disease- booking visit"
	label variable bookfetalmove "Fetal movements present or absent- booking visit"
	label variable bookhisthypothyr "History of hypothyroidism- booking visit"
	label variable bookintendbirth "Intended place of delivery- booking visit"
	label variable bookdatelastbirth "Most recent delivery date"
	label variable bookhistpsy "History of mental illness- booking visit"
	label variable bookhistgdm "History of gestational diabetes mellitus- booking visit"
	label variable bookhistperi "History of previous perinatal mortality- booking visit"
	label variable bookhistrti "History of reproductive tract infection- booking visit"
	label variable bookhistpph "History of postpartum hemorrhage"
	label variable bookrecobirth "Recommended place of delivery- booking visit"
	label variable bookttdose "Tetanus toxoid booster dose- booking visit"
	label variable bookttguide "Tetanus toxoid vaccinations given according to guidelines- booking visit"
	label variable bookhistute "History of uterine anomaly or injury- booking visit"
	label variable bookhistutesur "History of uterine surgery- booking visit"
	label variable bookvagbleed "Vaginal bleeding- booking visit"
	label variable bookhistbloodspec "History of blood disorder specified- booking visit"
	label variable bookhistcs "History of Caesarean section- booking visit"
	label variable bookexamsfh "Symphysiofundal height measurement- booking visit"
	label variable bookexamabd "Abdomen examination- booking visit"
	label variable bookexamheart "Heart examination- booking visit"
	label variable bookexamlimb "Lower limb examination- booking visit"
	label variable bookexamlung "Lung examination- booking visit"
	label variable bookexamhead "Head and neck examination- booking visit"
	label variable bookexamedema "Edema examination- booking visit"
	label variable booklmpknown "Last menstrual period date known or not- booking visit"
	label variable bookhistivf "Conception by in-vitro fertilization- booking visit"
	label variable bookhistinfert "History of infertility- booking visit"
	label variable bookutifollow "Follow-up scheduled for urinary tract infections- booking visit"
	label variable bookrefchronic "Woman referred for chronic conditions- booking visit"
	label variable bookhisteclamp "History of eclampsia in previous pregnancies- booking visit"
	label variable bookhistdvt "History of deep vein thrombosis in previous pregnancies- booking visit"
	label variable bookhisthtnsymp "Woman complaints of cerebral or visual symptoms suggestive of high blood pressure- booking visit"
	label variable bookexampalp "Palpation to check for presentation- booking visit"
	label variable bookexamfh "Fetal heart sound examination- booking visit"
	label variable bookvisitspec "Name of the anc module- booking visit"
	label variable bookprimi "Is this the first pregnancy- booking visit"
	label variable bookmedpres "Medicine prescriptions- booking visit"
	label variable bookhistotherchronic "History of other chronic conditions- booking visit"
	label variable bookexamheadabn "Head and neck examination abnormal- booking visit"
	label variable bookexamabdabn "Abdomen examination abnormal- booking visit"
	label variable bookexambreastabn "Breast examination abnormal- booking visit"
	label variable bookexamlimbabn "Lower limb examination abnormal- booking visit"
	label variable bookexamlungabn "Lung examination abnormal- booking visit"
	label variable bookexamheartabn "Heart examination abnormal- booking visit"
	label variable bookhistmed "History of taking medications- booking visit"
	label variable bookparity "Parity of the woman- booking visit"
	label variable bookhistpreterm "History of preterm labor in previous pregnancies- booking visit"
	label variable bookhistprevdvt "History of deep vein thrombosis in previous pregnancies- booking visit"
	label variable bookhistaph "History of antepartum hemorrhage in previous pregnancies- booking visit"
	label variable bookhistghtn "History of gestational hypertension in previous pregnancies- booking visit"
	label variable bookhistpreecl "History of preeclampsia in previous pregnancies- booking visit"
	label variable bookhistpuersep "History of peurperal sepsis in previous pregnancies- booking visit"
	label variable bookgestage "Gestational age at visit- booking visit"
	label variable bookhighrisk "High risk condition detected- booking visit"
	label variable bookhistcscompl "History of complicated Caesarean sections- booking visit"
	label variable bookcounsifa "Counseling for iron and folic acid tablets- booking visit"
	label variable bookancbook "MCH handbook given- booking visit"
	label variable bookhistthrom "History of episodes of thrombosis- booking visit"
	label variable bookhistclex "History of clexane use- booking visit"
	label variable bookcounsdanger "Counseling for danger signs- booking visit"
	label variable bookcounsnut "Counseling for nutrition and food intake- booking visit"
	label variable bookcounsbf "Counseling for breast feeding- booking visit"
	label variable bookcounslabor "Counseling for signs and symptoms of labor- booking visit"
	label variable bookcounspku "Counseling for PKU screening- booking visit"
	label variable bookcounsfamhist "Counseling for family history of conditions- booking visit"
	label variable bookfamspec "Family history of specific conditions- booking visit"
	label variable bookseenby "Woman seen by- booking visit"
	label variable bookhomeorclinic "Home visit or clinic visit- booking visit"
	label variable bookbackupfile "Information first entered on paper and then into the eRegistry- booking visit"

	/*
	*Create a small dataset with only uniqueid and date of booking visit
	keep uniqueid bookdate

	*Save as "int_anc booking visit_small dataset to get mother's age"
	save "data_temp/int_anc booking visit_small.dta", replace

	joinby uniqueid 
	*/
	*Open the saved dataset

	count // how many observations do we have at the start
	joinby uniqueid using "data_temp/$TAG Clinical Demographics.dta", unm(b)
	count // how many obs do we have after merging
	 
	 
	tab _merge // see where the obs came from
	tab _merge, nol
	//keep if _merge==3 // keep all people. people just in demographic file and not in green file are abortions
	drop _merge
	
	/*
		for the abortions, we now have "day of showing up and getting registered in demographic file"
		as "day of booking" (ie bookdate)
	*/
	
	gen is_demographic_not_greenfile=1 if missing(bookevent)
	sort bookevent
	gen temp=_n
	tostring temp, replace force
	replace bookdate=datecreated if missing(bookevent)
	replace bookevent=temp if missing(bookevent)
	drop temp
	
	// drop women whose 2nd, 3rd, etc pregnancies have LMPs before the first pregnancy's booking date
	sort uniqueid bookdate
	bysort uniqueid: gen min_event_date=bookdate if _n==1 // makes new variabe of woman's first event
	replace min_event_date=min_event_date[_n-1] if missing(min_event_date) // fills down empty spaces	
	
	bysort uniqueid (bookdate): gen booking_number=_n
	drop if booklmp < min_event_date & !missing(booklmp) & booking_number!=1
	drop min_event_date
	// TODO in the future, make it more general
	// i.e. so that that the 3rd pregnacy cannot have an LMP before the 2nd pregnancy
	

	di 1
	**Create a new variable for gestational age**

	*Convert dates into days// Dates are already formatted right in these datasets//
	foreach X in "bookdate" "booklmp" "dob" {
		display "`X'"

		gen year=substr(`X',1,4) // extract first four characters
		gen month=substr(`X',6,2) // extract 6-7 characters
		gen day=substr(`X',9,2) // extract 9-10 characters
		destring year, replace
		destring month, replace
		destring day, replace
		gen new`X' = mdy(month, day, year)
		drop year month day 
		format new`X' %td
		summarize new`X'
	}
di 2
	ren newdob newdobcorrect
	ren newbookdate newbookdatecorrect
	ren newbooklmp newbooklmpcorrect
di 
	// create unique set of ids so tht we know if it is first booking or second booking
	preserve
	keep bookevent uniqueid newbookdate booking_number
	reshape wide bookevent newbookdate, i(uniqueid) j(booking_number)
	save "data_temp/bookevent_ids.dta", replace
	restore



	*Generate an intermediate variable- difference between lmp and date of visit
	generate bookgestageinter= newbookdatecorrect-newbooklmpcorrect
	//Gives gestational age in days//

	*Calculate EDD based on LMP
	generate expecteddateofdelivery = newbooklmpcorrect + 280

	gen age1 = newbookdatecorrect  - newdobcorrect
	summarize age1

	gen age = floor(age1/365)
	summarize age

	gen agecat = .
	summarize agecat

	///missing values////

	replace agecat = 1 if (age <=20)
	replace agecat = 2 if (age >=21) & (age <=25)
	replace agecat = 3 if (age >=26) & (age <=30)
	replace agecat = 4 if (age >=31) & (age <=35)
	replace agecat = 5 if (age >=36) & (age <=40)
	replace agecat = 6 if (age >=41) & (age <=45)
	replace agecat = 7 if (age >=46) & (age <=50)
	replace agecat = 8 if (age >50)

	count if missing(age)
	count if missing(agecat)

	*Describe data 
	describe

	save "data_temp/IDENTIFIABLE $TAG ANC demographics and booking.dta", replace

	keep uniqueid bookevent newbooklmpcorrect 
	duplicates drop
	save "data_temp/booklmp.dta",replace

	*******************
	**** ANTENATAL CARE VISITS
	*Open ANC visits .dta file 
	if(`IS_CONTROL'==1){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANC Follow up sheet.csv", varnames(1) encoding("UTF-8") clear
		capture ren v2 programstageinstance
		sort programstageinstance eventdate
		bysort programstageinstance (eventdate): gen visitnum=_n
		keep if visitnum!=1
		drop visitnum
		
		gen identificationdocumentnumber=""
		
		gen ancallergiesdrugsandorseverefood=.
		gen ancpenicillinallergy=.
		gen anchistoryofothermedicineallergy=.
		gen ancothermedisineallergyspecified=.
		gen anchistroyofothersevereallergy=.
		gen ancothersevereallergyspecified=.
		gen anchistoryofchronichypertension=.
		gen anchistoryofotherchronicconditio=.
		gen anchistoryofblooddisorder =.
		gen ancfetalmovement=.
		gen  anchypothyreoidism =.
		//gen ancintendedplaceofbirth=.
		//gen ancdateoflastdelivery =.
		//gen ancrecommendedplaceofbirth =.
		//gen anctetanusboosterdose =.
		gen anchistoryofblooddisorderspecif =.
		gen ancfundalheightmeasurement =.
		gen ancedema =.
		//gen ancutifollowupscheduled =.
		gen ancreferralneededforotherchronic =.
		gen anchistoryofdeepveinthrombosisdv =.
		gen whichancvisitisthis=.
		gen  ancmedicineprescription =.
		//gen v81=.
		gen anchighriskdesignatedwoman =.
		gen anccounselingaboutironandfolicac =.
		
		//gen ancmchhandbookprovided=.
		gen ancpreviousepisodesofthrombosis=.
		gen anchistoryofklexaneprovidedtothe=.
		gen anccounselingaboutdangersignsdur=.
		gen anccounselingonnutrioninpregnanc=.
		gen anccounselingaboutbreastfeeding=.
		gen anccounselingaboutlaborsigns=.
		gen anccounselingaboutpkuscreening=.
		gen ancppcvisitundertakenbywhom=.
		gen ppcwasthisinformationfirstcollec =.
		
		
		gen ancsuspectedprematureruptureofme=.
		gen ancsuspectedpretermprematurerupt =.
		gen  anceclampticconvulsions =.
		gen ancvaginalbleeding =.
		gen anchypertensioncerebralorvisuals =.
		gen  anc_gestationalageatvisitweeks =.
		gen  ancotheridentifiedconditions =.
		gen anchistoryofdiabetesmellitusiori=.
		gen anchistoryofrenaldisease =.
		gen anchistoryofbronchialasthma=.
		gen anchistoryofepilepsy =.
		gen anchistoryofcardiacdisease =.
		gen anchistoryofmentaldisturbance =.
		gen ancreproductivetractinfectionrti =.
		gen v42=.
		gen  anchomevisitoratthehealthclinic =.
		
		// in intervention, this is just a booking variable
		drop ancvaccinatedforttaccordingtogui
		
	}
	else {
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Antenatal care visit.csv", varnames(1) encoding("UTF-8") clear
	}
	
	capture ren v2 programstageinstance

	*Rename variables
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber ancsuspectedprematureruptureofme ///
	ancsuspectedpretermprematurerupt anceclampticconvulsions ///
	ancvaginalbleeding ancfetalmovement ancbodyweightkg ///
	ancdiastolicbloodpressuremmhg ancsystolicbloodpressuremmhg ///
	ancedema ancfetalheartsoundfhs ancfundalheightmeasurement ///
	anchypertensioncerebralorvisuals ancfetalpresentationcheckedbypal ///
	whichancvisitisthis ancmedicineprescription anc_gestationalageatvisitweeks ///
	anchighriskdesignatedwoman ancotheridentifiedconditions ///
	anchistoryofchronichypertension anchistoryofdiabetesmellitusiori ///
	anchistoryofrenaldisease anchypothyreoidism anchistoryofbronchialasthma ///
	anchistoryofblooddisorder anchistoryofblooddisorderspecif ///
	anchistoryofepilepsy anchistoryofcardiacdisease anchistoryofmentaldisturbance ///
	ancreproductivetractinfectionrti anchistoryofdeepveinthrombosisdv ///
	anchistoryofotherchronicconditio v42 ancreferralneededforotherchronic ///
	ancallergiesdrugsandorseverefood anchistoryofothermedicineallergy ///
	ancothermedisineallergyspecified anchistroyofothersevereallergy ///
	ancothersevereallergyspecified ancpenicillinallergy ///
	ancothermedicationsthewomaniscur anccounselingaboutironandfolicac ///
	ancpreviousepisodesofthrombosis anchistoryofklexaneprovidedtothe ///
	anccounselingaboutlaborsigns anccounselingaboutpkuscreening ///
	anccounselingaboutdangersignsdur anccounselingonnutrioninpregnanc ///
	anchomevisitoratthehealthclinic ppcwasthisinformationfirstcollec ///
	ancppcvisitundertakenbywhom) (anevent uniqueid anprogstage andate ///
	anlong anlat anorgname anorgcode anorgunit anidnumber anprom anpprom ///
	aneclamp anvagbleed anfetalmove  anweight anbpdiast ///
	anbpsyst anexamedema anexamfh anexamsfh anhisthtnsymp ///
	anexampalp anvisitweeks anmedpres angestage anhighrisk anothercond ///
	anhisthtn anhistdm anhistrd anhisthypothyr anhistasthma ///
	anhistblood anhistbloodspec anhistepi anhistcardiac ///
	anhistpsy anhistrti anhistdvt anhistchronic anhistchronicspec ///
	anrefchronic anallerfood anallerdrug anallerdrugspec anallersev ///
	anallersevspec anallerpen anothermed ancounsifa anhistthr anchistclex ///
	ancounslabor ancounspku ancounsdanger ancounsnut anhomeorclinic anbackupfile ///
	anseenby)

	*Label variables 
	label variable anevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets" 
	label variable anprogstage "Name of anc module"
	label variable andate "Date- antenatal visit"
	label variable anlong "Longitude coordinates of clinic- antenatal visit"
	label variable anlat "Latitude coordinates of clinic- antenatal visit"
	label variable anorgname "Name of clinic- antenatal visit"
	label variable anorgcode "System generated code of clinic"
	label variable anorgunit "System generated additional code of clinic"
	label variable anidnumber "Woman's ID number"
	label variable anprom "Premature rupture of membranes- antenatal visit"
	label variable anpprom "Preterm premature rupture of membranes- antenatal visit"
	label variable aneclamp "Eclampsia in current pregnancy- antenatal visit"
	label variable anvagbleed "Vaginal bleeding in current pregnancy- antenatal visit"
	label variable anfetalmove  "Fetal movements present or absent- antenatal visit"
	label variable anweight "Woman's weight- antenatal visit"
	label variable anbpdiast "Diastolic blood pressure- antenatal visit"
	label variable anbpsyst "Systolic blood pressure- antenatal visit"
	label variable anexamedema "Edema examination- antenatal visit"
	label variable anexamfh "Fetal heart sounds examination- antenatal visit"
	label variable anexamsfh "Symphysiofundal height measurement- antenatal visit"
	label variable anhisthtnsymp "Woman complaints of cerebral or visual symptoms suggestive of high blood pressure- antenatal visit"
	label variable anexampalp "Palpation to check for presentation- antenatal visit" 
	label variable anvisitweeks "Week of the antenatal care visit- antenatal visit"
	label variable anmedpres "Medicine prescription- booking visit"
	label variable angestage "Gestational age at visit- antenatal visit"
	label variable anhighrisk "High risk pregnancy or not- antenatal visit"
	label variable anothercond "Other conditions identified- antenatal visit"
	label variable anhisthtn "History of chronic hypertension- antenatal visit"
	label variable anhistdm "History of diabetes mellitus- antenatal visit"
	label variable anhistrd "History of renal disease- antenatal visit"
	label variable anhisthypothyr "History of hypothryroidism- antenatal visit"
	label variable anhistasthma "History of bronchial asthma- antenatal visit"
	label variable anhistblood "History of blood disorders- antenatal visit"
	label variable anhistbloodspec "History of blood disorders specified- antenatal visit"
	label variable anhistepi "History of epilepsy- antenatal visit"
	label variable anhistcardiac "History of cardiac disease- antenatal visit"
	label variable anhistpsy "History of mental illness- antenatal visit"
	label variable anhistrti "History of reproductive tract infections- antenatal visit"
	label variable anhistdvt "History of deep vein thrombosis- antenatal visit"
	label variable anhistchronic "History of chronic conditions- antenatal visit"
	label variable anhistchronicspec "History of chronic conditions specified- antenatal visit"
	label variable anrefchronic "Referral for chronic conditions- antenatal visit"
	label variable anallerfood "Allergy to food- antenatal visit"
	label variable anallerdrug "Allergy to other drugs- antenatal visit"
	label variable anallerdrugspec "Allergy to other drug specified- antenatal visit"
	label variable anallersev "Other severe allergy- antenatal visit"
	label variable anallersevspec "Other severe allergy specified- antenatal visit"
	label variable anallerpen "Allergy to penicillin- antenatal visit"
	label variable anothermed "Other medications woman is taking- antenatal visit"
	label variable ancounsifa "Counseling for iron and folic acid tablets- antenatal visit"
	label variable anhistthr "History of episodes of thrombosis- antenatal visit"
	label variable anchistclex "History of clexane use- antenatal visit"
	label variable ancounslabor "Counseling for signs and symptoms of labor- antenatal visit"
	label variable ancounspku "Counseling for PKU screening- antenatal visit"
	label variable ancounsdanger "Counseling for danger signs- antenatal visit"
	label variable ancounsnut "Counseling for nutrition and food intake- antenatal visit"
	label variable anhomeorclinic "Home visit or clinic visit- antenatal visit"
	label variable anbackupfile "Information first entered on paper and then into the eRegistry- antenatal visit"
	label variable anseenby "Woman seen by- antenatal visit"
display "MAYBE WRONG 1"
	count
	joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
	count
display "MAYBE WRONG 2"
	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "andate" {
		display "`X'"

		gen year=substr(`X',1,4) // extract first four characters
		gen month=substr(`X',6,2) // extract 6-7 characters
		gen day=substr(`X',9,2) // extract 9-10 characters
		destring year, replace
		destring month, replace
		destring day, replace
		gen new`X' = mdy(month, day, year)
		drop year month day 
		format new`X' %td
		summarize new`X'
	}
	display "MAYBE WRONG 3"
	drop andate
	ren newandate andate
display "MAYBE WRONG 4"
	count if andate < newbookdatecorrect1
	count if andate < newbookdatecorrect1 & !missing(newbookdatecorrect1)


	**
	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if andate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*
	**

	count
	joinby uniqueid bookevent using "data_temp/booklmp.dta", unm(b)
	count

	tab _merge
	drop if _merge==2
	drop _merge

	gen angestationalage = andate - newbooklmpcorrect
	drop newbooklmpcorrect


	save "data_temp/IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", replace

	sort uniqueid bookevent andate
	//duplicates drop uniqueid bookevent andate, force
	bysort uniqueid bookevent: gen eventnumber=_n

	ren anccounselingaboutbreastfeeding anccounselingaboutbf

	 
	
	codebook uniqueid
	if(`IS_CONTROL'==1){
		ren con_anc_gestationaageatvisitweek con_anc_gestageatvisitweek
		ren con_anc_gestationaageatvisitsize con_anc_gestageatvisitsize
		reshape wide an* con* usrecommendationscomments, i(uniqueid bookevent) j(eventnumber)
	}
	else {
		reshape wide an*, i(uniqueid bookevent) j(eventnumber)
	}
	codebook uniqueid

	save "data_temp/wide_IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", replace

	******************
	******************
	*******************
	*** LAB STUF
	*******************
	import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Lab results.csv", varnames(1) encoding("UTF-8") clear
	*Open lab .dta file 
	*Rename variables
	if(`IS_CONTROL'==1){
	
		// GENERATING anc_gestationalageatvisitweeks
		gen uniqueid =v2
		count
		joinby uniqueid  using "data_temp/booklmp.dta", unm(b)
		drop bookevent
		count

		tab _merge
		drop if _merge==2
		drop _merge
		/*
		FIX DATE HERE
		*/
		capture drop temp
		gen temp=substr(eventdate,1,10)
		gen date = date(temp, "YMD")
		format %td date
		
		gen anc_gestationalageatvisitweeks = floor((date - newbooklmpcorrect)/7)
		drop newbooklmpcorrect uniqueid temp date
		replace anc_gestationalageatvisitweeks=. if anc_gestationalageatvisitweeks<2 | anc_gestationalageatvisitweeks>42
		// FINISHED GENERATING anc_gestationalageatvisitweeks
		
		gen identificationdocumentnumber=""
		gen labwherewerethetestsperformed=.
		gen labtestsperformedatotherspecifie =.
		gen ancorppcvisit =.
		gen labcbcwhitebloodcells =.
		gen labcbcredbloodcellcount =.
		gen labanemiatreatmentresponse =.
		gen labcbcmeancorpuscularvolume =.
		gen  labcbcplatelets =.
		gen labsecondopiniononinitiatedtreat =.
		gen laboralglucosechallengetestogctm =.
		
	}
	
	ren v2 programstageinstance
	

	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber anc_gestationalageatvisitweeks ///
	labwherewerethetestsperformed labtestsperformedatotherspecifie ///
	ancorppcvisit labcbcwhitebloodcells labcbcredbloodcellcount ///
	labcbchemoglobin labanemiatreatmentresponse labcbchematocrit ///
	labcbcmeancorpuscularvolume labcbcplatelets labbloodgrouping ///
	labrhdtyping labindirectcoombs laburinestickprotein laburinesticksugar ///
	ancurineanalysisforurinarytracki labsecondopiniononinitiatedtreat ///
	labrandombloodsugarmgdl labfastingbloodsugarmgdl ///
	laboralglucosechallengetestogctm ancotherlabtest1 ///
	anclabresultofotherlabtest1 ancotherlabtest2 anclabresultofotherlabtest2 ///
	ancotherlabtest3 anclabresultofotherlabtest3) (labevent uniqueid labprogstage ///
	labdate lablong lablat laborgname laborgcode laborgunit labidnumber ///
	labgestage labplace labplacespec labanpp labwbc labrbc labhb labanemresp ///
	labhct labmcv labplatelets labbloodgp labrh labict laburpro laburglu ///
	laburuti laburutirep labbloodglu labfastbloodglu labogct labother1 labotherres1 ///
	labother2 labotherres2 labother3 labotherres3)

	*Label variables 
	label variable labevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable labprogstage "Name of anc module"
	label variable labdate "Date- lab test"
	label variable lablong "Longitude coordinates of clinic- lab test"
	label variable lablat "Latitude coordinates of clinic- lab test"
	label variable laborgname "Name of clinic- lab test"
	label variable laborgcode "System generated code of clinic"
	label variable laborgunit "System generated additional code of clinic"
	label variable labidnumber "Woman's ID number"
	label variable labgestage "Gestational age at visit- lab test"
	label variable labplace "Laboratory in which test is performed- lab test"
	label variable labplacespec "Laboratory in which test is performed specified- lab test"
	label variable labanpp "Does the lab test correspond to anc visit or ppc visit"
	label variable labwbc "White blood cell count- lab test"
	label variable labrbc "Red blood cell count- lab test"
	label variable labhb "Hemoglobin- lab test"
	label variable labanemresp "Hemoglobin to measure response to anemia treatment- lab test"
	label variable labhct "Hematocrit- lab test"
	label variable labmcv "Mean corpuscular volume- lab test"
	label variable labplatelets "Platelet count- lab test"
	label variable labbloodgp "Blood grouping- lab test"
	label variable labrh "Rh typing- lab test"
	label variable labict "Indirect Coomb's test- lab test"
	label variable laburpro "Urine stick protein- lab test" 
	label variable laburglu "Urine stick glucose- lab test"
	label variable laburuti "Urine analysis for urinary tract infection- lab test"
	label variable laburutirep "Urine analysis repeat for follow-up of urinary tract infection- lab test"
	label variable labbloodglu "Random blood sugar- lab test"
	label variable labfastbloodglu "Fasting blood sugar- lab test"
	label variable labogct "Oral glucose challenge test- lab test"
	label variable labother1 "Other lab test 1- lab test"
	label variable labotherres1 "Result of other lab test 1- lab test"
	label variable labother2 "Other lab test 2- lab test"
	label variable labotherres2 "Result of other lab test 2- lab test"
	label variable labother3 "Other lab test 3- lab test"
	label variable labotherres3 "Result of other lab test 3- lab test"

	count
	joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
	count

	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "labdate" {
		display "`X'"

		gen year=substr(`X',1,4) // extract first four characters
		gen month=substr(`X',6,2) // extract 6-7 characters
		gen day=substr(`X',9,2) // extract 9-10 characters
		destring year, replace
		destring month, replace
		destring day, replace
		gen new`X' = mdy(month, day, year)
		drop year month day 
		format new`X' %td
		summarize new`X'
	}
	drop labdate
	ren newlabdate labdate

	count if labdate < newbookdatecorrect1
	count if labdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

	**
	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if labdate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*
	**

	* LAB STUFF
	save "data_temp/IDENTIFIABLE $TAG Clinical Lab results.dta", replace

	sort uniqueid bookevent labdate
	//duplicates drop uniqueid bookevent labdate, force
	bysort uniqueid bookevent: gen eventnumber=_n

	codebook uniqueid
	if(`IS_CONTROL'==0){
		reshape wide lab*, i(uniqueid bookevent) j(eventnumber)
	}
	else {
		reshape wide lab* us*, i(uniqueid bookevent) j(eventnumber)
	}
	codebook uniqueid

	save "data_temp/wide_IDENTIFIABLE $TAG Clinical Lab results.dta", replace


	*********************
	*********************
	*********************
	** ULTRASOUND STUFF
	*********************
	*********************
	*Open ultrasound .dta file 
	if(`IS_CONTROL'==1){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Ultrasound.csv", varnames(1) encoding("UTF-8") clear
		
		// GENERATING anc_gestationalageatvisitweeks
		gen uniqueid =v2
		count
		joinby uniqueid  using "data_temp/booklmp.dta", unm(b)
		drop bookevent
		count

		tab _merge
		drop if _merge==2
		drop _merge
		/*
		FIX DATE HERE
		*/
		capture drop temp
		gen temp=substr(eventdate,1,10)
		gen date = date(temp, "YMD")
		format %td date
		
		gen anc_gestationalageatvisitweeks = floor((date - newbooklmpcorrect)/7)
		drop newbooklmpcorrect uniqueid temp date
		replace anc_gestationalageatvisitweeks=. if anc_gestationalageatvisitweeks<2 | anc_gestationalageatvisitweeks>42
		// FINISHED GENERATING anc_gestationalageatvisitweeks
		
		gen identificationdocumentnumber=""
		gen usreason =.
		gen usperformanceofultrasound =.
		gen usperformedelsewhere =.
		gen usectopicpregnancy =.
		gen usmolarpregnancy =.
		gen usfetusdesignationformultiplepre =.
		gen  usplacentalocation =.
		gen usplacentapreviagrade=.
		gen  usplacentaorientation =.
		gen usfetalsex =.
		gen  usanomalies =.
		gen usanomaliesspecified =.
		gen  uspelvicmass =.
		gen usovariancysts=.
		gen usfibroids =.
		
	}
	else if(`IS_CONTROL'==0){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Ultrasound results.csv", varnames(1) encoding("UTF-8") clear
	}

	capture rename v2 programstageinstance

	*Rename variables
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber anc_gestationalageatvisitweeks usreason ///
	usperformanceofultrasound usperformedelsewhere usectopicpregnancy ///
	usmolarpregnancy ancusnumberoffetuses usfetusdesignationformultiplepre ///
	usfetusheartactivity usplacentalocation usplacentapreviagrade ///
	usplacentaorientation usfetalpresentation usfetalsex ///
	usamnioticfluiddeepestpocket usamnioticfluidindexcm usamnioticfluidquantity ///
	usgestationalsac usgestationalsacinmm usgestationalsacinweeks ///
	uscrownrumplengthmm uscrownrumplengthweeks usbiparietaldiametermm ///
	usbiparietaldiameterweeks usfemurlengthmm usfemurlengthweeks ///
	usabdominalcircumferencemm usabdominalcircumferenceweeks ///
	usestimatedgestationalageegaweek usestimatedgestationalageegadays ///
	usestimatedfetalweightgm usultrasoundestimateddateofdeliv usanomalies ///
	usanomaliesspecified uspelvicmass usovariancysts usfibroids ///
	ussuspectedintrauterinegrowthres ussuspectedlargeforgestationalag ///
	usrecommendationscomments) (usevent uniqueid usprogstage usdate uslong uslat ///
	usorgname usorgcode usorgunit usidnumber usgestage usreason usplace ///
	usplaceother usectopic usmolar usnumberfetus usmultifetdesignation ///
	usfh usplacenplace usplacenprev usplacenor uspres ussex usamnideeppoc ///
	usamniindex usamniquant usgestsac usgestsacmm usgestsacweek uscrlmm uscrlweeks ///
	usbpdmm usbpdweeks usfemurmm usfemurweeks usacmm usacweeks usegaweeks ///
	usegadays usefw usedd usanom usanomspec usmass usovcyst usfibroid usiugr ///
	uslga uscomments)

	*Label variables 
	label variable usevent "System generated additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable usprogstage "Name of anc module"
	label variable usdate "Date- ultrasound"
	label variable uslong "Longitude coordinates of clinic- ultrasound"
	label variable uslat "Latitude coordinates of clinic- ultrasound"
	label variable usorgname "Name of clinic- ultrasound"
	label variable usorgcode "System generated code of clinic"
	label variable usorgunit "System generated additional code of clinic"
	label variable usidnumber "Woman's ID number"
	label variable usgestage "Gestational age at visit- ultrasound"
	label variable usreason "Indication for ultrasound- ultrasound"
	label variable usplace "Place where ultrasound is performed- ultrasound"
	label variable usplaceother "Place where ultrasound is performed specified- ultrasound"
	label variable usectopic "Ectopic pregnancy- ultrasound"
	label variable usmolar "Molar pregnancy- ultrasound"
	label variable usnumberfetus "Number of fetuses- ultrasound"
	label variable usmultifetdesignation "Designation of fetus in multiple pregnancy- ultrasound"
	label variable usfh "Fetal heart- ultrasound"
	label variable usplacenplace "Placental location- ultrasound"
	label variable usplacenprev "Placenta previa grade- ultrasound"
	label variable usplacenor "Placenta orientation- ultrasound"
	label variable uspres "Fetal presentation- ultrasound"
	label variable ussex "Fetal sex- ultrasound"
	label variable usamnideeppoc "Amniotic fluid deep pocket- ultrasound"
	label variable usamniindex "Amniotic fluid index- ultrasound"
	label variable usamniquant "Amniotic fluid quantity- ultrasound"
	label variable usgestsac "Gestational sac- ultrasound"
	label variable usgestsacmm "Gestational sac in millimeter- ultrasound"
	label variable usgestsacweek "Gestational sac in weeks- ultrasound"
	label variable uscrlmm "Crown rump length in millimeter- ultrasound"
	label variable uscrlweeks "Crown rump length in weeks- ultrasound"
	label variable usbpdmm "Bipareital diameter in millimeter- ultrasound"
	label variable usbpdweeks "Bipareital diameter in weeks- ultrasound"
	label variable usfemurmm "Femur length in millimeter- ultrasound"
	label variable usfemurweeks "Femur length in weeks- ultrasound"
	label variable usacmm "Abdominal circumference in millimeter- ultrasound"
	label variable usacweeks "Abdominal circumference in weeks- ultrasound"
	label variable usegaweeks "Estimated gestational age in weeks- ultrasound"
	label variable usegadays "Estimated gestational age in days- ultrasound"
	label variable usefw "Estimated fetal weight- ultrasound"
	label variable usedd "Expected date of delivery- ultrasound"
	label variable usanom "Fetal anomalies- ultrasound"
	label variable usanomspec "Fetal anomalies specified- ultrasound"
	label variable usmass "Pregnancy with pelvic mass- ultrasound"
	label variable usovcyst "Ovarian cyst- ultrasound"
	label variable usfibroid "Fibroid uterus- ultrasound"
	label variable usiugr "Suspected intrauterine growth restriction- ultrasound"
	label variable uslga "Suspected large for gestational age- ultrasound"
	label variable uscomments "Comments and recommendations- ultrasound"


	count
	joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
	count

	tab _merge
	drop if _merge==2 // dropping ids
	drop _merge

	foreach X in "usdate" {
		display "`X'"

		gen year=substr(`X',1,4) // extract first four characters
		gen month=substr(`X',6,2) // extract 6-7 characters
		gen day=substr(`X',9,2) // extract 9-10 characters
		destring year, replace
		destring month, replace
		destring day, replace
		gen new`X' = mdy(month, day, year)
		drop year month day 
		format new`X' %td
		summarize new`X'
	}
	drop usdate
	ren newusdate usdate

	count if usdate < newbookdatecorrect1
	count if usdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

	gen bookevent = bookevent1
	foreach X of varlist bookevent* {
		if("`X'"=="bookevent"){
			continue
		}
		di "`X'"
		local XVAL=subinstr("`X'","bookevent","",.)
		replace bookevent = bookevent`XVAL' if usdate >= newbookdatecorrect`XVAL'
		drop bookevent`XVAL'
	}

	drop newbookdatecorrect*

	// FIX THIS VARIABLE usedd
	// change it from being a string into
	// being a date
	capture drop temp
	gen temp=substr(usedd,1,10)
	//drop usedd
	replace temp=lower(temp)
	capture drop date
	gen date = date(temp, "YMD")
	format %td date
	//bro date usedd
	drop temp
	drop usedd
	rename date usedd

	save "data_temp/IDENTIFIABLE $TAG Clinical Ultrasound results.dta", replace

	sort uniqueid bookevent usdate
	//duplicates drop uniqueid bookevent andate, force
	bysort uniqueid bookevent: gen eventnumber=_n

	codebook uniqueid
	reshape wide us*, i(uniqueid bookevent) j(eventnumber)
	codebook uniqueid

	save "data_temp/wide_IDENTIFIABLE $TAG Clinical Ultrasound results.dta", replace


	use "data_temp/IDENTIFIABLE $TAG Clinical Ultrasound results.dta", clear

	keep uniqueid bookevent usgestage usdate
	gen us_expected_due_date=usdate-usgestage*7+280
	format %td us_expected_due_date

	drop if missing(usgestage)
	drop if usgestage==0
	bysort uniqueid bookevent (usdate): egen latestDate=max(usdate)
	keep if latestDate==usdate
	drop latestDate

	keep uniqueid bookevent us_expected_due_date

	duplicates drop

	collapse (mean) us_expected_due_date, by(uniqueid bookevent)
	replace us_expected_due_date = round(us_expected_due_date)

	save "data_temp/IDENTIFIABLE $TAG clinical ultrasounds expected due date.dta", replace

	************************
	************************
	**** RISK FACTORS
	if(`IS_CONTROL'==0){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANCRisks.csv", varnames(1) encoding("UTF-8") clear

		capture ren v2 programstageinstance

		*Rename variables
		rename (event programstageinstance programstage eventdate longitude latitude ///
		organisationunitname organisationunitcode organisationunit ///
		identificationdocumentnumber ancrisktype riskdescription ///
		anc_gestationalageatvisitweeks createdeventidentifier) (riskevent uniqueid ///
		riskprogstage riskdate risklong risklat riskorgname riskorgcode riskorgunit ///
		riskidnumber risktype riskdesx riskgestage riskdesy)

		*Label variables 
		label variable riskevent "System generated additional identifier"
		label variable uniqueid "System generated identifier common to all datasets"
		label variable riskprogstage "Name of anc module"
		label variable riskdate "Date- risk detection"
		label variable risklong "Longitude coordinates of clinic- risk detection"
		label variable risklat "Latitude coordinates of clinic- risk detection"
		label variable riskorgname "Name of clinic- risk detection"
		label variable riskorgcode "System generated code of clinic"
		label variable riskorgunit "System generated additional code of clinic"
		label variable riskidnumber "Woman's ID number"
		label variable risktype "Type of risk identified- risk detection"
		label variable riskdesx "Description of risk- risk detection"
		label variable riskgestage "Gestational age- risk detection"
		label variable riskdesy "Description of risk identified additional- risk detection"

		count
		joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
		count

		tab _merge
		drop if _merge==2 // dropping ids
		drop _merge

		foreach X in "riskdate" {
			display "`X'"

			gen year=substr(`X',1,4) // extract first four characters
			gen month=substr(`X',6,2) // extract 6-7 characters
			gen day=substr(`X',9,2) // extract 9-10 characters
			destring year, replace
			destring month, replace
			destring day, replace
			gen new`X' = mdy(month, day, year)
			drop year month day 
			format new`X' %td
			summarize new`X'
		}
		drop riskdate
		ren newriskdate riskdate

		count if riskdate < newbookdatecorrect1
		count if riskdate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

		**
		gen bookevent = bookevent1
		foreach X of varlist bookevent* {
			if("`X'"=="bookevent"){
				continue
			}
			di "`X'"
			local XVAL=subinstr("`X'","bookevent","",.)
			replace bookevent = bookevent`XVAL' if riskdate >= newbookdatecorrect`XVAL'
			drop bookevent`XVAL'
		}

		drop newbookdatecorrect*
		**

		save "data_temp/IDENTIFIABLE $TAG Clinical ANCRisks.dta", replace

		sort uniqueid bookevent riskdate
		//duplicates drop uniqueid bookevent riskdate, force
		bysort uniqueid bookevent: gen eventnumber=_n

		codebook uniqueid
		reshape wide risk*, i(uniqueid bookevent) j(eventnumber)
		codebook uniqueid

		save "data_temp/wide_IDENTIFIABLE $TAG Clinical ANCRisks.dta", replace


		*********************
		*********************
		*********************
		** MANAGEMENTS
		*********************
		*********************

		*Open ultrasound .dta file 
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL ANCManagements.csv", varnames(1) encoding("UTF-8") clear

		capture ren v2 programstageinstance

		*Rename variables
		rename (event programstageinstance programstage eventdate longitude latitude ///
		organisationunitname organisationunitcode organisationunit ///
		identificationdocumentnumber anc_gestationalageatvisitweeks ///
		managementdetails managementtype managementperformed createdeventidentifier) ///
		(manevent uniqueid manprogstage mandate manlong manlat manorgname manorgcode ///
		manorgunit manidnumber mangestage mandetail mantypex manperf mantypey)

		*Label variables 
		label variable manevent "System generated additional identifier"
		label variable uniqueid "System generated identifier common to all datasets"
		label variable manprogstage "Name of anc module"
		label variable mandate "Date- management"
		label variable manlong "Longitude coordinates of clinic- management"
		label variable manlat "Latitude coordinates of clinic- management"
		label variable manorgname "Name of clinic- management"
		label variable manorgcode "System generated code of clinic"
		label variable manorgunit "System generated additional code of clinic"
		label variable manidnumber "Woman's ID number"
		label variable mangestage "Gestational age- management"
		label variable mandetail "Details of treatment or referral- management"
		label variable mantypex "Type of management- management"
		label variable manperf "Whether management was performed or not"
		label variable mantypey "Type of management additional- management"


		count
		joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
		count

		tab _merge
		drop if _merge==2 // dropping ids
		drop _merge

		foreach X in "mandate" {
			display "`X'"

			gen year=substr(`X',1,4) // extract first four characters
			gen month=substr(`X',6,2) // extract 6-7 characters
			gen day=substr(`X',9,2) // extract 9-10 characters
			destring year, replace
			destring month, replace
			destring day, replace
			gen new`X' = mdy(month, day, year)
			drop year month day 
			format new`X' %td
			summarize new`X'
		}
		drop mandate
		ren newmandate mandate

		count if mandate < newbookdatecorrect1
		count if mandate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

		**
		gen bookevent = bookevent1
		foreach X of varlist bookevent* {
			if("`X'"=="bookevent"){
				continue
			}
			di "`X'"
			local XVAL=subinstr("`X'","bookevent","",.)
			replace bookevent = bookevent`XVAL' if mandate >= newbookdatecorrect`XVAL'
			drop bookevent`XVAL'
		}

		drop newbookdatecorrect*
		**

		save "data_temp/IDENTIFIABLE $TAG Clinical ANCManagements.dta", replace


		sort uniqueid bookevent mandate
		//duplicates drop uniqueid bookevent andate, force
		bysort uniqueid bookevent: gen eventnumber=_n

		codebook uniqueid
		reshape wide man*, i(uniqueid bookevent) j(eventnumber)
		codebook uniqueid

		save "data_temp/wide_IDENTIFIABLE $TAG Clinical ANCManagements.dta", replace
	}

	*********************
	*********************
	*********************
	** PREVIOUS PREGNANCIES
	*********************
	*********************

	*Open ultrasound .dta file 
	if(`IS_CONTROL'==1){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Previous pregnancy table.csv", varnames(1) encoding("UTF-8") clear
		gen identificationdocumentnumber =""
	}
	else if(`IS_CONTROL'==0){
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Previous pregnancies.csv", varnames(1) encoding("UTF-8") clear
	}

	capture ren v2 programstageinstance

	*Open 'previous pregnancies' dataset
	rename (event programstageinstance programstage eventdate longitude latitude ///
	organisationunitname organisationunitcode organisationunit ///
	identificationdocumentnumber ancpreviouspregnancygestation ///
	ancpreviouspregnancyoutcome previousplaceofbirth ancmodeofpreviousdelivery ///
	ancgenderofchildfrompreviou ancpreviouspregnancybirthweig ///
	anchistoryofgestationaldiabe anchistoryofgestationalhyper ///
	anchistoryofpreeclampsiainp anchistoryofeclampsiainprev ///
	anchistoryofpuerperalsepsis anchistoryofantepartumhemorr ///
	ancpostpartumhemorrhageinpre anchistoryofdvtinpreviousp ///
	previouscomplicationsnone)(prevevent uniqueid prevprogstage prevdate prevlong ///
	prevlat prevorgname prevorgcode prevorgunit previdnumber prevgestagebirth ///
	prevoutcome prevbirthplace prevmodedelivery prevgender prevbirthweight ///
	prevgdm prevhtn prevpreeclampsia preveclampsia prevpuersep prevaph ///
	prevpph prevdvt prevnocompl)


	*Label variables 
	label variable prevevent "System generate additional identifier"
	label variable uniqueid "System generated identifier common to all datasets"
	label variable prevprogstage "Name of anc module"
	label variable prevdate "Date- previous pregnancy"
	label variable prevlong "Longitude coordinates of clinic- previous pregnancy"
	label variable prevlat "Latitude coordinates of clinic- previous pregnancy"
	label variable prevorgname "Name of clinic- previous pregnancy"
	label variable prevorgcode "System generated code of clinic"
	label variable prevorgunit "System generated additional code of clinic"
	label variable previdnumber "Woman's ID number"
	label variable prevgestagebirth "Gestational age at birth- previous pregnancy"
	label variable prevoutcome "Outcome of pregnancy- previous pregnancy"
	label variable prevbirthplace "Place of birth- previous pregnancy"
	label variable prevmodedelivery "Mode of delivery- previous pregnancy"
	label variable prevgender "Gender of baby- previous pregnancy"
	label variable prevbirthweight "Birth weight- previous pregnancy"
	label variable prevgdm "Gestational diabetes- previous pregnancy"
	label variable prevhtn "Gestational hypertension- previous pregnancy"
	label variable prevpreeclampsia "Preeclampsia- previous pregnancy"
	label variable preveclampsia "Eclampsia- previous pregnancy"
	label variable prevpuersep "Peurperal sepsis- previous pregnancy"
	label variable prevaph "Antepartum hemorrhage- previous pregnancy"
	label variable prevpph "Postpartum hemorrhage- previous pregnancy"
	label variable prevdvt "Deep vein thrombosis- previous pregnancy"
	label variable prevnocompl "No complications- previous pregnancy"


	save "data_temp/IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", replace


	sort uniqueid prevdate
	duplicates drop uniqueid prevdate, force
	bysort uniqueid: gen eventnumber=_n

	codebook uniqueid
	reshape wide prev*, i(uniqueid) j(eventnumber)
	codebook uniqueid

	save "data_temp/wide_IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", replace

	*************************************
	*********** HOSPITAL BIRTH OUTCOMES
		
	if(`IS_CONTROL'==1){
		*Open ultrasound .dta file 
		import delimited "$DATA_RAW/$CLINICAL_OR_CONTROL Hospital Birth Outcome.csv", varnames(1) encoding("UTF-8") clear

		ren ancpreviouspregnancyoutcome pregnancyoutcome // TODO these names seem bad, and should probably be changed
		drop if missing(pregnancyoutcome) & missing(con_woman_first_name) & missing(con_name_hosp_birth)
		
		capture ren v2 programstageinstance
		rename event hboevent
		rename programstageinstance uniqueid
		
		// fixing va names that are too long
		capture rename ancpreviouspregnancybirthweightg ancprevpregbirthweightg
		capture rename indicationforcsectionmentionedin indicforcsectionmentionedin
		capture rename ancdiastolicbloodpressuremmhg ancdiastbloodpressuremmhg
		
		foreach X of varlist programstage-usrecommendationscomments {
			ren `X' hbo`X'
		}
		
		bro if uniqueid=="B9FWlWn5xrR"
		
		count
		joinby uniqueid using "data_temp/bookevent_ids.dta", unm(b)
		count

		tab _merge
		drop if _merge==2 // dropping ids
		drop _merge

		foreach X in "hbodate" {
			display "`X'"

			gen year=substr(`X',1,4) // extract first four characters
			gen month=substr(`X',6,2) // extract 6-7 characters
			gen day=substr(`X',9,2) // extract 9-10 characters
			destring year, replace
			destring month, replace
			destring day, replace
			gen new`X' = mdy(month, day, year)
			drop year month day 
			format new`X' %td
			summarize new`X'
		}
		drop hbodate
		ren newhbodate hbodate

		count if hbodate < newbookdatecorrect1
		count if hbodate < newbookdatecorrect1 & !missing(newbookdatecorrect1)

		**
		gen bookevent = bookevent1
		foreach X of varlist bookevent* {
			if("`X'"=="bookevent"){
				continue
			}
			di "`X'"
			local XVAL=subinstr("`X'","bookevent","",.)
			replace bookevent = bookevent`XVAL' if hbodate >= newbookdatecorrect`XVAL'
			drop bookevent`XVAL'
		}

		drop newbookdatecorrect*
		
		save "data_temp/IDENTIFIABLE $TAG Hospital Birth Outcome.dta", replace


		sort uniqueid hbodate
		duplicates drop uniqueid hbodate, force
		bysort uniqueid: gen eventnumber=_n

		codebook uniqueid
		reshape wide hbo*, i(uniqueid) j(eventnumber)
		codebook uniqueid

		order bookevent
		save "data_temp/wide_IDENTIFIABLE $TAG Hospital Birth Outcome.dta", replace
	}
	*************************************
	*********** START MERGING TOGETHER IMPORTANT DATASETS

	use "data_temp/IDENTIFIABLE $TAG ANC demographics and booking.dta", clear

	count
	joinby uniqueid bookevent using "data_temp/IDENTIFIABLE $TAG clinical ultrasounds expected due date.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge

	gen expected_due_delivery=newbooklmpcorrect+280
	format %td expected_due_delivery
	replace expected_due_delivery=us_expected_due_date if missing(expected_due_delivery)
	codebook expected_due_delivery

	ren demoidnumber MotherIDNO
	gen is_clinical_int=1

	*****
	count
	joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Clinical Antenatal care visit.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge

	*****
	count
	joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Clinical Lab results.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge

	*****
	count
	joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Clinical Ultrasound results.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge

	*****
	if(`IS_CONTROL'==0){
		count
		joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Clinical ANCRisks.dta", unm(b)
		count

		tab _merge
		keep if _merge==1 | _merge==3
		tab _merge

		drop _merge

		*****
		count
		joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Clinical ANCManagements.dta", unm(b)
		count

		tab _merge
		keep if _merge==1 | _merge==3
		tab _merge

		drop _merge
	}
	*****
	count
	joinby uniqueid using "data_temp/wide_IDENTIFIABLE $TAG Clinical Previous pregnancies.dta", unm(b)
	count

	tab _merge
	keep if _merge==1 | _merge==3
	tab _merge

	drop _merge
	
	if(`IS_CONTROL'==1){
		count
		joinby uniqueid bookevent using "data_temp/wide_IDENTIFIABLE $TAG Hospital Birth Outcome.dta", unm(b)
		count

		tab _merge
		keep if _merge==1 | _merge==3
		tab _merge

		drop _merge
	}

	if(`IS_CONTROL'==0){
		gen isTrial1Intervention=1
	}
	else {
		gen isTrial1Control=1
		capture tostring bookexamhead, replace
		capture tostring bookexamheadabn, replace
		capture tostring bookexamheart, replace
		capture tostring bookexamheartabn, replace
		capture tostring bookexamlung, replace
		capture tostring bookexamlungabn, replace
		capture tostring bookexamabd, replace
		capture tostring bookexamabdabn , replace
		capture tostring bookexamlimb , replace
		capture tostring bookexamlimbabn, replace
		capture tostring bookexambr , replace
		capture tostring  bookexambreastabn , replace
		capture destring  bookidnumber , replace
		capture tostring  bookallerfood , replace
		capture tostring  bookallerdrug , replace
		capture tostring  bookallerdrugspec , replace
		capture tostring  bookallersev , replace
		capture tostring  bookallersevspec , replace
		capture tostring  bookhistotherch , replace
		capture tostring  bookfetalmove , replace
		capture tostring  bookintendbirth , replace
		capture tostring  bookdatelastbirth , replace
		capture tostring bookhistbloodspec , replace
		capture tostring  bookrefchronic , replace
		capture tostring  bookvisitspec , replace
		capture tostring  bookmedpres , replace
		capture tostring  mobile, replace
		capture destring  phone, replace
		capture tostring  consang , replace
		capture destring  anidnumber1 , replace
		capture tostring  anallerfood1 , replace
		capture tostring anallerdrug1 , replace
		capture tostring anallerdrugspec1, replace
		capture tostring  anallersev1 , replace	
		capture tostring anallersevspec1 , replace
		capture tostring anfetalmove1 , replace
		capture tostring  anhistbloodspec1 , replace
		capture tostring anrefchronic1 , replace
	}
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
	
	******** added these on december 24, 2017. these are doubles in demogrphic data******
	drop if uniqueid=="Hrt6pxBniy9"
	drop if uniqueid=="UZ53c6fKhTy"
	drop if uniqueid=="ye9fvoVSalO"
	drop if uniqueid=="McathozJMdD"
	

	/* TAMARA STOP if bookevent=="HERE */
	save "data_temp/IDENTIFIABLE $TAG.dta", replace

}

use "data_temp/IDENTIFIABLE INT.dta", replace
append using "data_temp/IDENTIFIABLE CON.dta", force

tab isTrial1Control
tab isTrial1Intervention

//
//PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS

/*
capture drop dup
duplicates tag MotherIDNO bookdate, gen("dup")
keep if dup>0
*/

replace dataextractorusername=subinstr(lower(dataextractorusername)," ","",.)

sort MotherIDNO bookdate
bro MotherIDNO bookdate dataextractorusername
gen temp_khadejeh=0
replace temp_khadejeh=1 if dataextractorusername=="khadejeh"
bysort MotherIDNO bookdate: egen has_khadejeh=max(temp_khadejeh)

drop if has_khadejeh==1 & dataextractorusername!="khadejeh"
drop temp_khadejeh has_khadejeh

//END OF PRIORITIZE khadejeh OVER ALL OTHER DATA EXTRACTORS
//

// WE DEFINITELY NEED TO FIX THIS
duplicates drop MotherIDNO bookdate, force
// FIX THIS ABOVE, WITH PROPER REMOVING OF DUPLICATES

//keep if _n<=500 // <- RUN EVERYTHING WITH ONLY 500 PEOPLE
save "data_temp/IDENTIFIABLE trial_1.dta", replace
//use "data_temp/IDENTIFIABLE trial_1.dta", clear


// CREATE AN EXCEL FILE FOR DOUBLE ENTRY
use "data_temp/IDENTIFIABLE CON.dta", clear
codebook bookevent

duplicates tag MotherIDNO, gen("duplicated_motheridno")
keep if duplicated_motheridno==1
sort MotherIDNO bookdate dataextractorusername
order bookevent MotherIDNO bookdate dataextractorusername andate* usdate* labdate*

export excel using "data_clean/double_entered.xlsx", replace firstrow(var)


