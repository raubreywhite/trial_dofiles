DHIS2_BookingVisit <- function(isControl,
                               keepDoubleBookings=FALSE,
                               IS_GAZA=FALSE
                               ) {
  FOLDERS <- DHIS2_Folders(isControl = isControl)

  if (isControl) {
    # read in ANC followup, keep the first obs per pregnancy
    d <- fread(
      sprintf(
        "%s/%s ANC Follow up sheet.csv",
        FOLDERS$FOLDER_DATA,
        FOLDERS$CLINICAL_OR_CONTROL
      ),
      encoding = "UTF-8"
    )
    setnames(d, 2, "programstageinstance")
    setorder(d, programstageinstance, `Event date`)
    d[, visitNum := 1:.N, by = .(programstageinstance)]
    firstObsPerPreg <- d[visitNum == 1]
    firstObsPerPreg[, visitNum := NULL]

    firstObsPerPreg[, `Event` := NULL]
    firstObsPerPreg[, `Program stage` := NULL]
    firstObsPerPreg[, `Event date` := NULL]
    firstObsPerPreg[, `Longitude` := NULL]
    firstObsPerPreg[, `Latitude` := NULL]
    firstObsPerPreg[, `Organisation unit name` := NULL]
    firstObsPerPreg[, `Organisation unit code` := NULL]
    firstObsPerPreg[, `Organisation unit` := NULL]
    firstObsPerPreg[, `Con_Abortion` := NULL]
    firstObsPerPreg[, `ANC Other medications the woman is currently taking` := NULL]

    # Merge in the first ANC observation so that this file
    # looks the same as the interventiond data

    d <- fread(
      sprintf(
        "%s/%s ANC Green File.csv",
        FOLDERS$FOLDER_DATA,
        FOLDERS$CLINICAL_OR_CONTROL
      ),
      encoding = "UTF-8"
    )
    setnames(d, 2, "programstageinstance")
    nrow(d)
    d <- merge(d, firstObsPerPreg, by = "programstageinstance", all.x = T)
    nrow(d)

    d[, identificationdocumentnumber := as.character(NA)]
    d[, ancallergiesdrugsandorseverefood := as.numeric(NA)]
    d[, ancallergiesdrugsandorseverefood := as.numeric(NA)]
    d[, ancpenicillinallergy := as.numeric(NA)]
    d[, anchistoryofothermedicineallergy := as.numeric(NA)]
    d[, ancothermedisineallergyspecified := as.numeric(NA)]
    d[, anchistroyofothersevereallergy := as.numeric(NA)]
    d[, ancothersevereallergyspecified := as.numeric(NA)]
    d[, anchistoryofchronichypertension := as.numeric(NA)]
    d[, anchistoryofotherchronicconditio := as.numeric(NA)]
    d[, anchistoryofblooddisorder := as.numeric(NA)]
    d[, ancfetalmovement := as.numeric(NA)]
    d[, anchypothyreoidism := as.numeric(NA)]
    d[, ancintendedplaceofbirth := as.numeric(NA)]
    d[, ancdateoflastdelivery := as.numeric(NA)]
    d[, ancrecommendedplaceofbirth := as.numeric(NA)]
    d[, anctetanusboosterdose := as.numeric(NA)]
    d[, anchistoryofblooddisorderspecif := as.numeric(NA)]
    d[, ancfundalheightmeasurement := as.numeric(NA)]
    d[, ancedema := as.numeric(NA)]
    d[, ancutifollowupscheduled := as.numeric(NA)]
    d[, ancreferralneededforotherchronic := as.numeric(NA)]
    d[, anchistoryofdeepveinthrombosisdv := as.numeric(NA)]
    d[, whichancvisitisthis := as.numeric(NA)]
    d[, ancmedicineprescription := as.numeric(NA)]
    d[, v81 := as.numeric(NA)]
    d[, anchighriskdesignatedwoman := as.numeric(NA)]
    d[, anccounselingaboutironandfolicac := as.numeric(NA)]

    d[, ancmchhandbookprovided := as.numeric(NA)]
    d[, ancpreviousepisodesofthrombosis := as.numeric(NA)]
    d[, anchistoryofklexaneprovidedtothe := as.numeric(NA)]
    d[, anccounselingaboutdangersignsdur := as.numeric(NA)]
    d[, anccounselingonnutrioninpregnanc := as.numeric(NA)]
    d[, anccounselingaboutbreastfeeding := as.numeric(NA)]
    d[, anccounselingaboutlaborsigns := as.numeric(NA)]
    d[, anccounselingaboutpkuscreening := as.numeric(NA)]
    d[, ancppcvisitundertakenbywhom := as.numeric(NA)]
    d[, paperbackupused := as.numeric(NA)]

    # capture tostring anclmpstatus, replace
    # capture tostring ancotherfamilyconcernspecified, replace
    # capture tostring ancpallor, replace
  } else {
    print("CHECKINGCHECKING****")
    print(FOLDERS)
    # d <- fread(
    #   sprintf(
    #     "%s/%s Booking Visit.csv",
    #     FOLDERS$FOLDER_DATA,
    #     FOLDERS$CLINICAL_OR_CONTROL
    #   ),
    #   encoding = "UTF-8"
    # )
    # print(sprintf(
    #   "%s/%s Booking Visit.csv",
    #   FOLDERS$FOLDER_DATA,
    #   FOLDERS$CLINICAL_OR_CONTROL
    # ))
    # print("TESTING")
    # print(d)
    # print(names(d))
    
   
    
    file_to_read_in <- sprintf(
      "%s/%s Booking Visit.csv",
      FOLDERS$FOLDER_DATA,
      FOLDERS$CLINICAL_OR_CONTROL
    )
    print("FILE TO READ IN:")
    print(file_to_read_in)
    print(file.exists(file_to_read_in))
    d <- fread(
      file_to_read_in,
      encoding = "UTF-8"
    )
   
    
    setnames(d, 2, "programstageinstance")
    
    d[,confamilyhistoryofbronchialastma:=as.numeric(NA)]
    d[,confamilyhistoryofcardiacdisease:=as.numeric(NA)]
    d[,usrecommendationscommentsx:=as.numeric(NA)]   
    d[,conabortion:=as.numeric(NA)]
    d[,congravida:=as.numeric(NA)]
    d[,conld:=as.numeric(NA)]
    d[,conlivingchildren:=as.numeric(NA)]
    d[,conpara:=as.numeric(NA)]
    d[,confamilyhistoryofinbornerrorofmetabolism:=as.numeric(NA)]
    d[,confamilyhistoryofrenaldisease:=as.numeric(NA)]
    d[,confamilyhistoryofhypertension:=as.numeric(NA)]
    d[,confamilyhistoryofdiabetesmellitus:=as.numeric(NA)]
    d[,anchistoryofantepartumhemorrhageinpreviouspregnancy:=as.numeric(NA)]
    d[,confamilyhistoryofcongenitalanomaly:=as.numeric(NA)]
    d[,confamilyhistoryofblooddisease:=as.numeric(NA)]
    d[,conage16or40:=as.numeric(NA)]
    d[,conintendedplaceofbirth:=as.numeric(NA)]
    d[,conrecommendedplaceofbirth:=as.numeric(NA)]
    d[,conancfilenumber:=as.numeric(NA)]
    d[,conancgestationaageatvisitweeks:=as.numeric(NA)]
    d[,conancgestationaageatvisitsize:=as.numeric(NA)]
    d[,usrecommendationscommentsy:=as.numeric(NA)]
    d[,consupplementsyesno:=as.numeric(NA)]
    d[,conancsupplementprescription:=as.numeric(NA)]
  }

  for (i in names(d)) setnames(d, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])

  if(IS_GAZA){
    if(!"identificationdocumentnumber" %in% names(d)){
      message("no identification document number -- we create one")
      d[,identificationdocumentnumber:=1:.N]
    }
    
    setnames(d,"ancgestationalageatvisitweeks","ancgestationalageatvisitweeks")
  }
  
  setnames(d, "event", "bookevent")
  setnames(d, "programinstance", "uniqueid")
  setnames(d, "programstage", "bookprogstage")
  setnames(d, "eventdate", "bookdate")
  setnames(d, "longitude", "booklong")
  setnames(d, "latitude", "booklat")
  setnames(d, "organisationunitname", "bookorgname")
  setnames(d, "organisationunitcode", "bookorgcode")
  setnames(d, "organisationunit", "bookorgunit")
  setnames(d, "identificationdocumentnumber", "bookidnumber")
  setnames(d, "ancdiastolicbloodpressuremmhg", "bookbpdiast")
  setnames(d, "anceclampticconvulsions", "bookeclamp")
  setnames(d, "ancpallor", "bookpallor")
  setnames(d, "anclmpdate", "booklmp")
  setnames(d, "ancsuspectedpretermprematureruptureofmembranesprom", "bookpprom")
  setnames(d, "ancsuspectedprematureruptureofmembranesprom", "bookprom")
  setnames(d, "ancsystolicbloodpressuremmhg", "bookbpsyst")

  tryCatch({
    setnames(d, "ancallergiesdrugsandorseverefood", "bookallerfood")
  }, error = function(err) {
    setnames(d, "ancallergiesdrugsandorseverefoodallergies", "bookallerfood")
  })

  setnames(d, "ancpenicillinallergy", "bookallerpen")
  setnames(d, "anchistoryofothermedicineallergy", "bookallerdrug")
  setnames(d, "ancothermedisineallergyspecified", "bookallerdrugspec")
  setnames(d, "anchistroyofothersevereallergy", "bookallersev")
  setnames(d, "ancothersevereallergyspecified", "bookallersevspec")
  setnames(d, "anchistoryofchronichypertension", "bookhisthtn")
  setnames(d, "anchistoryofdiabetesmellitusioriiinsulindependent", "bookhistdm")
  setnames(d, "anchistoryofrenaldisease", "bookhistrd")

  tryCatch({
    setnames(d, "anchistoryofotherchronicconditio", "bookhistotherch")
  }, error = function(err) {
    setnames(d, "anchistoryofotherchronicconditions", "bookhistotherch")
  })

  setnames(d, "anchistoryofblooddisorder", "bookhistblood")
  setnames(d, "ancbodyheightm", "bookheight")
  setnames(d, "ancbodyweightkg", "bookweight")
  setnames(d, "ancmedicalexaminationofbreastsandnipples", "bookexambr")
  setnames(d, "anchistoryofbronchialasthma", "bookhistasthma")
  setnames(d, "anchistoryofcardiacdisease", "bookhistcard")
  setnames(d, "ancconsecutiveabortions", "bookhistabort")
  setnames(d, "anchistoryofepilepsy", "bookhistepi")
  setnames(d, "ancfamilyhistoryofblooddisease", "bookfamblood")
  setnames(d, "ancfamilyhistoryofbronchialastma", "bookfamasthma")
  setnames(d, "ancfamilyhistoryofcardiacdisease", "bookfamcardiac")
  setnames(d, "ancfamilyhistoryofcongenitalanomaly", "bookfamcong")
  setnames(d, "ancfamilyhistoryofdiabetesmellitus", "bookfamdm")
  setnames(d, "ancfamilyhistoryofhypertension", "bookfamhtn")
  setnames(d, "ancfamilyhistoryofinbornerrorofmetabolism", "bookfammeta")
  setnames(d, "ancfamilyhistoryofrenaldisease", "bookfamrd")
  setnames(d, "ancfetalmovement", "bookfetalmove")
  setnames(d, "anchypothyreoidism", "bookhisthypothyr")
  setnames(d, "ancintendedplaceofbirth", "bookintendbirth")
  setnames(d, "ancdateoflastdelivery", "bookdatelastbirth")
  setnames(d, "anchistoryofmentaldisturbance", "bookhistpsy")
  setnames(d, "anchistoryofgestationaldiabetesmellitusinanypreviouspregnancy", "bookhistgdm")
  setnames(d, "anchistoryofanypreviousperinataldeath", "bookhistperi")
  setnames(d, "ancreproductivetractinfectionrti", "bookhistrti")
  setnames(d, "anchistoryofpostpartumhemorrhageinanypreviouspregnancy", "bookhistpph")
  setnames(d, "ancrecommendedplaceofbirth", "bookrecobirth")
  setnames(d, "anctetanusboosterdose", "bookttdose")
  setnames(d, "ancvaccinatedforttaccordingtoguidelines", "bookttguide")
  setnames(d, "anchistoryofuterineanomalyorinjury", "bookhistute")
  setnames(d, "anchistoryofuterinesurgeryexcludingcs", "bookhistutesur")
  setnames(d, "ancvaginalbleeding", "bookvagbleed")

  tryCatch({
    setnames(d, "anchistoryofblooddisorderspecif", "bookhistbloodspec")
  }, error = function(err) {
    setnames(d, "anchistoryofblooddisorderspecified", "bookhistbloodspec")
  })

  setnames(d, "anchistoryofcsections2", "bookhistcs")
  setnames(d, "ancfundalheightmeasurement", "bookexamsfh")
  setnames(d, "ancmedicalexaminationofabdomen", "bookexamabd")
  setnames(d, "ancmedicalexaminationofheart", "bookexamheart")
  setnames(d, "ancmedicalexaminationoflowerlimbs", "bookexamlimb")
  setnames(d, "ancmedicalexaminationoflungs", "bookexamlung")
  setnames(d, "ancmedicalexaminationofheadandneck", "bookexamhead")
  setnames(d, "ancedema", "bookexamedema")
  setnames(d, "anclmpstatus", "booklmpknown")
  setnames(d, "ancinvitrofertilizationivf", "bookhistivf")
  setnames(d, "ancinfertility1yearpriortocurrentpregnancy", "bookhistinfert")
  setnames(d, "ancutifollowupscheduled", "bookutifollow")

  tryCatch({
    setnames(d, "ancreferralneededforotherchronic", "bookrefchronic")
  }, error = function(err) {
    setnames(d, "ancreferralneededforotherchronicconditions", "bookrefchronic")
  })

  setnames(d, "anchistoryofeclampsiainanypreviouspregnancy", "bookhisteclamp")

  tryCatch({
    setnames(d, "anchistoryofdeepveinthrombosisdv", "bookhistdvt")
  }, error = function(err) {
    setnames(d, "anchistoryofdeepveinthrombosisdvtoutsidepreviouspregnancies", "bookhistdvt")
  })

  setnames(d, "anchypertensioncerebralorvisualsymptoms", "bookhisthtnsymp")
  setnames(d, "ancfetalpresentationcheckedbypalpation", "bookexampalp")
  setnames(d, "ancfetalheartsoundfhs", "bookexamfh")
  setnames(d, "whichancvisitisthis", "bookvisitspec")
  setnames(d, "ancprimigravidapg", "bookprimi")
  setnames(d, "ancmedicineprescription", "bookmedpres")

  tryCatch({
    setnames(d, "v81", "bookhistotherchronic")
  }, error = function(err) {
    setnames(d, "anchistoryofotherchronicconditionspecified", "bookhistotherchronic")
  })

  setnames(d, "ancabnormalfindingsofheadandneckspecified", "bookexamheadabn")
  setnames(d, "ancabnormalfindingsofabdomenspecified", "bookexamabdabn")
  setnames(d, "ancabnormalfindingsofbreastsandornipplesspecified", "bookexambreastabn")
  setnames(d, "ancabnormalfindingsoflowerlimbs", "bookexamlimbabn")
  setnames(d, "ancabnormalfindingsoflungs", "bookexamlungabn")
  setnames(d, "ancabnormalfindingsofheart", "bookexamheartabn")
  setnames(d, "ancothermedicationsthewomaniscurrentlytaking", "bookhistmed")
  setnames(d, "anchistoryofmultiparity", "bookparity")
  setnames(d, "anchistoryofanypreviouspretermbirth", "bookhistpreterm")
  setnames(d, "anchistoryofdvtrelatedtoanypreviouspregnancy", "bookhistprevdvt")
  setnames(d, "anchistoryofantepartumhemorrhageinpreviouspregnancies", "bookhistaph")
  setnames(d, "anchistoryofgestationalhypertentioninanypreviouspregnancy", "bookhistghtn")
  setnames(d, "anchistoryofpreeclampsiainanypreviouspregnancy", "bookhistpreecl")
  setnames(d, "anchistoryofpuerperalsepsisinanypreviouspregnancy", "bookhistpuersep")
  setnames(d, "ancgestationalageatvisitweeks", "bookgestage")
  setnames(d, "anchighriskdesignatedwoman", "bookhighrisk")
  setnames(d, "anchistoryofanycomplicatedcsection", "bookhistcscompl")

  tryCatch({
    setnames(d, "anccounselingaboutironandfolicac", "bookcounsifa")
  }, error = function(err) {
    setnames(d, "anccounselingaboutironandfolicacidsupplementation", "bookcounsifa")
  })

  setnames(d, "ancmchhandbookprovided", "bookancbook")
  setnames(d, "ancpreviousepisodesofthrombosis", "bookhistthrom")

  tryCatch({
    setnames(d, "anchistoryofklexaneprovidedtothe", "bookhistclex")
  }, error = function(err) {
    setnames(d, "anchistoryofklexaneprovidedtothepatient", "bookhistclex")
  })

  tryCatch({
    setnames(d, "anccounselingaboutdangersignsdur", "bookcounsdanger")
  }, error = function(err) {
    setnames(d, "anccounselingaboutdangersignsduringpregnancy", "bookcounsdanger")
  })

  tryCatch({
    setnames(d, "anccounselingonnutrioninpregnanc", "bookcounsnut")
  }, error = function(err) {
    setnames(d, "anccounselingonnutrioninpregnancy", "bookcounsnut")
  })

  setnames(d, "anccounselingaboutbreastfeeding", "bookcounsbf")
  setnames(d, "anccounselingaboutlaborsigns", "bookcounslabor")
  setnames(d, "anccounselingaboutpkuscreening", "bookcounspku")
  setnames(d, "ancotherrelevantfamilyhistoryconcern", "bookcounsfamhist")
  setnames(d, "ancotherfamilyconcernspecified", "bookfamspec")
  setnames(d, "ancppcvisitundertakenbywhom", "bookseenby")
  setnames(d, "anchomevisitoratthehealthclinic", "bookhomeorclinic")

  if("paperbackupused" %in% names(d)) setnames(d, "paperbackupused", "bookbackupfile")
  if("ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem" %in% names(d)) setnames(d, "ppcwasthisinformationfirstcollectedonpaperandthenenteredintothesystem", "bookbackupfile")
  if(!"bookbackupfile" %in% names(d)) stop("cant find bookbackupfile")
  
  setnames(d,"confamilyhistoryofinbornerrorofmetabolism","bookfamhistinbornmetab")
  setnames(d,"confamilyhistoryofdiabetesmellitus","bookfamhistdiab")
  setnames(d,"confamilyhistoryofcongenitalanomaly","bookfamhistcongenanom")
  setnames(d,"anchistoryofantepartumhemorrhageinpreviouspregnancy","bookhistantparthemprevpreg")
  
  # more name changing
  
  
  
  # delete people with duplicate eventdates (these are obvious duplicates)
  nrow(d)
  d <- unique(d, by = c("uniqueid", "bookdate"))
  nrow(d)

  # delete people with duplicae LMPs (again, duplicate pregnancies(
  nrow(d)
  d <- unique(d, by = c("uniqueid", "booklmp"))
  nrow(d)


  data_DHIS2_Demographics <- DHIS2_Demographics(isControl = isControl, IS_GAZA=IS_GAZA)
  nrow(d)
  d <- merge(d, data_DHIS2_Demographics, by = c("uniqueid"), all = T)
  nrow(d)
  
  # if control and dont have booking, then delete
  if(isControl){
    d <- d[!is.na(bookevent)]
  } else {
    #if intervention and dont have booking, then bookorgname="ppconly"
    d[is.na(bookorgname),bookorgname:="ppconly"]
  }
  #
  # for the abortions, we now have "day of showing up and getting registered in demographic file"
  # as "day of booking" (ie bookdate)
  #

  d[, ident_dhis2_df_no_gf_or_bf := ifelse(is.na(bookevent), 1, 0)]
  xtabs(~d$ident_dhis2_df_no_gf_or_bf)
  d[, ident_dhis2_booking := ifelse(is.na(bookevent), 0, 1)]
  xtabs(~d$ident_dhis2_booking)
  
  #
  setorder(d, bookevent)
  # if(IS_GAZA){
  #   d[,bookdate:=as.Date(bookdate, "%d/%m/%Y")]
  #   d[is.na(bookevent), bookdate := as.Date(datecreated, "%d/%m/%Y")]
  # } else {
  #   d[is.na(bookevent), bookdate := datecreated]
  # }
  
  if(IS_GAZA){
    d[,bookdate:=stringr::str_remove_all(bookdate," 12:00 AM$")]
    d[,bookdate := stringr::str_replace(bookdate, "/([0-9][0-9])$","/20\\1")]
    #str(d$bookdate)
    #print(unique(d$bookdate)[1:10])
    d[,bookdate:=as.Date(bookdate, "%d/%m/%Y")]
    d[is.na(bookevent), bookdate := as.Date(datecreated, "%d/%m/%Y")]
    #str(d$bookdate)
    #print(unique(d$bookdate)[1:10])
  } else {
    d[is.na(bookevent), bookdate := datecreated]
  }
  
  d[is.na(bookevent), bookevent := sprintf("%s%s", ifelse(isControl,"CON","INT"), 1:.N)]

  #d[demoidnumber==852188531]
  #d[uniqueid=="AqJL4IiVSJv"]
  #2017-09-10
  
  
  if(IS_GAZA){
    d[, booklmp := stringr::str_replace(booklmp, "/([0-9][0-9])$","/20\\1")]
    d[, booklmp := as.Date(booklmp,format="%m/%d/%Y")]
    #str(d$booklmp)
    #print(unique(d$booklmp)[1:10])
  } else{
    d[, booklmp := as.Date(booklmp)]
  }
   if (length(unique(d$dob)) == 1) {
     d[, dob := NULL]
     d[, dob := as.Date("1980-01-01")]
   } else {
     #print(unique(d$dob)[1:30])
     if(IS_GAZA){
       d[, dob := as.Date(dob, format="%m/%d/%Y")]
     } else {
       d[, dob := as.Date(dob)]
     }
   }
  
  # drop women whose 2nd, 3rd, etc pregnancies
  # have LMPs before the first pregnancy's booking date
  setorder(d, bookdate)
  d[, booking_number := 1:.N, by = .(uniqueid)]
  
  warning("check this")
  for (i in 1:max(d$booking_number)) {
    d[, min_event_date := ifelse(booking_number == i, as.Date(bookdate), as.Date("1970-01-01"))]
    d[, min_event_date := as.Date(max(min_event_date), origin = "1970-01-01"), by = .(uniqueid)]
    d[, keep := ifelse(booklmp < min_event_date & !is.na(booklmp) & booking_number > i, FALSE, TRUE)]
    print(i)
    print(nrow(d))
    d <- d[keep == TRUE]
    print(nrow(d))
  }
  d[, booking_number := 1:.N, by = .(uniqueid)]
  d[, min_event_date := NULL]

  
  # Calculate EDD based on LMP
  d[, expecteddateofdelivery := booklmp + 280]
  d[, ident_expected_delivered:= expecteddateofdelivery < min(CLINIC_INTERVENTION_DATE,CLINIC_CONTROL_DATE)]
  # Create a new variable for gestational age
  d[, age := floor(as.numeric(difftime(bookdate, dob, units = "days")) / 365)]
  # small cleaning
  d[,bookevent:=as.character(bookevent)]
  # generating some important analysis variables
  d[,ident_dhis2_control:=isControl]
  d[,ident_dhis2_b4_2017_01_15:= bookdate<as.Date("2017-01-15")]
  d[,bookorgname:=unlist(ExtractOnlyEnglishLetters(bookorgname))]
  ConvertAllFactorsToChar(d)
  
  if(file.exists(file.path(FOLDER_DATA_RAW,"structural_data/remove_from_dhis2.xlsx"))){
    toremove <- readxl::read_excel(file.path(FOLDER_DATA_RAW,"structural_data/remove_from_dhis2.xlsx"))
    setDT(toremove)
    warning("make sure this actually works")
    d <- d[!(
      bookevent %in% na.omit(toremove$bookevent) |
        uniqueid %in% na.omit(toremove$uniqueid)
    )]
  }
  #### data extractor
  d[,dataextractor:=unlist(ExtractOnlyEnglishLetters(dataextractor))]
  xtabs(~d$dataextractor)
  d[dataextractor %in% c(
    "aiza",
    "aziz",
    "aziza",
    "azizia",
    "azza"
  ),dataextractor:="aziza"]
  
  d[dataextractor %in% c(
    "ibtesam"
  ),dataextractor:="ibtesam"]
  
  d[dataextractor %in% c(
    "kadejeh",
    "khadejah",
    "khadeje",
    "khadejeh",
    "khadejej",
    "khaejeh",
    "kjadejeh",
    "khdejeh"
  ),dataextractor:="khadejeh"]
  
  d[dataextractor %in% c(
    "mervet",
    "mervett",
    "mevet"
  ),dataextractor:="mervett"]
  
  d[dataextractor %in% c(
    "naja",
    "najah"
  ),dataextractor:="najah"]
  
  d[dataextractor %in% c(
    "omar",
    "omr"
  ),dataextractor:="omar"]
  
  d[dataextractor %in% c(
    "tamara"
  ),dataextractor:="tamara"]
  
  d[dataextractor %in% c(
    "yusra"
  ),dataextractor:="yusra"]
  
  d[dataextractor %in% c(
    "",
    "test",
    "tttttttt"
  ),dataextractor:="unknown"]
  
  xtabs(~d$dataextractor)
  
  if(!keepDoubleBookings){
    # drop if there are multiple bookings for the same personid on the same date
    d[,keep:=TRUE]
    #print("0")
    setorder(d,demoidnumber,bookdate)
    d[,shiftdate:=shift(bookdate),by=demoidnumber]
    #print("00")
    d[shiftdate==bookdate,keep:=FALSE]
    #print("000")
    d <- d[keep==TRUE]
    #print("0000")
    d[,shiftdate:=NULL]
    d[,keep:=NULL]
  } else {
    setorder(d,demoidnumber,bookdate)
    print("00000")
    d[,numberbookings:=.N,by=.(demoidnumber,bookdate)]
    xtabs(~d$numberbookings)
    d <- d[numberbookings>1]
  }
  
  #####
  # multiple bookings for same person
  #d[ident_dhis2_booking==TRUE,c("demoidnumber","uniqueid","bookevent","bookdate")]
  #setorder(d,demoidnumber,bookdate)
  #d[ident_dhis2_booking==TRUE,booknumber:=1:.N,by=demoidnumber]
  #xtabs(~d$booknumber)
  
  #print(names(d))
 
  #print(d[1,])
  
  #d[demoidnumber==401404496,c("demoidnumber","uniqueid","bookevent","bookdate")]

  d<- Removeduplicate(d=d,tag="demobook",isControl=isControl)
  print("000000")
  
  
  return(d)
  
  print("d has been returned")
}



