CreatingFurtherVariablesNormal <- function(d){
  d[,nbcdateofdelivery_1:=as.Date(nbcdateofdelivery_1)]
  
  d[,agecat:=cut(age,
                  breaks=c(0,20,25,30,35,40,45,50,100),
                  include.lowest=T)]
  d[,agemarriagecat:=cut(agemarriage,
                                    breaks=c(0,20,25,30,35,40,100),
                                    include.lowest=T)]
  d[,agepregnancycat:=cut(agepregnancy,
                                     breaks=c(0,20,25,30,35,40,100),
                                     include.lowest=T)]
  
  d[,educationcat:=cut(education,
                                  breaks=c(0,9,13,100),
                                  include.lowest=T)]
  xtabs(~d$agecat)
  xtabs(~d$agemarriagecat)
  xtabs(~d$agepregnancycat)
  xtabs(~d$educationcat)
  
  d[,avgincome := income/members]
  
  d[,avgincomecat:=cut(avgincome,
                                  breaks=c(0,200,900,1824,3054,100000),
                                  include.lowest=T)]
  d[,incomecat:=cut(income,
                               breaks=c(0,200,900,1824,3054,100000),
                               include.lowest=T)]
  
  xtabs(~d$avgincomecat)
  
  
}

CreatingFurtherVariablesMahima <- function(d){
  # avicenna, gov hospital, hospital sheets private+abortion (birth data)
  # last menstrual period from green file
  # combining multiple data sources to make one variable
  #d[,mahima_dateofbirth_1:=avicenna]
  
  # this gives us a small data.table that we can look at
  #d[ident_dhis2_dhis2hbo==T,
  #  c("dhis2hbodate_1","dhis2hbodateofdeliveryhospital_1")]
  
 # d[ident_hbo==T,
    # c("hbodate_1","hbodateofdeliveryhospital_1","hboddateofbirth_1")]
  
  nam <- names(d)[stringr::str_detect(names(d),"^abbdate_[0-9]*$")]
  num <- stringr::str_replace(nam,"abbdate_","")
  for(i in num){
    print(i)
    #d[ident_avic_abb==TRUE & !is.na(abbdate_1),
    #  mahima_dateofbirth_1:=abbdate_1]
    d[ident_avic_abb==TRUE & !is.na(get(sprintf("abbdate_%s",i))),
      (sprintf("mahima_dateofbirth_%s",i)):=get(sprintf("abbdate_%s",i))]
  }
  

  nam <- names(d)[stringr::str_detect(names(d),"^dhis2hbodate_[0-9]*$")]
  num <- stringr::str_replace(nam,"dhis2hbodate_","")
  for(i in num){
    print(i)
    #d[ident_dhis2_dhis2hbo==TRUE & !is.na(dhis2hbodate_1),
    # mahima_dateofbirth_1:=dhis2hbodate_1]
    d[ident_dhis2_dhis2hbo==TRUE & !is.na(get(sprintf("dhis2hbodate_%s",i))),
      (sprintf("mahima_dateofbirth_%s",i)):=get(sprintf("dhis2hbodate_%s",i))]
  }
  
  
  nam <- names(d)[stringr::str_detect(names(d),"^hbodate_[0-9]*$")]
  num <- stringr::str_replace(nam,"hbodate_","")
  for(i in num){
    print(i)
     #d[ident_hbo==TRUE & !is.na(hbodate_1),
    # mahima_dateofbirth_1:=hbodate_1]
    d[ident_hbo==TRUE & !is.na(get(sprintf("hbodate_%s",i))),
      (sprintf("mahima_dateofbirth_%s",i)):=get(sprintf("hbodate_%s",i))]
  }

  #xtabs(~d$mahima_dateofbirth_1)
  
  # calculating gestational age at birth
  
  
  nam <- names(d)[stringr::str_detect(names(d),"^mahima_dateofbirth_[0-9]*$")]
  num <- stringr::str_replace(nam,"mahima_dateofbirth_","")
  for(i in num){
    print(i)
   # d[,mahima_gestageatbirthwk_1:=round(as.numeric(difftime(
     # mahima_dateofbirth_1,
     # booklmp,
      #units="weeks")),digits=1)]
    d[,(sprintf("mahima_gestageatbirthwk_%s",i)):=round(as.numeric(difftime(
      get(sprintf("mahima_dateofbirth_%s", i)),
      booklmp,
      units="weeks")),digits=1)]
  }
  
  #creating a new variable with all of the hospital entered gestational ages
 
  # Avicenna entered gest ages
  
  #d[ident_avic_abb==TRUE & !is.na(abbbabypregnancynoofweeks_1),
  #  mahima_hospenteredgestage_1:=abbbabypregnancynoofweeks_1]
  
  nam <-names(d)[stringr::str_detect(names(d),"^abbbabypregnancynoofweeks_[0-9]*$")]
  num <- stringr::str_replace(nam, "abbbabypregnancynoofweeks_", "")
  #here we are replacing abb with get("abb")
  for (i in num){
    print(i)
    d[ident_avic_abb==TRUE & !is.na(get(sprintf("abbbabypregnancynoofweeks_%s",i))),
      (sprintf("mahima_hospenteredgestage_%s", i)):=get(sprintf("abbbabypregnancynoofweeks_%s", i))]
  }
  
  
  #private/clinics  hbo entered gest ages
 
  nam <-names(d)[stringr::str_detect(names(d),"^dhis2hbogestagedeliv_[0-9]*$")]
  num <- stringr::str_replace(nam, "dhis2hbogestagedeliv_", "")
  nam
  num
  #here we are replacing abb with get("abb")
  for (i in num){
    print(i)
    d[ident_dhis2_dhis2hbo==TRUE & !is.na(get(sprintf("dhis2hbogestagedeliv_%s",i))),
      (sprintf("mahima_hospenteredgestage_%s", i)):=get(sprintf("dhis2hbogestagedeliv_%s", i))]
  }

  #Govt
 
  nam <-names(d)[stringr::str_detect(names(d),"^hbogestagedeliv_[0-9]*$")]
  num <- stringr::str_replace(nam, "hbogestagedeliv_", "")
  nam
  num
  #here we are replacing abb with get("abb")
  for (i in num){
    print(i)
    d[ident_hbo==TRUE & !is.na(get(sprintf("hbogestagedeliv_%s",i))),
      (sprintf("mahima_hospenteredgestage_%s", i)):=get(sprintf("hbogestagedeliv_%s", i))]
  }
  
  ######Creating new variables for gestational age prevalences
  ###making two variables: preterm and postterm gestational ages####
  ####use mahima gest ages to determine post or preterm births
  ####use calculated to see how many low or high gest ages
  #### preterm: <38 weeks, postterm: >=41 weeks
  
  #created rounded variable for gestational ages at birth
  d[,mahima_gestageatbirthwk_1_rounded:=floor(mahima_gestageatbirthwk_1)]
  d[,mahima_gestageatbirthwk_1_cats:=cut(mahima_gestageatbirthwk_1,
                                 breaks=c(-30,0,24.7,32.7,37.7,41.7,44,9999),
                                 include.lowest=T)]
  d[,mahima_hospenteredgestage_1_cats:=cut(mahima_hospenteredgestage_1,
                                    breaks=c(-30,0,24,32,37,41,44,9999),
                                    include.lowest=T)]
  
  xtabs(~d$mahima_gestageatbirthwk_1_cats)
  xtabs(~d$mahima_hospenteredgestage_1_cats)
 
  #calculate prevalences: from my data set that i want
  # we can just do d[rows that i want, c("gestagecats", "gestageenteredcats")]
  #sum each source alone for each of these two variables
  #then can calculate the prevalence

  
  
  # to detect some women with wrong gestational age
  
  d[mahima_gestageatbirthwk_1>250]$motheridno
  d[motheridno==854006681,c("booklmp","bookdate","abbdate_1","dhis2hbodate_1","hbodate_1")]
  
  # "a cross tab" - "tab" command in stata
  #xtabs(~d$mahima_gestageatbirthwk_1)
  #xtabs(~d$mahima_gestageatbirthwk_2)
  #sum(!is.na(d$booklmp))
  #sum(!is.na(d$mahima_dateofbirth_1))
  #sum(!is.na(d$mahima_gestageatbirthwk))
  #hist(d$mahima_gestageatbirthwk)
  
  
  
  ####SYSTEM GENERATED GESTATIONAL AGES
  ####USE LAST VISIT UsEDD AND BIRTHDATE FROM HOSPITALS
  d$abbbabybirthdate_1
  d$dhis2hbodate_1
  d$hbodate_1
  d$expecteddateofdelivery
  #d$usedd_1
  #d[ident_TRIAL_1==T,
  #  c(usedd_1,
  #    usedd_2)]
  
  ###Make the last edd date for each woman
  ###Use mahimadateofbirthvariable_
  ####then make a variable for gestational ages 
  ####for system generated
  
  #Making usedd for the last one
  nam <- names(d)[stringr::str_detect(names(d),"^usedd_[0-9]*$")]
  #num <- stringr::str_replace(nam,"usedd_","")
  d[,lastusedd:=as.character(NA)]
  for(i in nam ){
    print(i)
    #d[ident_us==TRUE & !is.na(usedd_1),
    # systemgenerated_1:=usedd_1]
    d[ !is.na(get(i)) &
        get(i)!="",
       lastusedd:=get(i)]
  }
  unique(d[,c("usedd_1","lastusedd")])
  d[usedd_1=="2017-07-12" & lastusedd=="2017-06-27", c(nam), with=F] 
 d[,lastusedd:=as.Date(lastusedd)]
 
 d$lastusedd

#using lastusedd and minus from hbodates 
#so that we can get actual gA from usedd
#get the diff btwn the hbodob and usedd
 #lastuseddminusdob tis is the diff in days between hosp and usedd
 d[,lastuseddminusdob:=round(as.numeric(
    difftime(mahima_dateofbirth_1,lastusedd, units="weeks")),digits=1)]
  d$lastuseddminusdob
class(d$lastuseddminusdob)

  
#calculate the gA from diff plus 40(usgestage_expected)   
d[,usgestage_expected:=40]
d[,mahima_gA_1_us:=lastuseddminusdob+40]
unique(d$mahima_gA_1_us)  
class(d$mahima_gA_1_us)
 
#d[,us_dateofconception_1:=usdate_1-usgestage_1*7]
#d$us_dateofconception_1



###Making first usedd if its at <=21 gA variable
nam <- names(d)[stringr::str_detect(names(d),"^usedd_[0-9]*$")]
num <- stringr::str_replace(nam,"usedd_","")
d[,first_1_21_usedd:=as.character(NA)]
for(i in num ){
  print(i)
  
  var_usedd <- sprintf("usedd_%s",i)
  var_usgestage <- sprintf("usgestage_%s",1)
  
  d[!is.na(get(var_usedd)) &
      get(var_usedd) != "" &
      !is.na(get(var_usgestage)) &
      get(var_usgestage) > 0 &
      get(var_usgestage) <= 21 &
      is.na(first_1_21_usedd),
    first_1_21_usedd:=get(var_usedd)]
}
d$first_1_21_usedd
sum(!is.na(d$first_1_21_usedd))

#Making gA for first_1_21_usedd

d[,first_1_21_usedd_diffbtwnHBO:=round(as.numeric(
  difftime(mahima_dateofbirth_1,first_1_21_usedd, units="weeks")),digits=1)]

d[,first_1_21_usedd_gA:=first_1_21_usedd_diffbtwnHBO+40]

d$first_1_21_usedd_gA



###Make a combination of variables
#make sure they are the same types and
#decide the type. make sure the second one is the same as the edd

d[,comboUSandLMPgA:=mahima_gestageatbirthwk_1]
d[!is.na(first_1_21_usedd_gA),comboUSandLMPgA:=first_1_21_usedd_gA]


d[,mahima_gA_1_us_cats:=cut(mahima_gA_1_us,
                            breaks=c(-30,0,24.7,32.7,37.7,41.7,44,9999),
                            include.lowest=T)]

d[,first_1_21_usedd_gA_cats:=cut(first_1_21_usedd_gA,
                                 breaks=c(-30,0,24.7,32.7,37.7,41.7,44,9999),
                                 include.lowest=T)]

d[,comboUSandLMPgA_cats:=cut(comboUSandLMPgA,
                             breaks=c(-30,0,24.7,32.7,37.7,41.7,44,9999),
                             include.lowest=T)]


###Making unified hospital birth data variables for CISMAC###

# name of hospital
# -dhis2hboconnamehospbirth_1,
d[matching=="Avicenna",merged_namehospbirth:=abbname_1]
d[matching=="Governmental",merged_namehospbirth:=hboorganisationunitname_1]
d[matching=="Private",merged_namehospbirth:=dhis2hboconnamehospbirth_1]
d[matching=="PaperHBO",merged_namehospbirth:=paperhbo_placeofdelivery]

#type of hospital delivered in
#vars <- names(d)[stringr::str_detect(names(d),"^merged_namehospbirth")]
#for(v in vars){
 # d[get(v)=="First visit",(v):="1"]
  #d[get(v)=="Second visit",(v):="2"]
 # d[get(v)=="Beyond second visit",(v):="3"]
#}


# outcome
# - dhis2hbopregoutcome_1
d[matching=="Avicenna",merged_pregoutcome:=abbbabybirthresult_1]
d[matching=="Governmental",merged_pregoutcome:=hboprevpregoutcome_1]
d[matching=="Private",merged_pregoutcome:=dhis2hbopregoutcome_1]
d[matching=="PaperHBO",merged_pregoutcome:=paperhbo_outcome]
xtabs(~d$merged_pregoutcome+d$matching)

# abortion
# - dhis2hbopregoutcome_1
d[!is.na(merged_pregoutcome),merged_abortion:=merged_pregoutcome %in% c("ABO")]
xtabs(~d$merged_abortion+d$matching)

# gestational age
# - dhis2hbogestagedeliv_1
d[matching=="Avicenna",merged_gestagedeliv:=as.numeric(stringr::str_extract(abbbabypregnancynoofweeks_1,"^[0-9][0-9]"))]
d[matching=="Governmental",merged_gestagedeliv:=hbogestagedeliv_1]
d[matching=="Private",merged_gestagedeliv:=dhis2hbogestagedeliv_1]
d[matching=="PaperHBO",merged_gestagedeliv:=paperhbo_gestationalageatbirthweeks]
unique(d$merged_namehospbirth)



# weight
# - dhis2hbopregbweight_1
d[matching=="Avicenna",merged_pregbweight:=as.numeric(abbbabyweight_1)]
d[matching=="Governmental",merged_pregbweight:=hboprevpregbweight_1]
d[matching=="Private",merged_pregbweight:=dhis2hbopregbweight_1]
d[matching=="PaperHBO",merged_pregbweight:=paperhbo_weightgrams]

# date of delivery
# - dhis2hbodateofdeliveryhospital_1
d[matching=="Avicenna",merged_datedeliv:=abbbabybirthdate_1]
d[matching=="Governmental",merged_datedeliv:=as.character(hbodateofdeliveryhospital_1)]
d[matching=="Private",merged_datedeliv:=dhis2hbodateofdeliveryhospital_1]
d[matching=="PaperHBO",merged_datedeliv:=paperhbo_birthdate]
xtabs(~d$matching+d$merged_datedeliv)

# hemo
# - dhis2hbolabcbchemoglobin_1
d[matching=="Avicenna",merged_birthhemo:=alabtestresult_1]
d[matching=="Governmental",merged_birthhemo:=hboconlabcbchemoglobin_1]
d[matching=="Private",merged_birthhemo:=dhis2hbolabcbchemoglobin_1]
d[matching=="PaperHBO",merged_birthhemo:=paperhbo_hbgatadmissiontohospital]




# blood p
# - dhis2hbosystbp_1
# - dhis2hbodiastbp_1
warning("need to fix blood pressure")
#d[matching=="Avicenna",merged_birthhemo:=alabtestresult_1]
#d[matching=="Governmental",merged_birthhemo:=hboconlabcbchemoglobin_1]
#d[matching=="Private",merged_birthhemo:=dhis2hbolabcbchemoglobin_1]

# mode of deliv
# - dhis2hbomodedeliv_1
d[matching=="Avicenna",merged_modedeliv:=abbbabybirthtype_1]
d[matching=="Governmental",merged_modedeliv:=hbomodeprevdeliv_1]
d[matching=="Private",merged_modedeliv:=dhis2hbomodedeliv_1]
d[matching=="PaperHBO",merged_modedeliv:=paperhbo_modeofdelivery]

# presentation at deliv
# - dhis2hbousfetalpresentation_1
d[matching=="Avicenna",merged_presentationdeliv:=abbbabybirthtype_1]
d[matching=="Governmental",merged_presentationdeliv:=hbousfetalpresentation_1]
d[matching=="Private",merged_presentationdeliv:=dhis2hbousfetalpresentation_1]
d[matching=="PaperHBO",merged_presentationdeliv:=paperhbo_presentationatdelivery]

# indic for csection
# - dhis2hboindicforcsec_1
warning("merged_indic_csection:=abbbabybirthtype_1")
d[matching=="Avicenna",merged_indic_csection:=abbbabybirthtype_1]
d[matching=="Governmental",merged_indic_csection:=hboindiccsectioninanycol_1]
d[matching=="Private",merged_indic_csection:=dhis2hboindicforcsec_1]
d[matching=="PaperHBO",merged_indic_csection:=paperhbo_indicationforcesarian]




}


  
  
  
 

