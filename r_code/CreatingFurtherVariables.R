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
                                 breaks=c(-30,0,24.7,37.7,41.7,44,9999),
                                 include.lowest=T)]
  d[,mahima_hospenteredgestage_1_cats:=cut(mahima_hospenteredgestage_1,
                                    breaks=c(-30,0,24,37,41,44,9999),
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
  
  
  #Diabetes indicators
  xtabs(~d$bookhistgdm,addNA=T)
  xtabs(~d$ident_dhis2_booking+ d$bookhistgdm,addNA=T)
}


CreatingFurtherVariablesPNIPH <-function(d){
  # abstract 2018
  d[,pniph_agecat:=cut(age,
                       breaks=c(0,20,25,30,35,40,45,50,100),
                       include.lowest=T)]
  d[,pniph_agecat2:=cut(age,
                        breaks=c(0,20,30,40,100),
                        include.lowest=T)]
  d[,pniph_agemarriagecat:=cut(agemarriage,
                               breaks=c(0,20,25,30,35,40,100),
                               include.lowest=T)]
  d[,pniph_agemarriagecat2:=cut(agemarriage,
                                breaks=c(0,20,30,40,100),
                                include.lowest=T)]
  d[,pniph_agepregnancycat:=cut(agepregnancy,
                                breaks=c(0,20,25,30,35,40,100),
                                include.lowest=T)]
  d[,pniph_agepregnancycat2:=cut(agepregnancy,
                                 breaks=c(0,20,30,40,100),
                                 include.lowest=T)]
  
  d[,pniph_educationcat:=cut(education,
                             breaks=c(0,9,13,100),
                             include.lowest=T)]
  
  d[,pniph_educationcat2:=cut(education,
                              breaks=c(0,11,12,16,100),
                              include.lowest=T)]
  levels(d$pniph_educationcat2)
  
  d[,pniph_avgincome := income/members]
  
  d[,pniph_avgincomecat:=cut(pniph_avgincome,
                             breaks=c(0,200,900,1824,3054,100000),
                             include.lowest=T)]
  d[,pniph_incomecat:=cut(income,
                          breaks=c(0,200,900,1824,3054,100000),
                          include.lowest=T)]
  d[,pniph_incomecat2:=cut(income,
                           breaks=c(0,1500,3000,5000,100000),
                           include.lowest=T)]
  
  d[,pniph_householdcat:=cut(members,
                             breaks=c(0,3,5,30),
                             include.lowest=T)]

  d[,pniph_nbcdaysatvisit:=as.numeric(nbcdate_1-nbcdateofdelivery_1)]
  
  # number of ppc visits
  d[,pniph_num_ppc_visits:=0]
  vars <- names(d)[stringr::str_detect(names(d),"ppcevent")]
  for(i in vars){
    d[!is.na(get(i)),pniph_num_ppc_visits:=pniph_num_ppc_visits+1]
  }
  xtabs(~d$pniph_num_ppc_visits, addNA=T)

  
  #gestage at last anc visit, includes booking visit
  vars_gestages <- c("bookgestage",
    names(d)[stringr::str_detect(names(d),"^angestage_")]
  )
  
  d[,pniph_gestageatlastvisit:=as.numeric(NA)]
  for(i in vars_gestages){
    d[!is.na(get(i)) & get(i)>0, pniph_gestageatlastvisit:=get(i)]
  }
  d$gestageatlastvisit
  
  ###creating gest age categories at last visit
  d[,pniph_gestageatlastvisit_cats:=cut(pniph_gestageatlastvisit,
    breaks=c(0,36,40,42,9999),
    include.lowest=T)]
  d$pniph_gestageatlastvisit_cats
  unique(d$gestageatlastvisit)
  
  
  ###make cpo gest age into categories
  d[,pniph_cpogestage_1_cats:=cut(cpogestage_1,
    breaks=c(0,25,38,41,99999),
    include.lowest=T)]
  
  # Has anemia/hypertension/bleeding
  d[,pniph_risktype_anemia:=F]
  d[,pniph_risktype_chronichypertension:=F]
  d[,pniph_risktype_vaginalbleeding:=F]
  vars <- names(d)[stringr::str_detect(names(d),"^risktype_")]
  for(i in vars){
    d[get(i)%in%c("MildAnemia",
                  "ModerateAnemia",
                  "SevereAnemia"),pniph_risktype_anemia:=T]
  d[get(i)%in%c("ChronicHypertension"),pniph_risktype_chronichypertension:=T]
  d[get(i)%in%c("MildBleeding"),pniph_risktype_mildbleeding:=T]
 
    
  }
  
  
  
  
  
}

