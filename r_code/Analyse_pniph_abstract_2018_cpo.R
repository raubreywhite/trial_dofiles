Analyse_pniph_abstract_2018_cpo<- function(d){
  
  vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
  vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
  vars_cpoevent <-names(d)[stringr::str_detect(names(d), "^cpoevent_")]

  
  # everyone starts off as false
  d[,exposure_postpartumhemorrhage:=FALSE]
  # go through all the visits. if any visit=1
  # then the variable = TRUE
  for(i in vars_cpopostpartumhemorrhage){
    d[get(i)==1,exposure_postpartumhemorrhage:=TRUE]
  }
  
  # everyone starts off as false
  d[,exposure_puerpalsepsis:=FALSE]
  # go through all the visits. if any visit=1
  # then the variable = TRUE
  for(i in vars_cpopuerpalsepsis){
    d[get(i)==1,exposure_puerpalsepsis:=TRUE]
  }

  #THESE ARE DEMOGRAPHIC THINGS YOU CAN ADD IN
  # IE THESE ARE MORE ROWS
  d[,pniph_000EVERYONE:=1]
  vars_demo <- c(
    "pniph_000EVERYONE",
    "pniph_agecat2",
    "pniph_avgincomecat",
    "pniph_incomecat2",
    "pniph_agemarriagecat2",
    "pniph_agepregnancycat2",
    "pniph_educationcat2",
    "pniph_householdcat",
    "pniph_cpogestage_1_cats",
    "pniph_gestageatlastvisit_cats",
    names(d)[stringr::str_detect(names(d),"^ancounsdanger_")],
    names(d)[stringr::str_detect(names(d),"^ancounslabor_")],
    names(d)[stringr::str_detect(names(d),"^ancounsnut_")],
    names(d)[stringr::str_detect(names(d),"^cpomodedelivery_")],
    names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]        
             
    )
 
 
 # THESE ARE VARIABLES THAT YOU CAN USE TO CREATE MORE COLUMNS
  vars_for_columns <- c(
    "exposure_postpartumhemorrhage",
    "exposure_puerpalsepsis",
    "cpopregoutcome_1"
    )
  
  smallD <- d[ident_dhis2_control==F &
                bookdate>="2017-09-01" & bookdate<="2018-09-01" &
                ident_dhis2_booking==TRUE & 
                ident_dhis2_an==TRUE & 
                ident_dhis2_cpo==TRUE & 
                ident_dhis2_ppc==TRUE,
              
              c(
                "bookorgdistrict",
                vars_for_columns,
                "ident_hr_clinic",
                vars_demo
              ),with=F]
  checkingD <- copy(smallD)
  
  tryCatch({
    # number of visits
    tab <- d[ident_dhis2_control==F &
               bookdate>="2017-09-01" & bookdate<="2018-09-01" &
               ident_dhis2_booking==TRUE & 
               ident_dhis2_an==TRUE & 
               ident_dhis2_cpo==TRUE & 
               ident_dhis2_ppc==TRUE,.(
      NWomen=.N
    ),keyby=.(
      bookorgname
    )]
    
    openxlsx::write.xlsx(tab, 
                         file.path(
                           FOLDER_DROPBOX_RESULTS,
                           "pniph",
                           "abstracts_2018",
                           "cpo_list_of_clinics.xlsx"))
  },error = function(e){
    print("*****Analyse_pniph_abstract_2018_cpo() NOT WORKING cpo_list_of_clinics.xlsx")  
  })
  
  # duplicate the dataset
  # make one of them have a district of palestine
  # then put them on top of each other
  smallDPalestine <- copy(smallD)
  smallDPalestine[,bookorgdistrict:="0PALESTINE"]
  
  smallD <- rbind(smallD,smallDPalestine)
  smallD[,id:=1:.N]
  
  long <- melt.data.table(smallD, id.vars=c(
    "id",
    vars_for_columns,
    "bookorgdistrict"
  ),variable.factor = F, value.factor = F)
  
  long[,variable:=sprintf("%s=%s",variable,value)]
  
  # THIS IS WHERE YOU MAKE YOUR TABLE
  uglytable <- long[,
                    .(
                      NoHemorr_NoSepsis=sum(exposure_postpartumhemorrhage==F & exposure_puerpalsepsis==F,na.rm=T),
                      NoHemorr_YESSepsis=sum(exposure_postpartumhemorrhage==F & exposure_puerpalsepsis==T,na.rm=T),
                      YESHemorr_NoSepsis=sum(exposure_postpartumhemorrhage==T & exposure_puerpalsepsis==F,na.rm=T),
                      YESHemorr_YESSepsis=sum(exposure_postpartumhemorrhage==T & exposure_puerpalsepsis==T,na.rm=T),
                      NumAbortions=sum(cpopregoutcome_1=="XXXXX")
                    ),
                    keyby=.(
                      bookorgdistrict,
                      variable
                    )
                    ]
  
  setorder(uglytable, bookorgdistrict, variable)
  
  # do these numbers match up?
  # with districts, you can check didstricts
  f <- checkingD[is.na(pniph_incomecat2) &
                   bookorgdistrict=="Y" &
                   exposure_postpartumhemorrhage==F & 
                   exposure_puerpalsepsis==F
                 ]
  nrow(f) #12
  # do these numbers match up?
  # with palestine, dont include district
  f <- checkingD[is.na(ancounsdanger_10) &
                   exposure_postpartumhemorrhage==F & 
                   exposure_puerpalsepsis==T
                 ]
  nrow(f) #4
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "cpo.xlsx"))
  
}

OLDAnalyse_pniph_abstract_2018_cpo<- function(d){  
  smallCPOd <- d[ident_dhis2_control==F &
                   bookdate>="2017-09-01" & bookdate<="2018-09-01" &
                   ident_dhis2_booking==TRUE & 
                   ident_dhis2_an==TRUE & 
                   ident_dhis2_cpo==TRUE & 
                   ident_dhis2_ppc==TRUE,
                 c("cpopergoutcome_1",
                   "cpomodedelivery_1",
                   "cpoplaceofbirth_1")]
  
  
  #this is for cpo people
  unique(d$village)
  unique(d$city)
  unique(d$camp)
  unique(d$bookhighrisk)
  unique(d$anhighrisk_1)
  
  #Make categories for variables #ers and percents for each cat
  #cpopregoutcome_1 NA, "LIVE", "STILL","ABO","NEO_DEATH","INF_DEATH","LATE_DEATH"
  unique(d$cpopregoutcome_1)
  #cpoplaceofbirth_1 GOV",
  #"PH",
  #"PC",
  #"NGO",
  #"UNRWA",
  #"TRANS",
  #"HOME",
  #NA
  #cpomodedelivery_1
  #"Spontaneous vaginal",
  #"Caesarian section",
  #"Assisted vaginal",
  #"VACCUUM",
  #NA
  #conintendedplaceofbirth
  #conrecommendedplaceofbirth
  unique(d$cpoplaceofbirth_1)
  unique(d$cpomodedelivery_1)
  unique(d$conintendedplaceofbirth)
  unique(d$conrecommendedplaceofbirth)
  unique(d$cpogestage_1)
  #make into categories 0-24, 25-38, <38
  unique(d$cpopregoutcome_1)
  #Gestational age at last anc visit less than 36 and more than 36 weeks
  #Average anc visits per woman
  #numbers and percents of woman who received counseling on the following
  d$ancounsdanger_1
  d$ancounslabor_1
  d$ancounsnut_1
  
  ####creating variable for gestational age at last visit
  #choose the variables you want first from the visit
  #create a new variable as numeric
  #goes through each of the variables for gest age and saves it as we see it.
  #will save this last one
  ###need to change the first row because have at booking and have at anc
  vars_gestages <- c(
    "bookgestage", 
    names(d)[stringr::str_detect(names(d),"^angestage_")]
  )
  d[,gestageatlastvisit:=as.numeric(NA)]
  for(i in vars_gestages){
    d[!is.na(get(i)) & get(i)>0, gestageatlastvisit:=get(i)]
  }
  sum(is.na(d$gestageatlastvisit))
  #  vars_gestages <- names(d)[stringr::str_detect(names(d),"^angestage_")]
  sum(is.na(d$angestage_1) | d$angestage_1==0)
  
  
}


XXXXAnalyse_pniph_abstract_2018_cpo<- function(d){
  
  vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
  vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
  vars_cpoevent <-names(d)[stringr::str_detect(names(d), "^cpoevent_")]
 
   # everyone starts off as false
  d[,exposure_postpartumhemorrhage:=FALSE]
  # go through all the visits. if any visit=1
  # then the variable = TRUE
  for(i in vars_cpopostpartumhemorrhage){
    d[get(i)==1,exposure_postpartumhemorrhage:=TRUE]
  }
  
  # everyone starts off as false
  d[,exposure_puerpalsepsis:=FALSE]
  # go through all the visits. if any visit=1
  # then the variable = TRUE
  for(i in vars_cpopuerpalsepsis){
    d[get(i)==1,exposure_puerpalsepsis:=TRUE]
  }
  
  d[exposure_postpartumhemorrhage==F & exposure_puerpalsepsis==F,
    exposure_postpartumhemorrhage_and_puerpalsepsis:="NoHemorr/NoSepsis"]
  d[exposure_postpartumhemorrhage==F & exposure_puerpalsepsis==T,
    exposure_postpartumhemorrhage_and_puerpalsepsis:="NoHemorr/YESSepsis"]
  d[exposure_postpartumhemorrhage==T & exposure_puerpalsepsis==F,
    exposure_postpartumhemorrhage_and_puerpalsepsis:="YESHemorr/NoSepsis"]
  d[exposure_postpartumhemorrhage==T & exposure_puerpalsepsis==T,
    exposure_postpartumhemorrhage_and_puerpalsepsis:="YESHemorr/YESSepsis"]
  
  
  d[,pniph_000EVERYONE:=1]
  vars_demo <- c(
    "pniph_000EVERYONE",
    "pniph_agecat2",
    "pniph_avgincomecat",
    "pniph_incomecat2",
    "pniph_agemarriagecat2",
    "pniph_agepregnancycat2",
    "pniph_educationcat2",
    
    names(d)[stringr::str_detect(names(d),"^ancounsdanger_")],
    names(d)[stringr::str_detect(names(d),"^ancounslabor_")],
    names(d)[stringr::str_detect(names(d),"^ancounsnut_")]
  )
  
  smallD <- d[ident_dhis2_control==F &
                bookdate>="2017-09-01" & bookdate<="2018-09-01" &
                ident_dhis2_booking==TRUE & 
                ident_dhis2_an==TRUE & 
                ident_dhis2_cpo==TRUE & 
                ident_dhis2_ppc==TRUE,
                
              c(
                "bookorgdistrict",
                "exposure_postpartumhemorrhage_and_puerpalsepsis",
                "ident_hr_clinic",
                vars_demo
              ),with=F]
  
  # duplicate the dataset
  # make one of them have a district of palestine
  # then put them on top of each other
  smallDPalestine <- copy(smallD)
  smallDPalestine[,bookorgdistrict:="0PALESTINE"]
  
  smallD <- rbind(smallD,smallDPalestine)
  smallD[,id:=1:.N]
  
  long <- melt.data.table(smallD, id.vars=c(
    "id",
    "exposure_postpartumhemorrhage_and_puerpalsepsis",
    "bookorgdistrict"
  ),variable.factor = F, value.factor = F)
  
  long[,variable:=sprintf("%s=%s",variable,value)]
  
  uglytable <- long[,
                    .(
                      N=.N
                    ),
                    keyby=.(
                      bookorgdistrict,
                      exposure_postpartumhemorrhage_and_puerpalsepsis,
                      variable
                    )
                    ]
  uglytable <- dcast.data.table(uglytable,
                   bookorgdistrict+variable~exposure_postpartumhemorrhage_and_puerpalsepsis,
                   value.var="N")
  
  uglytable[is.na(`NoHemorr/NoSepsis`),`NoHemorr/NoSepsis`:=0]
  uglytable[is.na(`NoHemorr/YESSepsis`),`NoHemorr/YESSepsis`:=0]
  uglytable[is.na(`YESHemorr/NoSepsis`),`YESHemorr/NoSepsis`:=0]
  uglytable[is.na(`YESHemorr/YESSepsis`),`YESHemorr/YESSepsis`:=0]
  
  setorder(uglytable, bookorgdistrict, variable)
  
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "cpo.xlsx"))
  
}

OLDAnalyse_pniph_abstract_2018_cpo<- function(d){  
smallCPOd <- d[ident_dhis2_control==F &
                       bookdate>="2017-09-01" & bookdate<="2018-09-01" &
                       ident_dhis2_booking==TRUE & 
                       ident_dhis2_an==TRUE & 
                       ident_dhis2_cpo==TRUE & 
                       ident_dhis2_ppc==TRUE,
                  c("cpopergoutcome_1",
                    "cpomodedelivery_1",
                    "cpoplaceofbirth_1")]
              

  #this is for cpo people
  unique(d$village)
  unique(d$city)
  unique(d$camp)
  unique(d$bookhighrisk)
  unique(d$anhighrisk_1)
  
  #Make categories for variables #ers and percents for each cat
  #cpopregoutcome_1 NA, "LIVE", "STILL","ABO","NEO_DEATH","INF_DEATH","LATE_DEATH"
  unique(d$cpopregoutcome_1)
  #cpoplaceofbirth_1 GOV",
  #"PH",
  #"PC",
  #"NGO",
  #"UNRWA",
  #"TRANS",
  #"HOME",
  #NA
  #cpomodedelivery_1
  #"Spontaneous vaginal",
  #"Caesarian section",
  #"Assisted vaginal",
  #"VACCUUM",
  #NA
  #conintendedplaceofbirth
  #conrecommendedplaceofbirth
  unique(d$cpoplaceofbirth_1)
  unique(d$cpomodedelivery_1)
  unique(d$conintendedplaceofbirth)
  unique(d$conrecommendedplaceofbirth)
  unique(d$cpogestage_1)
  #make into categories 0-24, 25-38, <38
  
  #Gestational age at last anc visit less than 36 and more than 36 weeks
  #Average anc visits per woman
  #numbers and percents of woman who received counseling on the following
  d$ancounsdanger_1
  d$ancounslabor_1
  d$ancounsnut_1
  
  ####creating variable for gestational age at last visit
  #choose the variables you want first from the visit
  #create a new variable as numeric
  #goes through each of the variables for gest age and saves it as we see it.
  #will save this last one
  ###need to change the first row because have at booking and have at anc
  vars_gestages <- c(
    "bookgestage", 
    names(d)[stringr::str_detect(names(d),"^angestage_")]
    )
  d[,gestageatlastvisit:=as.numeric(NA)]
  for(i in vars_gestages){
    d[!is.na(get(i)) & get(i)>0, gestageatlastvisit:=get(i)]
  }
  sum(is.na(d$gestageatlastvisit))
#  vars_gestages <- names(d)[stringr::str_detect(names(d),"^angestage_")]
   sum(is.na(d$angestage_1) | d$angestage_1==0)
  
  
}
  
 
  
#   
#   
#   
#   
#   vars_cpodvt <- names(d)[stringr::str_detect(names(d),"^cpodvt_")]
#   vars_cpoeclampsia <-names(d)[stringr::str_detect(names(d),"^cpoeclampsia_")]
#   vars_cpopreeclampsia <- names(d)[stringr::str_detect(names(d),"^cpopreeclampsia_")]
#   vars_cpocomplicationsnone <- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
#   vars_cpoantepartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
#   vars_cpopostpartumhemorrhage <- names(d)[stringr::str_detect(names(d),"^cpopostpartumhemorrhage_")]
#   vars_cpopuerpalsepsis <- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
#   vars_ancounsdanger <- names(d)[stringr::str_detect(names(d),"^ancounsdanger_")]
#   vars_ancounslabor <- names(d)[stringr::str_detect(names(d),"^ancounslabor_")]
#   vars_ancounsnut <- names(d)[stringr::str_detect(names(d),"^ancounsnut_")]
#   
#   #vars_cpopregoutcome <-names(d)[stringr::str_detect(names(d),"^cpopregoutcome_")]
#   
#   d[,pniph_000EVERYONE:=1]
#   vars_demo <- c(
#     "pniph_000EVERYONE",
#     "pniph_agecat2",
#     "pniph_avgincomecat",
#     "pniph_incomecat2",
#     "pniph_agemarriagecat2",
#     "pniph_agepregnancycat2",
#     "pniph_educationcat2"
#   )
#   
#   smallD <- d[ident_dhis2_control==F &
#                 bookdate>="2017-09-01" & bookdate<="2018-09-01" &
#                 ident_dhis2_booking==TRUE & 
#                 ident_dhis2_an==TRUE & 
#                 ident_dhis2_cpo==TRUE & 
#                 ident_dhis2_ppc==TRUE,
#               c(
#                 "bookorgdistrict",
#                 vars_cpodvt,
#                 vars_cpoeclampsia,
#                 vars_cpopreeclampsia,
#                 vars_cpocomplicationsnone,
#                 vars_cpoantepartumhemorrhage,
#                 vars_cpopostpartumhemorrhage,
#                 vars_cpopuerpalsepsis,
#                 vars_ancounsdanger,
#                 vars_ancounslabor,
#                 vars_ancounsnut,
#                 vars_demo
#               ),with=F]
#  
#   # duplicate the dataset
#   # make one of them have a district of palestine
#   # then put them on top of each other
#   smallDPalestine <- copy(smallD)
#   smallDPalestine[,bookorgdistrict:="0PALESTINE"]
#   
#   smallD <- rbind(smallD,smallDPalestine)
#   smallD[,id:=1:.N]
#   
#   # please create me a 7 unit long list
#   res <- vector("list",length=length(vars_demo))
#   
#   # i want all the vars that are not in vars_demo
#   varsIwant <- names(smallD)
#   varsIwant <- varsIwant[!varsIwant %in% vars_demo]
#   
#   for(i in 1:length(res)){
#     var <- vars_demo[i]
#     res[[i]] <- copy(smallD)
#     res[[i]][,DEMO:=sprintf("%s=%s",var,get(var))]
#     # only keep the vars that I want
#     res[[i]] <- res[[i]][,c(varsIwant,"DEMO"),with=F]
#   }
#   # compress the list into 1 data.table
#   smallD <- rbindlist(res)
#   
#   long <- melt.data.table(smallD, id.vars=c(
#     "id",
#     "DEMO",
#     "bookorgdistrict"
#   ),variable.factor = F, value.factor = F)
#   
#   
#   uglytable <- long[,
#                     .(
#                       denominator=.N,
#                       is_NA=sum(is.na(value)),
#                       not_NA=sum(!is.na(value)),
#                       value0=sum(value==0,na.rm=T),
#                       value1=sum(value==1,na.rm=T),
#                       value2=sum(value==2,na.rm=T),
#                       value3=sum(value==3,na.rm=T)
#                     ),
#                     keyby=.(
#                       bookorgdistrict,
#                       DEMO,
#                       variable
#                       )
#                     ]
#   
#   openxlsx::write.xlsx(uglytable, 
#                        file.path(
#                          FOLDER_DROPBOX_RESULTS,
#                          "pniph",
#                          "abstracts_2018",
#                          "cpo_descriptives.xlsx"))
#   
#   ###General numbers for cpo stuff###
#   sink()
#   smallCPOd <- d[ident_dhis2_control==F &
#                    bookdate>="2017-09-01" & bookdate<="2018-09-01" &
#                    ident_dhis2_booking==TRUE & 
#                    ident_dhis2_an==TRUE & 
#                    ident_dhis2_cpo==TRUE & 
#                    ident_dhis2_ppc==TRUE,
#                  c("cpopergoutcome_1",
#                    "cpomodedelivery_1",
#                    "cpoplaceofbirth_1")]
#   
#   
#   
# # 
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   #for demographic
# #   agecat
# #   avgincomecat
# #   incomecat
# #   agemarriagecat
# #   agepregnancycat
# #   educationcat
# #   village
# #   city
# #   camp
# #   #members <3   3-5   >5
# #   
# #   bookevent
# #   bookhighrisk
# #   anchighrisk
# #   
# #   #unique bookrogname 
# #   
# #   #gest age at last visit (less than 36 and more than 36 weeks)
# #   #avg anc visits per woman
# #   
# #   
# #   vars_cpoantepartumhemorrhage	<- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
# #   vars_cpopuerpalsepsis	<- names(d)[stringr::str_detect(names(d),"^cpopuerpalsepsis_")]
# #   vars_cpoantepartumhemorrhage	<- names(d)[stringr::str_detect(names(d),"^cpoantepartumhemorrhage_")]
# #   vars_cpocomplicationsnone	<- names(d)[stringr::str_detect(names(d),"^cpocomplicationsnone_")]
# #   vars_ancounsdanger	<- names(d)[stringr::str_detect(names(d),"^ancounsdanger_")]
# #   vars_ancounsnut	<- names(d)[stringr::str_detect(names(d),"^ancounsnut_")]
# #   vars_ancounslabor	<- names(d)[stringr::str_detect(names(d),"^ancounslabor_")]
# #   
# #   
# #   
# #   
# #   smallD <- d[ident_dhis2_control==F &
# #                 bookyearmonth>=09-2017 & bookyearmonth<=09-2018,
# #               ident_dhis2_anc==T,
# #               ident_dhis2_ppc==T,
# #               ident_dhis2_cpo==T,
# #               
# #               # ident_dhis2_df_no_gf_or_bf==F
# #               
# #               c(agecat,
# #                 avgincomecat,
# #                 incomecat,
# #                 agemarriagecat,
# #                 agepregnancycat,
# #                 educationcat,
# #                 vars_cpoantepartumhemorrhage,
# #                 vars_cpopuerpalsepsis,
# #                 vars_cpoantepartumhemorrhage,
# #                 vars_cpocomplicationsnone,
# #                 vars_ancounsdanger,
# #                 vars_ancounsnut,
# #                 vars_ancounslabor
# #                 
# #               )] 
# 
# 
# 
