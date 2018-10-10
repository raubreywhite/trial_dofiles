Analyse_pniph_abstract_2018_ppc<- function(){
  
}

Analyse_pniph_abstract_2018_nbc<- function(){
  
}

Analyse_pniph_abstract_2018_clex<- function(d){
  
  # this only for booking people
  d[,x_bookmedpress_clex:=as.numeric(stringr::str_detect(tolower(bookmedpres),"cle"))]
  
  xtabs(~d[ident_dhis2_control==F&
             bookyear==2017]$x_bookmedpress_clex)
  unique(d[ident_dhis2_control==F&
              bookyear==2017&
              x_bookmedpress_clex==TRUE]$bookmedpres)
  
  smallD <- d[ident_dhis2_control==F&
                bookyear==2017 &
                ident_dhis2_booking==TRUE,
    c(
    "bookevent",
    "bookhistclex",
    "bookhistthrom",
    "bookhistabort",
    "bookhistivf",
    "bookhistinfert",
    "bookgestage",
    "bookhistblood",
    "bookhistprevdvt",
    "bookorgdistrict",
    "bookorgname",
    "bookhistpreterm",
    "x_bookmedpress_clex"
    )]
  
  
  #"bookhistmed"
  #"bookmedpres"
  
  # copying bookhistclex to bookhistclex2
  # this lets us have it as a column and as a row
  smallD[,bookhistclex2:=as.character(bookhistclex)]
  
  # duplicate the entire dataset
  # and for one of the copied datasets
  # set "bookhistclex2" to "Everyone"
  # this is like "Palestine" vs "Districts"
  temp <- copy(smallD)
  temp[,bookhistclex2:="Everyone"]
  smallD <- rbind(smallD,temp)
  
  long <- melt.data.table(smallD,
                          id.vars=c("bookevent","bookhistclex2"))
  
  uglytable <- long[,
              .(
                denominator=.N,
                is_NA=sum(is.na(value)),
                not_NA=sum(!is.na(value)),
                value0=sum(value==0,na.rm=T),
                value1=sum(value==1,na.rm=T),
                avgerage=round(mean(as.numeric(value),na.rm=T),digits = 1)
                ),
              keyby=.(variable,bookhistclex2)
              ]
  uglytable
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "clexbookcases.xlsx"))
  

  #this is for anc people
  
  # creating a unified variable for history of clex
  d[,unified_anchistclex:=as.numeric(NA)]
  vars <- names(d)[stringr::str_detect(names(d),"^anchistclex_")]
  for(i in vars){
    # if time-specific variable=1, then set unified var=1
    d[get(i)==1,unified_anchistclex:=1]
    # if time-specific variable=0 & unified variable missing
    # then set unified var=0
    d[get(i)==0 & is.na(unified_anchistclex),unified_anchistclex:=0]
  }
  
 
   vars_anhistthr <- names(d)[stringr::str_detect(names(d),"^anhistthr_")]
   vars_anchistclex <-names(d)[stringr::str_detect(names(d),"^anchistclex_")]
   vars_anorgname <- names(d)[stringr::str_detect(names(d),"^anorgname_")]
   vars_anhistblood <- names(d)[stringr::str_detect(names(d),"^anhistblood_")]
   vars_anhistbloodspec <- names(d)[stringr::str_detect(names(d),"^anhistbloodspec_")]
   vars_angestage <- names(d)[stringr::str_detect(names(d),"^angestage_")]
   
   # searching inside anmedpres for "clex" and
   # then storing it in vars_x_anmedpres_clex
   vars_anmedpres <-names(d)[stringr::str_detect(names(d),"^anmedpres_")]
   vars_x_anmedpres_clex <- c()
   for(i in vars_anmedpres){
     newVar <- sprintf("x_%s_clex",i)
     d[,(newVar):=as.numeric(stringr::str_detect(tolower(get(i)),"cle"))]
     vars_x_anmedpres_clex <- c(vars_x_anmedpres_clex, newVar)
   }

   
   # when specifying column names as variables
   # (i.e. "look inside the variable")
   # you need to add a ",with=F" to data.table
   # or else it wont work
  smallD <- d[ident_dhis2_control==F&
                bookyear==2017 &
                ident_dhis2_an==TRUE,
              c(
                "bookevent",
                "unified_anchistclex",
                vars_anchistclex,
                vars_anhistthr,
                vars_anorgname,
                vars_anhistblood,
                vars_anhistbloodspec,
                vars_angestage,
                vars_x_anmedpres_clex
              ),with=F]
  
  
  #anothermed_1		
  #anmedpres_1
  #"bookhistmed"
  #"bookmedpres"
  
  # copying anhistclex to anhistclex2
  # this lets us have it as a column and as a row
  smallD[,unified_anchistclex:=as.character(unified_anchistclex)]
  
  # duplicate the entire dataset
  # and for one of the copied datasets
  # set "bookhistclex2" to "Everyone"
  # this is like "Palestine" vs "Districts"
  temp <- copy(smallD)
  temp[,unified_anchistclex:="Everyone"]
  smallD <- rbind(smallD,temp)
  
  long <- melt.data.table(smallD,
                          id.vars=c("bookevent","unified_anchistclex"))
  
  uglytable <- long[,
                    .(
                      denominator=.N,
                      is_NA=sum(is.na(value)),
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T),
                      value1=sum(value==1,na.rm=T),
                      avgerage=round(mean(as.numeric(value),na.rm=T),digits = 1)
                    ),
                    keyby=.(variable,unified_anchistclex)
                    ]
  uglytable
  openxlsx::write.xlsx(uglytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "clexanccases.xlsx"))
  
  
 
  


  
  

  
  
  laborgcode_14
  laborgunit_1
  laborgname_12
  labother1_14
  labotherres1_1
  labgestage_14
  labplatelets_12
  
  cpodvt_4
  
  prevdvt_4
  prevpreeclampsia_1
  preveclampsia_1
  
  prevoutcome_3
  cpopregoutcome_1
  cpopreeclampsia_6
  cpoeclampsia_1
  
  
  
  
}

Analyse_pniph_abstract_2018<- function(){
  Analyse_pniph_abstract_2018_ppc()
  Analyse_pniph_abstract_2018_nbc()
  Analyse_pniph_abstract_2018_clex()
}