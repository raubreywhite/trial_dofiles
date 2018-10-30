

Analyse_clex_abstract_2018_clex<- function(d){ 
  copyVars <- c("bookhistclex","x_booktext_clex")
  newData <- vector("list",length=length(copyVars))
  for(i in 1:length(copyVars)){
    useVars <- c(
      copyVars[i],
      "x_thrombolytic_problems",
      "bookhistabort"
    )
    
    newData[[i]] <- d[,useVars,with=F]
    setnames(newData[[i]],copyVars[i],"groupVar")
    newData[[i]][,groupVar:=sprintf("%s=%s",copyVars[i],groupVar)]
  }
  newData <- rbindlist(newData)
  
  tab <- newData[,.(
    N=.N,
    
    x_thrombolytic_problems_NA=sum(is.na(x_thrombolytic_problems)),
    x_thrombolytic_problems_0=sum(x_thrombolytic_problems==FALSE,na.rm=T),
    x_thrombolytic_problems_1=sum(x_thrombolytic_problems==TRUE,na.rm=T),
    
    bookhistabort_NA=sum(is.na(bookhistabort)),
    bookhistabort_0=sum(bookhistabort==0,na.rm=T),
    bookhistabort_1=sum(bookhistabort==1,na.rm=T)
  ),
  keyby=.(
    groupVar
  )]
  tab
  
  xtabs(~d$x_thrombolytic_problems,addNA=T)
  
  # this only for booking people
  # for converting text to numbers(go inside this variable and lookfor cle 
  # to lower function::::Translate characters in character vectors
  # in particular from upper to lower case or vice versa.)
  
  
  # is there something written in these variables?
  d[,x_bookmedpress:=!is.na(bookmedpres) & bookmedpres!=""]
  d[,x_bookhistmed:=!is.na(bookhistmed) & bookhistmed!=""]
  
  # initialize the variable as NA
  d[,pniph_bookclex:=as.logical(NA)]
  
  # if any of these three variables are 0/FALSE, then pniph_bookclex=FALSE
  d[bookhistclex==0,pniph_bookclex:=FALSE]
  d[x_bookmedpress_clex==FALSE,pniph_bookclex:=FALSE]
  d[x_bookhistmed_clex==FALSE,pniph_bookclex:=FALSE]
  
  # if any of these three variables are 1/TRUE, then pniph_bookclex=TRUE
  d[bookhistclex==TRUE,pniph_bookclex:=TRUE]
  d[x_bookmedpress_clex==TRUE,pniph_bookclex:=TRUE]
  d[x_bookhistmed_clex==TRUE,pniph_bookclex:=TRUE]
  
  xtabs(~d$pniph_bookclex,addNA=T)
  
  ########################
  # combine anchistclex and anmedpres_
  vars <- c(
    names(d)[stringr::str_detect(names(d),"^anchistclex_")],
    names(d)[stringr::str_detect(names(d),"^anmedpres_")]
  )
  outcome <- "pniph_anchistclex_medpres"
  
  d[,(outcome):=as.logical(NA)]
  # if they respond at all, set them to FALSE
  for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
  # if they respond with 1, set them to TRUE
  for(i in vars) d[get(i)==1,(outcome):=TRUE]
  for(i in vars) d[stringr::str_detect(tolower(get(i)),"clex"),(outcome):=TRUE]
  
  ########################
  # combine anhistthrom
  vars <- c(
    names(d)[stringr::str_detect(names(d),"^anhistthr_")],
    names(d)[stringr::str_detect(names(d),"^anhistbloodspec_")]
  )
  outcome <- "pniph_anhistthrom_bloodspec"
  
  d[,(outcome):=as.logical(NA)]
  # if they respond at all, set them to FALSE
  for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
  # if they respond with 1, set them to TRUE
  for(i in vars) d[get(i)==1,(outcome):=TRUE]
  for(i in vars) d[stringr::str_detect(tolower(get(i)),"thromb"),(outcome):=TRUE]
  
  ########################
  # combine anhistblood
  vars <- names(d)[stringr::str_detect(names(d),"^anhistblood_")]
  outcome <- "pniph_anhistblood"
  
  d[,(outcome):=as.logical(NA)]
  # if they respond at all, set them to FALSE
  for(i in vars) d[!is.na(get(i)),(outcome):=FALSE]
  # if they respond with 1, set them to TRUE
  for(i in vars) d[get(i)==1,(outcome):=TRUE]
  
  
  ############################
  #### UGLY TABLE FOR BOOKING
  
  ###want to add ident_hr_clinic to count out of total bookorgnames
  #(d$ident_hr_clinic)
  
  vars <- c(
    "ident_dhis2_booking",
    "bookhistabort",
    "bookhistivf",
    "bookhistinfert",
    "bookhistpreterm"
  )
  #"bookhistthrom",
  #"bookhistprevdvt",
  #"bookhistblood"
  
  res <- vector("list",length=length(vars))
  
  for(i in 1:length(res)){
    newData1 <- d[ident_dhis2_control==F &
                    bookyear %in% c(2017,2018) &
                    ident_dhis2_booking==TRUE
                  ,
                  c(
                    "pniph_bookclex",
                    "bookhistclex",
                    "x_bookmedpress_clex",
                    "x_bookhistmed_clex",
                    "x_bookmedpress",
                    "x_bookhistmed",
                    "bookorgdistrict",
                    "x_thrombolytic_problems",
                    "bookhistprevdvt",
                    "bookhistblood",
                    vars[i]
                  ),
                  with=F
                  ]
    newData2 <- copy(newData1)
    newData2[,bookorgdistrict:="0PALESTINE"]
    
    newData <- rbind(newData1,newData2)
    
    setnames(newData,vars[i],"variableOfInterest")
    
    temp <- newData[,
                    .(
                      N=.N,
                      mixedBookClexYes=sum(pniph_bookclex==TRUE,na.rm=T),
                      bookhistclex=sum(bookhistclex==TRUE,na.rm=T),
                      x_bookmedpress_clex=sum(x_bookmedpress_clex==TRUE,na.rm=T),
                      x_bookhistmed_clex=sum(x_bookhistmed_clex==TRUE,na.rm=T),
                      x_bookmedpress=sum(x_bookmedpress==TRUE,na.rm=T),
                      x_bookhistmed=sum(x_bookhistmed==TRUE,na.rm=T)
                    ),
                    keyby=.(
                      bookorgdistrict,
                      variableOfInterest,
                      x_thrombolytic_problems,
                      bookhistprevdvt
                    )]
    temp[,variable:=sprintf("%s - %s",vars[i],variableOfInterest)]
    temp[,variableOfInterest:=NULL]
    
    res[[i]] <- temp
  }
  res <- rbindlist(res)
  
  setcolorder(res,c("bookorgdistrict",
                    "variable",
                    "bookhistthrom",
                    "bookhistprevdvt"
  ))
  setorder(res,
           bookorgdistrict,
           variable,
           bookhistthrom,
           bookhistprevdvt
  )
  
  openxlsx::write.xlsx(res, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "clexbookcases.xlsx"))
  
  ############################
  #### UGLY TABLE FOR ANC
  
  vars <- c(
    "pniph_anhistthrom_bloodspec",
    "pniph_anhistblood",
    "ident_dhis2_an"
  )
  res <- vector("list",length=length(vars))
  
  for(i in 1:length(res)){
    newData1 <- d[ident_dhis2_control==F&
                    bookyear %in% c(2017,2018) &  #ask richard---orbookyear==2018&
                    ident_dhis2_booking==TRUE &
                    ident_dhis2_an==TRUE
                  ,
                  c(
                    "pniph_bookclex",
                    "pniph_anchistclex_medpres",
                    "bookorgdistrict",
                    vars[i]
                  ),
                  with=F
                  ]
    newData2 <- copy(newData1)
    newData2[,bookorgdistrict:="0PALESTINE"]
    
    newData <- rbind(newData1,newData2)
    
    setnames(newData,vars[i],"variableOfInterest")
    
    temp <- newData[,
                    .(
                      N=.N,
                      bookClexYes=sum(pniph_bookclex==TRUE,na.rm=T),
                      #ancClexYes=sum(pniph_anchistclex_medpres==TRUE,na.rm=T),
                      bookandancClexYes=sum(pniph_bookclex==TRUE & pniph_anchistclex_medpres==TRUE,na.rm=T)
                    ),
                    keyby=.(
                      bookorgdistrict,
                      variableOfInterest
                    )]
    temp[,variable:=sprintf("%s - %s",vars[i],variableOfInterest)]
    temp[,variableOfInterest:=NULL]
    
    res[[i]] <- temp
  }
  res <- rbindlist(res)
  setcolorder(res,c("bookorgdistrict","variable"))
  setorder(res,bookorgdistrict,variable)
  
  openxlsx::write.xlsx(res, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "clexanccases.xlsx"))
  
  
}


