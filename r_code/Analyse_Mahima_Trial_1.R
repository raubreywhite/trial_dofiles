Analyse_BookingDescriptives <- function(d=NULL){
  #RColorBrewer::display.brewer.all() 
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # to make graphs
  
  toPlot <- d[ident_dhis2_booking==1,
              .(numWomen=.N),
              by=.(bookyearmonth)
              ]
  
  setorder(toPlot,bookyearmonth)
  
  p <- ggplot(data=toPlot, mapping=aes(x=bookyearmonth,y=numWomen))
  p <- p + geom_bar(stat="identity")
  p <- p + geom_label(mapping=aes(label=numWomen), size=1)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "bookings_by_month.png"),
         plot=p)
  
  # if we use "by=.()" it wont be sorted
  # if we use "keyby=.()" it will be sorted
  variablesIWant <- c(
    "bookyearmonth",
    "ident_dhis2_control",
    "bookorgname",
    "bookgestage"
  )
  data_clinic <- d[ident_dhis2_booking==1 & ident_TRIAL_1==1,variablesIWant,with=F]
  data_palestine <- d[ident_dhis2_booking==1 & ident_TRIAL_1==1,variablesIWant,with=F]
  data_palestine[,bookorgname:="0Palestine"]
  
  dataForTable_month <- rbind(data_clinic,data_palestine)
  dataForTable_year <- rbind(data_clinic,data_palestine)
  dataForTable_year[,bookyearmonth:="Total"]
  
  dataForTable <- rbind(dataForTable_month,dataForTable_year)
  
  tab <- dataForTable[,
                      .(
                        meangestage=mean(bookgestage, na.rm=T),
                        mediangestage=median(bookgestage, na.rm=T),
                        numWomen=.N
                      ),
                      keyby=.(
                        bookyearmonth, ident_dhis2_control, bookorgname
                      )
                      ]
  tab[,meangestage:=round(meangestage,digits=0)]
  tab[,mediangestage:=round(mediangestage,digits=0)]
  tab
  
  openxlsx::write.xlsx(tab, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "booking_descriptives",
                         "numberandmeangestage.xlsx"))
  
  p <- ggplot(tab[bookyearmonth!="Total"], aes(x=ident_dhis2_control, y=meangestage))
  p <- p + geom_boxplot()
  p <- p + scale_x_discrete("Arm A clinics")
  p <- p + scale_y_continuous("Mean gestage")
  p1nocaption <- p + labs(title="Clinics' Mean Gestational Age in Trial 1")
  p1withcaption <- p1nocaption + labs(caption=GraphCaption())
  #p1withcaption <- coord_flip()
  p1withcaption
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1bycontrol.png"),
         plot=p1withcaption,
         height=210,
         width=297,
         units="mm")
  
  p <- ggplot(tab[bookorgname!="0Palestine"], aes(x=bookyearmonth, y=meangestage))
  p <- p + geom_boxplot()
  p <- p + scale_x_discrete("Booking Month")
  p <- p + scale_y_continuous("Mean gestage")
  p <- p + labs(title="Clinics' Mean Gestational Age in Trial 1")
  p2 <- p + labs(caption=GraphCaption())
  p2
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1bymonth.png"),
         plot=p2,
         height=210,
         width=297,
         units="mm")
  
  p3 <- gridExtra::grid.arrange(p1nocaption,p2)
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "meangestageintrial1_together.png"),
         plot=p3,
         height=210,
         width=297,
         units="mm")
  
}

Analyse_EnteredVsCalculated <- function(d){
  ## comparing mahima gestational age calculated vs entered
  
  #dev.off() try to run this code if get weird graphic errors
  p <- ggplot(d[ident_TRIAL_1==TRUE], aes(x=mahima_gestageatbirthwk_1, y=mahima_hospenteredgestage_1))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + labs(title="Entered and Calculated Gestational Ages")
  p <- p + scale_x_continuous("Calculated Gestational Ages")
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "entered_and_calculated_gest_ages.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
}


Analyse_BookingRawVsClean <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # directly pull in the raw data
  FOLDERS <- DHIS2_Folders(isControl = T)
  rawC <- fread(
    sprintf(
      "%s/%s ANC Green File.csv",
      FOLDERS$FOLDER_DATA,
      FOLDERS$CLINICAL_OR_CONTROL
    ),
    encoding = "UTF-8"
  )
  setnames(rawC, 2, "uniqueid")
  setnames(rawC, "Event date","bookdate")
  setnames(rawC, "Event", "bookevent")
  rawC[,ident_dhis2_control:=TRUE]
  
  FOLDERS <- DHIS2_Folders(isControl = F)
  rawI <- fread(
    sprintf(
      "%s/%s Booking Visit.csv",
      FOLDERS$FOLDER_DATA,
      FOLDERS$CLINICAL_OR_CONTROL
    ),
    encoding = "UTF-8"
  )
  setnames(rawI, 2, "uniqueid")
  setnames(rawI, "Event date","bookdate")
  setnames(rawI, "Event", "bookevent")
  rawI[,ident_dhis2_control:=TRUE]
  
  # now we have a "minimum viable dataset" for the raw data
  raw <- rbind(rawC[,c("uniqueid","bookevent","bookdate","ident_dhis2_control")],
               rawI[,c("uniqueid","bookevent","bookdate","ident_dhis2_control")])
  # now just do a little bit of cleaning so that it can be graphed
  raw[,bookyearmonth:=YearMonth(bookdate)]
  raw[,bookdate:=NULL]
  
  raw[,type:="Raw"]
  d[,type:="Clean"]
  
  # create a joint dataset, only using the variable names that
  # exist in "raw"
  plotData <- rbind(
    raw,
    d[,names(raw),with=F]
  )
  

  
  # to make graphs
  
  toPlot <- plotData[,.(numBookings=.N),
                      by=.(bookyearmonth,type)]
  # specify that "raw" should be first, not "clean"
  toPlot[,type:=factor(type,levels=c("Raw","Clean"))]
  
  p <- ggplot(data=toPlot[bookyearmonth>="2017-01"],
              mapping=aes(x=bookyearmonth,y=numBookings,fill=type))
  p <- p + geom_bar(stat="identity",position="dodge",width=0.7,colour="white")
  p <- p + scale_x_discrete("Booking Month")
  p <- p + scale_y_continuous("Number of bookings")
  p <- p + scale_fill_brewer("Data",palette="Dark2")
  p <- p + labs(title="Comparing number of bookings in raw versus clean data")
  p <- p + labs(caption=GraphCaption())
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "compare_raw_to_clean_bookings.png"),
         plot=p,
         height=210,
         width=297,
         units="mm")
}


IndicatorsOsloDemographics <- function(d){
  
  
  toAnalyse <- copy(d[ident_TRIAL_1==TRUE])
  toAnalyse[,bookorgdistrict:="Palestine"]
  toAnalyse[,bookorgdistricthashed:="Palestine"]
  toAnalyse <- rbind(toAnalyse,d[ident_TRIAL_1==TRUE])
  
  # we will just switch this one over
  toAnalyse[,aggregationVariable:=bookorgdistricthashed]
  # this is how to run a t-test
  # t.test(OUTCOME ~ GROUPINGVARIABLE, data=DATA)
  # res <- t.test(education~ident_dhis2_control,data=toAnalyse)
  
  ### YEARS OF EDUCATION
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(education~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable
  ]
  pvalueInfo[,pvalue:=sprintf("p=%s",formatC(pvalue,digits=2,format="f"))]
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable))
  p <- p + geom_boxplot(mapping=aes(y=education,fill=ident_dhis2_control))
  p <- p + geom_text(data=pvalueInfo,mapping=aes(y=0,label=pvalue))
  p <- p + scale_fill_brewer("Trial Arm",palette="Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Education differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "education_years.png"),
    plot=p)
  
  ### AGE
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(age~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=age))
  p <- p+ geom_boxplot(mapping = aes(y=age, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "age_years.png"),
    plot=p)
  
  
  ### AGE at marrige
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agemarriage~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agemarriage))
  p <- p+ geom_boxplot(mapping = aes(y=agemarriage, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at marriage differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "agemarriage_years.png"),
    plot=p)
  
  
  ### AGE at first pregnancy
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(agepregnancy~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=agepregnancy))
  p <- p+ geom_boxplot(mapping = aes(y=agepregnancy, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=10,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p + labs(title="Age at first pregnancy differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "agepregnancy_years.png"),
    plot=p)
  
  #income
  pvalueInfo <- toAnalyse[,.(
    pvalue=t.test(income~ident_dhis2_control)$p.value
  ),
  by=aggregationVariable]
  
  pvalueInfo[,pvalue:=sprintf("p=%s", formatC(pvalue,digits = 2,format = "f"))]
  
  
  p <- ggplot(toAnalyse,aes(x=aggregationVariable, y=income))
  p <- p+ geom_boxplot(mapping = aes(y=income, fill=ident_dhis2_control))
  p <- p+ geom_text(data=pvalueInfo, mapping=aes(y=-7000,label=pvalue))
  p <- p+ scale_fill_brewer('Trial arm',palette = "Dark2")
  p <- p+scale_x_discrete("District")
  p <- p+scale_y_continuous("Income")
  p <- p + labs(title="Income differences in control vs intervention")
  p <- p + labs(caption=GraphCaption())
  
  p
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "mahima",
    "trial_1",
    "income_continuous.png"),
    plot=p)
  
}

Analyse_Medical_History_Trial_1 <- function(d){
  xtabs(~d$bookhistgdm,addNA=T)
  
  xtabs(~d$bookhistdm,addNA = T)
  
  xtabs(~d$bookhisthtn,addNA=T)
  
  xtabs(~d$bookhistperi, addNA=T)
  
  xtabs(~d$bookhistcs, addNA=T)
  
  sum(d$ident_TRIAL_1==1,na.rm=T)
  d[,rowid:=1:.N]
  #tamara keep on adding new variables below
  temp <- d[ident_TRIAL_1==TRUE,c(
    "rowid","ident_dhis2_control",
    "bookhistgdm",
    "bookhistdm",
    "bookhisthtn"
  )]
  
  long <- melt.data.table(
    temp,
    id.vars = c("rowid","ident_dhis2_control"))
  
  tab <- long[,.(
    has_var=sum(value, na.rm=T),
    notmissing_var=sum(!is.na(value)),
    missing_var=sum(is.na(value))
  ),by=.(variable,ident_dhis2_control)]
  tab
  
  # this is the harder way
  tab <- d[ident_TRIAL_1==TRUE,.(
    has_bookhistgdm=sum(bookhistgdm, na.rm=T),
    notmissing_bookhistgdm=sum(!is.na(bookhistgdm)),
    missing_bookhistgdm=sum(is.na(bookhistgdm)),
    
    has_bookhistdm=sum(bookhistdm, na.rm=T),
    notmissing_bookhistdm=sum(!is.na(bookhistdm)),
    missing_bookhistdm=sum(is.na(bookhistdm)),
    
    
    has_bookhisthtn=sum(bookhisthtn, na.rm = T),
    notmissing_bookhisthtn=sum(!is.na(bookhisthtn)),
    missing_bookhisthtn=sum(is.na(bookhisthtn)),
    
    has_bookhistperi=sum(bookhistperi, na.rm=T),
    notmissing_bookhistperi=sum(!is.na(bookhistperi)),
    missing_bookhistperi=sum(is.na(bookhistperi)),
    
    has_bookhistcs=sum(bookhistcs, na.rm=T),
    notmissing_bookhistcs=sum(!is.na(bookhistcs)),
    missing_bookhistcs=sum(is.na(bookhistcs))
    
    
  ),
  by=.(ident_dhis2_control)]
  
  tab
  
  
}

Analyse_Mahima_Trial_1 <- function(d){
  Analyse_BookingRawVsClean(d[ident_dhis2_booking==1])
  Analyse_EnteredVsCalculated(d)
  Analyse_BookingDescriptives(d[ident_dhis2_booking==1])
  IndicatorsOsloDemographics(d)
  #Analyse_Medical_History_Trial_1(d)
  warning("Analyse_Medical_History_Trial_1(d)")
  
}

