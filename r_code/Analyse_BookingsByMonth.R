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
    "booking_descriptives",
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
    "booking_descriptives",
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
    "booking_descriptives",
    "meangestageintrial1bymonth.png"),
         plot=p2,
         height=210,
         width=297,
         units="mm")
  
  p3 <- gridExtra::grid.arrange(p1nocaption,p2)
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "booking_descriptives",
    "meangestageintrial1_together.png"),
         plot=p3,
         height=210,
         width=297,
         units="mm")
  
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
  ggsave(filename="~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results/booking_descriptives/compare_raw_to_clean_bookings.png",
         plot=p,
         height=210,
         width=297,
         units="mm")
  
  
  
  
  
  
}

