BASE_LINE_STATISTICAL_ANALYSIS <- function(d){

  bookings <- d[,
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  
  print(bookings)
  bookings[,cohort:="Bookings"]
  print(bookings)

  
  Trial1 <- d[ident_TRIAL_1==TRUE,
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  Trial1[,cohort:="Trial 1"]
  print(Trial1)    
  
  EPD <- d[ident_TRIAL_1==TRUE & 
         isExpectedToHaveDelivered==TRUE,
              .(
                numWomen=.N
              ),
              by=.(
                ident_dhis2_control
              )]
  EPD[,cohort:="Expected to be delivered"]
  print(EPD)    
  
  
  MATCH <- d[ident_TRIAL_1==TRUE & 
             isExpectedToHaveDelivered==TRUE &
               !is.na(ident_avic_any),
           .(
             numWomen=.N
           ),
           by=.(
             ident_dhis2_control
           )]
  MATCH[,cohort:="Matched with Avicenna"]
  print(MATCH)    
  
  
  plotData <- rbind(bookings,
                    Trial1,
                    EPD,
                    MATCH)
  
  print(plotData)
  
  # turn cohort into a factor, with levels in the right order
  plotData[,cohort:=factor(cohort,
                           levels=c(
                             "Bookings",
                             "Trial 1",
                             "Expected to be delivered",
                             "Matched with Avicenna"
                           ))]
  
  plotData[,intOrControl:="Intervention"]
  plotData[ident_dhis2_control==TRUE,intOrControl:="Control"]
  
  print(plotData)
  
  setorder(plotData,cohort,-intOrControl)
  plotData[,cumulative_y:=cumsum(numWomen)-numWomen/2,by=cohort]
  
p <- ggplot(plotData)
p <- p + geom_bar(mapping=aes(x=cohort,y=numWomen,fill=intOrControl),stat="identity",colour="black")
p <- p + geom_text(mapping=aes(x=cohort,y=cumulative_y,label=numWomen))
p <- p + theme_gray(base_size = 16)
p <- p + scale_fill_brewer("",palette="BuPu")
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Number of women in 2017")
p <- p + labs(title="Trial 1")
p <- p + labs(caption=sprintf("Date of data extraction: %s",CLINIC_CONTROL_DATE))
p
ggsave(filename=file.path(FOLDER_DROPBOX_RESULTS,"baselinecohort.png"),
       plot=p,
       height=210,
       width=297,
       units="mm")
  
  
  
  
}
