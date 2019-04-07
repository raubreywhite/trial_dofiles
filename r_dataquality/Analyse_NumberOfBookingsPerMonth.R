
Analyse_NumberOfBookingsPerMonth <- function(d,location="pal"){
  # 'd' is all of palestine
  # 'pd' is pal/wb/gaza depending on what we ask for
  # this is to save time, so we dont have to load
  # data in over and again
  x <- GetFoldersAndData(d=d,location=location)
  pd <- x$pd
  folders <- x$folders
  
  #stringr::str_subset(names(pd), "^ident")
  
  # here we do a row selection, followed by "count everyone"
  uglytable <- pd[ident_dhis2_booking==T & 
                    ident_dhis2_control==F & 
                    bookdate>="2017-01-15",
                  .(
                    N=.N
                  ),
                  keyby=.(
                    ident_gaza,
                    bookyearmonth
                  )]
  
  uglytable[,loc:="Gaza"]
  uglytable[ident_gaza==F,loc:="West Bank"]
  
  p <- ggplot(uglytable,aes(x=bookyearmonth,y=N,
                            fill=loc))
  
  #geom col vs geom point : points for points and col for rectagular shapes
  # color black for borders not inside aes, it will think it is a variable
  p <- p + geom_col(colour="black")
  #to make the text bigger
  p <- p + theme_gray(base_size = 16)
  #label for caption
  p <- p + labs(caption=GraphCaption())
  ##label for title
  p <- p + labs(title="XXX")
  # tolabel x and y
  p <- p + scale_x_discrete("Booking year-month")
  p <- p + scale_y_continuous("Number of bookings")
  # to make fill section nicer
  p <- p + scale_fill_brewer("",palette="Set1")
  # to make x axis is vertical for names
  p <- p + theme(axis.text.x = element_text(angle = 90,
                                            hjust = 1,
                                            vjust = 0.5))
  # this code to make the whole picture vertical(to flip it)
  #p <- p + coord_flip()
  
  p
  
  ggsave(filename=file.path(
    folders$dropbox,
    "data_quality",
    "number_of_bookings_per_month.png"),
    # physical size for A4 paper
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  # compare number of people within each bookorgname by month
  # here we do a row selection, followed by "count everyone"
  
  uglytable <- pd[ident_dhis2_booking==T & 
                    ident_dhis2_control==F & 
                    bookdate>="2017-01-01",
                  .(
                    N=.N
                  ),
                  keyby=.(
                    bookorgdistrict,
                    ident_gaza,
                    bookyearmonth,
                    bookorgname,
                    ident_hr_clinic
                  )]
  
  uglytable[ident_gaza==T,loc:="Gaza"]
  uglytable[ident_gaza==F,loc:="West Bank"]
  setcolorder(uglytable,"loc")
  
  #melt.data.table is for wide -> long
  #dcast.data.table is for long -> wide
  results <- dcast.data.table(
    uglytable, 
    loc+bookorgdistrict+ident_hr_clinic+bookorgname~bookyearmonth, 
    value.var = "N",
    fill=0)
  results
  
  # delete the useless months that we dont want
  results[,`2017-01`:=NULL]
  results[,`2017-02`:=NULL]
  results[,`2017-03`:=NULL]
  results[,`2017-04`:=NULL]
  results[,`2017-05`:=NULL]
  results[,`2017-06`:=NULL]
  results[,`2017-07`:=NULL]
  results[,`2017-08`:=NULL]
  results[,`2017-09`:=NULL]
  results[,`2017-10`:=NULL]
  results[,`2017-11`:=NULL]
  results[,`2017-12`:=NULL]
  results[,`2018-01`:=NULL]
  results[,`2018-02`:=NULL]
  results[,`2018-03`:=NULL]
  results[,`2018-04`:=NULL]
  results[,`2018-05`:=NULL]
  results[,`2018-06`:=NULL]
  results[,`2018-07`:=NULL]
  results[,`2018-08`:=NULL]
  results[,`2018-09`:=NULL]
  
  openxlsx::write.xlsx(results,
                       file.path(
                         folders$dropbox,
                         "data_quality",
                         "number_of_bookings_per_month_restricted.xlsx"))
  
  #pdlong <- LoadDataLongFromNetworkPal()
  
  
  
}


