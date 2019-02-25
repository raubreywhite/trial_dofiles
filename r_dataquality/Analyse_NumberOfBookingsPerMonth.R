
Analyse_NumberOfBookingsPerMonth <- function(d,location="pal"){
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
  
}


