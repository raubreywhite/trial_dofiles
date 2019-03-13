Bookdate_vs_booklmp <- function(d,location="wb"){
  x <- GetFoldersAndData(d=d,location=location)
  pd <- x$pd
  folders <- x$folders
  
  # # looking at some people who have wrong dates
  # uglytable <- pd[bookdate < booklmp & ident_dhis2_booking==T,c(
  #   "ident_dhis2_control",
  #   "motheridno",
  #   "bookevent",
  #   "bookdate",
  #   "booklmp",
  #   "matching"
  # )]
  
  uglytable <- pd[
    ident_dhis2_booking==T & 
    !is.na(bookdate) &
    !is.na(booklmp),
    .(
        N=.N
      ),keyby=.(
        ident_TRIAL_1,
        ident_dhis2_control,
        bookdate_less_booklmp=bookdate<booklmp
      )]
  uglytable <- uglytable[!is.na(ident_TRIAL_1)]
  uglytable[,denom:=sum(N),by=.(ident_TRIAL_1,ident_dhis2_control)]
  uglytable <- uglytable[bookdate_less_booklmp==T]
  uglytable[,perc_bookdate_less_booklmp:=round(100*N/denom,1)]
  uglytable <- uglytable[,c(
    "ident_TRIAL_1",
    "ident_dhis2_control",
    "perc_bookdate_less_booklmp",
    "denom"
  )]
  
  openxlsx::write.xlsx(
    uglytable,
    file.path(
      folders$dropbox,
      "data_quality",
      "bookdate_less_than_booklmp.xlsx"
      )
  )
}