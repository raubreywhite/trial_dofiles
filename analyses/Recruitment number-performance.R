

###### SETUP ENDS ######

#d <- LoadDataFileFromNetworkGaza()
d <- LoadDataFileFromNetworkWB()

td <- d[ident_dhis2_booking==1 & ident_phase1clinic==1 | ident_phase2clinic==1 | ident_phase3clinic==1 & bookyearmonth %in% c(2018-01:2018-06)]
bookingint <- td[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
), keyby=.(
  bookorgname,
  bookyearmonth
)]

bookingint
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
#value.var: you have your rows, your columns, and the variable you are shifting: which is this
bookingint <- dcast(bookingint, bookorgname ~ bookyearmonth, value.var="numBOOK")  

openxlsx::write.xlsx(bookingint, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "booking_descriptives",
                       "recutperform.xlsx"))
