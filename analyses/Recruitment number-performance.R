
setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_pniphabstracts2018", list.files("r_pniphabstracts2018", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_trial2", list.files("r_trial2", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

###### SETUP ENDS ######

#d <- LoadDataFileFromNetworkGaza()
d <- LoadDataFileFromNetworkWB()

td <- d[ident_dhis2_booking==1 &
        ident_dhis2_control==F &
        bookyear%in% c(2016:2019)]
bookingint <- td[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
), keyby=.(
   bookorgname,
  # bookorgunit,
 # str_TRIAL_2_Cluster,
  bookyear
)]

bookingint
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
#value.var: you have your rows, your columns, and the variable you are shifting: which is this
bookingint <- dcast(bookingint, bookorgname ~ bookyear, value.var="numBOOK")  

openxlsx::write.xlsx(bookingint, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "booking_descriptives",
                       "recriut.xlsx"))








td2 <- d[ident_dhis2_booking==1 &
          ident_dhis2_control==F &
          bookyearmonth %in% c(2018-01:2018-12)]
bookingint2 <- td2[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
), keyby=.(
  bookorgname,
  # bookorgunit,
  # str_TRIAL_2_Cluster,
  bookyearmonth
)]

bookingint2
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
#value.var: you have your rows, your columns, and the variable you are shifting: which is this
bookingint2 <- dcast(bookingint2, bookorgname ~ bookyear, value.var="numBOOK")  

openxlsx::write.xlsx(bookingint2, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "booking_descriptives",
                       "recriut2.xlsx"))
