nrow(d[bookyear=="2014"])
nrow(d[bookyear=="2015"& ident_dhis2_control==T])
nrow(d[bookyear=="2016" & ident_dhis2_control==T])
nrow(d[bookyear=="2017" & ident_dhis2_control==T])
nrow(d[bookdate>="2017-01-15" & bookdate<="2017-09-15" &
       ident_dhis2_control==T & ident_TRIAL_1_clinics==T])

nrow(d[bookyear=="2019"& ident_dhis2_control==F & ident_dhis2_booking==T])
nrow(d[bookyear=="2019"& ident_dhis2_control==F & ident_hr_clinic==T & ident_dhis2_booking==T])


# number of booking in high risk clinics:
  
d[bookyear=="2019"& ident_dhis2_control==F & ident_dhis2_booking==T, c("motheridno")]

bookhighrisk <- d[bookyear=="2019"& ident_dhis2_control==F & ident_hr_clinic==T
                  
                  , c("motheridno","bookorgname")]

openxlsx::write.xlsx(bookhighrisk, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "data_quality",
                       "bookhighrisk.xlsx"))



d[motheridno==401940218 ,c(bookorgname)]

