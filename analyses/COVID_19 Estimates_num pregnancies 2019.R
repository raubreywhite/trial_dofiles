# pregnancies in 2019

nrow(d[ident_dhis2_booking==T & bookyear==2019])

nrow(d[booklmp_original>="2018-03-01" & booklmp_original<="2019-03-01"])



##### COVID-19 estimates #######

bookings <- d[bookyearmonth>="2020-01",
              c("ident_dhis2_booking",
                "ident_dhis2_an",
                "ident_dhis2_ppc",
                "bookyearmonth",
                "bookorgdistrict")]

uglytable <- bookings[!is.na(bookorgdistrict),.(
                        
                     BookedANC=sum(ident_dhis2_booking==T, na.rm=T),
                     numANCwhovisited=sum(ident_dhis2_an==T, na.rm=T),
                     numbookedPPC=sum(ident_dhis2_ppc==T, na.rm=T)),
                     keyby=.(bookorgdistrict,
                             bookyearmonth)]
