nrow(d[bookyear=="2014"])
nrow(d[bookyear=="2015"& ident_dhis2_control==T])
nrow(d[bookyear=="2016" & ident_dhis2_control==T])
nrow(d[bookyear=="2017" & ident_dhis2_control==T])
nrow(d[bookdate>="2017-01-15" & bookdate<="2017-09-15" &
       ident_dhis2_control==T & ident_TRIAL_1_clinics==T])

