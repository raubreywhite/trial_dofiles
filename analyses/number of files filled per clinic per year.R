nrow(d[bookyear=="2014"])
nrow(d[bookyear=="2015"& ident_dhis2_control==T])
nrow(d[bookyear=="2016" & ident_dhis2_control==T])
nrow(d[bookyear=="2017" & ident_dhis2_control==T])
nrow(d[bookdate>="2017-01-01" & bookdate<="2017-09-30" &
       ident_dhis2_control==F & ident_TRIAL_1_clinics==T])

