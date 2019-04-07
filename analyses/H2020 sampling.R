#Total pregnancy registrations in primary healthcare clinics (Jan-Dec 2018)
#Total newborn registrations in primary healthcare clinics (Jan-Dec 2018)
#No. of referral (high-risk) clinics (2018)
#Total pregnancy registrations in referral clinics (Jan-Dec 2018)
#Total newborn registrations in referral clinics (Jan-Dec 2018)


xtabs(d$bookorgdistrict=="bethlahem")

#TO KNOW THE number of booking per district per clinic

xtabs(~d[ident_dhis2_control==F & bookorgdistrict=="Y"]$bookorgname)



d[bookyear==2018 &ident_dhis2_control==F,
  .(
    numclinics=length(unique(bookorgname)),

    numbookings=sum(ident_dhis2_booking)
  ),keyby=.(bookorgdistrict)]



d[lubridate::year(nbcdate_1)==2018 &ident_dhis2_control==F,
  .(
    numclinics=length(unique(bookorgname)),
    numnewborn=sum(ident_dhis2_nbc,na.rm = T)
  ),keyby=.(bookorgdistrict)]




# for High risk clinics
d[bookyear==2018 &ident_dhis2_control==F & ident_hr_clinic==T,
  .(
    numclinics=length(unique(bookorgname)),
    numbookings=sum(ident_dhis2_booking)
  ),keyby=.(bookorgdistrict)]





