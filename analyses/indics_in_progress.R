

####### Indicators #######

# run data processing first, then anemia, attendance, and gdm scripts


# WHO indicators:
# ANC coverage, skilled attendant at birth, ppc 2 days with in birth
# exclusive breastfeeding, antiobiotic treatment for suspected pneumonia

# Health indicators: diabetes, htn??,anemia
#
# anemia and attendance- run regular script
# 

### bookings and anc ####

indicators <- d

indicators[is.na(bookdate) & 
             ident_dhis2_ppc==T,
                     bookdate:=(ppcdate_1)]

xtabs(~indicators[is.na(bookdate) & ident_dhis2_ppc==T]$bookdate)

uglytab <- d[,.(
              numBookedTotal=sum(ident_dhis2_booking==T, na.rm=T),
              numBookedNoANC=sum(ident_dhis2_booking==T &
                                   is.na(ident_dhis2_an)),
              numBookedandANC=sum(ident_dhis2_booking==T &
                                    ident_dhis2_an==T, na.rm=T),
              numPPCTotal=sum(ident_dhis2_ppc==T, na.rm=T),
              numPPCandBooked=sum(ident_dhis2_ppc==T &
                      ident_dhis2_booking==T &
                       is.na(ident_dhis2_an), na.rm=T),
              numPPCandBookedAndANce=sum(ident_dhis2_booking==T & 
                             ident_dhis2_an==T & 
                             ident_dhis2_ppc==T, na.rm=T)),
             keyby=.(bookyear)]

openxlsx::write.xlsx(uglytab,file.path())


### PPC ### 
 ppc <- d[(ident_dhis2_booking==T & bookdate>="2017-01-01")|
            (ident_dhis2_ppc==T & ppcdate_1>="2017-01-01"),]
 
varppcdate <-  names(ppc)[stringr::str_detect(names(ppc),"^ppdate_")]
varoutcome <- sprintf(ppcdaysafterdelivcalc_%s,s)

for (i in varppcdate){
  
  ppc[!is.na(get(varppcdate))
      & !is.na(cpodate_1), (varoutcome):=difftime((varppcdate),
                                              cpodate_1, 
                                              units="days")]
  
  
  ppc[is.na(cpodate_1) & !is.na(nbcdateofdelivery_1), varoutcome]
  
  
}


# ANC, PPC, Both
ppcancboth <- ppc[,.(BookedANC=sum(ident_dhis2_booking==T, na.rm=T),
                     BookedandANC=sum(ident_dhis2_booking==T &
                                        ident_dhis2_an==T, na.rm=T),
                     allPPC=sum(ident_dhis2_ppc==T, na.rm = T),
                     PPCnoanc=sum(ident_dhis2_ppc==T & 
                                    ident_dhis2_booking==0, rm)),
                  keyby=.(bookyear)]
 
# come back for anc
