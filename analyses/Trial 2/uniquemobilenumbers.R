nrow(d)
nrow(d[(ident_dhis2_control==F &
        ident_dhis2_booking==T)])


as.numeric(d$mobile)
uniquemoblienumbers <-unique(d[ident_dhis2_control==F &
                                 ident_dhis2_booking==T,
                               c("bookyearmonth",
                                 "bookorgdistrict",
                                 "mobile")])

openxlsx::write.xlsx(uniquemoblienumbers,file.path(FOLDER_DATA_RESULTS,
                                                   "satisfaction",
                                                   "uniquemobilenumbers_districts.xlsx"))
                                
#Feb 3, 2019
#Manual check for 050 numbers is 175
#Manual check for 052 numbers is 466
#Manual check for 053 numbers is: 171
#Manual check for 054 numbers is: 126
#Manual check for 055 numbers is: 40
#Manual check for 058 numbers is: 148

#these total to: 1126 different out of 32323





d[,mobile_number_israeli:=FALSE]
 
 vars<- uniquemoblienumbers[stringr::str_detect(uniquemoblienumbers, "^052")]
 # for(i in vars){
 #   
 #   
 #   
 # }
