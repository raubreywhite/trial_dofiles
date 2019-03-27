Bookdate_before_booklmp <- function(d,location="wb"){
  
  x <- GetFoldersAndData(d=d,location=location)
  pd <- x$pd
  folders <- x$folders
  
  print("something happens")
  badpeople <- pd[
    ident_bad_bookdate_before_booklmp==T & 
    ident_TRIAL_1==T
    ]
  
  badpeople[,usgestage_lmp:=usdate_1-usgestage_1*7]
  badpeople[,usgestage_lmpfromedd:=usedd_1-40*7]
  
  setcolorder(badpeople,c(
    "bookorgname",
    "ident_dhis2_control",
    "bookdate",
    "booklmp",
    "booklmp_original",
    "firstname",
    "bookgestage",
    "usdate_1",
    "usgestage_1",
    "usgestage_lmp",
    "usgestage_lmpfromedd"
    ))
  
  openxlsx::write.xlsx(badpeople[,1:200], file=file.path(
    folders$results,
    "quality_control",
    "bookdate_before_booklmp.xlsx"
  ))
}
