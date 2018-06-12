HBO_Completeness <-function(d){

  nrow(d)

  sum(d$ident_TRIAL_1==TRUE,na.rm=T)
  
  sum(d$ident_TRIAL_1==TRUE &
      d$isExpectedToHaveDelivered==TRUE,
      na.rm=T)
  
  sum(d$ident_TRIAL_1==TRUE &
        d$isExpectedToHaveDelivered==TRUE &
        is.na(d$ident_avic_any),
        na.rm=T)
  
  sum(d$ident_TRIAL_1==TRUE &
        d$ident_dhis2_control==TRUE
      ,na.rm=T)
  sum(d$ident_TRIAL_1==TRUE &
        d$ident_dhis2_control==FALSE
      ,na.rm=T)
  
  x <- c("hello","my","name","is")
  stringr::str_detect(x, "m")
  x[stringr::str_detect(x, "m")]
  
  vars <- c(
    "motheridno",
    "bookorgdistrict",
    "bookorgname",
    "bookdate",
    "firstname",
    "fathersname",
    "middlename",
    "familyname1",
    "familyname2",
    "husbandsname",
    "dob",
    "booklmp",
    names(d)[stringr::str_detect(names(d),"hbo")]
  )
  
  
 vars <- vars[!stringr::str_detect(vars,"latitude")] 
 vars <- vars[!stringr::str_detect(vars,"numberofmembersinhousehold")] 
 vars <- vars[!stringr::str_detect(vars,"longitude")] 
 vars <- vars[!stringr::str_detect(vars,"organisationunitcode")] 
 vars <- vars[!stringr::str_detect(vars,"monthlyhouseholdincomeils")]
 vars <- vars[!stringr::str_detect(vars,"ageatmarriage")]
 vars <- vars[!stringr::str_detect(vars,"educationinyears")]
 vars <- vars[!stringr::str_detect(vars,"created")]
 vars <- vars[!stringr::str_detect(vars,"mobilenumber")]
 vars <- vars[!stringr::str_detect(vars,"lastupdated")]
 vars <- vars[!stringr::str_detect(vars,"trackedentity")]
 vars <- vars[!stringr::str_detect(vars,"inactive")]
 
 vars
 

    tokeep <- d[
      isExpectedToHaveDelivered==TRUE &
      ident_TRIAL_1==TRUE &
      is.na(d$ident_avic_any),
      vars,
      with=FALSE]
    
   setorder(tokeep,bookorgname,bookdate)
   # data.frame sort would probably look something like this
   # code probably isnt 100% corret here
   #tokeep <- tokeep[order(bookorgname,bookdate),]
   
    openxlsx::write.xlsx(x=tokeep,
                         file=file.path(FOLDER_DATA_MBO,"HBO_Completeness.xlsx"))
    
    
    
    
    
    
}