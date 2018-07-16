SaveFullFileToNetwork <- function(d){
  # this saves the file to the network
  print("SAVING FILES TO NETWORK")
  saveRDS(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
  fwrite(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.csv"))
  #openxlsx::write.xlsx(d,file.path(FOLDER_DATA_CLEAN,"full_data_from_r.xlsx"))
  print("FINISHED SAVING FILES TO NETWORK")
}

SaveAnonymousOslo <- function(d){
  
  ## here we delete the sensitive variables
  d[,trackedentity:=NULL]
  d[,dummy:=NULL]
  d[,idtype:=NULL]
  d[,motheridno:=NULL]
  d[,firstname:=NULL]
  d[,datecreated:=NULL]
  d[,fathersname:=NULL]
  d[,middlename:=NULL]
  d[,familyname1:=NULL]
  d[,familyname2:=NULL]
  d[,husbandsname:=NULL]
  d[,street:=NULL]
  d[,village:=NULL]
  d[,city:=NULL]
  d[,camp:=NULL]
  d[,mobile:=NULL]
  d[,phone:=NULL]
  d[,email:=NULL]
  d[,cosang:=NULL]
  d[,dob:=NULL]
  d[,income:=NULL]
  d[,education:=NULL]
  d[,agemarriage:=NULL]
  d[,agepregnancy:=NULL]
  d[,members:=NULL]
  d[,age:=NULL]
  
  # this is what i do if i know the entire name of the variable
  d[,bookdate:=NULL]
  d[,booklong:=NULL]
  d[,booklat:=NULL]
  d[,bookorgname:=NULL]
  d[,bookorgcode:=NULL]
  d[,bookorgunit:=NULL]
  d[,bookidnumber:=NULL]
  d[,demoorgname:=NULL]
  d[,demoorgunit:=NULL]
  
  # this is what i do if i know part of the variable name
  d[,names(d)[stringr::str_detect(names(d),"latitude")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"organisationunit")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"womanfirst")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"womanfamily")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"address")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"dataextractor")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"alternateidentificationnum")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"firstname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"fathersname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandsfamilyname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"husbandsname")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"middlename")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"village")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"city")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"dateofbirth")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"mobile")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"education")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"ageatmarriage")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"ageatfirstpreg")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"monthlyhouseholdincome")]:=NULL]
  d[,names(d)[stringr::str_detect(names(d),"numberofmembers")]:=NULL]
  
  badNames <- names(d)[stringr::str_detect(names(d),"name")]
  goodNames <- badNames[stringr::str_detect(badNames,"labtestname")]
  badNames <- badNames[!badNames %in% goodNames]
  d[,(badNames):=NULL]
  
  d[,booklmp:=NULL]
  d[,bookdatelastbirth:=NULL]
  d[,dateupdated:=NULL]
  d[,expecteddateofdelivery:=NULL]
  d[,calc_expected_due_delivery:=NULL]
  d[,avgincome:=NULL]
  
  # this saves the file to the dropbox
  print("SAVING FILES TO DROPBOX")
  saveRDS(d[ident_dhis2_booking==1],"~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_data_from_r.rds")
  fwrite(d[ident_dhis2_booking==1],"~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Data/anon_data_from_r.csv")
  print("FINISHED SAVING FILES TO DROPBOX")
  
  print("REMOVING D FROM MEMORY AS IT IS NOW DESTROYED BY ANONYMIZING")
  # delete the dataset "d" to make space in the memory
  rm("d", envir=.GlobalEnv)
  
  # garbage cleaning
  gc()
}

LoadDataFileFromNetwork <- function(d){
  print("RELOADING DATASET FROM NETWORK DRIVE")
  d <- readRDS(file.path(FOLDER_DATA_CLEAN,"full_data_from_r.rds"))
  print("FINISHED RELOADING DATASET FROM NETWORK DRIVE")
  
  return(d)
}

SaveAllDataFiles <- function(d){
  
  # this is a bit weird, ignore it for the moment
  lists <- c()
  for(i in 1:ncol(d)) if(is.list(d[[i]])) lists <- c(lists,i)
  for(i in lists){
    n <- names(d)[i]
    d[,(n):=as.character(get(n))]
  }
  
  # start saving our files
  SaveFullFileToNetwork(d)
  SaveAnonymousOslo(d)
  

}