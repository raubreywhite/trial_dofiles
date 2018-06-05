DHIS2_Remove_If_All_Cols_Empty <- function(d,isControl,continuousNumberLimit=5){
  if(isControl){
    n <- names(d)
    # identifying the variable after identifcationdocument number
    startPoint <- which(n=="organisationunit")+1
    
    # delete numberNotMissing if it exists
    d[,numberNotMissing:=NULL]
    # how many columns are in the dataset?
    endPoint <- ncol(d)
    
    # initialize number not missing to zero
    d[,numberNotMissing:=0]
    for(i in startPoint:endPoint){
      var <- n[i]
      if(var %in% c("bookevent","eventnum","numberNotMissing")) next
      
      # if continuous variable (i.e. lots of options)
      # say it is missing if variable==0
      if(length(unique(d[[i]]))>continuousNumberLimit){
        # this is a continuous variable, so we will count "0" as missing
        # identify if missing, then take NOT (!)
        d[!(
          is.na(get(var)) | 
            stringr::str_trim(get(var))=="" |
            get(var)==0
        ),numberNotMissing:=numberNotMissing+1]
      } else {
        # this is not a cnotinuous variable, so we will ignore 0
        d[!(
          is.na(get(var)) | 
            stringr::str_trim(get(var))==""
        ),numberNotMissing:=numberNotMissing+1]
      }
    }
    xtabs(~d$numberNotMissing)
    print(nrow(d))
    d <- d[numberNotMissing>0]
    print(nrow(d))
    d[,numberNotMissing:=NULL]
  } else {
    n <- names(d)
    # identifying the variable after identifcationdocument number
    startPoint <- which(n=="identificationdocumentnumber")+1
    
    # delete numberNotMissing if it exists
    d[,numberNotMissing:=NULL]
    # how many columns are in the dataset?
    endPoint <- ncol(d)
    
    # initialize number not missing to zero
    d[,numberNotMissing:=0]
    for(i in startPoint:endPoint){
      var <- n[i]
      if(var %in% c("bookevent","eventnum","numberNotMissing")) next
      
      # if continuous variable (i.e. lots of options)
      # say it is missing if variable==0
      if(length(unique(d[[i]]))>5){
        # this is a continuous variable, so we will count "0" as missing
        # identify if missing, then take NOT (!)
        d[!(
          is.na(get(var)) | 
          stringr::str_trim(get(var))=="" |
          get(var)==0
            ),numberNotMissing:=numberNotMissing+1]
      } else {
        # this is not a cnotinuous variable, so we will ignore 0
        d[!(
          is.na(get(var)) | 
            stringr::str_trim(get(var))==""
        ),numberNotMissing:=numberNotMissing+1]
      }
    }
    xtabs(~d$numberNotMissing)
    print(nrow(d))
    d <- d[numberNotMissing>0]
    print(nrow(d))
    d[,numberNotMissing:=NULL]
  }
  return(d)
}