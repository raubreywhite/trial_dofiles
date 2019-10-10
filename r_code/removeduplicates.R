

Removeduplicate <- function(d,tag,isControl,maxObsPerWomanDate=1){
  if(tag=="demobook"){
    d[,eventdate:=bookdate]
  }
  
  names(d)
 d[,n:=.N, by=.(uniqueid,eventdate)] 
  xtabs(~d$n)
  d[,numMissing:=0]

  n=names(d)
  for (i in n){
    if(i %in% c("n", "minNumMissing", "isDuplicate", "numMissing")){
      next
      # next means jump up to line 31, that means dont do the codes for 
    }
  # i is not a variable in this data set, we need tolook at the value of i
    #d[is.na(ancvaginalbleeding), numMissing:=numMissing+1]
    d[is.na(get(i)), numMissing:=numMissing+1]
  }
  
  xtabs(~d$numMissing)
  d[,minNumMissing:=min(numMissing), by=.(uniqueid,eventdate)]
  d[,isDuplicate:=F] 
  d[numMissing!=minNumMissing,isDuplicate:=T]
  xtabs(~d$isDuplicate)

  # getting rid of rows with lots of zereos
  d[isDuplicate==F,n:=.N, by=.(uniqueid,eventdate)] 

  d[,numMissing:=0]
  n=names(d)
  for (i in n){
    print(i)
    if(i %in% c("n", "minNumMissing", "isDuplicate", "numMissing")){
      next
      # next means jump up to line 31, that means dont do the codes for 
    }
    # i is not a variable in this data set, we need tolook at the value of i
    #d[is.na(ancvaginalbleeding), numMissing:=numMissing+1]
    
    d[isDuplicate==F & get(i)==0, numMissing:=numMissing+1]

  }
  
  
  d[isDuplicate==F,minNumMissing:=min(numMissing), by=.(uniqueid,eventdate)] 

  d[isDuplicate==F & numMissing!=minNumMissing,isDuplicate:=T]
  xtabs(~d$isDuplicate)
  if(!is.null(maxObsPerWomanDate)){
    #now we are trying to see if there are still any duplicates
    #if there are still duplicates, take the first maxObsPerWomanDate obs
    #1:.N THAT MEANS 1 UNTIL THE TOTAL NUMBER OF ROWS THAT WE HAVE FOR EVERY WOMAN-DATE
    d[isDuplicate==F,rownum:=1:.N, by=.(uniqueid,eventdate)] 
    d[isDuplicate==F & rownum>maxObsPerWomanDate,isDuplicate:=T]
  }
  xtabs(~d$isDuplicate)
  
  tab <- d[,.(
    totalnum=.N,
    duplicatednum=sum(isDuplicate==TRUE)
  ),by=.()]
  
  tab[,xtag:=tag]
  tab[,xisControl:=isControl]
  
  saveRDS(tab,file = file.path(FOLDER_DATA_RAW,
                               "possible_duplicates", 
                               sprintf("autodup_%s_%s.RDS", tag, isControl)))
 d <- d[isDuplicate==F] 
 d[,n:=NULL ]
 d[,minNumMissing:=NULL ]
 d[,isDuplicate:=NULL ]
 d[,numMissing:=NULL ]
 if(tag=="demobook"){
   d[,eventdate:=NULL]
 }
 
 return(d)
}






  