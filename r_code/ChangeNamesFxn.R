#making a function for changing names
#no quotes they are variables
#if we have quotes they will be a value that cant be changed via fxn
NamesToChange <- function(d,
                          badname,
                          goodname){
  for(i in seq_along(badname)){
    if(badname[i] %in% names(d)) setnames(d,badname[i],goodname)
  }
  if(!goodname %in% names(d)){
    print(names(d))
    stop(sprintf("cant find %s", goodname))
  }
  
}


