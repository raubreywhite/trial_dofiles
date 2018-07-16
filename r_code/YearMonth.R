# this is a good function that ONLY works
# when var is a SINGLE variable
# if var is a vector, the "if" function WILL TOTALLY BREAK
YearMonth.int <- function(var){
  
  if(is.na(var)) return(NA)
  
  returnValue <- sprintf("%s-%s",
                       lubridate::year(var),
                       formatC(lubridate::month(var),width=2,flag="0"))
  # this way just cuts off the first 7 characters
  # this way is a bit unsafe, because if "var" is not 
  # in the format yyyy-mm-dd, then its going to just
  # take the first 7 charaters, no matter what, and this
  # can be totally wrong
  #returnValue <- stringr::str_sub(var,1,7)
  
  return(returnValue)
}

# this ensures that YearMonth.int ONLY
# receives ONE SINGLE VARIABLE at a time
YearMonth <- Vectorize(YearMonth.int)