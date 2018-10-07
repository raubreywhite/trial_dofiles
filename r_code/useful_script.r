#Reshaping the data set
setcolorder2<- function(dx,var){
  
  # get all the names from dx
  # (this doesn't "do" anything, it just gets the names)
  # we will use this later
  n <- names(dx)
  
  # if there are vars that do not exist in n (names(dx))
  # then you have a problem
  if(sum(!var %in% n)>0){
    stop(paste0(
      var[!var %in% n],
      " not in dataset"))
  }
  # put the four names we care about at the front
  # (but now we have duplicates!)
  n <- c(var, n)
  # get rid of the duplicates
  n <- unique(n)
  
  # tells dx "PUT THE COLUMNS IN THIS ORDER ('n')
  # (here we actually "do something")
  # this is pass by reference
  setcolorder(dx,n)
  
  
  
  
  
}