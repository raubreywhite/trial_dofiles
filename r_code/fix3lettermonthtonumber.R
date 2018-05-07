Fix3LetterMonthToNumber <- function(var){
  var <- stringr::str_replace_all(var,"JAN","01")
  var <- stringr::str_replace_all(var,"FEB","02")
  var <- stringr::str_replace_all(var,"MAR","03")
  var <- stringr::str_replace_all(var,"APR","04")
  var <- stringr::str_replace_all(var,"MAY","05")
  var <- stringr::str_replace_all(var,"JUN","06")
  var <- stringr::str_replace_all(var,"JUL","07")
  var <- stringr::str_replace_all(var,"AUG","08")
  var <- stringr::str_replace_all(var,"SEP","09")
  var <- stringr::str_replace_all(var,"OCT","10")
  var <- stringr::str_replace_all(var,"NOV","11")
  var <- stringr::str_replace_all(var,"DEC","12")
  
  return(var)
}

Fix2DigitYear <- function(var){
  var <- stringr::str_replace_all(var,"-([0-9][0-9])$","-20\\1")
  var <- stringr::str_replace_all(var,"-([0-9][0-9]) ","-20\\1 ")
  var <- stringr::str_replace_all(var,"^([0-9][0-9])-([0-9][0-9])-([0-9][0-9][0-9][0-9])","\\3-\\2-\\1")
  var <- unlist(stringr::str_extract_all(var,"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
  return(var)
}

