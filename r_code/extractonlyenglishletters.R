ExtractOnlyEnglishLetters <- function(var){
  lapply(stringr::str_extract_all(stringr::str_to_lower(var),"[a-z]"),paste0,collapse="")
}

ExtractOnlyEnglishLettersAndNumbers <- function(var){
  lapply(stringr::str_extract_all(stringr::str_to_lower(var),"[a-z0-9]"),paste0,collapse="")
}

GetRidOfEnglish <- function(var){
  unlist(lapply(stringr::str_replace_all(stringr::str_to_lower(var),"[a-z0-9 \\\\\\.]",""),paste0,collapse=""))
}


