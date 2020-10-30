LoadDataLongFromNetworkPal <- function(){
  d <- LoadDataFileFromNetworkPal()

  # renaming variables that were identified 
  #as duplicates bcuz similar stubs at bottom
  
  setnames(d,"bookyear", "bookyearx")
  setnames(d,"bookbmicat","bookcatbmix")
  
  setnames(d,"bookorgdistricthashed","bookhashedorgdistrictx")
  setnames(d,"bookgestagedays","bookgAdays")
  setnames(d,"bookhistotherchronic","bookchronicotherhistx")
  setnames(d,"bookhistbloodspec","bookspecbloodhistx")
  setnames(d,"bookdatelastbirth","booklastbdayx")
  setnames(d,"bookallersevspec","booksevspecx")
  setnames(d,"bookallerdrugspec","bookdrugspecall")
  setnames(d,"bookhistcscompl","bookcomplhistcsx")
  setnames(d,"bookexambreastabn","bookbreastabnx")
  setnames(d,"bookexamlimbabn","booklimbabnex")
  setnames(d,"bookexamheadabn","bookheadabnexam")
  setnames(d,"bookexamheartabn","bookheartabnexam")
  setnames(d,"bookhisthtnsymp","booksymphtnhistx")
  setnames(d,"bookcounsfamhist","bookcounsfamxhist")
  setnames(d,"booklmpknown","bookknownlmpx")
  setnames(d,"bookgestagedays_cats","bookdaysgacatsx")
  setnames(d,"bookexamabdabn","bookabdabnexams")
  setnames(d,"bookexamlungabn","booklungabnexamx")
  setnames(d,"bookhistutesur","bookutesurhistx")
  
  d[,uniquepregid_1:=1:.N]
  d[,uniquepregid_2:=1:.N]
  d[,uniquepregid_3:=1:.N]
  d[,uniquepregid_4:=1:.N]
  d[,uniquepregid_5:=1:.N]
  d[,uniquepregid_6:=1:.N]
  d[,uniquepregid_7:=1:.N]
  d[,uniquepregid_8:=1:.N]
  d[,uniquepregid_9:=1:.N]
  d[,uniquepregid_10:=1:.N]
  d[,uniquepregid_11:=1:.N]
  
  n <- names(d)
  ident <- stringr::str_subset(n,"^ident")
  book <- stringr::str_subset(n,"^book")
  stubs <- stringr::str_subset(n,"_[0-9]+$")
  stubs <- stringr::str_replace_all(stubs,"[0-9]","")
  stubs <- unique(stubs)
  stubs <- stubs[!stringr::str_detect(stubs,"^ident")]
  stubs <- stubs[!stringr::str_detect(stubs,"^amd")]
  stubs <- stubs[!stringr::str_detect(stubs,"^acs")]
  stubs <- stubs[!stringr::str_detect(stubs,"^alab")]
  stubs <- stubs[!stringr::str_detect(stubs,"^aob")]
  stubs <- stubs[!stringr::str_detect(stubs,"^abb")]
  stubs <- stubs[!stringr::str_detect(stubs,"^paperhbo")]
  stubs <- c(book,stubs)
  variablenames <- unique(paste0("^",stubs))
  
  # longnames is the names of variable in long format
  longnames = c()
  # widenames is the names of variable in wide format
  widenames = list()
  for(i in seq_along(variablenames)){
    print(i)
    # use ExtractOnlyEnglishLetters to get rid of ^ and _
    new_wide <- stringr::str_subset(names(d),variablenames[i])
    if(length(new_wide)==0) next
    new_wide <- na.omit(new_wide[1:10])
    widenames[[length(widenames)+1]] <- new_wide
    longnames <- c(longnames, ExtractOnlyEnglishLetters(variablenames[i])[[1]])
  }
  
  long = melt(d[,c(ident,unlist(widenames)),with=F],
                 measure = widenames,
                 value.name = longnames)
  
  # renaming possible duplicates to original
  setnames(long,"bookyearx","bookyear")
  
  # add others here
  setnames(long,"bookcatbmix","bookbmicat")
  setnames(long,"bookhashedorgdistrictx","bookorgdistricthashed")
  setnames(long,"bookgadays","bookgestagedays")
  setnames(long,"bookchronicotherhistx","bookhistotherchronic")
  setnames(long,"bookspecbloodhistx","bookhistbloodspec")
  setnames(long,"booklastbdayx","bookdatelastbirth")
  setnames(long,"booksevspecx","bookallersevspec")
  setnames(long,"bookdrugspecall","bookallerdrugspec")
  setnames(long,"bookcomplhistcsx","bookhistcscompl")
  setnames(long,"bookbreastabnx","bookexambreastabn")
  setnames(long,"booklimbabnex","bookexamlimbabn")
  setnames(long,"bookheadabnexam","bookexamheadabn")
  setnames(long,"bookheartabnexam","bookexamheartabn")
  setnames(long,"booksymphtnhistx","bookhisthtnsymp")
  setnames(long,"bookcounsfamxhist","bookcounsfamhist")
  setnames(long,"bookknownlmpx","booklmpknown")
  setnames(long,"bookdaysgacatsx","bookgestagedays_cats")
  setnames(long,"bookabdabnexams","bookexamabdabn")
  setnames(long,"booklungabnexamx","bookexamlungabn")
  setnames(long,"bookutesurhistx","bookhistutesur")
  
  
  
  xtabs(~long$variable)
  
  # deleting all the book variables after time point 1
  booknames <- stringr::str_subset(names(long),"^book")
  for(i in booknames){
    long[variable!=1, (i):=NA]
  }
  
  openxlsx::write.xlsx(d[1:110], file.path(FOLDER_DATA_RESULTS_PAL,
                                           "wide.xlsx"))
  
  openxlsx::write.xlsx(long[1:110], file.path(FOLDER_DATA_RESULTS_PAL,
                                            "long.xlsx"))
  
  return(long)
}
