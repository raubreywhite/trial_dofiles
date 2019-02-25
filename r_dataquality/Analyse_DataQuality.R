

Analyse_DataQualityVars <- function(d,location="wb"){
  x <- GetFoldersAndData(d=d,location=location)
  pd <- x$pd
  folders <- x$folders
  
  print("something happens")
  #analyze dates of birth for twins to know if the data is cleaned enuogh or not 
  
  p <- ggplot(pd,aes(x=mahima_dateofbirth_1,y=mahima_dateofbirth_2))
  p <- p + geom_point()
  p <- p + theme_gray(base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    folders$dropbox,
    "data_quality",
    "date_birth_twins.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)

  # order the dataset
  setorder(pd,motheridno,booknum)
  pd[,bookdateprevious:=shift(bookdate,n=1L),by=motheridno]
  
  p <- ggplot(pd[!is.na(bookdateprevious)], aes(x=bookdateprevious, y=bookdate))
  #this is for continous variables for making a nice lines
  # geom_abline is for decorative purposes
  # geom_line actually displays data
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    folders$dropbox,
    "data_quality",
    "bookdate_multiple_pregnancies.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)

  # for intervention
  p <- ggplot(pd[!is.na(bookdateprevious)& ident_dhis2_control==FALSE ], aes(x=bookdateprevious, y=bookdate))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    folders$dropbox,
    "data_quality",
    "bookdate_multiple_pregnancies_intervention.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  
  #checking avivennanum 
  # order the dataset
  xtabs(~pd$abbeventnum)
  setorder(pd,motheridno,abbeventnum)
  pd[!is.na(abbeventnum),abbdate_1_previous:=shift(abbdate_1,n=1),by=motheridno]
  
  
  p <- ggplot(pd[!is.na(abbdate_1_previous)], aes(x=abbdate_1_previous, y=abbdate_1))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    folders$dropbox,
    "data_quality",
    "avicenna_multiple_birth.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
}

Analyse_AutoDuplications <- function(d,location="wb"){
  x <- GetFoldersAndData(d=d,location=location)
  if(location=="pal") return()
  
  pd <- x$pd
  folders <- x$folders
  
  files <- list.files(
    file.path(
      folders$data_raw,
      "possible_duplicates"), 
    "^autodup_"
    )
  
  res <- vector("list",length=length(files))
  for(i in 1:length(res)){
    res[[i]] <- readRDS(file.path(
      folders$data_raw,
      "possible_duplicates",
      files[i]
      ))
  }
  res <- rbindlist(res)
  res[,perc:=duplicatednum/totalnum*100]
  openxlsx::write.xlsx(res,
                       file=file.path(
                         folders$dropbox,
                         "data_quality",
                         "autoduplications.xlsx"))
}
