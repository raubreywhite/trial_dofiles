
Analyze_Datacleaning <- function(d){
  print("something happens")
  #analyze dates of birth for twins to know if the data is cleaned enuogh or not 
  
  p <- ggplot(d,aes(x=mahima_dateofbirth_1,y=mahima_dateofbirth_2))
  p <- p + geom_point()
  p <- p + theme_gray(base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "data_cleaning",
    "date_birth_twins.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)

  # order the dataset
  setorder(d,motheridno,booknum)
  d[,bookdateprevious:=shift(bookdate,n=1L),by=motheridno]
  
  #xtabs(~d$booknum)
  #d[booknum==3]$motheridno
  #d[motheridno==401404496,c("booknum","bookdate","bookdateprevious")]
  
  #d[!is.na(bookdateprevious) & bookdateprevious>bookdate]$motheridno
  #d[motheridno==401111117,c("ident_dhis2_control","uniqueid","bookevent","booknum","bookdate","bookdateprevious")]
  
  p <- ggplot(d[!is.na(bookdateprevious)], aes(x=bookdateprevious, y=bookdate))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "data_cleaning",
    "bookdate_multiple_pregnancies.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)

  # for intervention
  p <- ggplot(d[!is.na(bookdateprevious)& ident_dhis2_control==FALSE ], aes(x=bookdateprevious, y=bookdate))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "data_cleaning",
    "bookdate_multiple_pregnancies_intervention.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  
  #checking avivennanum 
  # order the dataset
  xtabs(~d$abbeventnum)
  setorder(d,motheridno,abbeventnum)
  d[!is.na(abbeventnum),abbdate_1_previous:=shift(abbdate_1,n=1),by=motheridno]
  
  
  p <- ggplot(d[!is.na(abbdate_1_previous)], aes(x=abbdate_1_previous, y=abbdate_1))
  p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  p <- p + geom_abline(intercept = 31*10, slope = 1, colour="red")
  p <- p + geom_point()
  p <- p + coord_equal()
  p <- p + theme_grey (base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  
  ggsave(filename = file.path(
    FOLDER_DROPBOX_RESULTS,
    "data_cleaning",
    "avicenna_multiple_birth.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  # decided not to do hbo because not enough dual pregnancies
  xtabs(~d$hboeventnum)
  
  
}

