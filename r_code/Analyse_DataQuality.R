KappaValues <- function(){
  dk <- DHIS2_Master(keepDoubleBookings = TRUE) 
  
  nam <- names(dk)
  res <- vector("list",length=length(nam))
  for(i in 1:length(nam)){
    cat(sprintf("%s/%s\n",i,length(nam)))
    try({
      var <- nam[i]
      newData <- dk[,c(var,"dataextractor","motheridno","bookdate"),with=F]
      setnames(newData,var,"value")
      setorder(newData,motheridno,bookdate)
      newData[,obs:=1:.N,by=.(motheridno,bookdate)]
      newData[,dataextractor:=NULL]
      newData <- dcast.data.table(newData[obs %in% 1:2],motheridno+bookdate~obs)
      newData[,motheridno:=NULL]
      newData[,bookdate:=NULL]
      k <- rel::spi(na.omit(newData))
      
      p <- irr::agree(na.omit(newData))
      
      res[[i]] <- data.frame(var=var,perc_agreement=p$value,kap_est=k$est,kap_se=k$se,n=k$sample)
    },TRUE)
  }
  
  res <- rbindlist(res)
  res <- res[is.finite(kap_est)]
  res <- res[is.finite(kap_se)]
  setorder(res,-perc_agreement)
  res[perc_agreement>100,perc_agreement:=NA]
  res[perc_agreement<0,perc_agreement:=NA]
  res <- na.omit(res)
  res
  
  openxlsx::write.xlsx(res,file.path(FOLDER_DROPBOX_RESULTS,
                                     "data_quality",
                                     "kappa.xlsx"))
  
}

Analyse_DataQualityVars <- function(d){
  print("something happens")
  #analyze dates of birth for twins to know if the data is cleaned enuogh or not 
  
  p <- ggplot(d,aes(x=mahima_dateofbirth_1,y=mahima_dateofbirth_2))
  p <- p + geom_point()
  p <- p + theme_gray(base_size = 16)
  p <- p + labs(caption=GraphCaption())
  
  ggsave(filename=file.path(
    FOLDER_DROPBOX_RESULTS,
    "data_quality",
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
    "data_quality",
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
    "data_quality",
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
    "data_quality",
    "avicenna_multiple_birth.png"),
    height=210,
    width=297,
    units="mm",
    plot=p)
  
  # decided not to do hbo because not enough dual pregnancies
  xtabs(~d$hboeventnum)
  
  
}

Analyse_AutoDuplications <- function(){
  files <- list.files(
    file.path(
      FOLDER_DATA_RAW,
      "possible_duplicates"), 
    "^autodup_"
    )
  
  res <- vector("list",length=length(files))
  for(i in 1:length(res)){
    res[[i]] <- readRDS(file.path(
      FOLDER_DATA_RAW,
      "possible_duplicates",
      files[i]
      ))
  }
  res <- rbindlist(res)
  res[,perc:=duplicatednum/totalnum*100]
  openxlsx::write.xlsx(res,
                       file=file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "data_quality",
                         "autoduplications.xlsx"))
}

Analyse_DataQuality <- function(d){
  Analyse_DataQualityVars(d)
  Analyse_AutoDuplications()
  KappaValues()
}