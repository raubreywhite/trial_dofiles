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