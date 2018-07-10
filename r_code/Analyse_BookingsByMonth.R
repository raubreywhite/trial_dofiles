Analyse_BookingsByMonth <- function(d=NULL){
  if(!exists("d")) d <- LoadDataFileFromNetwork()
  
  # to make graphs
  d[,bookmonth:=lubridate::month(bookdate)]
  d[,bookmonth:=formatC(bookmonth,flag="0",width=2)]
  
  d[,bookyear:=lubridate::year(bookdate)]
  
  d[,bookyearmonth:=sprintf("%s-%s",bookyear,bookmonth)]
  
  
  toPlot <- d[ident_dhis2_booking==1,
              .(numWomen=.N),
              by=.(bookyearmonth)
              ]
  
  setorder(toPlot,bookyearmonth)
  
  p <- ggplot(data=toPlot, mapping=aes(x=bookyearmonth,y=numWomen))
  p <- p + geom_bar(stat="identity")
  p <- p + geom_label(mapping=aes(label=numWomen), size=1)
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
  ggsave(filename="~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results/bookings_by_month.png",
         plot=p)
}