#########################
##########HYT Log#########
##########################

d[,labhtxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_")]
for(i in vars){d[is.na(get(i)),labhtxxx:=0]}
for(i in vars){d[get(i)>49,labhtxxx:=get(i)]}
for(i in vars){d[!is.na(get(i)),labhtxxx:=1]}

d[labhtxxx==0, labhtTF1:="Miss"]
d[labhtxxx==1, labhtTF1:="Avail"]

Accu_HT<-(d[!is.na(bookorgname),
            c("bookorgname",
              "labhtTF1")])

xtabs(~bookorgname+labhtTF1,data=Accu_HT,addNA = TRUE)
XTBSHT<-as.data.frame.matrix(xtabs(~bookorgname+labhtTF1,data=Accu_HT,addNA = TRUE))

library(scales)

XTBSHT$Tot<-(XTBSHT$Avail+XTBSHT$Miss)
XTBSHT$XPer<-(percent(XTBSHT$Avail/XTBSHT$Tot))
XTBSHT$XPern<-(XTBSHT$Avail/XTBSHT$Tot)
XTBSHT$Miss=NULL
XTBSHT <- XTBSHT[order(XTBSHT$XPern),]
XTBSHT <- XTBSHT[c("XPer", "Tot", "Avail")]
XTBSHT
percent((sum(XTBSHT$Avail))/(sum(XTBSHT$Tot)))

############END###########

