###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

CheckFilesAndVariables(folder="e.reg-intervention")
CheckFilesAndVariables(folder="e.reg-control")

###### SETUP ENDS ######

####LOAD d from Network####
# PLACE OF DELIVERY INFORMATION CHECKING
# LATER ON, PUT THIS AUTOMATICALLY IN AN EXCEL REPPORT
d <- LoadDataFileFromNetwork()
#### bookvisitspec shouldnt be known  pcnidnumber_1  amdmotherbirthdate_1
###previdnumber_1   manidnumber  riskidnumber   d$hbodaltidnum_1   hbodaltidnum_1
###anidnumber_1     labid     usid

############END###########

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

d[is.na(ident_dhis2_ppc), ident_dhis2_ppc:=FALSE]

##########################
######Diabetes vs PPC#####
##########################

###Create labogctxxx which containts all labogct strings above 49###
d[,labogctxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labogct_")]
for(i in vars){d[get(i)>49,labogctxxx:=get(i)]}
#Create Binary Variable for if LABOGCTXXX >140, set all to FALSE except >140
d[,labogctbinary:=FALSE]
d[labogctxxx>140,labogctbinary:=TRUE]
PPC_OGCT<-(d[ident_dhis2_an==TRUE & !is.na(labogctxxx),
              c("ident_dhis2_ppc",
                "labogctxxx",
                "labogctbinary")])
nrow(PPC_OGCT[!is.na(labogctxxx)])
nrow(PPC_OGCT)
xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE))

############END###########

##########################
######Anemia vs PPC#######
##########################

###Create labhbxxx which containts all labhb strings below 17###
d[,labhbxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labhb_")]
for(i in vars){d[(get(i)<17),labhbxxx:=get(i)]}

###Create laghbxy which containts all labhb strings above 1###
d[,labhbxy:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"labhbxxx")]
for(i in vars){d[(get(i)>1),labhbxy:=get(i)]}

#Create Binary Variable for if LABHBXXX <11, set all to FALSE except <11
d[,labhbbinary:=FALSE]
d[labhbxy<11,labhbbinary:=TRUE]
PPC_HB<-(d[ident_dhis2_an==TRUE & !is.na(labhbxy),
              c("ident_dhis2_ppc",
                "labhbxy",
                "labhbbinary")])
nrow(PPC_HB[!is.na(labhbxy)])
nrow(PPC_HB)
xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE))
############END###########

##########################
####Hypertension vs PPC###
##########################

##Create labhtbinary
      ##Create anbpsystSSS
        d[,anbpsystSSS:=as.numeric()]
        vars <- names(d)[stringr::str_detect(names(d),"^anbpsyst_")]
        for(i in vars){d[get(i)>49,anbpsystSSS:=get(i)]}
        sort(unique(d$anbpsystSSS))

      ##Create anbpdiastDDD
          ##Create anbpdiastx
                d[,anbpdiastx:=as.numeric()]
                vars <- names(d)[stringr::str_detect(names(d),"^anbpdiast_")]
                for(i in vars){d[get(i)>49,anbpdiastx:=get(i)]}
                sort(unique(d$anbpdiastx))

          ##Create anbpdiastDDD
                d[,anbpdiastDDD:=as.numeric()]
                vars <- names(d)[stringr::str_detect(names(d),"^anbpdiastx")]
                for(i in vars){d[get(i)<200,anbpdiastDDD:=get(i)]}
                sort(unique(d$anbpdiastDDD))

d[,labhtbinary:=FALSE]
d[,labhtbinary:=anbpdiastDDD>90 | anbpsystSSS>140]                
PPC_HT<-(d[ident_dhis2_an==TRUE & !is.na(labhtbinary),
             c("ident_dhis2_ppc",
               "anbpdiastDDD",
               "anbpsystSSS",
               "labhtbinary")])


nrow(PPC_HT)

xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE))

############END###########

###########PLOT###########
library(CGPfunctions)
PlotXTabs(PPC_OGCT, ident_dhis2_ppc, labogctbinary, "side")
PlotXTabs(PPC_HB, ident_dhis2_ppc, labhbbinary, "side")
PlotXTabs(PPC_HT, ident_dhis2_ppc, labhtbinary, "side")
###########PLOT###########

###########TEST###########
xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE))
xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE))
xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE))
###########TEST###########


####################################################################################################################


##########################
######Urnie Stick Log#####
##########################

d[,laburgluxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^laburglu_")]
for(i in vars){d[is.na(get(i)),laburgluxxx:=0]}
for(i in vars){d[get(i) %in% c("POS","NEG"),laburgluxxx:=1]}

d[laburgluxxx==0, laburgluTF1:="Miss"]
d[laburgluxxx==1, laburgluTF1:="Avail"]

Accu_URGLU<-(d[!is.na(bookorgname),
               c("bookorgname",
                 "laburgluTF1")])

xtabs(~bookorgname+laburgluTF1,data=Accu_URGLU,addNA = TRUE)

XTBSU<-as.data.frame.matrix(xtabs(~bookorgname+laburgluTF1,data=Accu_URGLU,addNA = TRUE))

library(scales)

XTBSU$Tot<-(XTBSU$Avail+XTBSU$Miss)
XTBSU$XPer<-(percent(XTBSU$Avail/XTBSU$Tot))
XTBSU$Miss=NULL
XTBSU
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))

############END###########

####################################################################################################################


##########################
##########HB Log##########
##########################

d[,labhbxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labhb_")]
for(i in vars){d[is.na(get(i)),labhbxxx:=0]}
for(i in vars){d[(get(i)<17),labhbxxx:=get(i)]}
for(i in vars){d[!is.na(get(i)),labhbxxx:=1]}

d[labhbxxx==0, labhbTF1:="Miss"]
d[labhbxxx==1, labhbTF1:="Avail"]

Accu_HB<-(d[!is.na(bookorgname),
               c("bookorgname",
                 "labhbTF1")])

xtabs(~bookorgname+labhbTF1,data=Accu_HB,addNA = TRUE)
XTBSHB<-as.data.frame.matrix(xtabs(~bookorgname+labhbTF1,data=Accu_HB,addNA = TRUE))

library(scales)

XTBSHB$Tot<-(XTBSHB$Avail+XTBSHB$Miss)
XTBSHB$XPer<-(percent(XTBSHB$Avail/XTBSHB$Tot))
XTBSHB$XPern<-(XTBSHB$Avail/XTBSHB$Tot)
XTBSHB$Miss=NULL
XTBSHB <- XTBSHB[order(XTBSHB$XPern),]
XTBSHB <- XTBSHB[c("XPer", "Tot", "Avail")]
XTBSHB
percent((sum(XTBSHB$Avail))/(sum(XTBSHB$Tot)))

############END###########

####################################################################################################################

##########################
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

