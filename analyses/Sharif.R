###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

#CheckFilesAndVariables(folder="e.reg-intervention")
#CheckFilesAndVariables(folder="e.reg-control")
CheckFilesAndVariables(folder="e.reg-intervention", 
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)
CheckFilesAndVariables(folder="e.reg-control", 
                       REF_DATE = REF_CLINIC_CONTROL_DATE, 
                       CHECK_DATE = CLINIC_CONTROL_DATE)

d <- LoadDataFileFromNetwork()

###### SETUP ENDS ######


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
#####################################################################d$###############################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#################################
###########ANC vs PPC############
#################################

nrow(d[bookyearmonth>=2017 & ident_dhis2_booking==TRUE])

nrow(d[bookyearmonth>=2017 & ident_dhis2_ppc==TRUE])

nrow(d[bookyearmonth>=2017 & ident_dhis2_booking==TRUE & ident_dhis2_ppc==TRUE])

################END##############

####################################################################################################################

#################################
########Monthly Attendance#######
#################################

sd <-d[,c("bookyearmonth",
          "ident_dhis2_booking")]

tab <- sd[,.(numbooked=sum(ident_dhis2_booking==1)),
          keyby=.(bookyearmonth)]

tab

################END##############

####################################################################################################################

#################################
##########Diabetes vs PPC########
#################################

d[is.na(ident_dhis2_ppc), ident_dhis2_ppc:=FALSE]

###Create labogctxxx which containts all labogct strings above 49###
d[,labogctxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labogct_")]
for(i in vars){d[get(i)>49,labogctxxx:=get(i)]}
#Create Binary Variable for if LABOGCTXXX >140, set all to FALSE except >140
d[,labogctbinary:=FALSE]
d[labogctxxx>140,labogctbinary:=TRUE]
PPC_OGCT<-(d[ident_dhis2_booking==TRUE & !is.na(labogctxxx),
              c("ident_dhis2_ppc",
                "labogctxxx",
                "labogctbinary")])
nrow(PPC_OGCT[!is.na(labogctxxx)])
nrow(PPC_OGCT)
xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE))

################END##############

####################################################################################################################

#################################
##########Anemia vs PPC##########
#################################
d[,labhbxxx:=0]
vars <- stringr::str_subset(names(d),"^labhb_[0-9]+")
for(i in vars) d[(get(i)<17),labhbxxx:=labhbxxx+1]

d[is.na(ident_dhis2_ppc), ident_dhis2_ppc:=FALSE]

###Create labhbxxx which containts all labhb strings below 17###
d[,labhbxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^labhb_")]
for(i in vars){d[(get(i)<17),labhbxxx:=get(i)]}

###Create laghbxy which containts all labhb strings above 1###
d[,labhbxy:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"labhbxxx")]
for(i in vars){d[(get(i)>1),labhbxy:=get(i)]}

#Create Binary Variable for if LABHBXY <11, set all to FALSE except <11
d[,labhbbinary:=FALSE]
d[labhbxy<11,labhbbinary:=TRUE]
PPC_HB<-(d[bookyear>=2017 & ident_dhis2_booking==TRUE & !is.na(labhbxy),
              c("ident_dhis2_ppc",
                "labhbxy",
                "labhbbinary")])
nrow(PPC_HB[!is.na(labhbxy)])
nrow(PPC_HB)
xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE))
################END##############

####################################################################################################################

#################################
########Hypertension vs PPC######
#################################

d[is.na(ident_dhis2_ppc), ident_dhis2_ppc:=FALSE]

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
PPC_HT<-(d[bookyearmonth>=2017 & ident_dhis2_booking==TRUE & !is.na(labhtbinary),
             c("ident_dhis2_ppc",
               "anbpdiastDDD",
               "anbpsystSSS",
               "labhtbinary")])


nrow(PPC_HT)d$

xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE))

################END##############

####################################################################################################################

###############PLOT##############
library(CGPfunctions)
PlotXTabs(PPC_OGCT, ident_dhis2_ppc, labogctbinary, "side")
PlotXTabs(PPC_HB, ident_dhis2_ppc, labhbbinary, "side")
PlotXTabs(PPC_HT, ident_dhis2_ppc, labhtbinary, "side")
###############PLOT##############

###############TEST##############
xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labogctbinary,data=PPC_OGCT,addNA = TRUE))
xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhbbinary,data=PPC_HB,addNA = TRUE))
xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE)
chisq.test(xtabs(~ident_dhis2_ppc+labhtbinary,data=PPC_HT,addNA = TRUE))
###############TEST##############


####################################################################################################################


#################################
#############Diabetes############
#################################

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

################END##############

####################################################################################################################

#################################
##############HB Log#############
#################################

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

################END##############

####################################################################################################################

#################################
##############HYT Log############
#################################

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

##############END################

####################################################################################################################

#################################
#####Education vs Attendance#####
#################################

tiz18<-d$anevent_18
tiz17<-d$anevent_17
tiz16<-d$anevent_16
tiz15<-d$anevent_15
tiz14<-d$anevent_14
tiz13<-d$anevent_13
tiz12<-d$anevent_12
tiz11<-d$anevent_11
tiz10<-d$anevent_10
tiz09<-d$anevent_9
tiz08<-d$anevent_8
tiz07<-d$anevent_7
tiz06<-d$anevent_6
tiz05<-d$anevent_5
tiz04<-d$anevent_4
tiz03<-d$anevent_3
tiz02<-d$anevent_2
tiz01<-d$anevent_1

tizz18<-(!is.na(tiz18))
tizz17<-(!is.na(tiz17))
tizz16<-(!is.na(tiz16))
tizz15<-(!is.na(tiz15))
tizz14<-(!is.na(tiz14))
tizz13<-(!is.na(tiz13))
tizz12<-(!is.na(tiz12))
tizz11<-(!is.na(tiz11))
tizz10<-(!is.na(tiz10))
tizz09<-(!is.na(tiz09))
tizz08<-(!is.na(tiz08))
tizz07<-(!is.na(tiz07))
tizz06<-(!is.na(tiz06))
tizz05<-(!is.na(tiz05))
tizz04<-(!is.na(tiz04))
tizz03<-(!is.na(tiz03))
tizz02<-(!is.na(tiz02))
tizz01<-(!is.na(tiz01))

nrow(d[bookyearmonth>=2017 & tizz18==TRUE])
nrow(d[bookyearmonth>=2017 & tizz17==TRUE])
nrow(d[bookyearmonth>=2017 & tizz16==TRUE])
nrow(d[bookyearmonth>=2017 & tizz15==TRUE])
nrow(d[bookyearmonth>=2017 & tizz14==TRUE])
nrow(d[bookyearmonth>=2017 & tizz13==TRUE])
nrow(d[bookyearmonth>=2017 & tizz12==TRUE])
nrow(d[bookyearmonth>=2017 & tizz11==TRUE])
nrow(d[bookyearmonth>=2017 & tizz10==TRUE])
nrow(d[bookyearmonth>=2017 & tizz09==TRUE])
nrow(d[bookyearmonth>=2017 & tizz08==TRUE])
nrow(d[bookyearmonth>=2017 & tizz07==TRUE])
nrow(d[bookyearmonth>=2017 & tizz06==TRUE])
nrow(d[bookyearmonth>=2017 & tizz05==TRUE])
nrow(d[bookyearmonth>=2017 & tizz04==TRUE])
nrow(d[bookyearmonth>=2017 & tizz03==TRUE])
nrow(d[bookyearmonth>=2017 & tizz02==TRUE])
nrow(d[bookyearmonth>=2017 & tizz01==TRUE])

d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
         +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
         +tizz04+tizz03+tizz02+tizz01)
sum(d$Visits,a.rm = TRUE)
sum(d[bookyear>=2017]$Visits,na.rm=T)

bigd<-d[,c("uniqueid","Visits")]
bigd<-bigd[,.(N=.N),keyby=.(Visits)]
bigd

d[education>0 & education <50,educationaccu:=education]
smalld<-d[,c("uniqueid","educationaccu")]
smalld<-smalld[,.(N=.N),keyby=.(educationaccu)]
smalld <- smalld[-c(12,14,17,19,21,25,26,27,28,29,30,31,32,33,34,35,36),]
smalld


d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
           +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
           +tizz04+tizz03+tizz02+tizz01)
bigd<-d[,c("uniqueid","Visits")]
d[education>0 & education <50,educationaccu:=education]
smalld<-d[,c("uniqueid","educationaccu")]
newed<-merge(smalld,bigd,by=c("uniqueid"),all.y=T)
newed<-newed[,.(N=.N,numvisits=sum(Visits)),keyby=.(educationaccu)]
newed <- newed[-c(1,12,14,17,19,21,25,26,27,28,29,30,31,32,33,34,35,36),]
newed<-newed[,.(N=.N,numvisits=sum(Visits)),keyby=.(educationaccu)]
AVG<-(newed$numvisits/newed$N)
newed$AVG<-(newed$numvisits/newed$N)

##############END################

####################################################################################################################

#################################
##Age of Marriage vs Attendance##
#################################

tiz18<-d$anevent_18
tiz17<-d$anevent_17
tiz16<-d$anevent_16
tiz15<-d$anevent_15
tiz14<-d$anevent_14
tiz13<-d$anevent_13
tiz12<-d$anevent_12
tiz11<-d$anevent_11
tiz10<-d$anevent_10
tiz09<-d$anevent_9
tiz08<-d$anevent_8
tiz07<-d$anevent_7
tiz06<-d$anevent_6
tiz05<-d$anevent_5
tiz04<-d$anevent_4
tiz03<-d$anevent_3
tiz02<-d$anevent_2
tiz01<-d$anevent_1

tizz18<-(!is.na(tiz18))
tizz17<-(!is.na(tiz17))
tizz16<-(!is.na(tiz16))
tizz15<-(!is.na(tiz15))
tizz14<-(!is.na(tiz14))
tizz13<-(!is.na(tiz13))
tizz12<-(!is.na(tiz12))
tizz11<-(!is.na(tiz11))
tizz10<-(!is.na(tiz10))
tizz09<-(!is.na(tiz09))
tizz08<-(!is.na(tiz08))
tizz07<-(!is.na(tiz07))
tizz06<-(!is.na(tiz06))
tizz05<-(!is.na(tiz05))
tizz04<-(!is.na(tiz04))
tizz03<-(!is.na(tiz03))
tizz02<-(!is.na(tiz02))
tizz01<-(!is.na(tiz01))

nrow(d[bookyearmonth>=2017 & tizz18==TRUE])
nrow(d[bookyearmonth>=2017 & tizz17==TRUE])
nrow(d[bookyearmonth>=2017 & tizz16==TRUE])
nrow(d[bookyearmonth>=2017 & tizz15==TRUE])
nrow(d[bookyearmonth>=2017 & tizz14==TRUE])
nrow(d[bookyearmonth>=2017 & tizz13==TRUE])
nrow(d[bookyearmonth>=2017 & tizz12==TRUE])
nrow(d[bookyearmonth>=2017 & tizz11==TRUE])
nrow(d[bookyearmonth>=2017 & tizz10==TRUE])
nrow(d[bookyearmonth>=2017 & tizz09==TRUE])
nrow(d[bookyearmonth>=2017 & tizz08==TRUE])
nrow(d[bookyearmonth>=2017 & tizz07==TRUE])
nrow(d[bookyearmonth>=2017 & tizz06==TRUE])
nrow(d[bookyearmonth>=2017 & tizz05==TRUE])
nrow(d[bookyearmonth>=2017 & tizz04==TRUE])
nrow(d[bookyearmonth>=2017 & tizz03==TRUE])
nrow(d[bookyearmonth>=2017 & tizz02==TRUE])
nrow(d[bookyearmonth>=2017 & tizz01==TRUE])

d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
           +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
           +tizz04+tizz03+tizz02+tizz01)
sum(d$Visits,a.rm = TRUE)
sum(d[bookyear>=2017]$Visits,na.rm=T)

bigd<-d[,c("uniqueid","Visits")]
bigd<-bigd[,.(N=.N),keyby=.(Visits)]
bigd

d[agemarriage>=14 & agemarriage<=38,agemarriageaccu:=agemarriage]
smalld2<-d[,c("uniqueid","agemarriageaccu")]
smalld2<-smalld2[,.(N=.N),keyby=.(agemarriageaccu)]
d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
           +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
           +tizz04+tizz03+tizz02+tizz01)
smalld2<-d[,c("uniqueid","agemarriageaccu")]
bigd2<-d[,c("uniqueid","Visits")]
newed2<-merge(smalld2,bigd2,by=c("uniqueid"),all.y=T)
newed2<-newed2[,.(N=.N,numvisits=sum(Visits)),keyby=.(agemarriageaccu)]
newed2$AVG<-(newed2$numvisits/newed2$N)
tabyl(d$agemarriageaccu)
Hell<-newed2[,c("AVG","agemarriageaccu")]
Hell

############END###############

####################################################################################################################

##############################
####Income vs. Attendance#####
##############################

tiz18<-d$anevent_18
tiz17<-d$anevent_17
tiz16<-d$anevent_16
tiz15<-d$anevent_15
tiz14<-d$anevent_14
tiz13<-d$anevent_13
tiz12<-d$anevent_12
tiz11<-d$anevent_11
tiz10<-d$anevent_10
tiz09<-d$anevent_9
tiz08<-d$anevent_8
tiz07<-d$anevent_7
tiz06<-d$anevent_6
tiz05<-d$anevent_5
tiz04<-d$anevent_4
tiz03<-d$anevent_3
tiz02<-d$anevent_2
tiz01<-d$anevent_1

tizz18<-(!is.na(tiz18))
tizz17<-(!is.na(tiz17))
tizz16<-(!is.na(tiz16))
tizz15<-(!is.na(tiz15))
tizz14<-(!is.na(tiz14))
tizz13<-(!is.na(tiz13))
tizz12<-(!is.na(tiz12))
tizz11<-(!is.na(tiz11))
tizz10<-(!is.na(tiz10))
tizz09<-(!is.na(tiz09))
tizz08<-(!is.na(tiz08))
tizz07<-(!is.na(tiz07))
tizz06<-(!is.na(tiz06))
tizz05<-(!is.na(tiz05))
tizz04<-(!is.na(tiz04))
tizz03<-(!is.na(tiz03))
tizz02<-(!is.na(tiz02))
tizz01<-(!is.na(tiz01))

nrow(d[bookyearmonth>=2017 & tizz18==TRUE])
nrow(d[bookyearmonth>=2017 & tizz17==TRUE])
nrow(d[bookyearmonth>=2017 & tizz16==TRUE])
nrow(d[bookyearmonth>=2017 & tizz15==TRUE])
nrow(d[bookyearmonth>=2017 & tizz14==TRUE])
nrow(d[bookyearmonth>=2017 & tizz13==TRUE])
nrow(d[bookyearmonth>=2017 & tizz12==TRUE])
nrow(d[bookyearmonth>=2017 & tizz11==TRUE])
nrow(d[bookyearmonth>=2017 & tizz10==TRUE])
nrow(d[bookyearmonth>=2017 & tizz09==TRUE])
nrow(d[bookyearmonth>=2017 & tizz08==TRUE])
nrow(d[bookyearmonth>=2017 & tizz07==TRUE])
nrow(d[bookyearmonth>=2017 & tizz06==TRUE])
nrow(d[bookyearmonth>=2017 & tizz05==TRUE])
nrow(d[bookyearmonth>=2017 & tizz04==TRUE])
nrow(d[bookyearmonth>=2017 & tizz03==TRUE])
nrow(d[bookyearmonth>=2017 & tizz02==TRUE])
nrow(d[bookyearmonth>=2017 & tizz01==TRUE])

d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
           +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
           +tizz04+tizz03+tizz02+tizz01)
sum(d$Visits,a.rm = TRUE)
sum(d[bookyear>=2017]$Visits,na.rm=T)

bigd<-d[,c("uniqueid","Visits")]
bigd<-bigd[,.(N=.N),keyby=.(Visits)]
bigd

incomer<-na.omit(d$income)

d[,incomercca:=as.numeric()]
d[incomer<1000, incomercca:="1000"]
d[incomer>=1000 & incomer<1500 , incomercca:="1015"]
d[incomer>=1500 & incomer<2000 , incomercca:="1520"]
d[incomer>=2000 & incomer<2500 , incomercca:="2025"]
d[incomer>=2500 & incomer<3000 , incomercca:="2530"]
d[incomer>=3000 & incomer<3500 , incomercca:="3035"]
d[incomer>=3500 & incomer<4000 , incomercca:="3540"]
d[incomer>=4000 & incomer<4500 , incomercca:="4045"]
d[incomer>=4500 & incomer<5000 , incomercca:="4550"]
d[incomer>=5000 & incomer<5500 , incomercca:="5055"]
d[incomer>=5500 & incomer<6000 , incomercca:="5560"]
d[incomer>=6000 & incomer<6500 , incomercca:="6065"]
d[incomer>=6500 & incomer<7000 , incomercca:="6570"]
d[incomer>=7000 & incomer<7500 , incomercca:="7075"]
d[incomer>=7500 & incomer<8000 , incomercca:="7580"]
d[incomer>=8000 & incomer<8500 , incomercca:="8085"]
d[incomer>=8500 & incomer<9000 , incomercca:="8590"]
d[incomer>=9000 & incomer<9500 , incomercca:="9095"]
d[incomer>=9500 & incomer<10000 , incomercca:="95100"]
d[incomer>=10000 & incomer<100000 , incomercca:="10000"]
sort(unique(d$incomercca))

smalld<-d[,c("uniqueid","incomercca")]
smalld<-smalld[,.(N=.N),keyby=.(incomercca)]
smalld

smalld2<-d[,c("uniqueid","incomercca")]
smalld2<-smalld2[,.(N=.N),keyby=.(incomercca)]
d$Visits<-(tizz18+tizz17+tizz16+tizz15+tizz14+tizz13+tizz12
           +tizz11+tizz10+tizz09+tizz08+tizz07+tizz06+tizz05
           +tizz04+tizz03+tizz02+tizz01)
smalld2<-d[,c("uniqueid","incomercca")]
bigd2<-d[,c("uniqueid","Visits")]
newed2<-merge(smalld2,bigd2,by=c("uniqueid"),all.y=T)
newed2<-newed2[,.(N=.N,numvisits=sum(Visits)),keyby=.(incomercca)]
newed2$AVG<-(newed2$numvisits/newed2$N)
tabyl(d$incomercca)
Hell<-newed2[,c("AVG","incomercca")]
Hell

############END###############

##############################
##Education vs. MarriageAge###
##############################

d[agemarriage>=14 & agemarriage<=38,agemarriageaccu:=agemarriage]
d[education>0 & education <50,educationaccu:=education]
HELL<-aggregate(d[,educationaccu,], list(d$agemarriageaccu), mean , na.rm=TRUE)
HELL
cor.test(d$agemarriageaccu, d$educationaccu)

############END###############

##############################
############Other#############
##############################

#Para#
tabyl(d[ident_dhis2_booking==TRUE]$para)

#Gravida#
tabyl(d[ident_dhis2_booking==TRUE]$gravida)

#Age#
tabyl(d[ident_dhis2_booking==TRUE]$age)

############END###############

##############################
#########Para Vs Age##########
##############################

d[age>=16 & age<=41,ageaccu:=age]
d[para>=1 & para <=50,paraaccu:=para]
HELL4<-aggregate(d[,paraaccu,], list(d$ageaccu), mean , na.rm=TRUE)
HELL4
cor.test(d$ageaccu, d$paraaccu)

############END###############

##############################
########Para Vs income########
##############################

d[para>=1 & para <=50,paraaccu:=para]
HELL5<-aggregate(d[,paraaccu,], list(d$incomercca), mean , na.rm=TRUE)
HELL5
cor.test(d$incomercca, d$paraaccu)

############END###############

##############################
######Para Vs education#######
##############################

d[para>=1 & para <=50,paraaccu:=para]
HELL6<-aggregate(d[,paraaccu,], list(d$educationaccu), mean , na.rm=TRUE)
HELL6
cor.test(d$educationaccu, d$paraaccu)

############END###############

###################Urine Stick by month####
d[,laburgluxxx:=as.numeric()]
vars <- names(d)[stringr::str_detect(names(d),"^laburglu_")]
for(i in vars){d[is.na(get(i)),laburgluxxx:=0]}
for(i in vars){d[get(i) %in% c("POS","NEG"),laburgluxxx:=1]}

d[laburgluxxx==0, laburgluTF1:="Miss"]
d[laburgluxxx==1, laburgluTF1:="Avail"]

Accu_URGLU<-(d[!is.na(bookorgname) & bookyearmonth=="2019-11",
               c("bookorgname",
                 "laburgluTF1", "bookyearmonth")])


XTBSU<-as.data.frame.matrix(xtabs(~bookorgname+laburgluTF1,data=Accu_URGLU,addNA = TRUE))

library(scales)

XTBSU$Tot<-(XTBSU$Avail+XTBSU$Miss)
XTBSU$XPer<-(percent(XTBSU$Avail/XTBSU$Tot))
XTBSU$Miss=NULL
XTBSU
percent((sum(XTBSU$Avail))/(sum(XTBSU$Tot)))
sum(XTBSU$Tot)

############END###############
############END###############

### TRANSFER TO EXCEL ###

### openxlsx::write.xlsx(dAtAsEt*,file.path(FOLDER_DATA_RESULTS,"nAmE*.xlsx")) ###
