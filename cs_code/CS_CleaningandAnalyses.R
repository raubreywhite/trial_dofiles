
#############################
# create avicenna data set #
#############################
# RUNCS #

A <- avi


#############################
# clean avicenna data set #
#############################

nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])

nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])

nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])

nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])

nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])


nrow(A[is.na(abbbabybirthresult_1) & is.na(abbbabybirthresult_2) & is.na(abbbabybirthresult_3) & is.na(abbbabybirthresult_4) & !is.na(abbbabybirthresult_5)])


# gestage
A[,gestage:=as.numeric(abbbabypregnancynoofweeks_1)]
A[is.na(abbbabypregnancynoofweeks_1) & !is.na(abbbabypregnancynoofweeks_2), 
              gestage:=as.numeric(abbbabypregnancynoofweeks_2)]

# birth date
A[,birthdate:=substr(abbbabybirthdate_1 , start = 1 , stop = 9 )]
A[,birthdate:=lubridate::dmy(birthdate)]
xtabs(~A$birthdate)
xtabs(~A$abbbabybirthdate_1)
str(A$abbbabybirthdate_1)
A[,birthyear:=lubridate::year(birthdate)]
xtabs(~A$birthyear)

# hospital name
A[,hospname:=as.character(NA)]

A[stringr::str_detect(abbname_1,"جنين"), 
  hospname:="Jenin"]
A[stringr::str_detect(abbname_1,"رفيديا"),
  hospname:="Rafidia"]
A[stringr::str_detect(abbname_1,"سلفيت"),
  hospname:="Salfit"]
A[stringr::str_detect(abbname_1,"طوباس"),
  hospname:="Tubas"]
A[stringr::str_detect(abbname_1,"درويش "),
  hospname:="Qalqilia"]
A[stringr::str_detect(abbname_1,"نزال"),
  hospname:="Qalqilia"]
A[stringr::str_detect(abbname_1,"طولكرم"),
  hospname:="Tulkarem"]
A[stringr::str_detect(abbname_1,"يطا"),
  hospname:="Yatta"]
A[stringr::str_detect(abbname_1,"نابلس"),
  hospname:="Rafidia"]
A[stringr::str_detect(abbname_1,"الخليل"),
  hospname:="Hebron"]
A[stringr::str_detect(abbname_1,"رام الله"),
  hospname:="PMC"]
A[stringr::str_detect(abbname_1,"بيت جالا"),
  hospname:="Beit Jala"]
A[stringr::str_detect(abbname_1,"أريحا"),
  hospname:="Jericho"]
A[stringr::str_detect(abbname_1,"Rafidia"),
  hospname:="Rafidia"]
A[stringr::str_detect(abbname_1,"الشيخ زايد"),
  hospname:="PMC"]
xtabs(~A$hospname, addNA=T)
nrow(A[is.na(abbname_1)])

# mode of delivery #
A[,mode:=as.character(NA)]
A[stringr::str_detect(abbbabybirthtype_1,"Cesarean"), mode:="CS"]
A[stringr::str_detect(abbbabybirthtype_1,"Normal"), mode:="Normal"]
xtabs(~A$abbbabybirthtype_1)
# outcome
A[,outcome:=abbbabybirthresult_1]
xtabs(~A$outcome, addNA=T)

# define variables #
# define a birth/delivery
# parity nulli=0, multipara >=1
# previous cs none, atleast one
# singleton or multiple, refers to current preg
# presentation Cephalic, breech, transverse
# gest age <37, 37>=


# avicennapregs reg
setorder(A,cols="motheridno","amddate_1","abbbabyrecorddatecreation_1")
A[!is.na(abbbabyrecorddatecreation_1),numinavicenna:=.N, by=motheridno]
xtabs(~A$numinavicenna)

# onset

# data cleaning #

# notes
A[,prevcs:=as.logical(NA)]
A[stringr::str_detect(tolower(acsdatatext_1),"prev") &
    !stringr::str_detect(tolower(acsdatatext_1),"previa"), prevcs:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"prev") &
    !stringr::str_detect(tolower(acsdatatext_1),"previa"), prevcs:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"preius"), prevcs:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"cs"), prevcs:=TRUE]

# assumption
setorder(A,cols="motheridno","amddate_1","abbbabyrecorddatecreation_1")

# shift prev mode of delivery down one, if prevmode=="CS", and is missing prev cs, then prevcs==T
A[,prevmode:=shift(mode,n=1L),by=motheridno]
A[is.na(prevcs) & prevmode=="CS", prevcs:=TRUE]



# assumption
A[is.na(prevcs), prevcs:=FALSE]


# transverse, face, trasverse, breech, oblique lie
A[,pres:=as.character(NA)]
A[stringr::str_detect(tolower(acsdatatext_1),"breech"), pres:="breech"]
A[stringr::str_detect(tolower(acsdatatext_1),"brrech"), pres:="breech"]

A[stringr::str_detect(tolower(acsdatatext_1),"face"), pres:="cephalic"]
A[stringr::str_detect(tolower(acsdatatext_1),"obl"),  pres:="oblique"]
A[stringr::str_detect(tolower(acsdatatext_1),"tras"),  pres:="transverse"]
A[stringr::str_detect(tolower(acsdatatext_1),"trans"),  pres:="transverse"]

A[is.na(pres) & mode=="Normal", pres:="cephalic"]



A[,multpreg:=FALSE]
A[stringr::str_detect(tolower(acsdatatext_1),"twins"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twisn"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"quadrable"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twis"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twind"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"triplet"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"triblet"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"quadreplets"), multpreg:=TRUE]

# assumption
A[is.na(parity) & !is.na(abbbabyrecorddatecreation_1) & 
      (!is.na(abbbabyrecorddatecreation_2)|
         !is.na(abbbabyrecorddatecreation_3)|
         !is.na(abbbabyrecorddatecreation_4)|
         !is.na(abbbabyrecorddatecreation_5)|
         !is.na(abbbabyrecorddatecreation_6)|
         !is.na(abbbabyrecorddatecreation_7)), multpreg:=TRUE]

xtabs(~A$multpreg)


# parity
A[stringr::str_detect(tolower(acsdatatext_1),"multipara"), parity:="multi"]
A[stringr::str_detect(tolower(acsdatatext_1),"grand multi"), parity:="multi"]
A[stringr::str_detect(tolower(acsdatatext_1),"multipare"), parity:="multi"]

A[stringr::str_detect(tolower(acsdatatext_1),"primi gravida"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"primigravida"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"primi"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"primy"), parity:="nulli"]

xtabs(~A$parity, addNA=T)

# assumptions
# assumption multiparity
A[is.na(parity) & numinavicenna>1 & ident_avic_abb==T, parity:="multi"]
xtabs(~A$parity, addNA=T)
# assumptions for nulliparity
#nrow(A[is.na(parity) & numinavicenna==1])
#A[is.na(parity) & numinavicenna==1, parity:="nulli"]
#xtabs(~A$parity, addNA=T)


# induction
A[,induced:=as.logical(NA)]
A[stringr::str_detect(tolower(acsdatatext_1),"induction"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"inducion"), induced:=TRUE]

A[stringr::str_detect(tolower(acsdatatext_1),"F. IND. OF L"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"induced"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"induce"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"inductioin"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"indoction"), induced:=TRUE]

# if not specified that they were induced, then assuming they werent induced
A[is.na(induced), induced:=FALSE]


# failed induction
A[,failed_induction:=as.logical(NA)]
A[induced==T & 
    (stringr::str_detect(acsdatatext_1,"fail")|
       stringr::str_detect(acsdatatext_1,"faile")|
       stringr::str_detect(acsdatatext_1,"failed")|
       stringr::str_detect(acsdatatext_1,"failled")|
       stringr::str_detect(acsdatatext_1,"faild")|
       stringr::str_detect(acsdatatext_1,"failor")|
       stringr::str_detect(acsdatatext_1,"filed")|
       stringr::str_detect(acsdatatext_1,"faileur")|
       stringr::str_detect(acsdatatext_1,"falire")|
       stringr::str_detect(acsdatatext_1,"falied")), failed_induction:=TRUE]

# elective cs
A[,electivecs:=FALSE]
varsIwant <- names(A)[stringr::str_detect(names(A),"acsdatatext_")]

for(i in varsIwant){
  
  A[stringr::str_detect(tolower(get(i)),"elective cs"), electivecs:=TRUE]
  A[stringr::str_detect(tolower(get(i)),"elective"), electivecs:=TRUE]
}
xtabs(~A$electivecs, addNA=T)


# analyses we want #
A[,cs_parity:=parity]
A[,cs_pres:=pres]
A[,cs_gestage:=gestage]
A[,cs_prevcs:=prevcs]
A[!is.na(acsdatatext_1), cs_multpreg:=FALSE]
A[multpreg==T,cs_multpreg:=TRUE]

A[,cs_onset:=as.character()]
A[mode=="Normal",cs_onset:="spontaneous"] 
A[induced==T, cs_onset:="induced"]
A[induced %in% c(NA,FALSE) & mode=="CS",cs_onset:="prelabour"] # prelabour vs at labour induced
xtabs(~A$cs_onset, addNA=T)

A[,cs_csbirth:=FALSE] # id those who are true
A[mode=="CS",cs_birth:=TRUE]



# definition of robson groups
A[,cs_1:=FALSE]
A[cs_parity=="nulli" &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="spontaneous",cs_group_1:=TRUE]
xtabs(~A$cs_group_1, addNA=T)

A[,cs_group_2:=FALSE]
A[cs_parity=="nulli" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset %in% c("induced","prelabour"),cs_group_2:=TRUE]
xtabs(~A$cs_group_2, addNA=T)

A[cs_group_2==TRUE,cs_group_2a:=FALSE]
A[cs_parity=="nulli" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="prelabour" &
    cs_induced==TRUE,cs_group_2a:=TRUE]
xtabs(~A$cs_group_2a, addNA=T)


A[cs_group_2==TRUE,cs_group_2b:=FALSE]
A[cs_parity=="nulli" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced==TRUE,cs_group_2b:=TRUE]
xtabs(~A$cs_group_2b, addNA=T)


A[,cs_group_3:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==F &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="spontaneous",cs_group_3:=TRUE]
xtabs(~A$cs_group_3, addNA=T)


A[,cs_group_4:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset %in% c("induced","prelabour"),cs_group_4:=TRUE]
xtabs(~A$cs_group_4, addNA=T)


A[cs_group_4==TRUE,cs_group_4a:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T&
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced==TRUE,cs_group_4a:=TRUE]
xtabs(~A$cs_group_4a, addNA=T)


A[cs_group_4==TRUE,cs_group_4b:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="prelabour" &
    cs_induced==TRUE,cs_group_4b:=TRUE]
xtabs(~A$cs_group_4b, addNA=T)


A[,cs_group_5:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5:=TRUE]
xtabs(~A$cs_group_5, addNA=T)


A[cs_group_5==TRUE,cs_group_5a:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5a:=TRUE]
xtabs(~A$cs_group_5a, addNA=T)

A[cs_group_5==TRUE, cs_group_5b:=FALSE]
A[cs_parity=="multi" &
    cs_prevcs==T &
    cs_multpreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5b:=TRUE]
xtabs(~A$cs_group_5b, addNA=T)

A[,cs_group_6:=FALSE]
A[cs_parity=="nulli" &
    cs_multpreg==FALSE &
    cs_pres=="breech",cs_group_6:=TRUE]
xtabs(~A$cs_group_6, addNA=T)

A[,cs_group_7:=FALSE]
A[cs_parity=="multi" &
    cs_multpreg==FALSE &
    cs_pres=="breech",cs_group_7:=TRUE]
xtabs(~A$cs_group_7, addNA=T)



A[,cs_group_8:=FALSE]
A[cs_multpreg==T,cs_group_8:=TRUE]
xtabs(~A$cs_group_8, addNA=T)


A[,cs_group_9:=FALSE]
A[cs_multpreg==FALSE &
    cs_pres %in% c("transverse","oblique"),cs_group_9:=TRUE]
xtabs(~A$cs_group_9, addNA=T)


A[,cs_group_10:=FALSE]
A[cs_multpreg==FALSE &
    cs_gestage < 37 &
    cs_pres=="cephalic",cs_group_10:=TRUE]
xtabs(~A$cs_group_10, addNA=T)

A[,cs_group:=as.character(NA)]
A[cs_group_1==TRUE,cs_group:="1"]
A[cs_group_2==TRUE,cs_group:="2"]
A[cs_group_2a==TRUE,cs_group:="2a"]
A[cs_group_2b==TRUE,cs_group:="2b"]
A[cs_group_3==TRUE,cs_group:="3"]
A[cs_group_4==TRUE,cs_group:="4"]
A[cs_group_4a==TRUE,cs_group:="4a"]
A[cs_group_4b==TRUE,cs_group:="4b"]
A[cs_group_5==TRUE,cs_group:="5"]
A[cs_group_5a==TRUE,cs_group:="5a"]
A[cs_group_5b==TRUE,cs_group:="5b"]

A[cs_group_6==TRUE,cs_group:="6"]
A[cs_group_7==TRUE,cs_group:="7"]
A[cs_group_8==TRUE,cs_group:="8"]
A[cs_group_9==TRUE,cs_group:="9"]
A[cs_group_10==TRUE,cs_group:="10"]


# data elements

adq <- A[,.("denom"=.N,
            #"Matched"=nrow(ident_avic_abb==T & ident_mch==T, na.rm=T),
            "Num births"=sum(ident_avic_abb==T, na.rm=T),
            "Num cs"=sum(mode=="CS", na.rm=T),
            "parity"=sum(!is.na(cs_parity)),
            "prevcs"=sum(!is.na(cs_prevcs)),
            "numfet"=sum(!is.na(cs_multpreg)),
            "presentation"=sum(!is.na(cs_pres)),
            "gestage"=sum(!is.na(cs_gestage)),
            "onset"=sum(!is.na(cs_onset))),
         keyby=.(birthyear)]

adq[,"% parity":= round(100*parity/denom, digits=1)]
adq[,"% prevcs":= round(100*prevcs/denom, digits=1)]
adq[,"% numfet":= round(100*numfet/denom, digits=1)]
adq[,"% presentation":= round(100*presentation/denom, digits=1)]
adq[,"% gestage":= round(100*gestage/denom, digits=1)]
adq[,"% onset":= round(100*onset/denom, digits=1)]

# total number of births 

# report table #
reporttab <- A[!is.na(cs_group),
               .(N=.N,
                 "CS"=sum(mode=="CS",na.rm=T),
                 "# births"=sum(birth==TRUE, na.rm=T)),
               keyby=.(cs_group)]

reporttabyear <- A[!is.na(cs_group),
                   .(N=.N,
                     "CS"=sum(mode=="CS",na.rm=T),
                     "# births"=sum(birth==TRUE, na.rm=T)),
                   keyby=.(birthyear,
                           cs_group)]

reporttabyearhosp <- A[!is.na(cs_group),
                       .(N=.N,
                         "CS"=sum(mode=="CS",na.rm=T),
                         "# births"=sum(birth==TRUE, na.rm=T)),
                       keyby=.(hospname,
                               birthyear,
                               cs_group)]

# report table 1 (manual)
tab_1 <- A[!is.na(cs_group),.(
  
  "N CS in group"=sum(cs_birth==TRUE, na.rm==T),
  "Total N in group"=.N),
  keyby=.(cs_group)]

total <- A[!is.na(cs_group),.(
  
  "N CS in group"=sum(cs_birth==TRUE, na.rm==T),
  "Total N in group"=.N)]
total[,cs_group:="Total"]

tab_1agg <- rbind(tab_1,
                  total,
                  fill=T)

#  # women in group/total women delivered in setting
tab_1_agg[,denom:=sum(`Total N in group`)]
tab_1agg[,`Group Size (%)`:=round(`Total N in group`/denom, digits=2)]

# # CS in group/total women in group
tab_1agg[,`Group CS rate (%)`:=round(`N CS in group`/`Total N in group`, digits=2)]

# below, # CS in group/total women in setting
tab_1agg[,`Absolute group contribution to overall CS rate (%)`:=round(`N CS in group`/denom, digits=2)]

# below, # CS in group/total # of CS in setting
tab_1agg[, cs_denom:=sum(`N CS in group`)]
tab_1agg[,`Relative group contribution to overall CS rate (%)`:=round(`N CS in group`/cs_denom, digits=2)]


tab_1agg[,c(cs_denom, denom):=NULL]


# chi square by year and hosp?/ chi sq by hosp only


# robson group classes and percentages overall, and then by year
# id matched and came in for anc, ppc, anc & ppc

# completeness report per source

# demographic tables
# obstetric history and risks

#

### match with mch data for other outcomes ####
# ident_mch var for anyone matchin ppc or anc

# define variables #
# define a birth/delivery
# parity nulli=0, multipara >=1
# previous cs none, atleast one
# singleton or multiple, refers to current preg
# presentation Cephalic, breech, transverse
# gest age <37, 37>=
# birthyear


################################
# merge mch data #
################################


avimch <- merge(a,
                mch,
                by=c("motheridno","amddate_1","abbbabybirthdate_1"),
                all.x=T)

nrow(avimch)
nrow(A)
nrow(a)
nrow(avi)
nrow(mch)
xtabs(~avimch$ident_mch)



################################
#  hospital dE #
################################

# all data available
avimch[,cs_data_availability:=0]
for (i in c("cs_parity",
            "cs_multpreg",
            "cs_pres",
            "cs_gestage",
            "cs_onset",
            "cs_prevcs")){
  
  
  avimch[!is.na(get(i)), cs_data_availability:=cs_data_availability+1]
  
}
xtabs(~avimch$cs_data_availability, addNA=T)


# distributions and propotions \

freqtab <- avimch[ident_avic_abb==T,c("birthyear",
                    "hospname",
                    "cs_parity",
                    "cs_multpreg",
                    "cs_pres",
                    "cs_gestage",
                    "cs_onset",
                    "cs_prevcs")]

freqtab <- melth.data.table(freqtab,
                            id.vars="birthyear")

# overall freq tab
freqtaboverall <- freqtab[,.(Missing=sum(is.na(var)),
                          "Not Missing"=sum(!is.na(var, na/rm=T)),
                          "True"=sum(var==TRUE, na.rm=T),
                          "False"=sum(var==F, na.rm=T)), keyby=c(var)]

freqtaboverall[,"% True":=round(100*`True/`Not Missing`, digits=1)]
freqtaboverall[,"% False":=round(100*`False`/Not Missing`, digits=1)]
freqtaboverall[,"% Missing":=round(100*`Missing`/`Not Missing`, digits=1)]




# freq tab birthyear
freqtabyear <- freqtab[,.(Missing=sum(is.na(var)),
           "Not Missing"=sum(!is.na(var, na/rm=T)),
           "True"=sum(var==TRUE, na.rm=T),
           "False"=sum(var==F, na.rm=T)), keyby=c(birthyear,var)]

freqtabyear[,"% True":=round(100*`True/`Not Missing`, digits=1)]
freqtabyear[,"% False":=round(100*`False`/Not Missing`, digits=1)]
freqtabyear[,"% Missing":=round(100*`Missing`/`Not Missing`, digits=1)]


# freq tab hosp
freqtabhosp <- freqtab[,.(Missing=sum(is.na(var)),
           "Not Missing"=sum(!is.na(var, na/rm=T)),
           "True"=sum(var==TRUE, na.rm=T),
           "False"=sum(var==F, na.rm=T)), keyby=c(hospname,var)]

freqtabhosp[,"% True":=round(100*`True/`Not Missing`, digits=1)]
freqtabhosp[,"% False":=round(100*`False`/Not Missing`, digits=1)]
freqtabhosp[,"% Missing":=round(100*`Missing`/`Not Missing`, digits=1)]


# freq tab year hosp
freqtabhospyear <- freqtab[,.(Missing=sum(is.na(var)),
                          "Not Missing"=sum(!is.na(var, na/rm=T)),
                          "True"=sum(var==TRUE, na.rm=T),
                          "False"=sum(var==F, na.rm=T)), keyby=c(birthyear, hospname,var)]

freqtabhospyear[,"% True":=round(100*`True/`Not Missing`, digits=1)]
freqtabhospyear[,"% False":=round(100*`False`/Not Missing`, digits=1)]
freqtabhospyear[,"% Missing":=round(100*`Missing`/`Not Missing`, digits=1)]




################################
#  birth distributions #
################################
# cs distribution among hospitals #


# clean birth year?
# per hosp
births<- avimch[ident_avic_abb==T & birthyear>2017 & ident_avic_abb==T,
           .("Total Births"=sum(ident_avic_abb==T, na.rm=T),
              "# CS"=sum(mode=="CS")),
           keyby=.(hospname)]

births[,"%":=round(100*`# CS`/`Total Births`, digits=1)]


# per hosp per year

 #check to add ident_avic_abb data
birthshospyear<- A[birthyear>2017 & ident_avic_abb==T,
                   .("Total Births"=sum(ident_avic_abb==T, na.rm=T),
              "# CS"=sum(mode=="CS")),
           keyby=.(hospname,birthyear)]

birthshospyear[,"%":=round(100*`# CS`/`Total Births`, digits=1)]

# per year
birthshospyear<- A[birthyear>2017 & ident_avic_abb==T,
                   .("Total Births"=sum(ident_avic_abb==T , na.rm=T),
                     "# CS"=sum(mode=="CS")),
                   keyby=.(birthyear)]

birthshospyear[,"%":=round(100*`# CS`/`Total Births`, digits=1)]




# onset of labor: Spontaneous, induced, no labor (pre labour cs)
# what do we do with birthyear before 2017? use amddate_1 if abbbabybirth record is found??
# missing data
dataavail <- A[!is.na(abbbabyrecorddatecreation_1),.(N=.N,
                  "Not Missing mode of delivery"=sum(!is.na(cs_mode, na.rm=T)),
                  "Missing mode of delivery"=sum(is.na(cs_mode)),
                  "Not Missing parity"=sum(!is.na(cs_parity), na.rm=T),
                  "Missing parity"=sum(is.na(cs_parity)),
                  "Not Missing Multpreg"=sum(!is.na(cs_multpreg),na.rm=T),
                  "Missing Multpreg"=sum(is.na(cs_multpreg)),
                  "Not Missing Presentation"=sum(!is.na(cs_pres),na.rm=T),
                  "Missing Presentation"=sum(is.na(cs_pres)),
                  "Not Missing gestational age"=sum(!is.na(cs_gestage),na.rm=T),
                  "Missing Gestational Age"=sum(is.na(cs_gestage)),
                  "Missing Onset"=sum(is.na(cs_onset)),
                  "Not Missing Onset "=sum(!is.na(cs_onset),na.rm=T),
                  "Missing Previous CS"=sum(is.na(cs_prevcs)),
                  "Not Missing Previous CS"=sum(!is.na(cs_prevcs),na.rm=T),
                  "Missing Induced"=sum(is.na(cs_induced)),
                  "Not Missing Induced"=sum(!is.na(cs_induced),na.rm=T)),
               keyby=.(birthyear)]

dataavail[birthyear>2017]













########################
# data avail mch #
########################
# tab 1 # 


datavail <- avimch[,.("# births"=sum(ident_avic_abb==T, na.rm=T),
                      "# matched"=sum(ident_mch==T, na.rm=T),
                      "nullipara"=sum(cs_para=="", na.rm=T),
                      "multipara"=sum(cs_para=="", na.rm=T),
                      "has prevcs"=sum(cs_prevcs==T, na.rm=T),
                      "has gestage"=sum(gestage>0, na.rm=T),
                      "has pres"=sum(!is.na(cs_pres, na.rm=T)),
                      "has onset"=sum(!is.na(cs_onset, na.rm=T))),
                   keyby=c(birthyear,
                           ident_mch)]

avimch[ident_avic_abb==T]
# tables #

tabmergeddata <- a[,.("# total births"=nrow(ident_avic_abb==T),
               "# live births"=sum(ident_avic_abb==T &
                                     birthoutcome=="alive",na.rm=T),
               "# MCH eReg"=sum(ident_mch==T, na.rm=T),
               "# births mch data"=sum(ident_avic_abb==T & ident_mch==T, na.rm=T),
               "# births mch data and all cs data"=sum(all_cs_data==T &
                                                         ident_avic_abb==T & 
                                                         ident_mch==T, na.rm=T),
               "# births with ANC data"=sum(ident_avic_abb & 
                                              ident_dhis2_booking==T, na.rm=T),
               "# births with ANC data and all RGC data"=sum(ident_avic_abb & 
                                                               ident_dhis2_booking==T &
                                                               all_cs_data==T, na.rm=T),
               
               "# births with PPC data"=sum(all_cs_data==T & ident_dhis2_ppc==T,na.rm=T),
               "# births with PPC data and RGC data"=sum(ident_avic_abb & 
                                                           ident_dhis2_ppc==T &
                                                           all_cs_data==T, na.rm=T)),
            keyby=.(birthyear)]





# demographics #




###########################
# background and history #
###########################

tab <- avimch[ident_avic_any==T & ident_mch==T,
          .(N=.N,
            "Mean Income"=mean(income, na.rm=T),
            "Mean Average Monthly Income"= mean(avgincome, na.rm=T),
            "Median Monthly Income"=median(avgincome,na.rm=T),
            "Mean Age"=mean(age, na.rm=T),
            "Mean Age First Pregnancy"=mean(agepregnancy, na.rm=TRUE),
            "Mean Age at Marriage"=mean(agemarriage, na.rm=T),
            "Mean Education"= mean(education, na.rm=T),
            "Mean BMI"=mean(bookbmi, na.rm=T),
            
            "Proprtion of Parity"= mean(bookparity, na.rm=T),
            "Number book parity"=sum(bookparity==1, na.rm=T),
            "Mean Bookgestage"= mean(bookgestage, na.rm=T),
            "Proprtion Primi"=mean(bookprimi, na.rm=T),
            "NUmber of Primi at booking"=sum(bookprimi==1, na.rm=T),
            
            "Number of Perinatal Death"=sum(bookhistperi, na.rm=T),
            "Number Uterine Surgery at Booking"=sum(bookhistutesur==1, na.rm=T),
            "Number History of C-section"=sum(bookhistcs==1, na.rm=T),
            "Number History of CS complications"=sum(bookhistcscompl==1, na.rm=T),
            "Number History of Preterm Birth"=sum(bookhistpreterm, na.rm=T),
            "Number History of Ute"=sum(bookhistute==1, na.rm=T),
            "Number History of Abortion"=sum(bookhistabort==1, na.rm=T),
            
            "Number History of APH"=sum(bookhistaph==1, na.rm=T),
            "Number History of GDM"=sum(bookhistgdm==1, na.rm=T),
            "Number History of GHTN"=sum(bookhistghtn==1, na.rm=T),
            "Number History of Preterm Birth"=sum(bookhistpreterm==1, na.rm=T),
            
            "Number History of DM in Family" =sum(bookfamdm==1, na.rm=T),
            "Number History of HTN in Family"=sum(bookfamhtn==1, na.rm=T),
            "Number of Women with History of Clexane Use"=sum(bookhistclex==1, na.rm=T)),
          keyby=.(birthyear)]


# mch ereg birthoutcome replace "bo" with the var
# consistency tables #
consisttab <- A[ident_avic==T &
                  ident_mch==T,.("# matched"=.N,
                                 "Not missing Birthoutcome"=sum(!is.na(bo,na.rm=T)),
                                 "CS in avicenna"=sum(mode=="CS",na.rm=T),
                                 "CS in avicenna and mch"=sum(mode=="CS" &
                                                                bo=="CS", na.rm=T),
                                 "CS in avi and other in MCH"=sum(mode=="CS" &
                                                                    !bo=="CS", na.rm=T),
                                 
                                 "Other in avicenna"= sum(!mode=="CS", na.rm=T),
                                 "Other Avicenna and CS in MCH"=sum(!mode=="CS" &
                                                                      bo=="CS", na.rm==T)),
                keyby=.(birthyear)]


# risks and complications table
# id any risks and complications first
# any risk at pregnancy 




# distribution of births and cs


# reason for CS on the mch ereg














































