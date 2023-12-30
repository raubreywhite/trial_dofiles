
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


# transverse, face, trasverse, breech, oblique lie
A[,pres:=as.character(NA)]
A[stringr::str_detect(tolower(acsdatatext_1),"breech"), pres:="breech"]
A[stringr::str_detect(tolower(acsdatatext_1),"face"), pres:="cephalic"]
A[stringr::str_detect(tolower(acsdatatext_1),"obl"),  pres:="oblique"]
A[stringr::str_detect(tolower(acsdatatext_1),"tras"),  pres:="transverse"]
A[stringr::str_detect(tolower(acsdatatext_1),"trans"),  pres:="transverse"]



A[,multpreg:=FALSE]
A[stringr::str_detect(tolower(acsdatatext_1),"twins"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twisn"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"quadrable"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twis"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"twind"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"triplet"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"triblet"), multpreg:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"quadreplets"), multpreg:=TRUE]


# parity
A[stringr::str_detect(tolower(acsdatatext_1),"multipara"), parity:="multi"]
A[stringr::str_detect(tolower(acsdatatext_1),"grand multi"), parity:="multi"]
A[stringr::str_detect(tolower(acsdatatext_1),"multipare"), parity:="multi"]

A[stringr::str_detect(tolower(acsdatatext_1),"primi gravida"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"primigravida"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"primi"), parity:="nulli"]
A[stringr::str_detect(tolower(acsdatatext_1),"pg"), parity:="nulli"]


# induction
A[,induced:=as.logical(NA)]
A[stringr::str_detect(tolower(acsdatatext_1),"induction"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"inducion"), induced:=TRUE]

A[stringr::str_detect(tolower(acsdatatext_1),"F. IND. OF L"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"induced"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"induce"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"inductioin"), induced:=TRUE]
A[stringr::str_detect(tolower(acsdatatext_1),"indoction"), induced:=TRUE]



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

# analyses we want #
A[,cs_parity:=parity]
A[,cs_pres:=pres]
A[,cs_gestage:=gestage]
A[,cs_prevcs:=prevcs]
A[!is.na(acsdatatext_1), cs_multpreg:=FALSE]
A[multipreg==T,cs_multpreg:=TRUE]

A[,cs_onset:=as.character()]
A[,cs_induced:=induced()] # prelabour vs labour induced

A[,EDD:=as.date()] 
A[,cs_csbirth:=FALSE] # id those who are true
A[mode=="CS",cs_birth:=TRUE]



# define variables #
# define a birth/delivery
# parity nulli=0, multipara >=1
# previous cs none, atleast one
# singleton or multiple, refers to current preg
# presentation Cephalic, breech, transverse
# gest age <37, 37>=
# birthyear


# all data available
a[,all_cs_data:=FALSE]
a[!is.na(cs_parity) &
    !is.na(cs_numfetus) &
    !is.na(cs_pres) &
    !is.na(cs_gestage) &
    !is.na(cs_onset) &
    !is.na(cs_prevcs),all_cs_data:=TRUE]



# onset of labor: Spontaneous, induced, no labor (pre labour cs)

# missing data
dataavail <- a[,.(N=.N,
                  "Not Missing parity"=sum(!is.na(cs_parity), na.rm=T),
                  "Missing parity"=sum(is.na(cs_parity)),
                  "Not Missing Fetal Number"=sum(!is.na(cs_numfetus),na.rm=T),
                  "Missing Fetal Number"=sum(is.na(cs_numfetus)),
                  "Not Missing "=sum(!is.na(cs_pres),na.rm=T),
                  "Missing Presentation"=sum(is.na(cs_pres)),
                  "Not Missing Presentation "=sum(!is.na(),na.rm=T),
                  "Missing Gestational Age"=sum(is.na(cs_gestage)),
                  "Not Missing Gestational Age "=sum(!is.na(cs_gestage),na.rm=T),
                  "Missing Onset"=sum(is.na(cs_onset)),
                  "Not Missing Onset "=sum(!is.na(cs_onset),na.rm=T),
                  "Missing Previous CS"=sum(is.na(cs_prevcs)),
                  "Not Missing Previous CS"=sum(!is.na(cs_prevcs),na.rm=T),
                  "Missing Induced"=sum(is.na(cs_induced)),
                  "Not Missing Induced"=sum(!is.na(cs_induced),na.rm=T),
                  "All data available"=sum(all_cs_data==T, na.rm=T)),
               keyby=.(birthyear)]

# definition of robson groups
a[,cs_1:=FALSE]
a[cs_parity=="nulli" &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="spontaneous",cs_group_1:=TRUE]


a[,cs_group_2:=FALSE]
a[cs_parity=="nulli" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced",cs_group_2:=TRUE]

a[cs_group_2==TRUE,cs_group_2a:=FALSE]
a[cs_parity=="nulli" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced=="labour",cs_group_2a:=TRUE]

a[cs_group_2==TRUE,cs_group_2b:=FALSE]
a[cs_parity=="nulli" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced=="prelabour",cs_group_2b:=TRUE]

a[,cs_group_3:=FALSE]
a[cs_parity=="muti" &
    cs_prevcs %in% c(0,NA) &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="spontaneous",cs_group_3:=TRUE]


a[,cs_group_4:=FALSE]
a[cs_parity=="multi" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced",cs_group_4:=TRUE]

a[cs_group_4==TRUE,cs_group_4a:=FALSE]
a[cs_parity=="multi" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced=="labour",cs_group_4a:=TRUE]

a[cs_group_4==TRUE,cs_group_4b:=FALSE]
a[cs_parity=="multi" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37 &
    cs_onset=="induced" &
    cs_induced=="prelabour",cs_group_4b:=TRUE]


a[,cs_group_5:=FALSE]
a[cs_parity=="multi" &
    cs_prevcs >=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5:=TRUE]

a[cs_group_5==TRUE, cs_parity=="multi" &
    cs_prevcs==1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5a:=TRUE]

a[cs_group_5==TRUE, cs_parity=="multi" &
    cs_prevcs>=1 &
    cs_multipreg==FALSE &
    cs_pres=="cephalic" &
    cs_gestage>=37,cs_group_5b:=TRUE]

a[,cs_group_6:=FALSE]
a[cs_parity=="nulli" &
    cs_multipreg==FALSE &
    cs_pres=="breech",cs_group_6:=TRUE]

a[,cs_group_7:=FALSE]
a[cs_parity=="multi" &
    cs_multipreg==FALSE &
    cs_pres=="breech",cs_group_6:=TRUE]

a[,cs_group_8:=FALSE]
a[cs_numfetus>1,cs_group_8:=TRUE]

a[,cs_group_9:=FALSE]
a[cs_multipreg==FALSE &
    cs_pres %in% c("transverse","oblique"),cs_group_9:=TRUE]

a[,cs_group_10:=FALSE]
a[cs_multipreg==FALSE &
    cs_gestage < 37 &
    cs_pres=="cephalic",cs_group_10:=TRUE]

a[,cs_group:=as.character(NA)]
a[cs_group_1==TRUE,cs_group:="1"]
a[cs_group_2==TRUE,cs_group:="2"]
a[cs_group_2a==TRUE,cs_group:="2a"]
a[cs_group_2b==TRUE,cs_group:="2b"]
a[cs_group_3==TRUE,cs_group:="3"]
a[cs_group_4==TRUE,cs_group:="4"]
a[cs_group_4a==TRUE,cs_group:="4a"]
a[cs_group_4b==TRUE,cs_group:="4b"]
a[cs_group_5==TRUE,cs_group:="5"]
a[cs_group_5a==TRUE,cs_group:="5a"]
a[cs_group_5b==TRUE,cs_group:="5b"]

a[cs_group_6==TRUE,cs_group:="6"]
a[cs_group_7==TRUE,cs_group:="7"]
a[cs_group_8==TRUE,cs_group:="8"]
a[cs_group_9==TRUE,cs_group:="9"]
a[cs_group_10==TRUE,cs_group:="10"]


# data elements

adq <- a[,.("denom"=nrow(ident_avic==T, na.rm=T),
            "Matched"=nrow(ident_avic==T & ident_dhis2_booking==T, na.rm=T),
            "parity"=sum(!is.na(cs_parity)),
            "prevcs"=sum(!is.na(cs_prevcs)),
            "numfet"=sum(!is.na(cs_numfetus)),
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
reporttab <- a[!is.na(cs_group),
               .(N=.N,
                 "CS"=sum(mode=="CS",na.rm=T),
                 "# births"=sum(birth==TRUE, na.rm=T)),
               keyby=.(cs_group)]

reporttabyear <- a[!is.na(cs_group),
                   .(N=.N,
                     "CS"=sum(mode=="CS",na.rm=T),
                     "# births"=sum(birth==TRUE, na.rm=T)),
                   keyby=.(birthyear,
                           cs_group)]

reporttabyearhosp <- a[!is.na(cs_group),
                       .(N=.N,
                         "CS"=sum(mode=="CS",na.rm=T),
                         "# births"=sum(birth==TRUE, na.rm=T)),
                       keyby=.(hospname,
                               birthyear,
                               cs_group)]


# chi square by year and hosp?/ chi sq by hosp only

# robson group classes and percentages overall, and then by year
# id matched and came in for anc, ppc, anc & ppc

# completeness report per source

# demographic tables
# obstetric history and risks

#

### match with mch data for other outcomes ####
# ident_mch var for anyone matchin ppc or anc

########################
# merged data #
########################

# tables #

tabone <- a[,.("# total births"=nrow(ident_avic==T, na.rm=T),
               "# live births"=sum(ident_avic==T &
                                     birthoutcome=="alive",na.rm=T),
               "# MCH eReg"=sum(ident_mch==T, na.rm=T),
               "# births mch data"=sum(ident_avic==T & ident_mch==T, na.rm=T),
               "# births mch data and all cs data"=sum(all_cs_data==T &
                                                         ident_avic==T & 
                                                         ident_mch==T, na.rm=T),
               "# births with ANC data"=sum(ident_avic & 
                                              ident_dhis2_booking==T, na.rm=T),
               "# births with ANC data and all RGC data"=sum(ident_avic & 
                                                               ident_dhis2_booking==T &
                                                               all_cs_data==T, na.rm=T),
               
               "# births with PPC data"=sum(all_cs_data==T & ident_dhis2_ppc==T,na.rm=T),
               "# births with PPC data and RGC data"=sum(ident_avic & 
                                                           ident_dhis2_ppc==T &
                                                           all_cs_data==T, na.rm=T)),
            keyby=.(birthyear)]





# demographics #

###########################
# background and history #
###########################

tab <- ar[ident_avic==T & ident_mch==T,
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
consisttab <- a[ident_avic==T &
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



















































