###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=FALSE)

d <- LoadDataFileFromNetwork()

d <- d[ident_dhis2_nbc==T &
       ident_dhis2_an==TRUE & 
       ident_dhis2_control==F,
       stringr::str_subset(names(d),"^nbc"),with=F]


#stringr::str_replace("hello","e","3")
#stringr::str_replace(namegroups,"[0-9]$","")

namegroups <- names(d)
namegroups <- unique(stringr::str_replace(namegroups,"[0-9]$",""))


value.name<-c()
measure <- list()

value.name[1] <- "nbcsex_"
measure[[1]]<-stringr::str_subset(names(d),"^nbcsex_")

value.name[2] <-"nbcbcg_"
measure[[2]]<-stringr::str_subset(names(d),"^nbcbcg_")

value.name[3] <-"nbcbreastfeedingposition_"
measure[[3]]<-stringr::str_subset(names(d),"^nbcbreastfeedingposition_")

value.name[4] <-"nbchomevisitorattheclinic_"
measure[[4]]<-stringr::str_subset(names(d),"^nbchomevisitorattheclinic_")

value.name[5] <-"nbclabscreeningperformedforpku_"
measure[[5]]<-stringr::str_subset(names(d),"^nbclabscreeningperformedforpku_")

value.name[6] <-"nbcrespiratoryratebreathmin_"
measure[[6]]<-stringr::str_subset(names(d),"^nbcrespiratoryratebreathmin_")

value.name[7] <-"nbcweightgrams_"
measure[[7]]<-stringr::str_subset(names(d),"^nbcweightgrams_")

value.name[8] <-"nbcurination_"
measure[[8]]<-stringr::str_subset(names(d),"^nbcurination_")

value.name[9] <-"nbclabscreeningperformedforcongenitalhypothyreoidism_"
measure[[9]]<-stringr::str_subset(names(d),"^nbclabscreeningperformedforcongenitalhypothyreoidism_")

value.name[10] <-"nbccounselingondangersignsfornewborn_"
measure[[10]]<-stringr::str_subset(names(d),"^nbccounselingondangersignsfornewborn_")

value.name[11] <-"nbccounselingaboutumbilicalcordcare_"
measure[[11]]<-stringr::str_subset(names(d),"^nbccounselingaboutumbilicalcordcare_")

value.name[12] <-"nbccounselingaboutvitaminsandsupplements_"
measure[[12]]<-stringr::str_subset(names(d),"^nbccounselingaboutvitaminsandsupplements_")

value.name[13] <-"nbcvitaminaddropsgiven_"
measure[[13]]<-stringr::str_subset(names(d),"^nbcvitaminaddropsgiven_")

value.name[14] <-"nbcchilddesignation_"
measure[[14]]<-stringr::str_subset(names(d),"^nbcchilddesignation_")

value.name[15] <-"nbclabresultofpkuscreening_"
measure[[15]]<-stringr::str_subset(names(d),"^nbclabresultofpkuscreening_")

value.name[16] <-"nbclabresultofcongenitalhypothyreoidismchscreening_"
measure[[16]]<-stringr::str_subset(names(d),"^nbclabresultofcongenitalhypothyreoidismchscreening_")

value.name[17] <-"nbccyanosis_"
measure[[17]]<-stringr::str_subset(names(d),"^nbccyanosis_")

value.name[18]<-"nbcnewbornfeeding_"
measure[[18]]<-stringr::str_subset(names(d),"^nbcnewbornfeeding_")

value.name[19]<-"nbcconditionofbaby_"
measure[[19]]<-stringr::str_subset(names(d),"^nbcconditionofbaby_")

value.name[20]<-"nbcdateofdelivery_"
measure[[20]]<-stringr::str_subset(names(d),"^nbcdateofdelivery_")

value.name[21]<-"nbcgestationalageatdelivery_"
measure[[21]]<-stringr::str_subset(names(d),"^nbcgestationalageatdelivery_")

value.name[22]<-"nbcsuspectedcongenitalmalformation_"
measure[[22]]<-stringr::str_subset(names(d),"^nbcsuspectedcongenitalmalformation_")

value.name[23]<-"nbcsuspectedcongenitalmalformationspecified_"
measure[[23]]<-stringr::str_subset(names(d),"^nbcsuspectedcongenitalmalformationspecified_")


value.name[24]<-"nbcheadcircumferencecm_"
measure[[24]]<-stringr::str_subset(names(d),"^nbcheadcircumferencecm_")

value.name[25]<-"nbcheightcm_"
measure[[25]]<-stringr::str_subset(names(d),"^nbcheightcm_")

value.name[26]<-"nbcmorbidity_"
measure[[26]]<-stringr::str_subset(names(d),"^nbcmorbidity_")

value.name[27]<-"nbcdateofdelivery_"
measure[[27]]<-stringr::str_subset(names(d),"^nbcdateofdelivery_")




i=29


# these are control statements:
#for/if/else/else if/when
# they change the flow of the code
# CONTROLSTATEMENT(question){
#    DO CODE HERE
#} 
# e.g.
# for(i in 1:10){
#  print(i)
#}
#


for (i in seq_along(namegroups)){
  value.name[i] <-namegroups[i]
  measure[[i]]<-stringr::str_subset(names(d),namegroups[i])
}


long = melt(d, measure = measure, value.name = value.name)
# checking to see that long has 3* the number of rows (because it is now
# long by visit)
#nrow(d)
#nrow(long)

# checking to see that long has 1/3+1 the number of columns 
#ncol(d)
#ncol(long)

#if someone only has one visit, they structurally have the variables for 2/3 visits
#but the information is missing because they dont have those visits to begin with
#the long format data only contains data on a per visit basis
#so,we can remove the visits that dont exist in the long format table
#removing visits that dont exist because they didnt have a second or third visit

long <- long[!is.na(nbcevent_)]

#make the table with the data from the long format
#here we can add the rest of the variables that we want for sums, means,etc
long[,
  .(
    numVisits=.N,
    "Number of Delivery"= sum(!is.na(nbcdateofdelivery_)),
    "Mean Height"= round(mean(nbcheightcm_, na.rm = T),2),
    "Numberof BCG"= sum(!is.na(nbcbcg_),3),
    "Clinic Visit"= sum((nbchomevisitorattheclinic_==1),4),
    "Home Visit"= sum((nbchomevisitorattheclinic_==2),5),
    "PKUperformed"= sum((nbclabscreeningperformedforpku_==1),6),
    "Mean Respiratory Rate"= round(mean(nbcrespiratoryratebreathmin_, na.rm=T),7),
    "Mean Head Circumference"= round(mean(nbcheadcircumferencecm_, na.rm=T),8),
    "Mean Height"= round(mean(nbcweightgrams_, na.rm=T),9),
    "Number of Delivery Dates"= sum(!is.na(nbcdateofdelivery_),10),
    "Number of Suspected Congenital Malformation"= sum((nbcsuspectedcongenitalmalformationspecified_==1),11),
    "Number of Males"=sum((nbcsex_==1),12),
    "Breastfeeding Position_1"= sum((nbcbreastfeedingposition_==1), 13),
    "Breastfeeding Position_2"= sum((nbcbreastfeedingposition==2),14),
    "Urination_Normal"= sum((nbcurination_=="NORMAL"), 15),
    "Urination_Abnormal"= sum((nbcurination_=="ABNORMAL"), 16),
    "Number Screened for Congentiatl Hypothyroidism"= sum((nbclabscreeningperformedforcongenitalhypothyreoidism_))
  )]



d[ident_dhis2_an==TRUE & 
    ident_dhis2_control==F &
    ident_dhis2_nbc,
  .(
    numVisits1=sum(!is.na(nbcevent_1)),
    numVisits2=sum(!is.na(nbcevent_2)),
    numVisits3=sum(!is.na(nbcevent_3))
  )]


uglytable <- d[bookyear>="2016"& 
    ident_dhis2_control==F &
    ident_dhis2_nbc &
    !is.na(nbcdate_1) &
    !is.na(nbcdate_2),
    
  c("nbcidnumber_1",
    "nbcidnumber_2",
    "nbcdate_1",
    "nbcdate_2",
    "nbcbirthweightgrams_1",
    "nbcbirthweightgrams_2",
    "nbcnameofnewborn_1",
    "nbcnameofnewborn_2"
 
)]


openxlsx::write.xlsx(x=uglytable,file=file.path(
  FOLDER_DATA_RESULTS,
  "buthaina",
  sprintf("%s_nbc.xlsx",lubridate::today())))