###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

###### SETUP ENDS ######

###### Load in Data ######
d <- LoadDataFileFromNetwork()

#making small data set for PPC in 2019
gaza2019data <- d[bookyear==2019,]

# ANC Anemia

#### labhb at 35-37 weeks #####

gaza2019data[,haslabhbtermgA:=FALSE]

vars <- stringr::str_subset(names(gaza2019data),"^labgestage")
for (i in vars){
  print(i)
  gaza2019data[get(i)>=35 & get(i)<=37, haslabhbtermgA:=TRUE]
  
}

xtabs(~gaza2019data$haslabhbtermgA)

#labhb value for 35 to 37 weeks
smallD[,labhbatterm:=as.numeric(NA)]

vars <- stringr::str_subset(names(gaza2019data),"^labhb_")


for (var_labhb in vars){
  
  vargestage <-stringr::str_replace(var_labhb,"labhb", "labgestage")
  
  gaza2019data[haslabhbterm==TRUE &
           get(vargestage)>=35 &
           get(vargestage)<=37 &
           !is.na(get(var_labhb)) &
           get(var_labhb)!="",
         labhbatterm:=get(var_labhb)]
}

anemia <- gaza2019data[,.(N=.N,
                   Haslabatterm=sum(haslabhbterm==T, na.rm=T),         
                   validvalues=sum(labhbatterm>0 & labhbatterm<16, na.rm=T),
                   MildAnemia=sum(labhbatterm>=9 &
                                    labhbatterm<11,na.rm=T),
                   ModerateAnemia=sum(labhbatterm>7 &
                                        labhbatterm<=8.9,na.rm=T),
                   sevAnemia=sum(labhbatterm>0 & 
                                   labhbatterm<7,na.rm=T))]

openxlsx::write.xlsx(anemia, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_anemia.xlsx", lubridate::today())))



### ANC GHT ###
gaza2019data[,GHTlikely:=as.logical(NA)]
gaza2019data[ident_dhis2_booking==T, GHTlikely:=FALSE]

gaza2019data[,anbpsyst_0:=bookbpsyst]
gaza2019data[,anbpdiast_0:=bookbpdiast] 

varsanbpsyst <- stringr::str_subset(names(gaza2019data),"^anbpsyst_")
varsanbpdiast <- stringr::str_subset(names(gaza2019data),"^anbpdiast_")

gaza2019data[,anbpsysthigh:=as.numeric(NA)]
gaza2019data[,anbpdiasthigh:=as.numeric(NA)]

for(i in varsanbpsyst){
  gaza2019data[get(i)>140, anbpsysthigh:=get(i)]
 
}
xtabs(~gaza2019data$anbpsysthigh, addNA = T)


for(i in varsanbpdiast){
  gaza2019data[get(i)>=90, anbpdiasthigh:=get(i)]
  
}

xtabs(~gaza2019data$anbpdiasthigh, addNA = T)

HTN <- gaza2019data[,.(N=sum(ident_dhis2_booking==T, na.rm=T),
                      mildHTN=sum(anbpsysthigh>=140 & 
                                    anbpsysthigh<=149 &
                                    anbpdiasthigh>=90 &
                                    anbpdiasthigh<=99, na.rm=T),
                      modHTN=sum(anbpsysthigh>=150 & 
                                   anbpsysthigh<=159 &
                                   anbpdiasthigh>=100 &
                                   anbpdiasthigh<=109, na.rm=T),
                      sevHTN=sum(anbpsysthigh>=160 & 
                                 anbpdiasthigh>=110, na.rm=T)
)]

table(gaza2019data$anbpsysthigh)
table(gaza2019data$anbpdiasthigh)

openxlsx::write.xlsx(HTN, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_HTN.xlsx", lubridate::today())))

### ANC GDM ###
 #laburglu at booking
gaza2019data[,booklaburglu:=as.character(NA)]
gaza2019data[,booklaburglu:=laburglu_1]
xtabs(~gaza2019data$booklaburglu, addNA=T)

#random and fasting 
gaza2019data[,hastimelylab:=as.logical(NA)]
gaza2019data[bookgestage<=28, hastimelylab:=FALSE]

vars <- stringr::str_subset(names(gaza2019data),"^labgestage_")
for (i in vars){
  print(i)
  gaza2019data[get(i)>=24 & get(i)<=28, hastimelylab:=TRUE]
  
}


gaza2019data[,fastingbloodglue:=as.numeric(NA)]

vars <- stringr::str_subset(names(gaza2019data),"^labfastbloodglu_")


for (var_labfastbloodglu in vars){
  
  vargestage <-stringr::str_replace(var_labfastbloodglu,"labfastbloodglu", "labgestage")
  
  gaza2019data[hastimelylab==TRUE &
           !is.na(get(var_labfastbloodglu)),
         fastingbloodglue:=get(var_labfastbloodglu)]
}

xtabs(~gaza2019data$fastingbloodglue, addNA=T)
nrow(gaza2019data[!is.na(fastingbloodglue)])

gaza2019data[,fbslikelygdm:=as.logical(NA)]
gaza2019data[fastingbloodglue<=92 & fastingbloodglue>0,fbslikelygdm:=FALSE]
gaza2019data[fastingbloodglue>92,fbslikelygdm:=TRUE]
xtabs(~gaza2019data$fbslikelygdm, addNA=T)

GDM <- gaza2019data[,.(N=.N,
                       posbooklaburglu=sum(booklaburglu=="POS", na.rm=T),
                       hastimelylab=sum(hastimelylab==T, na.rm=T),
                       likelyGDM=sum(fbslikelygdm==T, na.rm=T))]

openxlsx::write.xlsx(GDM, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_GDM.xlsx", lubridate::today())))



### PPC ### 
# vars we want
#ppclungauscultation_1, normal, abnormal, etc
#ppcheartauscultationabnormalitiesspecified_1, all NA
#ppcheartauscultation_1, like lung
#ppcheartauscultationabnormalitiesspecified_1, all NA
#ppclungauscultationabnormalitiesspecified_1, NA
#ppclungauscultation_1, normal, abnormal, NA
#ppcmicturition_1, al NA
#ppcrespiratoryraterr_1, dont need this
#ppcsignsofsepsis_1,  all NA
#ppcincisiontearcondition_1, all NA
#ppcspesificationofabnormaldefecation_1 free text
#ppcsignsofshock_1, all NA
#ppccalfswelling_1, all NA

vars_ppcsevabdpain <- names(d)[stringr::str_detect(names(d),"^ppcsevereabdominal_")]
vars_ppclungausc <- names(d)[stringr::str_detect(names(d),"^ppclungauscultation_")]
vars_ppccoughing <- names(d)[stringr::str_detect(names(d),"^ppccoughing_")]
vars_ppcdiffbreath <- names(d)[stringr::str_detect(names(d),"^ppcdifficultybreathing_")]
vars_ppcinscisorperintear	<- names(d)[stringr::str_detect(names(d),"^ppcinscisionorperinealtear_")]
vars_ppcexcesstiredness	<- names(d)[stringr::str_detect(names(d),"^ppcexcessivetiredness_")]
vars_ppcsevheadache	<- names(d)[stringr::str_detect(names(d),"^ppcsevereheadache_")]
vars_ppcblurryvision <- names(d)[stringr::str_detect(names(d),"^ppcblurryvision_")]
vars_ppcheartauscult<- names(d)[stringr::str_detect(names(d),"^ppcheartauscultation_")]
vars_ppcextremlightheaded	<- names(d)[stringr::str_detect(names(d),"^ppcextremelylightheaded_")]
vars_ppcseizures	<- names(d)[stringr::str_detect(names(d),"^ppcseizures_")]
vars_ppclossofconsc	<- names(d)[stringr::str_detect(names(d),"^ppclossofconsciousness_")]
vars_ppcsignsofsepsis	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofsepsis_")]
vars_ppcsleepapathydepres	<- names(d)[stringr::str_detect(names(d),"^ppcsleeplessnessapathyorsymptomsofdepression_")]
vars_ppcvomiting	<- names(d)[stringr::str_detect(names(d),"^ppcvomiting_")]
vars_ppcsignsofedemainhandsorface	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofedemainhandsorface_")]
vars_ppclochiavaginaldischargecolor	<- names(d)[stringr::str_detect(names(d),"^ppclochiavaginaldischargecolor_")]
vars_ppcvagdischargebadodor	<- names(d)[stringr::str_detect(names(d),"^ppcvaginaldischargewithunpleasantodor_")]
vars_ppcbreastinspection	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspection_")]
vars_ppccalfswelling	<- names(d)[stringr::str_detect(names(d),"^ppccalfswelling_")]
vars_ppcruptureduterus	<- names(d)[stringr::str_detect(names(d),"^ppcruptureduterus_")]
vars_ppcsignsofdvt	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofdvt_")]
vars_ppcleakstool	<- names(d)[stringr::str_detect(names(d),"^ppccontinuousleakageofstool_")]
vars_ppcleakurine	<- names(d)[stringr::str_detect(names(d),"^ppccontinuousleakageofurine_")]
vars_ppcsignsofshock	<- names(d)[stringr::str_detect(names(d),"^ppcsignsofshock_")]
vars_ppcdvtcalfpain	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalfpain_")]
vars_ppcdvtcalfswelling	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalfswelling_")]
vars_ppcdvtcalftenderness	<- names(d)[stringr::str_detect(names(d),"^ppcdvtsymptomscalftenderness_")]
vars_ppcbreastcrackednipples	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectioncrackednipples_")]

vars_ppcbreastabnormalsecr	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionabnormalsecretion_")]
vars_ppcbreasthotsensation	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionhotsensation_")]
vars_ppcbreastredness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionredness_")]
vars_ppcbreastswelling	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectionbreastswelling_")]
vars_ppcbreasttenderness	<- names(d)[stringr::str_detect(names(d),"^ppcbreastinspectiontenderness_")]
vars_ppcincisiontearbleeding	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionbleeding_")]
vars_ppcincisiontearabnormaldischarge	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionabnormaldischarge_")]
vars_ppcincisiontearpain	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionpain_")]
vars_ppcincisiontearswelling	<- names(d)[stringr::str_detect(names(d),"^ppcincisiontearinspectionswelling_")]
vars_ppcdefecation	<- names(d)[stringr::str_detect(names(d),"^ppcdefecation_")]
vars_ppcabnormurination	<- names(d)[stringr::str_detect(names(d),"^ppcabnormalurination_")]
vars_ppcclammyskin 	<- names(d)[stringr::str_detect(names(d),"^ppcclammyskin_")]
vars_ppcevents <- names(d)[stringr::str_detect(names(d),"^ppcevent")]

smallD <- gaza2019data[ident_dhis2_ppc==T,c("bookorgdistrict",
                                            "bookyear",
                                            vars_ppcevents,
                                            vars_ppcsevabdpain,
                                            vars_ppclungausc,
                                            vars_ppccoughing,
                                            vars_ppcdiffbreath,
                                            vars_ppcinscisorperintear,
                                            vars_ppcexcesstiredness,
                                            vars_ppcsevheadache,
                                            vars_ppcblurryvision,
                                            vars_ppcheartauscult,
                                            vars_ppcextremlightheaded,
                                            vars_ppcseizures,
                                            vars_ppclossofconsc,
                                            vars_ppcsignsofsepsis,
                                            vars_ppcsleepapathydepres,
                                            vars_ppcvomiting,
                                            vars_ppcsignsofedemainhandsorface,
                                            vars_ppclochiavaginaldischargecolor, 
                                            vars_ppcvagdischargebadodor,
                                            vars_ppcbreastinspection,
                                            vars_ppccalfswelling,
                                            vars_ppcruptureduterus,
                                            vars_ppcsignsofdvt,
                                            vars_ppcleakstool,
                                            vars_ppcleakurine,
                                            vars_ppcsignsofshock,
                                            vars_ppcdvtcalfpain,
                                            vars_ppcdvtcalfswelling,
                                            vars_ppcdvtcalftenderness,
                                            vars_ppcbreastcrackednipples,
                                            vars_ppcbreastabnormalsecr,
                                            vars_ppcbreasthotsensation,
                                            vars_ppcbreastredness,
                                            vars_ppcbreastswelling,
                                            vars_ppcbreasttenderness,
                                            vars_ppcincisiontearbleeding,
                                            vars_ppcincisiontearabnormaldischarge,
                                            vars_ppcincisiontearpain,
                                            vars_ppcincisiontearswelling,
                                            vars_ppcdefecation,
                                            vars_ppcabnormurination,
                                            vars_ppcclammyskin ), 
                                            with=F]

smallD[,id:=1:.N]
long <- melt.data.table(smallD,
                        id.vars=c(
                          "id",
                          "bookorgdistrict",
                          "bookyear"
                          ))

uglytable <- long[,.(
                    denominator=.N,
                    is_NA=sum(is.na(value)),
                    not_NA=sum(!is.na(value)),
                    value0=sum(value==0,na.rm=T),
                    value1=sum(value==1,na.rm=T),
                    value2=sum(value==2,na.rm=T),
                    value3=sum(value==3,na.rm=T)),
                    keyby=.(bookyear,variable)]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_MCH_PHC_Conference.xlsx",lubridate::today())))

          




ppctab <- gaza2019data[,.(
                          Denom=.N,
                          Booked=sum(ident_dhis2_booking==T, na.rm=T),
                          AttendenANC=sum(ident_dhis2_an==T, na.rm=T),
                          AttendedPPC=sum(ident_dhis2_ppc==T,na.rm=T),
                          ANCandPPC=sum(ident_dhis2_an==T &
                                          ident_dhis2_ppc==T, na.rm=T)),
                        
                       keyby=.(bookyear)]


openxlsx::write.xlsx(ppctab, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_PPC.xlsx",lubridate::today())))


### NBC ### 

#gives the values inside the variables quotes 
paste0(unique(gaza2019data$nbcsuspectedcongenitalmalformation_1))
paste0(unique(gaza2019data$nbcsuspectedcongenitalmalformation_2))
paste0(unique(gaza2019data$nbcsuspectedcongenitalmalformation_3))

gaza2019data[ident_dhis2_nbc==T, nbcdownsyndrome:=FALSE]
gaza2019data[ident_dhis2_nbc==T, nbcconheart:=FALSE]
gaza2019data[ident_dhis2_nbc==T, nbcneuraltubedef:=FALSE]
gaza2019data[ident_dhis2_nbc==T, nbcnone:=FALSE]
gaza2019data[ident_dhis2_nbc==T, nbcother:=FALSE]

varsnbcsuspectedcong <-names(gaza2019data)[stringr::str_detect(names(gaza2019data),"^nbcsuspectedcongenitalmalformation_")]

for (i in varsnbcsuspectedcong){
  gaza2019data[get(i)=="Down syndrome", nbcdownsyndrome:=TRUE]
  gaza2019data[get(i)=="Neural tubal defect", nbcneuraltubedef:=TRUE]
  gaza2019data[get(i)=="Congenital heart defect",nbcconheart:= TRUE]
  gaza2019data[get(i)=="Other",nbcother:= TRUE]
  gaza2019data[get(i)=="None",nbcnone:= TRUE]
}

nbc <- gaza2019data[,.(N=.N,
                       numNBC=sum(ident_dhis2_nbc==T, na.rm=T),
                       none=sum(nbcnone==T, na.rm=T),
                       downsyndrome=sum(nbcdownsyndrome==T, na.rm=T),
                       neuraltubedefect=sum(nbcneuraltubedef==T, na.rm=T),
                       congenheart=sum(nbcconheart==T, na.rm=T),
                       other=sum(nbcother==T, na.rm=T))]

openxlsx::write.xlsx(nbc, 
                     file.path(
                       FOLDER_DATA_RESULTS_GAZA,
                       "conferences",
                       sprintf("%s_NBC.xlsx",lubridate::today())))





xtabs(~gaza2019data$nbcdownsyndrome, addNA=T)
xtabs(~gaza2019data$nbcneuraltubedef, addNA=T)
xtabs(~gaza2019data$nbcconheart, addNA=T)
xtabs(~gaza2019data$nbcother, addNA=T)
xtabs(~gaza2019data$nbcnone, addNA=T)
