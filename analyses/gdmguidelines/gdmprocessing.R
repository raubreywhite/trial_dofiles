
###Redefining opportinites
###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)


####LOAD d from Network####

d <- LoadDataFileFromNetwork()
d[,ident_WB:=TRUE]

###### SETUP ENDS ######

# add gaza data here as well
g <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                         "full_data_from_r.rds"))
g[,ident_WB:=FALSE]

smallD <- rbind(d,
                g,
                fill=T)









#########################################################
# DISTRIBUTIONS OF GLUCOSE VALUES FOR DIFFERENT TESTING #
#########################################################

labgestage <-names(smallD)[stringr::str_detect(names(smallD),"^labgestage_")]
labgluc <-names(smallD)[stringr::str_detect(names(smallD),"^labbloodglu_")]
labanpp <-names(smallD)[stringr::str_detect(names(smallD),"^labanpp_")]
labogct <-names(smallD)[stringr::str_detect(names(smallD),"^labogct_")]
labfastglu <-names(smallD)[stringr::str_detect(names(smallD),"^labfastbloodglu_")]
labdate <-names(smallD)[stringr::str_detect(names(smallD),"^labdate_")]

glucose <- smallD[ident_dhis2_booking==T,
                  c("bookevent",
                     "motheridno",
                     "bookyear",
                     "ident_WB",
                     labdate,
                     labgestage,
                     labgluc,
                     labogct,
                     labfastglu,
                    labanpp), with=F]


nrow(glucose)

long <- melt(glucose,
     id.vars = c("ident_WB","bookyear","motheridno","bookevent"),
     measure.vars = patterns("^labdate",
                             "^labgestage",
                             "labbloodglu",
                              "labogct",
                             "^labanpp",
                             "labfastbloodglu"),
     variable.name = "visitnum",
     value.name = c("labdate","labgestage", "labgluc","labocgt","labanpp","labfastbloodglu"))

as.data.table(long)

# distriubtions of values for each one of these before 24 weeks, 24-28 weeks, and after 28 weeks

long[,year:=as.factor(lubridate::year(labdate))]
xtabs(~long$year)

toplot <-long[ident_WB==T &
                labgestage>=24 &
                labgestage<=28 &
                labgluc>0 &
                !is.na(labgluc) &
                labanpp=="ANC"]

toplot <- toplot[ident_WB==T,.(numpervalue=.N),
                 keyby=.(year,labgluc)]

# export table with percentages of these values that have one reading 


p <- ggplot(toplot, 
            aes(x=labgluc, y=numpervalue, shape=year, color=year))

p <- p + geom_point()

p <- p + scale_x_continuous("Random Blood Glucose Values", lim=c(25,350))


p <- p + labs(title="Screening:Random Blood Glucose",
              subtitle = "24-28 Weeks",
              xlab="Random Blood Glucose Values")

p <- p + theme(axis.title.x =element_text("Random Blood Glucose Values"),
                 axis.title.y = element_blank())

p



# add cut off points for 5.1, 5.3, and 5.6 mmol/L
# calculator: https://www.omnicalculator.com/health/blood-sugar

# 5.1 mmol/L=91.8 g/dL
# 5.3 mmol/L=

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "gdmguidelines",
  sprintf("rbg_24_28_distribution_%s.png",lubridate::today())), 
  plot = p, width = 297, height = 210, units = "mm")






##############
# fastbloodglu
##############



toplot <-long[labgestage>=29 &
                labgestage<=37 &
                labgluc>0 &
                !is.na(labgluc) &
                labanpp=="ANC"]

toplot <- toplot[,.(numpervalue=.N),
                 keyby=.(ident_WB,year,labbloodglu)]



p <- ggplot(toplot, 
            aes(x=labbloodglu, y=numpervalue, shape=year, color=year))

p <- p + geom_point()

p <- p + scale_x_continuous("Random Blood Glucose Values", lim=c(25,350))


p <- p + labs(title="Screening:Random Blood Glucose",
              subtitle = ">28 Weeks",
              xlab="Random Blood Glucose Values")

p <- p + theme(axis.title.x =element_text("Random Blood Glucose Values"),
               axis.title.y = element_blank())

p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "gdmguidelines",
  sprintf("rbg_after_28_distribution_%s.png",lubridate::today())), 
  plot = p, width = 297, height = 210, units = "mm")




##############
# fastbloodglu
##############

toplot <-long[labgestage>=24 &
                labgestage<=28 &
                labfastbloodglu>0 &
                !is.na(labfastbloodglu) &
                labanpp=="ANC" &
                ident_WB==FALSE]

toplot <- toplot[,.(numpervalue=.N),
                 keyby=.(year,labfastbloodglu)]



p <- ggplot(toplot, 
            aes(x=labfastbloodglu, y=numpervalue, shape=year, color=year))

p <- p + geom_point()

p <- p + scale_x_continuous("Fasting Blood Glucose Values", lim=c(25,200))


p <- p + labs(title="Screening:Fasting Blood Glucose",
              subtitle = "24-28 Weeks",
              xlab="Fasting Blood Glucose Values")

p <- p + theme(axis.title.x =element_text("Fasting Blood Glucose Values"),
               axis.title.y = element_blank())

p

p <- p + geom_vline(xintercept = 92, color="red", linetype="solid")
p <- p + geom_vline(xintercept = 95, color="red", linetype="solid")
p <- p + geom_vline(xintercept = 101, color="red", linetype="solid")



p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "gdmguidelines",
  sprintf("fbs_24_28_distribution_%s.png",lubridate::today())), 
  plot = p, width = 297, height = 210, units = "mm")




##############
# fastbloodglu >28
##############

toplot <-long[labgestage>=29 &
                labgestage<=38 &
                labbloodglu>0 &
                !is.na(labfastbloodglu) &
                labfastbloodglu>0 &
                labanpp=="ANC" &
                ident_WB==FALSE]

toplot <- toplot[,.(numpervalue=.N),
                 keyby=.(year,labfastbloodglu)]


p <- ggplot(toplot, 
            aes(x=labfastbloodglu, y=numpervalue, shape=year, color=year))

p <- p + geom_point()

p <- p + scale_x_continuous("Fasting Blood Glucose Values", lim=c(25,200))
p <- p + scale_y_continuous("", lim=c(0,4))



p <- p + labs(title="Screening:Fasting Blood Glucose",
              subtitle = ">28 Weeks",
              xlab="Fasting Blood Glucose Values")

p <- p + theme(axis.title.x =element_text("Fasting Blood Glucose Values"),
               axis.title.y = element_blank())

p

p <- p + geom_vline(xintercept = 92, color="red", linetype="solid")
p <- p + geom_vline(xintercept = 95, color="red", linetype="solid")
p <- p + geom_vline(xintercept = 101, color="red", linetype="solid")



p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "gdmguidelines",
  sprintf("fbs_after_28_distribution_%s.png",lubridate::today())), 
  plot = p, width = 297, height = 210, units = "mm")



#########################################################
# VARIABLES FOR SCREENING OUTCOMES #
#########################################################

### use gestational ages from creatin further variables
smallD[,bookgestagedays_cats:=cut(bookgestagedays,
                                  breaks=c(-500,0,104,
                                           125,160,167,202,
                                           216,237,244,265,293),
                                  include.lowest=T)]

# MAKE BOOK VISIT FOR ANEMIA
smallD[,booklabhb:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabhb:=labhb_1]

# MAKE BOOK VISIT FOR Laburglu
smallD[,booklaburglu:=as.character(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]

smallD[,booklaburglu:=NULL]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,
       booklaburglu:=laburglu_1]
xtabs(~smallD$booklaburglu)
str(smallD$booklaburglu)
unique(smallD$booklaburglu)

smallD[,booklaburglu:=NULL]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7 & laburglu_1%in%c("NEG","POS"),
       booklaburglu:=laburglu_1]
xtabs(~smallD$booklaburglu)


# MAKE BOOK VISIT FOR LABBLOODGLU
smallD[,booklabbloodglu:=as.integer(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabbloodglu:=labbloodglu_1]
xtabs(~smallD$booklabbloodglu, addNA=T)

# MAKE BOOK VISIT FOR LABBLOODGLU_HIGH
smallD[,booklabbloodglu_high:=as.logical(NA)]
smallD[!is.na(booklabbloodglu),booklabbloodglu_high:=FALSE]
smallD[booklabbloodglu>=140 & booklabbloodglu<500,booklabbloodglu_high:=TRUE]
xtabs(~smallD$booklabbloodglu_high, addNA=T)

# MAKE BOOK VISIT FOR LABFASTBLOODGLU
smallD[,booklabfastbloodglu:=as.numeric(NA)]
smallD[abs(labT1gestagedays_1-bookgestagedays)<7,booklabfastbloodglu:=labfastbloodglu_1]
xtabs(~smallD$booklabfastbloodglu)

# MAKE BOOK VISIT FOR LABfastBLOODGLU_HIGH
smallD[,booklabfastbloodglu_high:=as.logical(NA)]
smallD[!is.na(booklabfastbloodglu),booklabfastbloodglu_high:=FALSE]
smallD[booklabfastbloodglu>126 ,booklabfastbloodglu_high:=TRUE]
xtabs(~smallD$booklabfastbloodglu_high, addNA=T)


###############
# hr at booking
###############
smallD[,bookhrhighsug:=as.logical(NA)]

smallD[(mandate_1-bookdate)<=7,bookhrhighsug:=FALSE] 
smallD[(mandate_1-bookdate)<=7 &
    manperf_1==1 &
    mantypex_1 %in% c("RefHighRisk","RefDiabetes","RefSpec"),bookhrhighsug:=TRUE] 

xtabs(~smallD$bookhrhighsug, addNA=T)


# anT1 in weeks to calculate sfhDiscrep via anexamsfh and anT1gestagedays to weeks
smallD[,anT1gestagedays_0:=bookgestagedays]
vars <- stringr::str_subset(names(smallD), "^anT1gestagedays_")
vars <- stringr::str_remove(vars, "^anT1gestagedays_")
for (i in vars){
  anT1gestagedays <- sprintf("anT1gestagedays_%s",i)
  anT1gAweeks <- sprintf("anT1gAweeks_%s",i)
  
  smallD[, (anT1gAweeks):=floor(get(anT1gestagedays)/7)]
}

gAscheck <- smallD[,c("bookgestagedays",
                      "bookgestage",
                      "anT1gAweeks_0",
                      "anT1gAweeks_1",
                      "anT1gestagedays_1",
                      "angestage_1",
                      "anT1gAweeks_2",
                      "anT1gestagedays_2",
                      "angestage_2",
                      "anT1gAweeks_3",
                      "angestage_3",
                      "anT1gestagedays_3")]


# uspres malpresentation variable
vars_source <- names(d)[stringr::str_detect(names(smallD),"^uspres_")]
vars_outcome <- stringr::str_replace(vars_source, "uspres_", "us_malpres_")

for(i in 1:length(vars_source)){
  var_source <- vars_source[i]
  var_outcome <- vars_outcome[i]
  smallD[get(var_source) %in% c("Trasverse", "Breech"), (var_outcome):="Yes"]
  
}


VisitVariables <- function(smallD,days,variableOfInterestName,variableOfInterestPattern,TruevaluesMin=NULL,TruevaluesMax=NULL,TruevaluesDiscrete=NULL,gestagedaysVariable="anT1gestagedays" ){
  
  if(!is.null(TruevaluesMin) & !is.null(TruevaluesMax) & !is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NOT NULL")
  }
  
  if(is.null(TruevaluesMin) & is.null(TruevaluesMax) & is.null(TruevaluesDiscrete)){
    stop ("ALL TRUE VALUES NULL")
  }
  
  
  # pull out a list of all of the gestage variables
  #browser()
  gestagedaysVariablewithcarrot <- sprintf("^%s",gestagedaysVariable)
  listOfGestAgeVars <- names(smallD)[stringr::str_detect(names(smallD),gestagedaysVariablewithcarrot)]
  listOfInterestVars <- stringr::str_replace(listOfGestAgeVars, gestagedaysVariable,variableOfInterestPattern)
  
  
  for(i in 1:length(days)){
    # name of new variable
    var <- sprintf("TrialOne_%s_%s",variableOfInterestName,names(days)[i])
    # initialize all as FALSE if has booking variable
    smallD[!is.na(ident_dhis2_booking),(var):=FALSE]
    #xtabs(~smallD[[var]])
    
    # loop through the "gestage"/"bp" variables
    for(j in 1:length(listOfGestAgeVars)){
      gestageVar <- listOfGestAgeVars[j]
      interestVar <- listOfInterestVars[j]
      
      #asking discrete question
      if(!is.null(TruevaluesDiscrete)){
        
        smallD[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar) %in% TruevaluesDiscrete ,(var):=TRUE]
        
      }else{ #asking non discrete questions
        
        
        smallD[!is.na(get(var)) & get(gestageVar) %in% days[[i]] & !is.na(get(interestVar)) & get(interestVar)>=TruevaluesMin & get(interestVar)<=TruevaluesMax, (var):=TRUE]
        
      }
      
      
      
    }
  }
  
  return(smallD)
  
  
}



###### identifying outcomes #######

# categories we want
days <- list(
  "00_14"=c(-500:104),
  "15_17"=c(105:125),
  "18_22"=c(126:160),
  "23_23"=c(161:167),
  "24_28"=c(168:202),
  "29_30"=c(203:216),
  "31_33"=c(217:237),
  "34_34"=c(238:244),
  "35_37"=c(245:265),
  "38_41"=c(266:293),
  
  #using below vectors for managementsinstead of using two seperate vectors
  
  "00_00"=0*7+c(0:6),
  "01_01"=1*7+c(0:6),
  "02_02"=2*7+c(0:6),
  "03_03"=3*7+c(0:6),
  "04_04"=4*7+c(0:6),
  "05_05"=5*7+c(0:6),
  "06_06"=6*7+c(0:6),
  "07_07"=7*7+c(0:6),
  "08_08"=8*7+c(0:6),
  "09_09"=9*7+c(0:6),
  "10_10"=10*7+c(0:6),
  "11_11"=11*7+c(0:6),
  "12_12"=12*7+c(0:6),
  "13_13"=13*7+c(0:6),
  "14_14"=14*7+c(0:6),
  "15_15"=15*7+c(0:6),
  "16_16"=16*7+c(0:6),
  "17_17"=17*7+c(0:6),
  "18_18"=18*7+c(0:6),
  "19_19"=9*7+c(0:6),
  "20_20"=20*7+c(0:6),
  "21_21"=21*7+c(0:6),
  "22_22"=22*7+c(0:6),
  #"23_23"=23*7+c(0:6),
  "24_24"=24*7+c(0:6),
  "25_25"=25*7+c(0:6),
  "26_26"=26*7+c(0:6),
  "27_27"=27*7+c(0:6),
  "28_28"=28*7+c(0:6),
  "29_29"=29*7+c(0:6),
  "30_30"=30*7+c(0:6),
  "31_31"=31*7+c(0:6),
  "32_32"=32*7+c(0:6),
  "33_33"=33*7+c(0:6),
  "34_34"=34*7+c(0:6),
  "35_35"=35*7+c(0:6),
  "36_36"=36*7+c(0:6),
  "37_37"=37*7+c(0:6),
  "38_38"=38*7+c(0:6),
  "39_39"=39*7+c(0:6),
  "40_40"=40*7+c(0:6),
  "41_41"=41*7+c(0:6),
  "42_42"=42*7+c(0:6)
  
)


###ANC Visits####
smallD[,anT1gestagedays_0:=bookgestagedays]

smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="anvisitnew",
  variableOfInterestPattern="anT1gestagedays",
  TruevaluesMin=-500,
  TruevaluesMax=260,
  gestagedaysVariable="anT1gestagedays")

smallD[,anT1gestagedays_0:=NULL]
xtabs(~smallD$TrialOne_anvisitnew_00_00)


### Lab RBS Normal ####
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,laburglu_0:=booklaburglu]
# normal urine glucose
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_exists",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = c("POS", "NEG"),
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,laburglu_0:=NULL]
xtabs(~smallD$TrialOne_laburglu_exists_15_17)

# lab urglu pos
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,laburglu_0:=booklaburglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="laburglu_pos",
  variableOfInterestPattern="laburglu",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete =c("POS"),
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,laburglu_0:=NULL]
nrow(smallD[laburglu_1=="POS" & labgestage_1>0 & labgestage_1<=14])
xtabs(~smallD$TrialOne_laburglu_pos_00_14)


# labbloodglu exist
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_exists",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labbloodglu_exists_15_17)

# high blood glucose
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_high",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=140,
  TruevaluesMax=500,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labbloodglu_high_00_14)
xtabs(~smallD$TrialOne_labbloodglu_high_18_22)


# random blood glu normal
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_normal",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=105,
  TruevaluesDiscrete =NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labbloodglu_normal_00_14)
xtabs(~smallD$TrialOne_labbloodglu_normal_18_22)




# Lab FBS likely GDM
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labbloodglu_likelyGDM",
  variableOfInterestPattern="labbloodglu",
  TruevaluesMin=92,
  TruevaluesMax=125,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labbloodglu_likelyGDM_24_28)



# Lab FBS exists
#http://perinatology.com/Reference/Reference%20Ranges/Glucose,%20fasting.htm
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_exists",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=50,
  TruevaluesMax=200,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_exists_15_17)

# Lab FBS Normal
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_normal",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=71,
  TruevaluesMax=91,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_normal_15_17)




# Lab FBS likely GDM
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_likelyGDM",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=92,
  TruevaluesMax=125,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_likelyGDM_24_28)


# Lab FBS Normal
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_normal",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=40,
  TruevaluesMax=95,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_normal_15_17)


# Lab FBS High 
smallD[,labT1gestagedays_0:=bookgestagedays]
smallD[,labfastbloodglu_0:=booklabfastbloodglu]
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="labfastbloodglu_high",
  variableOfInterestPattern="labfastbloodglu",
  TruevaluesMin=126,
  TruevaluesMax=500,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "labT1gestagedays")
smallD[,labT1gestagedays_0:=NULL]
smallD[,labfastbloodglu_0:=NULL]
xtabs(~smallD$TrialOne_labfastbloodglu_high_24_28)

#### US visits ####
# Has US visit
smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_exists",
  variableOfInterestPattern="usT1gestagedays",
  TruevaluesMin=10,
  TruevaluesMax=300,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~smallD$TrialOne_us_exists_00_14)

# US suspected IUGR
smallD <-VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_iugrSuspected",
  variableOfInterestPattern="usiugr",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete = 1,
  gestagedaysVariable ="usT1gestagedays")
xtabs(~smallD$TrialOne_us_iugrSuspected_00_14)

# US expected LGA
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_lgaSuspected",
  variableOfInterestPattern="uslga",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_lgaSuspected_00_14)

# US pres-malpresentation
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="us_malpres",
  variableOfInterestPattern="us_malpres",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete="Yes",
  gestagedaysVariable = "usT1gestagedays")
xtabs(~smallD$TrialOne_us_malpres_00_14)








####Referrals####
# Ref to HR
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHR",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHighRisk",
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefHighRisk" & manT1gestagedays_1>=15 & manT1gestagedays_1<=17])
nrow(smallD[mantypex_1=="RefHighRisk" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_refHR_00_14)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_refHR_00_14)
xtabs(~smallD$TrialOne_refHR_35_37)

# Ref to Hosp
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refHosp",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefHosp",
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefHosp" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_refHosp_00_14)
xtabs(~smallD[ident_dhis2_control==F]$mantypex_1, addNA=T)

# RefDiabetes
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refDiab",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete ="RefDiabetes",
  gestagedaysVariable = "manT1gestagedays")
nrow(smallD[mantypex_1=="RefDiabetes" & mangestage_1>=0 & mangestage_1<=14])
xtabs(~smallD$TrialOne_refDiab_00_14)


# Management Performed
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="manperf",
  variableOfInterestPattern="manperf",
  TruevaluesMin=1,
  TruevaluesMax=1,
  TruevaluesDiscrete = NULL,
  gestagedaysVariable = "manT1gestagedays")
xtabs(~smallD$TrialOne_manperf_18_22)


# refSpec
smallD <- VisitVariables(
  smallD=smallD,
  days=days,
  variableOfInterestName="refSpec",
  variableOfInterestPattern="mantypex",
  TruevaluesMin=NULL,
  TruevaluesMax=NULL,
  TruevaluesDiscrete =c("RefSpec") ,
  gestagedaysVariable = "manT1gestagedays")
xtabs(~smallD$TrialOne_refSpec_18_22)



######### Managements ############

# take into account the 4 weeks after 37



###########################
# FBS management for gaza
###########################


# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manFBSHigh_Diab_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labfastbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manFBSHigh_Diab_26_26)



# High RBG, RefHosp
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHosp_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_Hosp_24_24)


# High RBG, RefHR
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refHR_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_HR_24_24)

# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manRBGHigh_Diab_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    # working only on second check
    # var_secondcheck <- sprintf("TrialOne_labbloodglu_exists_%s_%s", 
    #                            week_later, 
    #                            week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_mangdm):=get(var_temp_mangdm)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manRBGHigh_Diab_24_24)



###########################
# FBS management for gaza
###########################


# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("T2_manFBSHigh_Diab_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("T2_labfastbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("T2_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$T2_manFBSHigh_Diab_26_26)




###########################
# FBS management for gaza
###########################


# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manFBSHigh_spec_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labfastbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refSpec_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manFBSHigh_spec_26_26)



# High RBG, RefDIAB
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:1), width=2, flag="0")
  
  #output variable
  var_mangdm <- sprintf("TrialOne_manFBSHigh_Diab_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_mangdm <- "temp_mangdm"
  
  #id source
  var_badgdm <- sprintf("TrialOne_labfastbloodglu_high_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_mangdm):=as.logical(NA)]
  
  # is false, if you have a bad hb
  smallD[get(var_badgdm)==TRUE, (var_temp_manperf):=FALSE]
  smallD[get(var_badgdm)==TRUE, (var_temp_mangdm):=FALSE]
  
  
  for(week_later in weeks_later){
    # working only on manerf check
    var_secondcheck <- sprintf("TrialOne_refDiab_%s_%s", 
                               week_later, 
                               week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_secondcheck)==TRUE, (var_temp_manperf):=TRUE]
    
    smallD[get(var_temp_mangdm)==FALSE  & get(var_secondcheck)==TRUE, (var_temp_mangdm):=TRUE]
  }
  #making var for high blood glu 
  smallD[,(var_mangdm):=as.logical(NA)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_mangdm):=get(var_temp_mangdm) & get(var_temp_manperf)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_mangdm):=NULL]
  
}
xtabs(~smallD$TrialOne_manFBSHigh_Diab_26_26)





########################## Referred for any management ##########################

############ Ref Hosp ####################
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:0), width=2, flag="0")
  
  #output variable
  var_refHosp <- sprintf("TrialOne_manRef_Hosp_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_refHosp <- "temp_refHosp"
  
  #id source
  var_refHospsource <- sprintf("TrialOne_refHosp_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_refHosp):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  smallD[get(var_refHospsource)==TRUE, (var_temp_manperf):=FALSE]
  
  # everyone
  #smallD[!is.na(get(var_refHospsource)), (var_temp_refHosp):=FALSE]
  
  # control
  smallD[get(var_refHospsource)==TRUE, (var_temp_refHosp):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  smallD[,(var_refHosp):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_refHosp):=get(var_temp_refHosp)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_refHosp):=get(var_temp_manperf) & 
           get(var_temp_refHosp)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_refHosp):=NULL]
  
}
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_Hosp_35_35)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_Hosp_35_35)
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_Hosp_32_32)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_Hosp_32_32)

checkHosp <- smallD[!is.na(TrialOne_manRef_Hosp_32_32) &
                      ident_dhis2_control==F, c("TrialOne_manperf_32_32",
                                                "TrialOne_refHosp_32_32",
                                                "TrialOne_manRef_Hosp_32_32")]


########## Ref HR for any reason at any time point #########
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:0), width=2, flag="0")
  
  #output variable
  var_refHR <- sprintf("TrialOne_manRef_HR_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_refHR <- "temp_refHR"
  
  #id source
  var_refHRsource <- sprintf("TrialOne_refHR_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_refHR):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  smallD[get(var_refHRsource)==TRUE, (var_temp_manperf):=FALSE]
  
  
  # control
  smallD[get(var_refHRsource)==TRUE, (var_temp_refHR):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  smallD[,(var_refHR):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_refHR):=get(var_temp_refHR)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_refHR):=get(var_temp_manperf) & 
           get(var_temp_refHR)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_refHR):=NULL]
  
}
xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_HR_35_35)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_HR_35_35)

xtabs(~smallD[ident_dhis2_control==T]$TrialOne_manRef_HR_20_20)
xtabs(~smallD[ident_dhis2_control==F]$TrialOne_manRef_HR_20_20)

checkHR <- smallD[!is.na(TrialOne_manRef_HR_20_20) &
                    ident_dhis2_control==F, c("TrialOne_manperf_20_20",
                                              "TrialOne_refHR_20_20",
                                              "TrialOne_manRef_HR_20_20")]



## ref spec

########## Ref HR for any reason at any time point #########
for(i in 0:37){
  
  # make sure everything has 2 digits (with 0 in front)
  week_current <- formatC(i, width=2, flag="0")
  weeks_later <- formatC(i+c(0:0), width=2, flag="0")
  
  #output variable
  var_refHR <- sprintf("TrialOne_manRef_spec_%s_%s", week_current, week_current)
  var_temp_manperf <- "temp_manperf"
  var_temp_refHR <- "temp_refHR"
  
  #id source
  var_refHRsource <- sprintf("TrialOne_refSpec_%s_%s", week_current, week_current)
  
  # no one has anything
  smallD[,(var_temp_manperf):=as.logical(NA)]
  smallD[,(var_temp_refHR):=as.logical(NA)]
  
  # is false, if you have a referral
  # intervention
  smallD[get(var_refHRsource)==TRUE, (var_temp_manperf):=FALSE]
  
  
  # control
  smallD[get(var_refHRsource)==TRUE, (var_temp_refHR):=TRUE]
  
  
  for(week_later in weeks_later){
    # working only on manperf check
    var_manperf <- sprintf("TrialOne_manperf_%s_%s", 
                           week_later, 
                           week_later)
    # if they have “bad management” (currently) and “good second check” then turn their management into “good management”
    smallD[get(var_temp_manperf)==FALSE & 
             get(var_manperf)==TRUE, (var_temp_manperf):=TRUE]
    
    
  }
  #making var for high blood glu 
  smallD[,(var_refHR):=as.logical(NA)]
  
  #control
  smallD[ident_dhis2_control==T,(var_refHR):=get(var_temp_refHR)]
  
  #intervention
  smallD[ident_dhis2_control==F,(var_refHR):=get(var_temp_manperf) & 
           get(var_temp_refHR)]
  
  #delete these variables because will use them in the subsequent loops we make
  
  smallD[,(var_temp_manperf):=NULL]
  smallD[,(var_temp_refHR):=NULL]
  
}

xtabs(~smallD$TrialOne_manRef_spec_35_35)


openxlsx::write.xlsx(smallD,
                     file.path(FOLDER_DATA_CLEAN,
                               "gdmguidelinesdata",
                               "gdmdata.xlsx"))

#####################################################################################
### Outcomes for screening guidelines
#####################################################################################

# load in gdmdata instead

###############
# hr at booking
###############
smallD[,bookhrhighsug:=as.logical(NA)]

smallD[(mandate_1-bookdate)<=7,bookhrhighsug:=FALSE] 
smallD[(mandate_1-bookdate)<=7 &
    manperf_1==1 &
    mantypex_1 %in% c("RefHighRisk","RefDiabetes","RefSpec"),bookhrhighsug:=TRUE] 

xtabs(~smallD$bookhrhighsug, addNA=T)



smallD[,bookhrhighsug:=as.logical(NA)]

smallD[(mandate_1-bookdate)<=7,bookhrhighsug:=FALSE] 
smallD[(mandate_1-bookdate)<=7 &
    manperf_1==1 &
    mantypex_1 %in% c("RefHighRisk","RefDiabetes","RefSpec"),bookhrhighsug:=TRUE] 

xtabs(~smallD$bookhrhighsug, addNA=T)



################
# B4 24 weeks
################
smallD[,Opportunity_GDM_screening_b4_24:=as.logical(NA)] 

# before 24
smallD[bookgestagedays_cats %in% c("(0,104]",
                                   "(104,125]",
                                   "(125,160]",
                                   "(160,167]"),Opportunity_GDM_screening_b4_24:=TRUE]

xtabs(~smallD$Opportunity_GDM_screening_b4_24, addNA=T)


################
# opportunities
################

#Screening before 24 weeks: Creating one var for 3 possibilities
#laburglu

smallD[,GDMscreeningontime_b4_24_bookurglu_normal:=as.logical(NA)]

smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         (!is.na(booklaburglu)), GDMscreeningontime_b4_24_bookurglu_normal:=FALSE]


smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         GDMscreeningontime_b4_24_bookurglu_normal==FALSE &
         (booklaburglu=="NEG"), 
       GDMscreeningontime_b4_24_bookurglu_normal:=TRUE]

xtabs(~smallD$GDMscreeningontime_b4_24_bookurglu_normal,addNA=T)



# fastbloodglu
smallD[,GDMscreeningontime_b4_24_bookfastbloodglu_normal:=as.logical(NA)]

smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         (!is.na(booklabfastbloodglu)), GDMscreeningontime_b4_24_bookfastbloodglu_normal:=FALSE]


smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         GDMscreeningontime_b4_24_bookfastbloodglu_normal==FALSE &
         (booklabfastbloodglu_high=="FALSE"), 
       GDMscreeningontime_b4_24_bookfastbloodglu_normal:=TRUE]

xtabs(~smallD$GDMscreeningontime_b4_24_bookfastbloodglu_normal,addNA=T)


# booklabbloodglu
smallD[,GDMscreeningontime_b4_24_bookbloodglu_normal:=as.logical(NA)]

smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         (!is.na(booklabbloodglu)), GDMscreeningontime_b4_24_bookbloodglu_normal:=FALSE]


smallD[Opportunity_GDM_screening_b4_24==TRUE & 
         GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
         (booklabbloodglu_high=="FALSE"), 
       GDMscreeningontime_b4_24_bookbloodglu_normal:=TRUE]


xtabs(~smallD$GDMscreeningontime_b4_24_bookbloodglu_normal,addNA=T)

# managements
##################################### QUESTION ##################################### 
### question: are we keeping the rbs and fbs at booking as well? 
### or would it be at any time point?####
###################################################################################

# do this week by week, because management has to be up to one week later
# gdm screening if have pos urglu and have h
smallD[,GDMscreeningontime_b4_24_manposurglu:=as.logical(NA)]
smallD[GDMscreeningontime_b4_24_bookurglu_normal==FALSE,GDMscreeningontime_b4_24_manposurglu:=FALSE]

xtabs(~smallD$GDMscreeningontime_b4_24_manposurglu, addNA=T)

smallD[GDMscreeningontime_b4_24_manposurglu==F &
         (!is.na(booklabbloodglu)|
            !is.na(booklabfastbloodglu)), 
         GDMscreeningontime_b4_24_manposurglu:=TRUE]

xtabs(~smallD$GDMscreeningontime_b4_24_manposurglu, addNA=T)


# management of high rbg and fbs
smallD[,GDMscreeningontime_b4_24_manhighrbs:=as.logical(NA)]
smallD[GDMscreeningontime_b4_24_bookbloodglu_normal==FALSE &
         is.na(GDMscreeningontime_b4_24_manposurglu) &
         (booklabbloodglu_high==T|booklabfastbloodglu_high==T),
           GDMscreeningontime_b4_24_manhighrbs:=FALSE]

xtabs(~smallD$GDMscreeningontime_b4_24_manhighrbs, addNA=T)

smallD[GDMscreeningontime_b4_24_manhighrbs==F &
         (bookhrhighsug==T), GDMscreeningontime_b4_24_manhighrbs:=TRUE]

xtabs(~smallD$GDMscreeningontime_b4_24_manhighrbs, addNA=T)



################################
## 24-28 weeks
################################



###################
#  opportunity
###################
smallD[,Opportunity_GDM_screening_24_28:=as.logical(NA)]


#24-28
smallD[TrialOne_anvisitnew_24_28==T |
         bookgestagedays_cats %in% c("(167,202]"),Opportunity_GDM_screening_24_28:=TRUE]

## Remove opportunities for people who had high blood glu
smallD[Opportunity_GDM_screening_24_28==TRUE &
         (TrialOne_labbloodglu_high_00_14==T|
            TrialOne_labbloodglu_high_15_17==T|
            TrialOne_labbloodglu_high_18_22==T|
            TrialOne_labbloodglu_high_23_23==T|
            TrialOne_labfastbloodglu_high_00_14==T|
            TrialOne_labfastbloodglu_high_15_17==T|
            TrialOne_labfastbloodglu_high_18_22==T|
            TrialOne_labfastbloodglu_high_23_23==T),
       Opportunity_GDM_screening_24_28:=FALSE]


xtabs(~smallD$Opportunity_GDM_screening_24_28, addNA=T)


###################
# screening 24-28
###################


smallD[,GDMscreeningontime_24_28:=as.logical(NA)]
smallD[Opportunity_GDM_screening_24_28==TRUE &
         (TrialOne_labfastbloodglu_exists_24_28==T|
            TrialOne_labbloodglu_exists_24_28==T),GDMscreeningontime_24_28:=T]

xtabs(~smallD$GDMscreeningontime_24_28,addNA=T)

# normal values
smallD[,GDMscreeningontime_24_28_normal:=as.logical(NA)]


smallD[GDMscreeningontime_24_28==T &
         (TrialOne_labfastbloodglu_exists_24_28==T|
            TrialOne_labbloodglu_exists_24_28==T), GDMscreeningontime_24_28_normal:=F]

smallD[Opportunity_GDM_screening_24_28==TRUE & 
         GDMscreeningontime_24_28_normal==F &
         ((TrialOne_labfastbloodglu_normal_24_28==T & 
             TrialOne_labfastbloodglu_exists_24_28==T)|
            (TrialOne_labbloodglu_exists_24_28==T & 
            TrialOne_labbloodglu_normal_24_28==T)),GDMscreeningontime_24_28_normal:=TRUE]

xtabs(~smallD$GDMscreeningontime_24_28_normal, addNA=T)

###################
# management 24-28
###################


# highrbg 24 weeks
smallD[,GDMscreeningontime_24_24_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         (TrialOne_labbloodglu_high_24_24==T |
            TrialOne_labfastbloodglu_high_24_24==T),GDMscreeningontime_24_24_manhighrbg:=F]

xtabs(~smallD$GDMscreeningontime_24_24_manhighrbg, addNA=T)


smallD[GDMscreeningontime_24_24_manhighrbg==F &
         (TrialOne_manRBGHigh_Diab_24_24==T|
            TrialOne_manRef_HR_24_24==T|
            TrialOne_manRef_spec_24_24==T|
            TrialOne_manFBSHigh_Diab_24_24==T),GDMscreeningontime_24_24_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_24_24_manhighrbg, addNA=T)


# highrbg 25 weeks
smallD[,GDMscreeningontime_25_25_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         (TrialOne_labbloodglu_high_25_25==T |
            TrialOne_labfastbloodglu_high_25_25==T),GDMscreeningontime_25_25_manhighrbg:=F]


smallD[GDMscreeningontime_25_25_manhighrbg==F &
         (TrialOne_manRBGHigh_Diab_25_25==T|
            TrialOne_manRef_HR_25_25==T|
            TrialOne_manRef_spec_25_25==T|
            TrialOne_manFBSHigh_Diab_25_25==T),GDMscreeningontime_25_25_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_25_25_manhighrbg, addNA=T)

# highrbg 26 weeks
smallD[,GDMscreeningontime_26_26_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         (TrialOne_labbloodglu_high_26_26==T |
            TrialOne_labfastbloodglu_high_26_26==T),GDMscreeningontime_26_26_manhighrbg:=F]


smallD[GDMscreeningontime_26_26_manhighrbg==F &
         (TrialOne_manRBGHigh_Diab_26_26==T|
            TrialOne_manRef_HR_26_26==T|
            TrialOne_manRef_spec_26_26==T|
            TrialOne_manFBSHigh_Diab_26_26==T), GDMscreeningontime_26_26_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_26_26_manhighrbg, addNA=T)

# highrbg 27 weeks
smallD[,GDMscreeningontime_27_27_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         (TrialOne_labbloodglu_high_27_27==T |
            TrialOne_labfastbloodglu_high_27_27==T),GDMscreeningontime_27_27_manhighrbg:=F]


smallD[GDMscreeningontime_27_27_manhighrbg==F &
         (TrialOne_manRBGHigh_Diab_27_27==T|
            TrialOne_manRef_HR_27_27==T|
            TrialOne_manRef_spec_27_27==T|
            TrialOne_manFBSHigh_Diab_27_27==T), GDMscreeningontime_27_27_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_27_27_manhighrbg, addNA=T)

# highrbg 28 weeks
smallD[,GDMscreeningontime_28_28_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
       GDMscreeningontime_24_28_normal==FALSE &
         (TrialOne_labbloodglu_high_28_28==T |
            TrialOne_labfastbloodglu_high_28_28==T),GDMscreeningontime_28_28_manhighrbg:=F]


smallD[GDMscreeningontime_28_28_manhighrbg==F &
         (TrialOne_manRBGHigh_Diab_28_28==T|
            TrialOne_manRef_HR_28_28==T|
            TrialOne_manRef_spec_28_28==T|
            TrialOne_manFBSHigh_Diab_28_28==T), GDMscreeningontime_28_28_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_28_28_manhighrbg, addNA=T)


# combined group

smallD[,GDMscreeningontime_24_28_manhighrbg:=as.logical(NA)]
smallD[GDMscreeningontime_24_28==T &
         GDMscreeningontime_24_28_normal==FALSE &
         (!is.na(GDMscreeningontime_24_24_manhighrbg)|
            !is.na(GDMscreeningontime_25_25_manhighrbg)|
            !is.na(GDMscreeningontime_26_26_manhighrbg)|
            !is.na(GDMscreeningontime_27_27_manhighrbg)|
            !is.na(GDMscreeningontime_28_28_manhighrbg)),
       GDMscreeningontime_24_28_manhighrbg:=F]

xtabs(~smallD$GDMscreeningontime_24_28_manhighrbg, addNA=T)


smallD[GDMscreeningontime_24_28_manhighrbg==F & 
         (GDMscreeningontime_24_24_manhighrbg==T|
          GDMscreeningontime_25_25_manhighrbg==T|
          GDMscreeningontime_26_26_manhighrbg==T|
          GDMscreeningontime_27_27_manhighrbg==T|
          GDMscreeningontime_28_28_manhighrbg==T),
       GDMscreeningontime_24_28_manhighrbg:=T]

xtabs(~smallD$GDMscreeningontime_24_28_manhighrbg, addNA=T)

if(smallD[ident_WB==T]){
  
  # intermediate values
  
  # intermediate values,  but dont want them for WB because management is in free text
  smallD[,GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  smallD[GDMscreeningontime_24_28==T &
           GDMscreeningontime_24_28_normal==FALSE &
           is.na(GDMscreeningontime_24_28_manhighrbg) &
           (TrialOne_labbloodglu_likelyGDM_24_28==T|
              TrialOne_labfastbloodglu_likelyGDM_24_28==T),GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~smallD$GDMscreeningontime_24_28_intmbg, addNA=T)
  # managment is repeat FBS with in 3 weeks
  
  # do this by one week intervals
  
  # 24 weeks
  smallD[GDMscreeningontime_24_28_intmbg==T &
           TrialOne_labfastbloodglu_likelyGDM_24_24==T|
           TrialOne_labbloodglu_likelyGDM_24_24==T,GDMscreeningontime_24_24_manintmbg:=FALSE]
  
  smallD[GDMscreeningontime_24_24_manintmbg==F &
           (TrialOne_labfastbloodglu_likelyGDM_24_24==T|
              TrialOne_labbloodglu_likelyGDM_24_24==T) &
           TrialOne_repeatFBS_27_27==T,GDMscreeningontime_24_24_manintmbg:=T]
  
  
  # 25 weeks
  smallD[GDMscreeningontime_24_28_intmbg==T &
           TrialOne_labfastbloodglu_likelyGDM_25_25==T,GDMscreeningontime_25_25_manintmbg:=FALSE]
  
  smallD[GDMscreeningontime_25_25_manintmbg==F &
           (TrialOne_labfastbloodglu_likelyGDM_25_25==T|
              TrialOne_labbloodglu_likelyGDM_25_25==T) &
           TrialOne_repeatFBS_28_28==T,GDMscreeningontime_25_25_manintmbg:=T]
  
  
  # 26 weeks
  smallD[GDMscreeningontime_24_28_intmbg==T &
           TrialOne_labfastbloodglu_likelyGDM_26_26==T|
           TrialOne_labbloodglu_likelyGDM_26_26==T,GDMscreeningontime_26_26_manintmbg:=FALSE]
  
  smallD[GDMscreeningontime_26_26_manintmbg==F &
           (TrialOne_labfastbloodglu_likelyGDM_26_26==T |
              TrialOne_labbloodglu_likelyGDM_26_26==T) &
           TrialOne_repeatFBS_29_29==T,GDMscreeningontime_26_26_manintmbg:=T]
  
  # 27 weeks
  smallD[GDMscreeningontime_24_28_intmbg==T &
           (TrialOne_labfastbloodglu_likelyGDM_27_27==T |
              TrialOne_labbloodglu_likely_GDM_27_27==T),GDMscreeningontime_27_27_manintmbg:=FALSE]
  
  smallD[GDMscreeningontime_27_27_manintmbg==F &
           (TrialOne_labfastbloodglu_likelyGDM_27_27==T |
              TrialOne_labbloodglu_likely_GDM_27_27==T) &
           TrialOne_repeatFBS_30_30==T,GDMscreeningontime_27_27_manintmbg:=T]
  
  
  # 28 weeks
  smallD[GDMscreeningontime_24_28_intmbg==T &
           (TrialOne_labfastbloodglu_likelyGDM_28_28==T |
              TrialOne_labbloodglu_likely_GDM_28_28==T),GDMscreeningontime_28_28_manintmbg:=FALSE]
  
  smallD[GDMscreeningontime_28_28_manintmbg==F &
           (TrialOne_labfastbloodglu_likelyGDM_28_28==T |
              TrialOne_labbloodglu_likely_GDM_28_28==T) &
           TrialOne_repeatFBS_31_31==T,GDMscreeningontime_28_28_manintmbg:=T]
  
  
  
  # combined variable
  
  smallD[,GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  smallD[GDMscreeningontime_24_28_intmbg==T &
           (!is.na(GDMscreeningontime_28_28_manintmbg)|
              !is.na(GDMscreeningontime_27_27_manintmbg)|
              !is.na(GDMscreeningontime_26_26_manintmbg)|
              !is.na(GDMscreeningontime_25_25_manintmbg)|
              !is.na(GDMscreeningontime_24_24_manintmbg)),GDMscreeningontime_24_28_manintmbg:=FALSE ]
  
  smallD[GDMscreeningontime_24_28_manintmbg==F &
           (GDMscreeningontime_24_24_manintmbg==T |
              GDMscreeningontime_25_25_manintmbg==T|
              GDMscreeningontime_26_26_manintmbg==T|
              GDMscreeningontime_27_27_manintmbg==T|
              GDMscreeningontime_28_28_manintmbg==T), GDMscreeningontime_24_28_manintmbg:=TRUE]
  
  xtabs(~smallD$GDMscreeningontime_24_28_manintmbg, addNA=T)
  
}else{
  
  # intermediate values,  but dont want them for WB because management is in free text
  smallD[,GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  smallD[GDMscreeningontime_24_28==T &
           GDMscreeningontime_24_28_normal==FALSE &
           is.na(GDMscreeningontime_24_28_manhighrbg) &
           (TrialOne_labbloodglu_likelyGDM_24_28==T),GDMscreeningontime_24_28_intmbg:=TRUE]
  
  xtabs(~smallD$GDMscreeningontime_24_28_intmbg, addNA=T)
  
  
  
  smallD[,GDMscreeningontime_24_28_intmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_24_24_manintmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_25_25_manintmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_26_26_manintmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_27_27_manintmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_28_28_manintmbg:=as.logical(NA)]
  smallD[,GDMscreeningontime_24_28_manintmbg:=as.logical(NA)]
  
}

xtabs(~smallD$GDMscreeningontime_24_28, addNA = T)
xtabs(~smallD$GDMscreeningontime_24_28_normal, addNA = T)
xtabs(~smallD$GDMscreeningontime_24_28_manhighrbg, addNA = T)
xtabs(~smallD$GDMscreeningontime_24_28_intmbg, addNA = T)


################
# > 28 weeks
################
smallD[,Opportunity_GDM_screening_after_28:=as.logical(NA)]

## defining opportunities
# after 28
smallD[bookgestagedays_cats %in% c("(202,216]",
                                   "(216,237]",
                                   "(237,244]",
                                   "(244,265]"),Opportunity_GDM_screening_after_28:=TRUE]
xtabs(~smallD$Opportunity_GDM_screening_after_28, addNA=T)

## defining successes
smallD[,GDMscreeningontime_after_28:=as.logical(NA)]
smallD[,GDMscreeningontime_after_28_normal:=as.logical(NA)]
smallD[,GDMscreeningontime_after_28_high:=as.logical(NA)]
smallD[,GDMscreeningontime_after_28_intmd:=as.logical(NA)]


# anyone who has a fasting or blood glu value and booked after 28 weeks
smallD[Opportunity_GDM_screening_after_28==TRUE, 
       GDMscreeningontime_after_28:=FALSE]

smallD[GDMscreeningontime_after_28==F &
         (!is.na(booklabbloodglu)|
            !is.na(booklabfastbloodglu)), 
       GDMscreeningontime_after_28:=TRUE]

#xtabs(~smallD$screenafter28, addNA=T)
xtabs(~smallD$GDMscreeningontime_after_28, addNA=T)

#normal Values/ negative values
smallD[,GDMscreeningontime_after_28_normal:=as.logical(NA)]

smallD[Opportunity_GDM_screening_after_28==TRUE & 
         GDMscreeningontime_after_28==T,
       GDMscreeningontime_after_28_normal:=FALSE]

smallD[GDMscreeningontime_after_28_normal==FALSE &
         ((booklabbloodglu_high==FALSE &
             !is.na(booklabbloodglu))|
            (booklabfastbloodglu_high==F &
               !is.na(booklabfastbloodglu))),GDMscreeningontime_after_28_normal:=T ]

xtabs(~smallD$GDMscreeningontime_after_28_normal, addNA=T)


# high values
smallD[,GDMscreeningontime_after_28_high:=as.logical(NA)]
smallD[GDMscreeningontime_after_28_normal==FALSE,GDMscreeningontime_after_28_high:=FALSE]


# management
smallD[GDMscreeningontime_after_28_high==FALSE &
         bookhrhighsug==T,
       GDMscreeningontime_after_28_high:=TRUE]
xtabs(~smallD$GDMscreeningontime_after_28_high, addNA=T)


xtabs(~smallD$GDMscreeningontime_after_28, addNA=T)
xtabs(~smallD$GDMscreeningontime_after_28_normal, addNA=T)
xtabs(~smallD$GDMscreeningontime_after_28_high, addNA=T)

#########################
# calculating outcomes
#########################


tab <- smallD[,.(N=.N,
                 Opport_screen_b424=sum(Opportunity_GDM_screening_b4_24==T, na.rm=T),
                 Screen_b4_24_normal=sum(GDMscreeningontime_b4_24_bookurglu_normal==T,na.rm=T),
                 Posurglu_b4_24=sum(!is.na(GDMscreeningontime_b4_24_manposurglu)),
                 manposurglu_b4_24=sum(GDMscreeningontime_b4_24_manposurglu==T,na.rm=T),
                 Oppt_screen_24_28=sum(Opportunity_GDM_screening_24_28==T,na.rm=T),
                 Screen_24_28_normal=sum(GDMscreeningontime_24_28_normal==T,na.rm=T),
                 High_24_28=sum(GDMscreeningontime_24_28_normal==F,na.rm=T),
                 High_24_28_highrbg=sum(!is.na(GDMscreeningontime_24_28_manhighrbg)),
                 Man_high_24_28=sum(GDMscreeningontime_24_28_manhighrbg==T,na.rm=T),
                 Oppt_screen_after_28=sum(Opportunity_GDM_screening_after_28==T,na.rm=T),
                 Screen_after_28=sum(GDMscreeningontime_after_28==T,na.rm=T),
                 High_after_28=sum(!is.na(GDMscreeningontime_after_28_high)),
                 manhighrbg_after_28=sum(GDMscreeningontime_after_28_high==T, na.rm=T)),
              keyby=.(bookyear,ident_gaza)]



openxlsx::write.xlsx(tab,
                     file.path(FOLDER_DATA_REsULTs,
                               "gdmguidelines",
                               "screenings_and_managements.xlsx"))



#########################
# PPC guidelines
#########################


ppcgdm <- smallD[ident_dhis2_ppc==T &
                   (GDMscreeningontime_normal_24_28==F|
                      GDMscreeningontime_normal_after-28==F),]

# anyone with gdm at anc
# do fbs at first ppc visit
# normal --> repeat after 6 weeks; normal do nothing, abnormal do after 12 weeks of delivery
# abnormal fbs at first ppc visit --> refer to diabetes clinic if high; if normal, do nothing

ppcgdm[]




















