# define our dates
CLINIC_INTERVENTION_DATE <- "2018-04-30"
CLINIC_CONTROL_DATE <- "2018-04-30"

# define the folders
if(.Platform$OS.type=="unix"){
  RAWmisc::InitialiseOpinionatedUnix(project="code_major/2018/pniph_ereg")
  setwd("/git/trial_dofiles")
  FOLDER_DATA_RAW <- RAWmisc::PROJ$RAW
  FOLDER_DATA_CLEAN <- RAWmisc::PROJ$SHARED_TODAY
} else {
  tryCatch({
    setwd("X:/data processing/trial_dofiles")
  }, error=function(err){
    setwd("Z:/data processing/trial_dofiles")
  })
  FOLDER_DATA_RAW <- file.path(getwd(),"../data_raw")
  FOLDER_DATA_CLEAN <- file.path(getwd(),"../data_clean")
  FOLDER_DATA_RESULTS <- file.path(getwd(),"../results/")
  FOLDER_DATA_MBO <- file.path(getwd(),"../results/mbo_r/")
  FOLDER_DROPBOX_RESULTS <- "~/../eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH/Results/"
}

# say which packages we want, and install if neccessary
# (note, this should really only happen once)
desiredPackages <- c("stringr",
                     "lubridate",
                     "data.table",
                     "bit64",
                     "readxl",
                     "openxlsx",
                     "bit64",
                     "haven",
                     "lubridate",
                     "ggplot2",
                     "irr",
                     "rel")
for(i in desiredPackages) if(!i %in% rownames(installed.packages())) install.packages(i)

library(data.table)
library(ggplot2)

# this loads in all the code in the "r_code" folder
# this is the same as going "library(r_code)" (except we cant do that
# because r_code isn't a package)
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# date stuff
MAX_YEAR <- stringr::str_sub(CLINIC_CONTROL_DATE,1,4)
MAX_MONTH <- substr(CLINIC_CONTROL_DATE,6,7)

DATE <- lubridate::today()

weekyear <- sprintf("%s-%s",lubridate::isoyear(lubridate::today()),lubridate::isoweek(lubridate::today()))
yearmonth <- sprintf("%s-%s",
        lubridate::year(lubridate::today()),
        lubridate::month(lubridate::today()))

####################
####################
# CODE STARTS HERE #
####################
####################

timeStart <- Sys.time()

# CLEAN DHIS
dhis=suppressWarnings(DHIS2_Master())
dhis <- dhis[ident_dhis2_booking==1]
keepMotherID <- unique(dhis$motheridno)

# CLEAN AVICENNA
avicenna=AVICENNA_Master(keepMotherID=keepMotherID, includeObs = FALSE)

# MERGE DHIS TO AVICENNA
nrow(dhis)
d <- MergeDHISToAVICENNA(
  dhis=dhis,
  avicenna=avicenna)
nrow(d)

d[,avicennanum:=NULL]
d[,minDate:=NULL]

# delete the datasets "dhis" and "avicenna" to make space in the memory
rm("dhis", envir=.GlobalEnv)
rm("avicenna", envir=.GlobalEnv)

# MERGE DATAFILE WITH HBO
nrow(d)
d <- MergeDHISToAVICENNA(
  dhis=d,
  avicenna=HBO_Master())
nrow(d)

# CALC INDICATORS OLSO
IndicatorsOsloGenerate(d)

timeEndAnalysis <- Sys.time()

# this file saves the dataset to the network drive
# then deletes identifying information
# then saves the anonymized dataset to the dropbox
# then reloads the normal dataset from the network drive
d <- SaveAllDataFiles(d)

timeEndTotal <- Sys.time()

print(sprintf("It took %s min to analyse the data",round(as.numeric(difftime(timeEndAnalysis,timeStart,units="min")),1)))
print(sprintf("It took %s min to save the data",round(as.numeric(difftime(timeEndTotal,timeEndAnalysis,units="min")),1)))
print(sprintf("Total time: %s min",round(as.numeric(difftime(timeEndTotal,timeStart,units="min")),1)))

##################
##################
# CODE ENDS HERE #
##################
##################

####
# PLACE OF DELIVERY INFORMATION CHECKING
# LATER ON, PUT THIS AUTOMATICALLY IN AN EXCEL REPORT
xtabs(~d$cpoplaceofbirth_1+d$ppcplaceofdelivery_1,addNA=T)
####


SaveWomenWithAbortionsIn2017(d)

abortions <- readRDS(file.path(FOLDER_DATA_MBO,"abortions.RDS"))$bookevent

ppcplaceofdelivery

tokeep <- d[
  isExpectedToHaveDelivered==TRUE &
  ident_TRIAL_1==TRUE &
  is.na(d$ident_avic_any) &
  is.na(d$ident_dhis2_dhis2hbo) &
  is.na(d$ident_hbo),
  c("bookevent",
    "motheridno",
    "bookorgdistrict",
    "bookorgname",
    "bookdate",
    "firstname",
    "fathersname",
    "middlename",
    "familyname1",
    "familyname2",
    "husbandsname",
    "dob",
    "mobile",
    "booklmp",
    "village",
    "bookintendbirth",
    "bookrecobirth",
    "calc_expected_due_delivery",
    "ppcplaceofdelivery_1",
    "ppcplaceofdelivery_2",
    "ppcplaceofdelivery_3",
    "ppcplaceofdelivery_4",
    "cpoplaceofbirth_1",
    "cpoplaceofbirth_2",
    "cpoplaceofbirth_3",
    "cpoplaceofbirth_4"
    )]

#gen IS_ABORTION="ABORTION" if bookevent==0492304932
tokeep[bookevent %in% abortions, IS_ABORTION:="ABORTION"]
tokeep[,bookevent:=NULL] # delete bookevent column
sum(tokeep$IS_ABORTION=="ABORTION",na.rm=T)/nrow(tokeep)

#count if IS_ABORTION=="ABORTION" <- STATA
#sum(tokeep$IS_ABORTION=="ABORTION",na.rm=T) <- R

identifiedWomen <- tokeep[,c("motheridno","bookdate")]
identifiedWomen[,identifiedMonth:=yearmonth]

previouslyIdentifiedWomen <- readRDS(file.path(FOLDER_DATA_CLEAN,"identified_women.RDS"))

temp <- merge(identifiedWomen,previouslyIdentifiedWomen,all.x=T,by=c("motheridno","bookdate"))
newWomen <- temp[is.na(identifiedMonth.y) | identifiedMonth.x==identifiedMonth.y]
newWomen[,identifiedMonth.x:=NULL] 
newWomen[,identifiedMonth.y:=NULL] 
newWomen[,identifiedMonth:=yearmonth]

tokeep <- merge(tokeep,newWomen,by=c("motheridno","bookdate"),all.x=T)

womenNew <- tokeep[!is.na(identifiedMonth)]
womenOld <- tokeep[is.na(identifiedMonth)]

womenNew[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),lubridate::month(bookdate))]
womenOld[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),lubridate::month(bookdate))]

for(i in unique(womenOld$bookyearmonth)){
  dir.create(file.path(FOLDER_DATA_MBO,i))
  dataTemp <- womenOld[bookyearmonth==i]
  openxlsx::write.xlsx(dataTemp,
                       file.path(FOLDER_DATA_MBO,i,"Palestine.xlsx"))
  for(j in unique(dataTemp$bookorgdistrict)){
    openxlsx::write.xlsx(dataTemp[bookorgdistrict==j],
                         file.path(FOLDER_DATA_MBO,i,sprintf("%s.xlsx",j)))
  }
}

if(nrow(womenNew)>0) openxlsx::write.xlsx(womenNew,
                                          file.path(FOLDER_DATA_MBO,"new",sprintf("%s.xlsx",yearmonth)))

saveRDS(rbind(previouslyIdentifiedWomen,newWomen),file.path(FOLDER_DATA_CLEAN,"identified_women.RDS"))

# mervett HBO_Completeness
HBO_Completeness(d)

# tamara kappa/percent agreement values
KappaValues(d)

BASE_LINE_STATISTICAL_ANALYSIS(d)
IndicatorsOsloAnalyse(d)

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


