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

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

tryCatch({
  setwd("X:/data processing/a research hRHR/trial 1/used/list of control files")
}, error=function(err){
  setwd("Z:/data processing/a research hRHR/trial 1/used/list of control files")
})

library(data.table)
library(ggplot2)

# import excel using "stamping_number.xlsx", clear firstrow
allNum <- readxl::read_excel("data_raw/stamping_number.xlsx")
recNum <- readxl::read_excel("data_raw/Control rec F.xlsx", sheet="Received files")

# turn into data tables
setDT(allNum)
setDT(recNum)

setnames(allNum,c(
  "clinic",
  "district",
  "estNumFiles",
  "codeStart",
  "codeEnd",
  "excludeStart",
  "excludeEnd",
  "notes"))

# only changes one name
#setnames(allNum,"District","newname2")

# only changes two names (or more if you want to put it in)
#setnames(allNum,
#         c("Clinic name in English",
#           "District"),
#         c("newname1",
#           "newname2"))

setnames(recNum,c("received"))

# CHECK TO SEE IF THERE ARE DOUBLES IN recNum
recNum[,numOfTimesObserved:=.N,by=received]
openxlsx::write.xlsx(
  x=recNum[numOfTimesObserved>1],
  file="results/doubles_in_received_numbers.xlsx"
)
# doesnt matter which one you use, but the above one is
# slightly easier to understand
# because you label all of the arguments
#openxlsx::write.xlsx(
#  recNum[numOfTimesObserved>1],
#  "doubles_in_received_numbers.xlsx"
#)

uniqueClinics <- unique(allNum$clinic)
minNumber <- min(allNum$codeStart)
maxNumber <- max(allNum$codeEnd)

# skeleton is a data frame for what we need to do 

skeleton <- expand.grid(
  clinic=uniqueClinics,
  sent=c(minNumber:maxNumber)
)

setDT(skeleton)

nrow(skeleton)
# joinby clinic using allNum, unm(b)

# skeleton is x
# allNum is y
# all.x=T means "keep everything in skeleton, even if not matched by something in allnum"
skeleton <- merge(skeleton,allNum,
                  by="clinic",all.x=T)
nrow(skeleton)

# only keep women whose numbers we actually want
nrow(skeleton)
skeleton <- skeleton[sent>=codeStart & sent<=codeEnd]
nrow(skeleton)


# find all the skeleton that exist in recieved
skeleton$sent == 2252
skeleton$sent[skeleton$sent == 2252]
skeleton$sent[skeleton$sent %in% recNum$received]
skeleton[skeleton$sent %in% recNum$received]

# find all received that dont exist in skeleton
recNum$received[!recNum$received %in% skeleton$sent]

openxlsx::write.xlsx(
  x=recNum[!recNum$received %in% skeleton$sent],
  file="results/possibley_wrong_numbers_inthercieved.xlsx"
)

###now we want a list of reciev recieved files per clinic

# clean the clinic names
skeleton[,clinic:=stringr::str_replace_all(clinic,"[0-9]","")]
skeleton[,clinic:=stringr::str_replace_all(clinic," ","")]
skeleton[,clinic:=stringr::str_replace_all(clinic,"-","")]
skeleton[,clinic:=stringr::str_to_lower(clinic)]
unique(skeleton$clinic)
xtabs(~skeleton$district,addNA=T)

skeleton[,codeStart:=NULL]

## for matched
wbMatched <- openxlsx::createWorkbook("Test")
for(i in 1:5){
  name <- unique(skeleton$district)[i]
  
  matched <- skeleton[skeleton$sent %in% recNum$received &
                        district==name]
  
  setorder(matched,clinic,sent)
  
  matched[,obsNum:=floor(0:(.N-1)/12),by=clinic]
  matched[,obsNumByFloor:=1:.N,by=.(clinic,obsNum)]
  # reshaping from long to wide
  # reshape wide sent, i(clinic obsNum) j(obsNumByFloor)
  matched <- dcast.data.table(matched,clinic+obsNum~obsNumByFloor,value.var = "sent")
  matched[,obsNum:=NULL]
  
  openxlsx::addWorksheet(wbMatched, name)
  openxlsx::writeData(wbMatched, sheet=i, matched)
}
openxlsx::saveWorkbook(wbMatched, "results/received_filesperclinic.xlsx", overwrite=T)

## for missing or not recieved until now

wbMissing <- openxlsx::createWorkbook("Test")
for(i in 1:5){
  name <- unique(skeleton$district)[i]
  
  missing <- skeleton[!skeleton$sent %in% recNum$received &
                        district==name]
  
  setorder(missing,clinic,sent)
  
  missing[,obsNum:=floor(0:(.N-1)/12),by=clinic]
  missing[,obsNumByFloor:=1:.N,by=.(clinic,obsNum)]
  # reshaping from long to wide
  # reshape wide sent, i(clinic obsNum) j(obsNumByFloor)
  missing <- dcast.data.table(missing,clinic+obsNum~obsNumByFloor,value.var = "sent")
  missing[,obsNum:=NULL]
  
  openxlsx::addWorksheet(wbMissing, name)
  openxlsx::writeData(wbMissing, sheet=i, missing)
}
openxlsx::saveWorkbook(wbMissing, "results/not_received_filesperclinic.xlsx", overwrite=T)


########### 2018-08-06
### CON ANC File Number
d <- LoadDataFileFromNetwork()
d <- d[bookdate>="2017-01-01" & bookdate<="2018-01-15"]
d <- d[ident_dhis2_control==1]
locationOfTheFirstIdentVariable <- min(which(stringr::str_detect(names(d),"^ident_")))
d <- d[,1:locationOfTheFirstIdentVariable]

recNum[,fromReceivedFile:=TRUE]
d[,fromDHIS2:=TRUE]
dx <- merge(d,recNum,all.x=T,all.y=T,by.x="conancfilenumber",by.y="received")

nrow(d)
nrow(dx)

xtabs(~dx$fromReceivedFile+dx$fromDHIS2,addNA=T)

# get all the names from dx
# (this doesn't "do" anything, it just gets the names)
# we will use this later
n <- names(dx)
# put the four names we care about at the front
# (but now we have duplicates!)
n <- c("conancfilenumber",
       "fromDHIS2",
       "fromReceivedFile",
       "numOfTimesObserved",
       n)
# get rid of the duplicates
n <- unique(n)

# tells dx "PUT THE COLUMNS IN THIS ORDER ('n')
# (here we actually "do something")
# this is pass by reference
setcolorder(dx,n)
# this is the same as this: (pass by value)
# this will temporarily use double the amount of memory
# (with=F says "n is not a column in dx, look inside it")
# dx <- dx[,n,with=F]

# lets sort the rows of the dataset (pass by reference)
setorder(dx,conancfilenumber,fromDHIS2,fromReceivedFile)
# pass by value
# dx <- dx[order(conancfilenumber,fromDHIS2,fromReceivedFile)]

openxlsx::write.xlsx(dx, "results/matched_received_with_filled_dhis2.xlsx")


# 
# d1 <- tryCatch({
#   fread("X:/data processing/data_raw/e.reg-control/2018-07-26/Control ANC Green File.csv")
# }, error=function(err){
#   fread("Z:/data processing/data_raw/e.reg-control/2018-07-26/Control ANC Green File.csv")
# })
# setnames(d1,2,"programstage")
# 
# d2 <- tryCatch({
#   fread("X:/data processing/data_raw/e.reg-control/2018-07-26/Control Demographics.csv")
# }, error=function(err){
#   fread("Z:/data processing/data_raw/e.reg-control/2018-07-26/Control Demographics.csv")
# })
# 
# d1 <- MakeDataTableNamesLikeStata(d1)
# d2 <- MakeDataTableNamesLikeStata(d2)
# 
# d2[,numRecords:=.N,by=.(instance)]
# xtabs(~d2$numRecords)
# d2[,numRecords:=NULL]
# 
# d1[,eventdate:=as.Date(eventdate)]
# d1 <- d1[eventdate>="2017-01-01" & eventdate<="2018-01-15"]
# 
# x <- merge(d1,d2,by.x="programstage",by.y="instance")
# nrow(x)
# nrow(d1)
# nrow(d2)
# 
# setorder(x,identificationdocumentnumbercontroldata,eventdate)
# 
# x[,numRecords:=.N,by=.(identificationdocumentnumbercontroldata,eventdate)]
# xtabs(~x$numRecords)
# x[numRecords>2][,1:10]
# 
# d1[,c("eventdate","conancfilenumber")]













