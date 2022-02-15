
###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)

###### SETUP ENDS ######

###### LOAD DATA ######
d <- LoadDataFileFromNetwork()


# duplicate bookings or files

# time difference between bookdates for multiple bookings to see differences

setorder(d,booknum)

d[,bookdateprevious:=shift(bookdate,n=1L),by=motheridno]
d[,diftimebook:=difftime(bookdate,
                         bookdateprevious,
                         units="days")]

xtabs(~d[ident_dhis2_booking==T]$diftimebook, addNA=T)
xtabs(~d[booknum==1]$diftimebook, addNA=T)


d[,fullname:=stringr::str_c(firstname,
                            ' ',
                            fathersname)]

d[,fullname:=stringr::str_c(fullname,
                            ' ',
                            middlename)]

d[,fullname:=stringr::str_c(fullname,
                            ' ',
                            familyname1)]

d[,fullname:=stringr::str_c(fullname,
                            ' ',
                            familyname2)]

d[, fullname:=stringr::str_replace(fullname,"  *"," ")]




duplicates <- d[,c("motheridno",
                   "fullname",
                   "familyname1",
                   "familyname2",
                   "ident_dhis2_booking",
                   "ident_dhis2_ppc",
                   "bookdate",
                   "booknum",
                   "diftimebook",
                   "bookorgname",
                   "bookorgdistrict")]

xtabs(~duplicates$diftimebook)

setorder(duplicates, bookorgdistrict,bookorgname,fullname, motheridno)

duplicates[,fullnamenum:=1:.N, by=fullname]
xtabs(~duplicates$fullnamenum)
xtabs(~duplicates$booknum, addNA=TRUE)

# duplicates[,halfnamenum:=1:.N, by=halfname]
# xtabs(~duplicates$halfnamenum)
# xtabs(~duplicates$halfnamenum, addNA=TRUE)


openxlsx::write.xlsx(unique(duplicates),file.path(FOLDER_DATA_RESULTS,
                                                  "quality_control",
                                                  sprintf("duplicates_%s.xlsx", lubridate::today())))





################
# demographics #
################

# 
# demo <- fread(fs::path(FOLDER_DATA_RAW,
#                        "e.reg-intervention",
#                        "2021-08-12",
#                        "Clinical Demographics.csv"))
# 
# 


demo <- fread("C:/data processing/data_raw/e.reg-intervention/2021-08-12/Clinical Demographics.csv", 
              encoding="UTF-8")


nrow(demo)

for (i in names(demo)) setnames(demo, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])

setnames(demo,"instance","uniqueid")
setnames(demo,"created","datecreated")
setnames(demo,"lastupdated","dateupdated")
setnames(demo,"organisationunit","demoorgunit")
setnames(demo,"organisationunitname","demoorgname")
# if(badname %in% names(d)) setnames(demo, badname, goodname)
if("trackedentitytype" %in% names(d)) setnames(demo, "trackedentitytype", "trackedentity")
# if(!goodname %in% names(d)) ERROR!!
if(!"trackedentity" %in% names(demo)) stop("cant find trackedentity")

setnames(demo,"inactive","dummy")
setnames(demo,"identificationdocumenttype","idtype")
if(!"identificationdocumentnumber" %in% names(demo)){
  warning("no identification document number -- we create one")
  demo[,identificationdocumentnumber:=1:.N]
}
setnames(demo,"identificationdocumentnumber","demoidnumber")
#setnames(demo,"areyouwillingtoreceivesmstextmessagesandremindersaboutyourvisits", "doyouwanttoreceivesms")


xtabs(~demo$identificationdocumentnumber)


demo[,datecreated:=stringr::str_extract(datecreated, "^.{10}")]
xtabs(~demo$datecreated)

demo[, datecreated:=as.Date(datecreated)]
xtabs(~demo$datecreated)


xtabs(~demo$datecreated)
str(demo$datecreated)


demo[,idnonum:=.N, by=demoidnumber]
xtabs(~demo[idnonum>1]$demoidnumber, addNA=T)




# number of digits (9, 8, 10)

demo[,idnumdigits:=nchar(as.integer(demoidnumber))]
xtabs(~demo$idnumdigits, addNA=T)

demo[,yearcreated:=lubridate::year(datecreated)]
xtabs(~demo$yearcreated)

demo[,demoorgname:=ExtractOnlyEnglishLetters(demoorgname)]
xtabs(~demo$demoorgname)

demo <- demo[!demoorgname %in% c("test", "testfacility")]




#############
# full name #
#############


demo[,fullname:=stringr::str_c(firstname,
                            ' ',
                            fathersname)]

demo[,fullname:=stringr::str_c(fullname,
                            ' ',
                            middlename)]

demo[,fullname:=stringr::str_c(fullname,
                            ' ',
                            womanfamilyname)]

demo[,fullname:=stringr::str_c(fullname,
                            ' ',
                            husbandsfamilyname)]

demo[, fullname:=stringr::str_replace(fullname,"  *"," ")]



#############
# half name #
#############

demo[,halfname:=stringr::str_c(firstname,
                               ' ',
                               fathersname)]

demo[,halfname:=stringr::str_c(halfname,
                               ' ',
                               middlename)]


demo[, halfname:=stringr::str_replace(halfname,"  *"," ")]



demo[,numhalfname:=.N, by=c("halfname","demoorgname")]

demo[numhalfname==7, c("fullname","datecreated")]



# merge bookordistrict stuff #

sData <- as.data.table(readxl::read_excel("../data_raw/structural_data/bookorgname.xlsx"))

setnames(demo, "demoorgname","bookorgname")
dData <- merge(
              demo,
              sData,
              by="bookorgname",
              all.x=T)


# names in english and numbers
dData[, numinname:=FALSE]
dData[stringr::str_detect(fullname,"[0-9]"), numinname:=TRUE]
xtabs(~dData$numinname, addNA=T)



iddata <- dData[,c("bookorgname",
                   "yearcreated",
                   "idnumdigits",
                   "numinname",
                   "idnonum")]

iddata[,denom:=.N, by=c("bookorgname","yearcreated")]

ag <- iddata[,.(N=.N,
                Num_digits_less_than_9=sum(idnumdigits<9, na.rm = T),
                Num_id_digits_9=sum(idnumdigits==9, na.rm=T),
                Num_id_digits_more_than_9=sum(idnumdigits>9, na.rm=T),
                Numinname=sum(numinname==T, na.rm=T),
                Numinnameandmultiple=sum(numinname==T &
                                           idnonum>1, na.rm=T)),
             keyby=.(yearcreated,bookorgname)]



ag[,prop_less_than_9:=round(Num_digits_less_than_9/N, digits=3)]
ag[,prop_id_digits_9:=round(Num_id_digits_9/N, digits=3)]
ag[,prop_id_digits_more_than_9:=round(Num_id_digits_more_than_9/N, digits=5)]
ag[,prop_numinname:=round(Numinname/N, digits=5)]
ag[,prop_numinnamemultipleidno:=round(Numinnameandmultiple/N, digits=5)]

tokeep <- ag[prop_id_digits_9<1.0]

openxlsx::write.xlsx(ag,file.path(FOLDER_DATA_RESULTS,
                                  "quality_control",
                                  "duplicates",
                                  sprintf("id_quality_digits_%s.xlsx", lubridate::today())))







toanalyze <- dData[numhalfname>1,c("uniqueid",
                     "bookorgname",
                     "demoorgunit",
                     "datecreated",
                     "dateupdated",
                     "idtype",
                     "demoidnumber",
                     "firstname",
                     "fathersname",
                     "middlename",
                     "womanfamilyname",
                     "husbandsfamilyname",
                     "dateofbirth",
                     "ageatmarriage",
                     "ageatfirstpregnancy",
                     "consanguinity",
                     "educationinyears",
                     "yearcreated", 
                     "idnonum",
                     "idnumdigits",
                     "fullname",                                                        
                     "halfname",                                                        
                     "numhalfname")]

setorder(toanalyze, halfname, yearcreated, demoorgunit)

openxlsx::write.xlsx(toanalyze,
                     file.path(FOLDER_DATA_RESULTS,
                               "quality_control",
                               "duplicates",
                               sprintf("halfnamedups_%s.xlsx",CLINIC_INTERVENTION_DATE)))




# either they have the same id number or they have a previous half name and name repeated but need to include # even if prevfile name is missing



#############
# analyses #
#############

# shift last names

toanalyze[,womanfamilynameprev:=shift(womanfamilyname,n=1L),by=c("halfname","bookorgname")]
toanalyze[,husbandsfamilynameprev:=shift(husbandsfamilyname,n=1L),by=c("halfname","bookorgname")]

toanalyze[,newhalfname:=halfname]
toanalyze[,halfnameprev:=shift(newhalfname,n=1L),by=c("halfname","bookorgname")]
toanalyze[,newhalfname:=NULL]

toanalyze[halfnameprev==halfname,prevfilesame:=1]
xtabs(~toanalyze$prevfilesame, addNA=T)

toanalyze[halfnameprev==halfname &
            womanfamilynameprev==womanfamilynameprev,prevfilesame:=2]
xtabs(~toanalyze$prevfilesame, addNA=T)


toanalyze[halfnameprev==halfname &
            husbandsfamilynameprev==husbandsfamilyname,prevfilesame:=3]
xtabs(~toanalyze$prevfilesame, addNA=T)

toanalyze[halfnameprev==halfname &
            womanfamilynameprev==womanfamilynameprev &
            husbandsfamilynameprev==husbandsfamilyname,prevfilesame:=4]
xtabs(~toanalyze$prevfilesame, addNA=T)


# differnces in id numbers for these cases
setorder(toanalyze,bookorgname,halfname,demoorgname,yearcreated)

toanalyze[!is.na(prevfilesame),yearcreatedbefore:=shift(yearcreated,n=1L),by=c("halfname","bookorgname")]

# shift year created to get difference
toanalyze[,yearDif:=as.numeric(yearcreated-yearcreatedbefore)]
xtabs(~toanalyze$yearDif, addNA=T)




# possible multiple names #


##################
# multiple fields #
##################

setorder(demo,demoorgname, halfname, womanfamilyname,husbandsfamilyname,demoidnumber, yearcreated)

demo[,numhalfname:=.N, by=c("halfname","demoorgname")]
xtabs(~demo$numhalfname)





