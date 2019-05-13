

demo <-fread("C:/data processing/data_raw/e.reg-intervention/2019-04-03/Clinical Demographics.csv", encoding = "UTF-8")

#str(demo$`Identification document number`)


for (i in names(demo)){
setnames(demo, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])

}
demo$organisationunitname

setnames(demo,"organisationunitname", "orgunitname")

demo <- demo[orgunitname!="(- Test facility)"]
demo <- demo[orgunitname!="(HR - Test facility)"]
demo <- demo[orgunitname!="Test 1"]


setnames(demo,"identificationdocumentnumber","motheridno")
openxlsx::write.xlsx(demo,"C:/data processing/a research hRHR/presenting data/idquality/demoarabic.xlsx")


demo[,motheridnumberstring:=as.character(motheridno)]

demo[,motheridnumberlength:= nchar(motheridnumberstring,allowNA = T)]


xtabs(~demo$motheridnumberlength, addNA = T)

nrow(demo[!is.na(motheridno)])

nrow(demo[is.na(motheridno)])

nrow(demo)


# Number of unique variables
length(unique(demo$motheridno))

#creating a table that shows how many rows the mother had in the original dataset
uglytable <- demo[,.(N=.N), by=.(motheridno)]

##double checking the missing, as compared to above table
##num of blank entries
uglytable[is.na(motheridno)]

#shows number of womenthat have double or triple ids
xtabs(~uglytable$N)

#looking to see date/for date entered
unique(demo$created)
str(demo$created)

demo[,datecreated:=as.Date(stringr::str_sub(created, 1,10))]

unique(demo$datecreated)

# creating year and months as dates
demo[,year:=lubridate::year(datecreated)]
demo[,month:=lubridate::month(datecreated)]
demo[,month0:=formatC(month, width=2, flag="0")]

#put them together now
demo[,yearmonth:=sprintf("%s-%s", year, month0)]
demo$yearmonth

setnames(demo,"identificationdocumenttype","idtype")


###export a list of duplicated id numbers including dates
demo[,N:=.N, by=.(motheridno)]
dup<- demo[N>1, c( "N",
                  "firstname",
                  "husbandsname",
                  "womanfamilyname",
                  "husbandsfamilyname",
                  "orgunitname",
                  "motheridno",
                  "yearmonth",
                  "idtype")]

setorder(dup, N, motheridno)

openxlsx::write.xlsx(dup,"C:/data processing/a research hRHR/presenting data/idquality/Duplicated_ID_by_Bookmonth.xlsx")

nrow(demo[is.na(idtype)])

nrow(demo[!is.na(idtype)])

xtabs(~demo$idtype, addNA = T)

xtabs(~demo$idtype + demo$motheridnumberlength, addNA = T)

xtabs(~demo$idtype + demo$datecreated, addNA = T)


