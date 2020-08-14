
# detailed death report
# 484 unique id numbers that should be merged

nncdeath <- data.table(readxl::read_excel(file.path("data_raw",
                                                    "2019-12",
                                                    "baby_death.xls")))


#### Cleaning ###

# removing header
nncdeath[,keep:=TRUE]

nncdeath[`Patient`=="Patient", keep := FALSE]
nncdeath[`Diagnosis`=="Diagnosis", keep := FALSE]
nncdeath[`Death Date`=="Death Date", keep := FALSE]
nncdeath[`Organization`=="Organization", keep := FALSE]
nncdeath[`Type of diagnosis`=="Type of diagnosis", keep := FALSE]
nncdeath[`Organization`=="Organization", keep := FALSE]
nncdeath[`Adm. Date`=="Adm. Date", keep := FALSE]
nncdeath[`Discharge Date`=="Discharge Date", keep := FALSE]
nncdeath[`Depatment`=="Depatment", keep := FALSE]
nncdeath[`Gender`=="Gender", keep := FALSE]


# convert to strings and then reconvert back to date
## good practice to put `` around variables, 
## especially if it has a space or theyll be recoginized as two dif vars

nncdeath[,`Date`:=as.character(Date)]
nncdeath[`Date`=="Date", keep := FALSE]

nncdeath[,`Discharge Date__1`:=as.character(`Discharge Date__1`)]
nncdeath[`Discharge Date__1`=="Discharge Date__1", keep := FALSE]


# ONLY keep var==T to get rid of all headers
nbd <-nncdeath[keep==T]


# filling in missing id values because the missing values are for the previous id
nbd[,`Patient`:=zoo::na.locf(`Patient`)]


# renaming
setnames(nbd,"Patient","idno")
setnames(nbd,"Type of diagnosis","nbd_diagnosistype")
setnames(nbd,"Diagnosis","nbd_diagnosis")
setnames(nbd,"Organization","nbd_org")
setnames(nbd,"Death Date", "deathdate")
setnames(nbd,"Adm. Date","nbd_admissiondate")
setnames(nbd,"Discharge Date","nbd_dischargedate")
setnames(nbd,"Date","nbd_date")
setnames(nbd,"Discharge Date__1", "nbd_firstdischargedate")
setnames(nbd,"Depatment","nbd_department")
setnames(nbd,"Gender","nbd_gender")

# reformating (where needed)
nbd[,deathdate:=as.Date(as.numeric(deathdate), origin="1900-01-01")]
nbd[,nbd_admissiondate:=as.Date(as.numeric(nbd_admissiondate), origin="1900-01-01")]
nbd[,nbd_dischargedate:=as.Date(as.numeric(nbd_dischargedate), origin="1900-01-01")]

nbd[,nbd_date:=stringr::str_remove_all(nbd_date," [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$")]
nbd[,nbd_date:=as.Date(nbd_date)]

nbd[,nbd_firstdischargedate:=stringr::str_remove_all(nbd_firstdischargedate," [0-9][0-9]:[0-9][0-9]:[0-9][0-9]$")]
nbd[,nbd_firstdischargedate:=as.Date(nbd_firstdischargedate)]

# organize the data set before giving an eventnum to make sure in order by date
# putting death date as the second one because want it to appear as the first
setorder(nbd,idno, deathdate, nbd_admissiondate, nbd_firstdischargedate, na.last=TRUE )

nbd[,eventnum:=1:.N,by=idno]

#nbd[,nbd_nbdsource:="TRUE"]

valueVars <- names(nbd)
valueVars <- valueVars[stringr::str_detect(valueVars,"^nbd_")]

nbdw <- dcast.data.table(data=nbd,
                         formula=as.formula(idno~eventnum),
                         value.var = valueVars)

# 
