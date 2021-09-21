###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)


###### SETUP ENDS ######

# Load in data

data <- as.data.table(fread("C:/data processing/data_raw/field_monitoring/2021-08-07.csv", encoding="UTF-8"))

#############
# clean names
#############
# make column names to lower case
for (i in names(data)) setnames(data, i, ExtractOnlyEnglishLettersAndNumbers(i)[[1]])
names(data)

for(i in names(data)) setnames(data,i,stringr::str_remove_all(i,"commentsfrom")[[1]])

setnames(data,c("implementername",
                "problemswithhardwarepcs",
                "problemswithhardwareconnectivityinternetetc",
                "currentpregnancyoutcome",
                "ultrasoundegectopicpregnancymissedabortionetc",
                "newborncare",
                "saveandcontinueeditdeletecompleteskipschedulereferraletc"),
         c("implementer",
           "hardware",
           "connectivityorinternet",
           "cpo",
           "us",
           "nbc",
           "buttonfunctionality"))

names(data)

##############
# clean vars #
##############

# time stamp
data[,date:=timestamp]
data[,date:=stringr::str_extract(date,"[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]")]
data[,date:=lubridate::as_date(date)]
xtabs(~data$date, addNA=T)

# month
data[,month:=as.numeric(month(date))]
xtabs(~data$month,addNA=T)

# year
data[,year:=as.numeric(stringr::str_extract(date,"^[0-9][0-9][0-9][0-9]"))]
xtabs(~data$year,addNA=T)


# hardware
xtabs(~data$hardware,data=data, addNA=T)
data[,hardware_none:=as.character(NA)]
data[stringr::str_detect(tolower(hardware), "no"), hardware_none:="none specified"]

data[,hardware_pc:=as.character(NA)]
data[stringr::str_detect(tolower(hardware),"pc"), hardware_pc:="pc"]

data[,hardware_keyboard:=as.character(NA)]
data[stringr::str_detect(tolower(hardware) ,"keyboard"), hardware_keyboard:="keyboard"]


data[,hardware_windows:=as.character(NA)]
data[stringr::str_detect(tolower(hardware) ,"window"), hardware_windows:="installation"]



# connectivityorinternet
unique(data$connectivityorinternet)
data[,connect_fibers:=as.character(NA)]
data[stringr::str_detect(tolower(connectivityorinternet) ,"fiber"), 
     connect_fibers:="cut or disconnected fiber lines"]

data[stringr::str_detect(tolower(connectivityorinternet) ,"slow"), 
connect_slow:="slow internet speed or bad access point or wiresless"]

data[stringr::str_detect(tolower(connectivityorinternet) ,"wireless"), 
                    connect_slow:="slow internet speed or bad access point or wiresless"]



data[stringr::str_detect(tolower(connectivityorinternet),"point"), 
     connect_slow:="slow internet speed or bad access point or wiresless"]

# registration
unique(data$registration)

data[,registration_none:=as.logical(NA)]
data[stringr::str_detect(tolower(registration) ,"^no$"),registration_none:=TRUE]

data[,registration_notinuse:=as.logical(NA)]
data[stringr::str_detect(tolower(registration) ,"did not enter"), 
     registration_notinuse:=TRUE]

data[,registration_entryerrors:=as.logical(NA)]
data[stringr::str_detect(tolower(registration) ,"we found 3 files with one name, and we told hanin to delete 2 files and collect the correct data in the last"), 
     registration_entryerrors:=TRUE]

data[stringr::str_detect(tolower(registration) ,"we found a patient entered into the system in her name, and we found another file with her ID number, and we solved this by deleting one of them."), 
     registration_entryerrors:=TRUE]

# anc
unique(data$anc)

data[,anc_none:=as.logical(NA)]
data[stringr::str_detect(tolower(anc),"^no"), anc_none:=TRUE]

data[,anc_gestageorlmp:=as.logical(NA)]
data[stringr::str_detect(tolower(anc),"gestational age"), anc_gestageorlmp:=TRUE]
data[stringr::str_detect(tolower(anc),"lmd"), anc_gestageorlmp:=TRUE]
data[stringr::str_detect(tolower(anc),"lmp"), anc_gestageorlmp:=TRUE]

data[,anc_openclosefile:=as.logical(NA)]
data[stringr::str_detect(tolower(anc),"open"), anc_openclosefile:=TRUE]
data[stringr::str_detect(tolower(anc),"close"), anc_openclosefile:=TRUE]


data[,anc_history:=as.logical(NA)]
data[stringr::str_detect(tolower(anc),"history"), anc_history:=TRUE]
data[stringr::str_detect(tolower(anc),"previous"), anc_history:=TRUE]

# cpo
unique(data$cpo)

data[,cpo_none:=as.logical(NA)]
data[stringr::str_detect(tolower(cpo),"^no"), cpo_none:=TRUE]


data[,cpo_notinuse:=as.logical(NA)]
data[stringr::str_detect(cpo,"^not fill"), cpo_notinuse:=TRUE]

data[,cpo_openorclose:=as.logical(NA)]
data[stringr::str_detect(tolower(cpo),"open"), cpo_openorclose:=TRUE]
data[stringr::str_detect(tolower(cpo),"close"), cpo_openorclose:=TRUE]


# ppc

unique(data$ppc)

data[,ppc_none:=as.logical(NA)]
data[stringr::str_detect(tolower(ppc),"^no"), ppc_none:=TRUE]

data[,ppc_notfill:=as.logical(NA)]
data[stringr::str_detect(tolower(ppc),"not fill"), ppc_notfill:=TRUE]

data[,ppc_closefile:=as.logical(NA)]
data[stringr::str_detect(tolower(ppc),"close"), ppc_closefile:=TRUE]

# us
unique(data$us)

data[,us_none:=as.logical(NA)]
data[stringr::str_detect(tolower(us),"^no"), us_none:=TRUE]

data[,us_ectopicpregnotfilled:=as.logical(NA)]
data[stringr::str_detect(tolower(us),"ectopic pregnancy and molar Pregnancy not entered"),
                         us_ectopicpregnotfilled:=TRUE]



# nbc
data[,nbc_none:=as.logical(NA)]
data[stringr::str_detect(tolower(nbc),"^no"), nbc_none:=TRUE]

data[,nbc_notfilled:=as.logical(NA)]
data[stringr::str_detect(tolower(nbc),"not fill"),
                         nbc_notfilled:=TRUE]
data[stringr::str_detect(tolower(nbc),"not entered"),
                         nbc_notfilled:=TRUE]



# management
unique(data$management)
data[,man_none:=as.logical(NA)]
data[stringr::str_detect(tolower(management),"^no"), man_none:=TRUE]

data[,man_unmanaged:=as.logical(NA)]
data[stringr::str_detect(tolower(management),"unmanged"), man_unmanaged:=TRUE]
data[stringr::str_detect(tolower(management),"not complet"), man_unmanaged:=TRUE]

data[,man_improperuse:=as.logical(NA)]
data[stringr::str_detect(tolower(management),
                      "yes answer entered for all management eg referring even though women not referred"),                                man_improperuse:=TRUE]

data[stringr::str_detect(tolower(management),"forget to enter"),man_improperuse:=TRUE]

data[,man_notworking:=as.logical(NA)]
data[stringr::str_detect(tolower(management),"The management of lap results not found after completing the                       laboratory entry appears after a while."),
     man_notworking:=TRUE]




# missed visit
data[,missedvisit_binary:=as.logical(NA)]
data[stringr::str_detect(tolower(missedvisits),"no"), missedvisit_binary:=FALSE]
data[stringr::str_detect(tolower(missedvisits),"yes"), missedvisit_binary:=TRUE]


# training
data[,training_nurse:=as.logical(NA)]
data[stringr::str_detect(tolower(training),"doctor"), training_nurse:=FALSE]
data[stringr::str_detect(tolower(training),"nurse"), training_nurse:=TRUE]

#browserextensionsaddedandnursetrained
data[,browserextensionsaddedandnursetrained_binary:=as.logical(NA)]
data[stringr::str_detect(tolower(browserextensionsaddedandnursetrained),"no"), 
     browserextensionsaddedandnursetrained_binary:=FALSE]

data[stringr::str_detect(tolower(browserextensionsaddedandnursetrained),"yes"), 
     browserextensionsaddedandnursetrained_binary:=TRUE]





###########################################
# completness 
###########################################








NumberOf0 <- function(x){
  if(length(x)==0) return(0)
  if(is.numeric(x[1])){
    sum(x==0,na.rm=T)
  } else {
    return(0)
  }
}

NumberOfEmptyQuotes <- function(x){
  if(length(x)==0) return(0)
  if(is.numeric(x[1])){
    return(0)
  } else if(lubridate::is.Date(x[1])){
    return(0)
  } else {
    sum(x=="",na.rm=T)
  }
}

MeanNo0 <- function(x){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(mean(y))
}

MedianNo0 <- function(x){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(median(y))
}

QuantileNo0 <- function(x,probs){
  if(!is.numeric(x)) return(NA)
  y <- x[x!=0 & !is.na(x)]
  if(length(y)==0) return(NA)
  return(quantile(y,probs=probs))
}


  long <- data
  
  # if you want to restrict the dataset, obviously just do it right after:
  # long[XXXXX,
  # the result of this is in wide format, you have two rows, one that
  # is for gaza, one that is for WB
  #resN <- long[, lapply(.SD, function(x) length(x)),keyby=.(clinicname)]
  # we now need to make this into long format, so we melt it and turn it into
  # long format. We keep "idvars=clinicname" so that we have an extra column
  # that retains the information if each row is gaza or WB
  #resN <- melt.data.table(resN,id.vars="clinicname")
  #setnames(resN,"value","resN")
  
  res <- long[, as.list(unlist(lapply(.SD, function(x)
    list(
      xIsNumeric=is.numeric(x),
      xN=length(x),
      xNA=sum(is.na(x)),
      xNotNA=sum(!is.na(x)),
      xNum0=NumberOf0(x),
      xNumEmptyQuotes=NumberOfEmptyQuotes(x),
      xUnique=length(unique(x)),
      xMean=mean(as.numeric(x),na.rm=T),
      xMedian=median(as.numeric(x),na.rm=T),
      xp0=as.numeric(quantile(as.numeric(x),probs=0,na.rm=T)),
      xp1=as.numeric(quantile(as.numeric(x),probs=0.01,na.rm=T)),
      xp5=as.numeric(quantile(as.numeric(x),probs=0.05,na.rm=T)),
      xp25=as.numeric(quantile(as.numeric(x),probs=0.25,na.rm=T)),
      xp75=as.numeric(quantile(as.numeric(x),probs=0.75,na.rm=T)),
      xp95=as.numeric(quantile(as.numeric(x),probs=0.95,na.rm=T)),
      xp99=as.numeric(quantile(as.numeric(x),probs=0.99,na.rm=T)),
      xp100=as.numeric(quantile(as.numeric(x),probs=1,na.rm=T)),
      xMeanNo0=MeanNo0(as.numeric(x)),
      xMedianNo0=MedianNo0(as.numeric(x)),
      xp0No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.0)),
      xp1No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.01)),
      xp5No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.05)),
      xp25No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.25)),
      xp75No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.75)),
      xp95No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.95)),
      xp99No0=as.numeric(QuantileNo0(as.numeric(x),probs=0.99)),
      xp100No0=as.numeric(QuantileNo0(as.numeric(x),probs=1))
      
      # QuantileNo0: ....we do this for all variables just to solve the problem of variables 
      # that have zero and zero means missing not value ...so the best mean for those variables is 
      # mean no 0 but for other variables the best mean is (mean) and this is ugly table , down here 
      #we can make the pretty table for those means 
    )))  
  ),keyby=.(clinicname,year)]
  
  # should give two things for gaza and 4 things for bookyear
 # xtabs(~clinicname+year, data=res)
  
  # turn it into long format
  res <- melt.data.table(res, id.vars=c("clinicname","year"))
  
  #cross tabs to look at how many variables 
  # split the variable name into 2 variables ("variable name", and "function")
  res[,variable:=stringr::str_replace(variable,"\\.1\\.","_1.")]
  res[,var:=stringr::str_split_fixed(variable,"\\.",2)[,1]]
  res[,method:=stringr::str_split_fixed(variable,"\\.",2)[,2]]
  
  # put all of the different values into wide format
  res <- dcast.data.table(res,clinicname+year+var~method,value.var = "value")
  
  # make it a bit prettier
  setorder(res,var)
  setcolorder(res,c("var"))
  
  # getting out the denominators (events!!!)
  res[,key2:=stringr::str_sub(var,1,2)]
  res[,key3:=stringr::str_sub(var,1,3)]
  res[,key:=key2]
 
  
  #denoms <- res[stringr::str_detect(var,"event")]
  denoms <- unique(res[,c("key3","clinicname","year","xNotNA")])
  setnames(denoms,"xNotNA","denom")
  # check each row if identiyin by above
  
  denomsBook <- long[,.(denom=.N),
                     keyby=.(clinicname,year)]
  
  nrow(res)
  
  # only keeping the rows that have the same "key" as those in our denominators
  res <- res[key3 %in% denoms$key3]
  nrow(res)
  res <- merge(res,denoms,by=c("key3","clinicname" ,"year"))
  nrow(res)
  ## end denominators
  
  # doing manual fixing of what is considered to be missing
  res[,notMissing:=xNotNA]
  # for numeric, 0s are missing
  res[xIsNumeric==TRUE & xUnique>5,notMissing:=xNotNA-xNum0]
  # for strings, "" are missing
  res[xIsNumeric==FALSE,notMissing:=xNotNA-xNumEmptyQuotes]
  
  # fix mean, median, p25, and p75 for when 0s are actually missing
  res[xIsNumeric==TRUE & xUnique>5,xMean:=xMeanNo0]
  res[xIsNumeric==TRUE & xUnique>5,xMedian:=xMedianNo0]
  res[xIsNumeric==TRUE & xUnique>5,xp25:=xp25No0]
  res[xIsNumeric==TRUE & xUnique>5,xp75:=xp75No0]
  res[xIsNumeric==TRUE & xUnique>5,xp0:=xp0No0]
  res[xIsNumeric==TRUE & xUnique>5,xp1:=xp1No0]
  res[xIsNumeric==TRUE & xUnique>5,xp5:=xp5No0]
  res[xIsNumeric==TRUE & xUnique>5,xp95:=xp95No0]
  res[xIsNumeric==TRUE & xUnique>5,xp99:=xp99No0]
  res[xIsNumeric==TRUE & xUnique>5,xp100:=xp100No0]
  
  
  res[,xMeanNo0:=NULL]
  res[,xMedianNo0:=NULL]
  res[,xp25No0:=NULL]
  res[,xp75No0:=NULL]
  res[,xp0No0:=NULL]
  res[,xp1No0:=NULL]
  res[,xp5No0:=NULL]
  res[,xp95No0:=NULL]
  res[,xp99No0:=NULL]
  res[,xp100No0:=NULL]
  
  # delete useless variables
  res[,xN:=NULL]
  res[,xNA:=NULL]
  res[,xNotNA:=NULL]
  res[,xNum0:=NULL]
  
  res[,key2:=NULL]
  res[,key3:=NULL]
  
  res[,xNumEmptyQuotes:=NULL]
  
  # make things pretty
  res[,varType:="Numeric"]
  res[xIsNumeric==0,varType:="String"]
  res[,xIsNumeric:=NULL]
  
  res[,percComplete:=round(100*notMissing/denom,1)]
  res[,xMean:=round(xMean,1)]
  
  res <- res[denom>0]
  
  setorder(res,clinicname,key,varType,var)
  setcolorder(res,c(
    "clinicname",
    "year",
    "key",
    "varType",
    "var",
    "percComplete",
    "denom",
    "notMissing"
  ))
  
  openxlsx::write.xlsx(res,
                       file.path(FOLDER_DATA_RAW,
                                 "field_monitoring",
                                 "data_completeness.xlsx"
                       ))
  
  



DataCompletion(data)

