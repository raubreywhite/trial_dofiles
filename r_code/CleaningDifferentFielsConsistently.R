SetVariableToMissing <- function(
  d,
  restriction=NULL,
  pattern,
  lowerUnacceptable,
  upperUnacceptable
){
  vars <- names(d)[stringr::str_detect(names(d),pattern)]
  if(length(vars)==0) stop("patternd oesnt exist")
  for(v in vars){
    d[,(v):=ExtractOnlyNumbersAndDecimalPoints(get(v))] # make sure that it is numeric
    
    toExecuteRestriction <- sprintf("(%s<=%s | %s>=%s)",v,lowerUnacceptable,v,upperUnacceptable)
    if(!is.null(restriction)){
      toExecuteRestriction <- sprintf("%s & %s",toExecuteRestriction,restriction)
    }
    toExecute <- sprintf("d[%s,%s:=NA]",toExecuteRestriction,v)
    # parse turns "text" into "something that can be run by R"
    # eval "runs this"
    eval(parse(text=toExecute))
  }
}

SetVariableToMultiple <- function(
  d,
  restriction=NULL,
  multiplier=1,
  pattern,
  lowerAcceptable,
  upperAcceptable
){
  vars <- names(d)[stringr::str_detect(names(d),pattern)]
  if(length(vars)==0) stop("patternd oesnt exist")
  for(v in vars){
    d[,(v):=ExtractOnlyNumbersAndDecimalPoints(get(v))] # make sure that it is numeric
    
    toExecuteRestriction <- sprintf("(%s<=%s & %s<=%s)",lowerAcceptable,v,v,upperAcceptable)
    if(!is.null(restriction)){
      toExecuteRestriction <- sprintf("%s & %s",toExecuteRestriction,restriction)
    }
    toExecute <- sprintf("d[%s,%s:=%s*%s]",toExecuteRestriction,v,v,multiplier)
    # parse turns "text" into "something that can be run by R"
    # eval "runs this"
    eval(parse(text=toExecute))
  }
}

CleaningDifferentFilesConsistently <- function(d){
  
  # pregnancy number of weeks at birth
  SetVariableToMissing(d=d,
                       restriction="matching=='Avicenna'",
                       pattern="^abbbabypregnancynoofweeks_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 50)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Governmental'",
                       pattern="^hbogestagedeliv_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 50)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Private'",
                       pattern="^dhis2hbogestagedeliv_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 50)
  
  # pregnancy number of weeks at birth - rescaling kg to g
  xtabs(~d$abbbabyweight_1)
  SetVariableToMultiple(d=d,
                       restriction="matching=='Avicenna'",
                       pattern="^abbbabyweight_",
                       multiplier=1000,
                       lowerAcceptable = 0,
                       upperAcceptable = 10)
  xtabs(~d$abbbabyweight_1)
  
  SetVariableToMultiple(d=d,
                       restriction="matching=='Governmental'",
                       pattern="^hboprevpregbweight_",
                       multiplier=1000,
                       lowerAcceptable = 0,
                       upperAcceptable = 10)
  
  SetVariableToMultiple(d=d,
                       restriction="matching=='Private'",
                       pattern="^dhis2hbopregbweight_",
                       multiplier=1000,
                       lowerAcceptable = 0,
                       upperAcceptable = 10)
  
  # pregnancy number of weeks at birth - setting missing
  xtabs(~d$abbbabyweight_1)
  SetVariableToMissing(d=d,
                       restriction="matching=='Avicenna'",
                       pattern="^abbbabyweight_",
                       lowerUnacceptable = 100,
                       upperUnacceptable = 999999)
  xtabs(~d$abbbabyweight_1)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Governmental'",
                       pattern="^hboprevpregbweight_",
                       lowerUnacceptable = 100,
                       upperUnacceptable = 999999)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Private'",
                       pattern="^dhis2hbopregbweight_",
                       lowerUnacceptable = 100,
                       upperUnacceptable = 999999)
  
  # hemoglobin
  xtabs(~d$alabtestresult_1)
  SetVariableToMissing(d=d,
                       restriction="matching=='Avicenna'",
                       pattern="^alabtestresult_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 999999)
  xtabs(~d$alabtestresult_1)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Governmental'",
                       pattern="^hboconlabcbchemoglobin_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 999999)
  
  SetVariableToMissing(d=d,
                       restriction="matching=='Private'",
                       pattern="^dhis2hbolabcbchemoglobin_",
                       lowerUnacceptable = 0,
                       upperUnacceptable = 999999)
  
  # cleaning all of the gestational ages in DHIS2
  for(x in c(
    "bookgestage",
    "^angestage_",
    "^labgestage_",
    "^usgestage_",
    "^riskgestage_",
    "^mangestage_",
    "^prevgestagebirth_",
    "^cpogestage_"
  )){
    SetVariableToMissing(d=d,
                         pattern=x,
                         lowerUnacceptable = 0,
                         upperUnacceptable = 50)
  }
  #vars <- names(d)[stringr::str_detect(names(d),"gest")]
  
  SetVariableToMissing(d=d,
                       pattern="^age$",
                       lowerUnacceptable = 10,
                       upperUnacceptable = 60)
  
  SetVariableToMissing(d=d,
                       pattern="^agepregnancy$",
                       lowerUnacceptable = 10,
                       upperUnacceptable = 60)
  
  SetVariableToMissing(d=d,
                       pattern="^agemarriage$",
                       lowerUnacceptable = 10,
                       upperUnacceptable = 99)
  
  SetVariableToMissing(d=d,
                       pattern="^education$",
                       lowerUnacceptable = -1,
                       upperUnacceptable = 30)
  
  SetVariableToMissing(d=d,
                       pattern="^income$",
                       lowerUnacceptable = -1,
                       upperUnacceptable = 99999999)
  
}


