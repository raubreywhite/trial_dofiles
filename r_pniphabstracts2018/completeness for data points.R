

# completness for newborn data points

completenbc<- function(){
  d <- LoadDataFileFromNetworkWB()
  CreatingFurtherVariablesPNIPH(d)
  vars_nbc	<- names(d)[stringr::str_detect(names(d),"^nbc")]
  
  
  # with=f just formatting with the fuction to make the code run
  
  d <- d[ident_dhis2_nbc==T,
    vars_nbc,
     with=F
    ]
  # to creat an id number for every pregnancies b/c it is now  wide format
  d[,id:=1:.N]
  
  long <- melt.data.table(d, id.vars=c(
    "id"
  ),variable.factor = F, value.factor = F)
  
  long[,visit:=stringr::str_extract(variable,"[0-9]*$")]
  
  denoms <- long[stringr::str_detect(variable,"^nbcevent_"),
                    .(
                      denominator=sum(!is.na(value))
                    ),
                    keyby=.(
                      visit)
                    ]
  
  uglytable <- long[,
                    .(
                      not_NA=sum(!is.na(value)),
                      value0=sum(value==0,na.rm=T)
                    ),
                    keyby=.(
                      variable,
                      visit)
                    ]
  
  prettytable <- merge(uglytable,
                     denoms,
                     by="visit")
  prettytable[,na:=denominator-not_NA]
  prettytable[,percentage_of_completeness:=round(not_NA/denominator*100)]
  
  
  openxlsx::write.xlsx(prettytable, 
                       file.path(
                         FOLDER_DROPBOX_RESULTS,
                         "pniph",
                         "abstracts_2018",
                         "nbc_completeness.xlsx"))
  
  
}