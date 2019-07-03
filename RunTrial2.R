###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_pniphabstracts2018", list.files("r_pniphabstracts2018", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_trial2", list.files("r_trial2", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

###### SETUP ENDS ######

#d <- LoadDataFileFromNetworkGaza()
d <- LoadDataFileFromNetworkWB()

td <- d[ident_dhis2_booking==1 & ident_TRIAL_2_and_3==T & bookyear %in% c(2017:2019)]
bookingtrial2 <- td[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
), keyby=.(
  bookorgname,
  bookyear
)]

bookingtrial2
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
bookingtrial2 <- dcast(bookingtrial2, bookorgname ~ bookyear)  

openxlsx::write.xlsx(bookingtrial2, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "trial_2",
                       "booking.xlsx"))


# timely
# this function is taken from Analyse_Mahima_random.R
IndicatorsOsloGenerate(td)

stringr::str_subset(names(td),"^custo")
xtabs(~td$custo_anvisit_timely_by_bookgestage)
xtabs(~td$custo_anbpdiast_timely_by_bookgestage)

xtabs(~custo_bookgestagecat+custo_anvisit_00_07,
      data=td[bookyear==2019 & bookorgname=="taffuh"],
      addNA=T)

timelyattendancetrial2 <- td[custo_bookgestagecat!="WAITING TO BE ASSIGNED", .(
  N=.N,
  #num_attendance00_07=sum(custo_anvisit_00_07, na.rm = T),
  denom00_07=sum(custo_bookgestagecat %in% c("00_07"),na.rm=T),
  # WE HAVE A PROBLEM WHERE SOME PEOPLE ARE ANVISITS IN 0-7 WEEKS
  # WHILE THEY WERE BOOKED IN 8-12 WEEKS.
  # TO SOLVE THIS, ADD THE RESTRICTION: custo_bookgestagecat %in% c("00_07")
  # INTO THE NUMERATOR AS WELL
  perc_attendance00_07=round(100*
                               sum(custo_anvisit_00_07, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07"),na.rm=T)
  ),
  
  denom08_12=sum(custo_bookgestagecat %in% c("00_07","08_12"),na.rm=T),
  perc_attendance08_12=round(100*
                               sum(custo_anvisit_08_12, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07","08_12"),na.rm=T)
  ),
  
  
  
  denom13_14=sum(custo_bookgestagecat %in% c(
    "00_07",
    "08_12",
    "13_14"),na.rm=T),
  perc_attendance13_14=round(100*
                               sum(custo_anvisit_08_12, na.rm = T)/
                               sum(custo_bookgestagecat %in% c(
                                 "00_07",
                                 "08_12",
                                 "13_14"),na.rm=T)
  ),
  
  denom15_17=sum(custo_bookgestagecat %in% c(
    "00_07",
    "08_12",
    "13_14",
    "15_17"),na.rm=T),
  perc_attendance15_17=round(100*
                               sum(custo_anvisit_15_17, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17"),na.rm=T)
  ),
  
  denom18_22=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22"),na.rm=T),
  perc_attendance18_22=round(100*
                               sum(custo_anvisit_18_22, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22"),na.rm=T)
  ),
  
  denom23_23=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23"),na.rm=T),
  perc_attendance23_23=round(100*
                               sum(custo_anvisit_23_23, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23"),na.rm=T)
  ),
  
  
  denom24_28=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23",
                                             "24_28" ),na.rm=T),
  perc_attendance24_28=round(100*
                               sum(custo_anvisit_24_28, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23",
                                                               "24_28"),na.rm=T)
  ),
  
  
  denom29_30=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23",
                                             "24_28",
                                             "29_30"),na.rm=T),
  perc_attendance29_30=round(100*
                               sum(custo_anvisit_29_30, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23",
                                                               "24_28",
                                                               "29_30"
                                                               ),na.rm=T)
  ),
  
  denom31_33=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23",
                                             "24_28",
                                             "29_30",
                                             "31_33"),na.rm=T),
  perc_attendance31_33=round(100*
                               sum(custo_anvisit_31_33, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23",
                                                               "24_28",
                                                               "29_30",
                                                               "31_33"
                               ),na.rm=T)
  ),
  
  
  denom34_38=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23",
                                             "24_28",
                                             "29_30",
                                             "31_33",
                                             "34_38"),na.rm=T),
  perc_attendance34_38=round(100*
                               sum(custo_anvisit_34_38, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23",
                                                               "24_28",
                                                               "29_30",
                                                               "31_33",
                                                               "34_38"
                               ),na.rm=T)
  ),
  denom39_99=sum(custo_bookgestagecat %in% c("00_07",
                                             "08_12",
                                             "13_14",
                                             "15_17",
                                             "18_22",
                                             "23_23",
                                             "24_28",
                                             "29_30",
                                             "31_33",
                                             "34_38",
                                             "39_99"),na.rm=T),
  perc_attendance39_99=round(100*
                               sum(custo_anvisit_39_99, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07",
                                                               "08_12",
                                                               "13_14",
                                                               "15_17",
                                                               "18_22",
                                                               "23_23",
                                                               "24_28",
                                                               "29_30",
                                                               "31_33",
                                                               "34_38",
                                                               "39_99"),na.rm=T)
  )
), keyby=.(
  bookyear,
  bookorgname
)]



openxlsx::write.xlsx(timelyattendancetrial2, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "trial_2",
                       "timelyattendance.xlsx"))

# attempt to make a long lab dataset
l1 <- stringr::str_subset(names(td),"^labgestage_")
l2 <- stringr::str_subset(names(td),"^labogct_")
l3 <- stringr::str_subset(names(td),"^labhb_")
l4 <- stringr::str_subset(names(td),"^labbloodglu_")

widelab <- td[,c("uniqueid","bookgestage",
                 l1,
                 l2,
                 l3,
                 l4),with=F]

longlab <- melt(widelab, id.vars = c("uniqueid","bookgestage"),
     measure = patterns(
       "^labgestage_",
       "^labogct_",
       "^labhb_",
       "^labbloodglu_"
     ),
     value.name=c(
       "labgestage",
       "labogct",
       "labhb",
       "labbloodglu"
     ))

longlab[,labgestagecat:=cut(labgestage,breaks=c(0,40,100))]
longlab[,bookgestagecat:=cut(bookgestage,breaks=c(0,40,100))]

# aggregate this down to each woman
each_woman <- longlab[,.(
  has_labhb=max(!is.na(labhb) & labhb>0)
),keyby=.(
  uniqueid,
  bookgestagecat,
  labgestagecat
)]


each_woman

# create the ugly table from the woman dataset
each_woman[,.(
  has_labhb = mean(has_labhb)
),
keyby=.(
  bookgestagecat,
  labgestagecat
)]

