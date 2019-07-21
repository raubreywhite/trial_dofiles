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

td <- d[ident_dhis2_booking==1 & ident_TRIAL_2_and_3==T & bookyear %in% c(2018)]
bookingtrial2 <- td[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
  
), keyby=.(
  bookyear,
  bookorgname,
  str_TRIAL_2_Cluster
)]


nrow(td)

bookingtrial2
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
#value.var: you have your rows, your columns, and the variable you are shifting: which is this
#bookingtrial2 <- dcast(bookingtrial2, bookorgname ~ bookyear, value.var="numBOOK")  
#bookingtrial2 <- dcast(bookingtrial2, str_TRIAL_2_Cluster ~ bookyear, value.var="numcluster")  

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

# xtabs(~custo_bookgestagecat+custo_anvisit_00_07,
#       data=td[bookyear==2019 & bookorgname=="taffuh"],
#       addNA=T)


timelyattendancetrial2 <- td[custo_bookgestagecat!="WAITING TO BE ASSIGNED", .(
  N=.N,
  bookwomen00_07=sum(custo_bookgestagecat %in% c("00_07"),na.rm=T),
  numerat00_07=sum(custo_anvisit_00_07, na.rm = T),
  
  bpsyst00_07=sum(custo_anbpsyst_00_07, na.rm = T),
  bpdiast00_07=sum(custo_anbpdiast_00_07, na.rm = T),
  
  denom00_07=sum(custo_bookgestagecat %in% c("00_07"),na.rm=T),
  # WE HAVE A PROBLEM WHERE SOME PEOPLE ARE ANVISITS IN 0-7 WEEKS
  # WHILE THEY WERE BOOKED IN 8-12 WEEKS.
  # TO SOLVE THIS, ADD THE RESTRICTION: custo_bookgestagecat %in% c("00_07")
  # INTO THE NUMERATOR AS WELL
  perc_attendance00_07=round(100*
                               sum(custo_anvisit_00_07, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07"),na.rm=T)
  ),
  
  bookwomen08_12=sum(custo_bookgestagecat %in% c("08_12"),na.rm=T),
  numerat08_12=sum(custo_anvisit_08_12, na.rm = T),
  
  bpsyst08_12=sum(custo_anbpsyst_08_12, na.rm = T),
  bpdiast08_12=sum(custo_anbpdiast_08_12, na.rm = T),
  
  denom08_12=sum(custo_bookgestagecat %in% c("00_07","08_12"),na.rm=T),
  perc_attendance08_12=round(100*
                               sum(custo_anvisit_08_12, na.rm = T)/
                               sum(custo_bookgestagecat %in% c("00_07","08_12"),na.rm=T)
  ),
  
  bookwomen13_14=sum(custo_bookgestagecat %in% c("13_14"),na.rm=T),
  numerat13_14=sum(custo_anvisit_13_14, na.rm = T),
  
  bpsyst13_14=sum(custo_anbpsyst_13_14, na.rm = T),
  bpdiast13_14=sum(custo_anbpdiast_13_14, na.rm = T),
  
  denom13_14=sum(custo_bookgestagecat %in% c(
    "00_07",
    "08_12",
    "13_14"),na.rm=T),
  perc_attendance13_14=round(100*
                               sum(custo_anvisit_13_14, na.rm = T)/
                               sum(custo_bookgestagecat %in% c(
                                 "00_07",
                                 "08_12",
                                 "13_14"),na.rm=T)
  ),
  bookwomen15_17=sum(custo_bookgestagecat %in% c("15_17"),na.rm=T),
  numerat15_17=sum(custo_anvisit_15_17, na.rm = T),
  
  bpsyst15_17=sum(custo_anbpsyst_15_17, na.rm = T),
  bpdiast15_17=sum(custo_anbpdiast_15_17, na.rm = T),
  
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
  bookwomen18_22=sum(custo_bookgestagecat %in% c("18_22"),na.rm=T),
  numerat18_22=sum(custo_anvisit_18_22, na.rm = T),
  
  bpsyst18_22=sum(custo_anbpsyst_18_22, na.rm = T),
  bpdiast18_22=sum(custo_anbpdiast_18_22, na.rm = T),

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
  bookwomen23_23=sum(custo_bookgestagecat %in% c("23_23"),na.rm=T),
  numerat23_23=sum(custo_anvisit_23_23, na.rm = T),
  
  bpsyst23_23=sum(custo_anbpsyst_23_23, na.rm = T),
  bpdiast23_23=sum(custo_anbpdiast_23_23, na.rm = T),
  
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
  
  bookwomen24_28=sum(custo_bookgestagecat %in% c("24_28"),na.rm=T),
  numerat24_28=sum(custo_anvisit_24_28, na.rm = T),
  
  bpsyst24_28=sum(custo_anbpsyst_24_28, na.rm = T),
  bpdiast24_28=sum(custo_anbpdiast_24_28, na.rm = T),
  
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
  
  bookwomen29_30=sum(custo_bookgestagecat %in% c("29_30"),na.rm=T),
  numerat29_30=sum(custo_anvisit_29_30, na.rm = T),
  
  bpsyst29_30=sum(custo_anbpsyst_29_30, na.rm = T),
  bpdiast29_30=sum(custo_anbpdiast_29_30, na.rm = T),
  
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
  bookwomen31_33=sum(custo_bookgestagecat %in% c("31_33"),na.rm=T),
  numerat31_33=sum(custo_anvisit_31_33, na.rm = T),
  
  bpsyst31_33=sum(custo_anbpsyst_31_33, na.rm = T),
  bpdiast31_33=sum(custo_anbpdiast_31_33, na.rm = T),
  
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
  
  bookwomen34_38=sum(custo_bookgestagecat %in% c("34_38"),na.rm=T),
  numerat34_38=sum(custo_anvisit_34_38, na.rm = T),
  
  bpsyst34_38=sum(custo_anbpsyst_34_38, na.rm = T),
  bpdiast34_38=sum(custo_anbpdiast_34_38, na.rm = T),
  
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
  bookwomen39_99=sum(custo_bookgestagecat %in% c("39_99"),na.rm=T),
  numerat39_99=sum(custo_anvisit_39_99, na.rm = T),
  
  bpsyst39_99=sum(custo_anbpsyst_39_99, na.rm = T),
  bpdiast39_99=sum(custo_anbpdiast_39_99, na.rm = T),
  
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
  str_TRIAL_2_Cluster
)]




openxlsx::write.xlsx(timelyattendancetrial2, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "trial_2",
                       "timelyattendance_BP.xlsx"))


nrow(d[custo_bookgestagecat!="WAITING TO BE ASSIGNED" & 
        bookorgname=="almanshar" &
          (bookdate > andate_1)])

