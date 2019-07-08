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
#value.var: you have your rows, your columns, and the variable you are shifting: which is this
bookingtrial2 <- dcast(bookingtrial2, bookorgname ~ bookyear, value.var="numBOOK")  

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


#OGCT and anemia screening
# attempt to make a long lab dataset
l1 <- stringr::str_subset(names(td),"^labgestage_")
l2 <- stringr::str_subset(names(td),"^labogct_")
l3 <- stringr::str_subset(names(td),"^labhb_")
l4 <- stringr::str_subset(names(td),"^labbloodglu_")
l5 <- stringr::str_subset(names(td),"^laburglu_")

#blood glu
td[, bookgluc:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labbloodglu_1) &
     labbloodglu_1>0,
   bookgluc:= TRUE]

# alternative metho: td[,bookgluc:=as.numeric(difftime(labdate_1     #,bookdate, units=days))<14 &!is.na(labbloodglu_1) &
# labbloodglu_1>0]

td[, bookglucurine:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(laburglu_1) &
     laburglu_1>0,
   bookglucurine:= TRUE]


widelab <- td[,c("bookevent",
                 "bookgestage",
                 "bookorgname",
                 "bookyear",
                 "bookgluc",
                 "bookglucurine",
                 l1,
                 l2,
                 l3,
                 l4,
                 l5),with=F]


#add labfasting blood glucose variable down here
longlab <- melt(widelab, id.vars = c("bookevent",
                                     "bookgestage",
                                     "bookorgname",
                                     "bookyear",
                                     "bookgluc",
                                     "bookglucurine"),
     measure = patterns(
       "^labgestage_",
       "^labogct_",
       "^labhb_",
       "^labbloodglu_",
       "^laburglu_"
     ),
     value.name=c(
       "labgestage",
       "labogct",
       "labhb",
       "labbloodglu",
       "laburglu"
     ))

#since labgestage are in whole numbers we dont have to worry about the decimal places
#if we had deciimals that we hve to take into consideration, we would have to either put a round paranthesis or use the round function around labgestage
longlab[,labgestagecat:=fancycut::fancycut(x=labgestage,
                                 "0-7"='[0,7]',
                                 "8-12"='[8,12]',
                                 "13-14"='[13,14]',
                                 "15-17"='[15,17]',
                                 "18-22"='[18,22]',
                                 "23-23"='[23,23]',
                                 "24-28"='[24,28]',
                                 "29-30"='[29,30]',
                                 "31-33"='[31,33]',
                                 "34-38"='[34,38]',
                                 "39-99"='[39,99]'
                                 )]
xtabs(~longlab$labgestagecat)
#after this step we see that there are alot that are missing, so we want to check the 
#original labgestage to make sure that we arent losing them when we make our cats

xtabs(~longlab[is.na(labgestagecat)]$labgestage)
#table of extent>0 was the result so most likely they dont have the actual gest age

#here we are seeing how many arent missing of the ones we had before 
#since the sum is 0, we know that they really dont exist so our code is correct
sum(!is.na(longlab[is.na(labgestagecat)]$labgestage))


longlab[,bookgestagecat:=fancycut::fancycut(x=bookgestage,
                                 "0-7"='[0,7]',
                                 "8-12"='[8,12]',
                                 "13-14"='[13,14]',
                                 "15-17"='[15,17]',
                                 "18-22"='[18,22]',
                                 "23-23"='[23,23]',
                                 "24-28"='[24,28]',
                                 "29-30"='[29,30]',
                                 "31-33"='[31,33]',
                                 "34-38"='[34,38]',
                                 "39-99"='[39,99]'
                                  )]

sum(!is.na(longlab[is.na(bookgestagecat)]$bookgestage))




# fancycut(
#   x = -10:10,
#   Zero = 0,
#   Small = '[0,2)',
#   Medium = '[2,5]',
#   Large = '(5,10]'
#)


# aggregate this down to each woman
each_woman <- longlab[!is.na(labgestage),.(
  has_labhb=max(!is.na(labhb) & labhb>0),
  has_labogct=max(!is.na(labogct) & labogct>0)
),keyby=.(
  bookorgname,
  bookyear,
  bookevent,
  bookgestagecat,
  labgestagecat,
  bookgluc,
  bookglucurine
)]




#did this before we did the loop that follows this code
# each_woman[bookgestagecat=="0-7",has_labhb_0_7:=FALSE]
# each_woman[bookgestagecat=="0-7" & 
#              labgestagecat=="0-7" &
#              has_labhb==1,
#               has_labhb_0_7:=TRUE]
# 
# each_woman[bookgestagecat %in% c("0-7","8-12"), 
#            has_labhb_8_12:=FALSE]
# each_woman[bookgestagecat %in% c("0-7","8-12") &
#              labgestagecat %in% c("0-7", "8-12") &
#              has_labhb==1,
#            has_labhb_8_12:=TRUE]
# 
# each_woman[bookgestagecat %in% c("0-7","8-12", "13-14"), 
#            has_labhb_13_14:=FALSE]
# each_woman[bookgestagecat %in% c("0-7","8-12", "13-14") &
#              labgestagecat %in% c("0-7", "8-12", "13-14") &
#              has_labhb==1,
#            has_labhb_13_14:=TRUE]

vars <- c("0-7",
          "8-12",
          "13-14",
          "15-17",
          "18-22",
          "23-23",
          "24-28",
          "29-30",
          "31-33",
          "34-38",
          "39-99")
for(v in c("has_labhb", "has_labogct","has_labfastbloodglu")){
  for(i in 1:length(vars)){
    #below we have a hyphen in the variable name
    #has_labhb_x <- sprintf("has_labhb_%s", vars[i])
    has_x <- sprintf("%s_%s",v, vars[i])
    
    #here we replace the hyphen with an underscore
    has_x <- stringr::str_replace_all(has_x, "-"   ,"_")
    
    #dt[(get, ():=get()]
    #this is the format we use when we want to "get"    
    #something or as assigning it into a variable (assignment
    #). we use get either in row selection or on the right 
    #side of assigning something to look inside 
    
    each_woman[bookgestagecat %in% vars[1:i], 
               (has_x):=FALSE]
    each_woman[bookgestagecat %in% vars[1:i] &
                 labgestagecat %in% vars[i] &
                get(v)==1,
               (has_x):=TRUE]
  
  }
}

each_woman

#max vs sum
#can have multiple 0s but can have multiple ones
#because 
#when you take the maximimum of nothing is negative infinity so get warning message.

#would do this way if we want to aggregate manually but can use the 
#lapply with .SD column function becuase its quicker
# each_woman[,
#            .(
#               has_labhb_0_7= max(has_labhb_0_7, na.rm=T),
#               has_labhb_8_12= max(has_labhb_8_12, na.rm=T),
#               has_labhb_13_14= max(has_labhb_13_14, na.rm=T)
#               ),
#            keyby=.(
#                   bookevent,
#                   bookgestagecat
#                   )
#            
#            ]


# mtcars[order(gear, cyl), lapply(.SD, mean), by = .(gear, cyl), 
#.SDcols = cols_chosen]

each_woman <- each_woman[,
           lapply(.SD, max, na.rm=T),
         
           keyby=.(
             bookyear,
             bookorgname,
             bookevent,
             bookgestagecat,
             bookgluc,
             bookglucurine
           ),
           .SDcols=c("has_labhb_0_7",
                     "has_labhb_8_12",
                     "has_labhb_13_14",
                     "has_labhb_15_17",
                     "has_labhb_18_22",
                     "has_labhb_23_23",
                     "has_labhb_24_28",
                     "has_labhb_29_30",
                     "has_labhb_31_33",
                     "has_labhb_34_38",
                     "has_labhb_39_99",
                     "has_labogct_24_28",
                     "has_labfastbloodglu_24_28"
    
                     
           )
           
           ]



#getting rid of the infinites
vars <- names(each_woman)

for (v in vars){
  each_woman[is.infinite(get(v)), (v):= NA]
}

#checking the information we have
longlab[bookevent=="zzghXNIsTfM"]

# making an ugly table
uglytable <- each_woman[,
  .(
    labhb_denom_0_7=sum(!is.na(has_labhb_0_7)),
    lab_hb_num_0_7= sum(has_labhb_0_7, na.rm = T),
    lab_hb_perc_0_7= round(100*mean(has_labhb_0_7, na.rm = T),
                           digits=1),
    
    labhb_denom_8_12=sum(!is.na(has_labhb_8_12)),
    labhb_num_8_12= sum(has_labhb_8_12, na.rm = T),
    lab_hb_perc_8_12= round(100*mean(has_labhb_8_12, na.rm = T),
                           digits=1),
    
    book_gluc_urine_denom=sum(!is.na(bookglucurine)),
    book_gluc_urine_num=sum(bookglucurine, na.rm=T),
    book_gluc_urine_perc= round(100*mean(bookglucurine, na.rm = T),
                          digits=1),
    
    book_gluc_denom=sum(!is.na(bookgluc)),
    book_gluc_num=sum(bookgluc, na.rm=T),
    book_gluc_perc= round(100*mean(bookgluc, na.rm = T),
                          digits=1),
    
    fastblogluc_denom_24_28=sum(!is.na(has_labfastbloodglu_24_28)),
    fastblogluc_num_24_28= sum(has_labfastbloodglu_24_28, na.rm = T),
    fastblogluc_perc_24_28= round(100*mean(has_labfastbloodglu_24_28,
                          na.rm = T),digits=1),
    
    ogct_denom_24_28=sum(!is.na(has_labogct_24_28)),
    ogct_num_24_28= sum(has_labogct_24_28, na.rm = T),
    ogct_perc_24_28= round(100*mean(has_labogct_24_28, na.rm = T),
                           digits=1)
    
  ),
  keyby=
  .(
    bookyear,
    bookorgname
  )
]

