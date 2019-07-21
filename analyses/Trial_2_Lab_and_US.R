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

td <- d[ident_dhis2_booking==1 & ident_TRIAL_2_and_3==T & bookyear=="2018"]
bookingtrial2 <- td[, .(
  numBOOK=sum(ident_dhis2_booking==1, na.rm = T)
), keyby=.(
  str_TRIAL_2_Cluster,
  bookorgname,
  bookyear
)]


#OGCT and anemia screening
#anc visits
a1 <- stringr::str_subset(names(td),"^angestage_")
a2 <- stringr::str_subset(names(td),"^andate_")
a3 <- stringr::str_subset(names(td),"^anevent_")


# attempt to make a long lab dataset
l1a <- stringr::str_subset(names(td),"^labgestage_")
l1b <- stringr::str_subset(names(td),"^labdate_")
l2 <- stringr::str_subset(names(td),"^labogct_")
l3 <- stringr::str_subset(names(td),"^labhb_")
l4 <- stringr::str_subset(names(td),"^labbloodglu_")
l5 <- stringr::str_subset(names(td),"^laburglu_")
l6 <- stringr::str_subset(names(td),"^labfastbloodglu_")

#ultrasound data sets
us1 <- stringr::str_subset(names(td),"^usgestage_")
us2 <- stringr::str_subset(names(td),"^usdate_")
us3 <- stringr::str_subset(names(td),"^uspres_")
us4 <- stringr::str_subset(names(td),"^usevent_")


#risks
r1a <- stringr::str_subset(names(td),"^riskgestage_")
r1b <- stringr::str_subset(names(td),"^riskdate_")
r2 <- stringr::str_subset(names(td),"^risktype_")

m1 <- stringr::str_subset(names(td),"^mandate_")
m2 <- stringr::str_subset(names(td),"^mantypey_")
m3 <- stringr::str_subset(names(td),"^mangestage_")



#blood hb
td[, bookhb:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labhb_1) &
     labhb_1>0,
   bookhb:= TRUE]

td[, book_anemia:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labhb_1) &
     labhb_1>0 & labhb_1<11,
   book_anemia:= TRUE]
# 
# # doing RISKS FOR ANEMIA
# td[book_anemia==T, book_anemia_risk := FALSE]
# # we need to cycle through all the risktypes
# # therefore we need to figure out HOW MANY risk types there are first
# numbers <- stringr::str_subset(names(td),"^risktype_")
# numbers <- stringr::str_remove(numbers,"risktype_")
# for(i in numbers){
#   risktype <- sprintf("risktype_%s",i)
#   riskdate <- sprintf("riskdate_%s",i)
#   
#   td[!is.na(book_anemia_risk) &
#        get(risktype) %in% c(
#          "MildAnemia",
#          "ModerateAnemia",
#          "SevereAnemia",
#          "HGBNotImproved"
#          ) &
#        lubridate::`%within%`(
#          get(riskdate), 
#          lubridate::interval(labdate_1,labdate_1+13)
#          ),
#      book_anemia_risk:= TRUE]
# }
# xtabs(~td$book_anemia_risk)

#bloodglu
td[, bookgluc:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(labbloodglu_1) &
     labbloodglu_1>0,
   bookgluc:= TRUE]



# alternative method: td[,bookgluc:=as.numeric(difftime(labdate_1     #,bookdate, units=days))<14 &!is.na(labbloodglu_1) &
# labbloodglu_1>0]

td[, bookglucurine:=FALSE]
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(laburglu_1) &
     laburglu_1>0,
   bookglucurine:= TRUE]
#if bookglucurine_NEG is false, then at booking the value is true. Did it this way so 
#we dont have to make another variable for the positive one
td[as.numeric(difftime(labdate_1,bookdate, units="days"))<14 &
     !is.na(laburglu_1) &
     laburglu_1=="NEG",
   bookglucurine_NEG:= TRUE]



widelab <- td[,c("bookevent",
                 "bookgestage",
                 "bookorgname",
                 "bookyear",
                 "bookhb",
                 "bookgluc",
                 "bookglucurine",
                 "book_anemia",
                 "bookglucurine_NEG",
                 "str_TRIAL_2_Cluster",
                 a1,
                 a2,
                 a3,
                 l1a,
                 l1b,
                 l2,
                 l3,
                 l4,
                 l5,
                 l6,
                 r1a,
                 r1b,
                 r2,
                 m1,
                 m2,
                 m3,
                 us1,
                 us2,
                 us3,
                 us4),with=F]


#add labfasting blood glucose variable down here
longlab <- melt(widelab, id.vars = c("bookevent",
                                     "bookgestage",
                                     "str_TRIAL_2_Cluster",
                                     "bookorgname",
                                     "bookyear",
                                     "bookhb",
                                     "bookgluc",
                                     "bookglucurine",
                                     "book_anemia",
                                     "bookglucurine_NEG"),
                measure = patterns(
                  "^angestage_",
                  "^andate_",
                  "^anevent_",
                  "^labgestage_",
                  "^labdate_",
                  "^labogct_",
                  "^labhb_",
                  "^labbloodglu_",
                  "^laburglu_",
                  "^labfastbloodglu_",
                  "^usgestage_",
                  "^usdate_",
                  "^uspres_",
                  "^usevent_",
                  "^riskgestage_",
                  "^riskdate_",
                  "^risktype_",
                  "^mandate_",
                  "^mantypey_",
                  "^mangestage_"
                 
                ),
                value.name=c(
                  "angestage",
                  "andate",
                  "anevent",
                  "labgestage",
                  "labdate",
                  "labogct",
                  "labhb",
                  "labbloodglu",
                  "laburglu",
                  "labfastbloodglu",
                  "usgestage",
                  "usdate",
                  "uspres",
                  "usevent",
                  "riskgestage",
                  "riskdate",
                  "risktype",
                  "mandate",
                  "mantypey",
                  "mangestage"
                ))

#since labgestage are in whole numbers we dont have to worry about the decimal places
#if we had deciimals that we hve to take into consideration, we would have to either put a round paranthesis or use the round function around labgestage
longlab[,angestagecat:=fancycut::fancycut(x=angestage,
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
xtabs(~longlab$angestagecat)


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

longlab[,usgestagecat:=fancycut::fancycut(x=usgestage,
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
sum(!is.na(longlab[is.na(usgestagecat)]$usgestage))


longlab[,riskgestagecat:=fancycut::fancycut(x=riskgestage,
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

longlab[,mangestagecat:=fancycut::fancycut(x=mangestage,
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

# lab risk

# declare all the people this is valid for
longlab[book_anemia==T,book_anemia_risk:=FALSE]

####### CREATING CLEAN DEFINITIONS FOR EACH ROW
longlab[,has_anvisit:= !is.na(anevent) & angestage>0]

longlab[,has_labhb:=!is.na(labhb) & labhb>0]
longlab[,has_labanemia:=!is.na(labhb) & labhb>0 & labhb<11]

# we need to figure out the first gestational age with anemia
# so we can use this variable with the risk data
longlab[,labgestage_first_anemia:=NULL]
longlab[has_labanemia==TRUE,
        labgestage_first_anemia:=min(labgestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(labgestage_first_anemia),labgestage_first_anemia:=NA]
longlab[,labgestage_first_anemia:=mean(labgestage_first_anemia,na.rm=T),
        by=bookevent]
longlab[!is.na(labgestage_first_anemia), has_labanemia_risk:=FALSE]
longlab[!is.na(has_labanemia_risk==TRUE) &
          risktype %in% c(
            "MildAnemia",
            "ModerateAnemia",
            "SevereAnemia",
            "HGBNotImproved"
          ) &
          riskgestage >= labgestage_first_anemia, has_labanemia_risk:=TRUE]

longlab[,has_labanemia_risk_man:=!is.na(has_labanemia_risk) & 
                                  mantypey%in%c("MildAnemiaTreatmentFollowup",
                                                "MildAneTreatment",
                                                "ModAnemiaTreatmentFollowup",
                                                "ModAneTreatment",
                                                "SevAne"
                                              
                                  ) ]

###Making variables for management process
longlab[,has_laburglu:=!is.na(laburglu)]
longlab[,has_laburglu_neg:= !is.na(has_laburglu) & laburglu=="NEG"]
longlab[,has_labbloodglu:=!is.na(has_laburglu) & !is.na(labbloodglu) & labbloodglu>0]
longlab[,has_labbloodglu_105_139:=!is.na(has_labbloodglu) & 
                                         labbloodglu>=105 & 
                                         labbloodglu<140]
longlab[,has_labbloodglu_140plus:=!is.na(has_labbloodglu) & labbloodglu>=140]

longlab[,has_laburglu_pos_rbs140plus:=  (has_laburglu_neg==FALSE) &
                                        (has_labbloodglu_140plus==TRUE)]


longlab[,has_labogct:= (has_labbloodglu_105_139==T) & !is.na(labogct) & labogct>0 ]
longlab[,has_labogct_140plus:=!is.na(has_labogct) & labogct>=140]

longlab[,has_labfastbloodglu:=!is.na(labfastbloodglu) & labfastbloodglu>0]

####diabetes risk and management
longlab[,has_labDM:=(has_labogct_140plus==TRUE) |
                    (has_labbloodglu_140plus==TRUE) |
                    (has_laburglu_pos_rbs140plus==TRUE)]

# START HERE we need to figure out the first gestational age with anemia
# so we can use this variable with the risk data
longlab[,labgestage_first_DM:=NULL]
longlab[has_labDM==TRUE,
        labgestage_first_DM:=min(labgestage,na.rm=T),
        by=bookevent]
longlab[is.infinite(labgestage_first_DM),labgestage_first_DM:=NA]
longlab[,labgestage_first_DM:=mean(labgestage_first_DM,na.rm=T),
        by=bookevent]
longlab[!is.na(labgestage_first_DM), has_labDM_risk:=FALSE]
longlab[!is.na(has_labDM_risk) &
          risktype %in% c(
            "GDM",
            "LikelyGDM",
            "LDM"
          ) &
          riskgestage >= labgestage_first_DM, has_labDM_risk:=TRUE]

longlab[,has_labDM_risk_man:=!is.na(has_labDM_risk) & 
          mantypey%in%c("DiabetesCondSpec",
                        "DiabetesDC",
                        "DiabetesHR",
                        "DiabetesRoutineBST",
                        "DiabetesUrineStickPositiveBST",
                        "GTTLikelyGDM",
                        "LikelyDiabetes",
                        "LikelyGDMLab",
                        "ScreeningGDM",
                        "ScreeningUrineGlucose"
                        
          ) ]

length(unique(longlab$bookevent))

#creating variables for ultrasound
longlab[,has_us:=!is.na(usevent) & !is.na(usdate)]


# aggregate this down to each woman
each_woman <- longlab[,.(
  has_anvisit=max(has_anvisit,na.rm=T),
  has_labhb=max(has_labhb,na.rm=T),
  has_labanemia=max(has_labanemia,na.rm=T),
  has_labanemia_risk=max(has_labanemia_risk,na.rm=T),
  has_labanemia_risk_man=max(has_labanemia_risk_man,na.rm=T),
  has_labanemia_risk_man2=sum(has_labanemia_risk_man,na.rm=T),
  has_labbloodglu=max(has_labbloodglu,na.rm=T),
  has_labogct=max(has_labogct,na.rm=T),
  has_labfastbloodglu=max(has_labfastbloodglu,na.rm=T),
  has_laburglu_neg=max(has_laburglu_neg,na.rm=T),
  has_labbloodglu_105_139=max(has_labbloodglu_105_139,na.rm=T),
  has_labogct_140plus=max(has_labogct_140plus,na.rm=T),
  has_labDM_risk=max(has_labDM_risk,na.rm=T),
  has_labDM_risk_man=max(has_labDM_risk_man,na.rm=T),
  has_us=max(has_us, na.rm=T)
  
  
    ),keyby=.(
          bookorgname,
          str_TRIAL_2_Cluster,
          bookyear,
          bookevent,
          bookgestagecat,
          bookhb,
          angestagecat,
          labgestagecat,
          usgestagecat,
          bookgluc,
          bookglucurine,
          riskgestagecat,
          mangestagecat
  )]
length(unique(each_woman$bookevent))

#getting rid of the infinites
vars <- names(each_woman)
for (v in vars){
  each_woman[is.infinite(get(v)), (v):= NA]
}

sum(each_woman$has_labhb,na.rm=T)
sum(each_woman$has_us,na.rm=T)



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

# expand.grid multipies all of the columns together
# to create all possible combinations
# below this, we have a double loop. The reason that we have
# a double loop is because we want to cycle through all combinations
# of two variables. Instead of |creating it as we go", we can create
# ALL COMBINATIONS of the two variables before hand. This means that we
# just cycle through this instead, which only requires ONE LOOP and therefore
# simplifies everything
overview <- expand.grid(
  v=c(
    "has_anvisit",
    "has_labhb",
    "has_labanemia", 
    "has_labogct",
    "has_labfastbloodglu",
    "has_labbloodglu",
    "has_labanemia_risk",
    "has_labanemia_risk_man",
    "has_labbloodglu",
    "has_labogct",
    "has_laburglu_neg",
    "has_labfastbloodglu",
    "has_laburglu_neg",
    "has_labbloodglu_105_139",
    "has_labogct_140plus",
    "has_labDM_risk",
    "has_labDM_risk_man",
    "has_us"),
  
    i=1:length(vars),
     stringsAsFactors=F
)
# overview is now a data.frame of all combinations of the above two variables
# we use stringsAsFactors=F to make sure that all strings are STRINGS
# (otherwise they will be factors)
# setDT turns it into data.table
setDT(overview)
overview[,ages:=vars[i]]
overview[,gestage_var:="labgestagecat"]
overview[v %in% c(
  "has_anvisit"
),gestage_var:="angestagecat"]
overview[v %in% c(
  "has_us"
),gestage_var:="usgestagecat"]
overview[v %in% c(
  "has_labanemia_risk",
  "has_labDM_risk"
),gestage_var:="riskgestagecat"]

overview[v %in% c(
  "has_labanemia_risk_man",
  "has_labDM_risk_man"
),gestage_var:="mangestagecat"]


# new variable to create
overview[,has_x:=stringr::str_replace_all(sprintf("%s_%s",v,ages),"-","_")]
overview

# this here is a single loop over all combinations of v and i
# this could replace the double loop below
for(j in 1:nrow(overview)){
  has_x = overview$has_x[j]
  gestage = overview$gestage_var[j]
  i = overview$i[j]
  v = overview$v[j]
  
  #dt[(get, ():=get()]
  #this is the format we use when we want to "get"    
  #something or as assigning it into a variable (assignment
  #). we use get either in row selection or on the right 
  #side of assigning something to look inside 
  
  each_woman[bookgestagecat %in% vars[1:i], 
             (has_x):=FALSE]
  each_woman[bookgestagecat %in% vars[1:i] &
               get(gestage) %in% vars[i] &
               get(v)==1,
             (has_x):=TRUE]
}

sum(each_woman$has_labanemia_0_7,na.rm=T)


# 
# for(v in c("has_labhb", "has_labanemia", "has_labogct","has_labfastbloodglu","has_labbloodglu")){
#   for(i in 1:length(vars)){
#     #below we have a hyphen in the variable name
#     #has_labhb_x <- sprintf("has_labhb_%s", vars[i])
#     has_x <- sprintf("%s_%s",v, vars[i])
#     
#     #here we replace the hyphen with an underscore
#     has_x <- stringr::str_replace_all(has_x, "-"   ,"_")
#     
#     #dt[(get, ():=get()]
#     #this is the format we use when we want to "get"    
#     #something or as assigning it into a variable (assignment
#     #). we use get either in row selection or on the right 
#     #side of assigning something to look inside 
#     
#     each_woman[bookgestagecat %in% vars[1:i], 
#                (has_x):=FALSE]
#     each_woman[bookgestagecat %in% vars[1:i] &
#                  labgestagecat %in% vars[i] &
#                  get(v)==1,
#                (has_x):=TRUE]
#     
#   }
# }


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
                           str_TRIAL_2_Cluster,
                           bookorgname,
                           bookevent,
                           bookgestagecat,
                           bookhb,
                           bookgluc,
                           bookglucurine
                         ),
                         .SDcols=c("has_anvisit_0_7",
                                   "has_anvisit_8_12",
                                   "has_anvisit_13_14",
                                   "has_anvisit_15_17",
                                   "has_anvisit_18_22",
                                   "has_anvisit_23_23",
                                   "has_anvisit_24_28",
                                   "has_anvisit_29_30",
                                   "has_anvisit_31_33",
                                   "has_anvisit_34_38",
                                   "has_anvisit_39_99",
                                   "has_labhb_0_7",
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
                                   "has_labanemia_24_28",
                                   "has_labanemia_31_33",
                                   "has_labanemia_34_38",
                                   "has_labanemia_risk_24_28",
                                   "has_labanemia_risk_31_33",
                                   "has_labanemia_risk_34_38",
                                   "has_labanemia_risk_man_24_28",
                                   "has_labanemia_risk_man_31_33",
                                   "has_labanemia_risk_man_34_38",
                                   "has_labfastbloodglu_0_7",
                                   "has_labfastbloodglu_8_12",
                                   "has_labfastbloodglu_13_14",
                                   "has_labfastbloodglu_15_17",
                                   "has_labfastbloodglu_18_22",
                                   "has_labfastbloodglu_23_23",
                                   "has_labfastbloodglu_24_28",
                                   "has_labfastbloodglu_29_30",
                                   "has_labfastbloodglu_31_33",
                                   "has_labfastbloodglu_34_38",
                                   "has_labfastbloodglu_39_99",
                                   "has_labbloodglu_0_7",
                                   "has_labbloodglu_8_12",
                                   "has_labbloodglu_13_14",
                                   "has_labbloodglu_15_17",
                                   "has_labbloodglu_18_22",
                                   "has_labbloodglu_23_23",
                                   "has_labbloodglu_24_28",
                                   "has_labbloodglu_29_30",
                                   "has_labbloodglu_31_33",
                                   "has_labbloodglu_34_38",
                                   "has_labbloodglu_39_99",
                                   "has_labogct_24_28",
                                   "has_labogct_29_30",
                                   "has_labogct_31_33",
                                   "has_labogct_34_38",
                                   "has_labogct_39_99",
                                   "has_laburglu_neg_0_7",
                                   "has_laburglu_neg_8_12",
                                   "has_laburglu_neg_13_14",
                                   "has_laburglu_neg_13_14",
                                   "has_laburglu_neg_15_17",
                                   "has_laburglu_neg_18_22",
                                   "has_laburglu_neg_23_23",
                                   "has_laburglu_neg_24_28",
                                   "has_laburglu_neg_29_30",
                                   "has_laburglu_neg_31_33",
                                   "has_laburglu_neg_34_38",
                                   "has_laburglu_neg_39_99",
                                   "has_labogct_140plus_29_30",
                                   "has_labogct_140plus_31_33",
                                   "has_labogct_140plus_34_38",
                                   "has_labogct_140plus_39_99",
                                   "has_labDM_risk_0_7",
                                   "has_labDM_risk_8_12",
                                   "has_labDM_risk_13_14",
                                   "has_labDM_risk_15_17",
                                   "has_labDM_risk_18_22",
                                   "has_labDM_risk_23_23",
                                   "has_labDM_risk_24_28",
                                   "has_labDM_risk_29_30",
                                   "has_labDM_risk_31_33",
                                   "has_labDM_risk_34_38",
                                   "has_labDM_risk_39_99",
                                   "has_labDM_risk_man_0_7",
                                   "has_labDM_risk_man_8_12",
                                   "has_labDM_risk_man_13_14",
                                   "has_labDM_risk_man_15_17",
                                   "has_labDM_risk_man_18_22",
                                   "has_labDM_risk_man_23_23",
                                   "has_labDM_risk_man_24_28",
                                   "has_labDM_risk_man_29_30",
                                   "has_labDM_risk_man_31_33",
                                   "has_labDM_risk_man_34_38",
                                   "has_labDM_risk_man_39_99",
                                   "has_us_0_7",
                                   "has_us_8_12",
                                   "has_us_13_14",
                                   "has_us_15_17",
                                   "has_us_18_22",
                                   "has_us_23_23",
                                   "has_us_24_28",
                                   "has_us_29_30",
                                   "has_us_31_33",
                                   "has_us_34_38",
                                   "has_us_39_99"
                                   
                                   
                         )
                         
                         ]



#getting rid of the infinites
vars <- names(each_woman)

for (v in vars){
  each_woman[is.infinite(get(v)), (v):= NA]
}

#checking the information we have
longlab[bookevent=="zzghXNIsTfM"]

# making an ugly table for lab screenings
uglytable <- each_woman[,
                        .(
                          N=.N,
                          
                          anvisit_num_0_7= sum(has_anvisit_0_7, na.rm = T),
                          anvisit_num_8_12= sum(has_anvisit_8_12, na.rm = T),
                          anvisit_num_13_14= sum(has_anvisit_8_12, na.rm = T),
                          anvisit_num_15_17= sum(has_anvisit_15_17, na.rm = T),
                          anvisit_num_18_22= sum(has_anvisit_18_22, na.rm = T),
                          anvisit_num_23_23= sum(has_anvisit_23_23, na.rm = T),
                          anvisit_num_24_28= sum(has_anvisit_24_28, na.rm = T),
                          anvisit_num_29_30= sum(has_anvisit_29_30, na.rm = T),
                          anvisit_num_31_33= sum(has_anvisit_31_33, na.rm = T),
                          anvisit_num_34_38= sum(has_anvisit_34_38, na.rm = T),
                          anvisit_num_39_99= sum(has_anvisit_31_33, na.rm = T),
                          
                          book_hb_num=sum(bookhb, na.rm=T),
                          anvisit_num_0_7= sum(has_anvisit_0_7, na.rm = T),
                          labhb_num_0_7= sum(has_labhb_0_7, na.rm = T),
                          
                          labhb_num_8_12= sum(has_labhb_8_12, na.rm = T),
                          labhb_num_13_14= sum(has_labhb_13_14, na.rm = T),
                          labhb_num_15_17= sum(has_labhb_15_17, na.rm = T),
                          labhb_num_18_22= sum(has_labhb_18_22, na.rm = T),
                          labhb_num_23_23= sum(has_labhb_23_23, na.rm = T),
                          
                          labhb_num_24_28= sum(has_labhb_24_28, na.rm = T),
                          labanemia_num_24_28= sum(has_labanemia_24_28, na.rm = T),
                          labanemia_risk_num_24_28= sum(has_labanemia_risk_24_28, na.rm = T),
                          labanemia_risk_man_num_24_28= sum(has_labanemia_risk_man_24_28, na.rm = T),
                          
                          
                          labhb_num_29_30= sum(has_labhb_29_30, na.rm = T),
                          
                          labhb_num_31_33= sum(has_labhb_31_33, na.rm = T),
                          labanemia_num_31_33= sum(has_labanemia_31_33, na.rm = T),
                          labanemia_risk_num_31_33= sum(has_labanemia_risk_31_33, na.rm = T),
                          labanemia_risk_man_num_31_33= sum(has_labanemia_risk_man_31_33, na.rm = T),
                          
                          
                          labhb_num_34_38= sum(has_labhb_34_38, na.rm = T),
                          labanemia_num_34_38= sum(has_labanemia_34_38, na.rm = T),
                          labanemia_risk_num_34_38= sum(has_labanemia_risk_34_38, na.rm = T),
                          labanemia_risk_man_num_34_38= sum(has_labanemia_risk_man_34_38, na.rm = T),
                          
                          
                          book_gluc_urine_num=sum(bookglucurine, na.rm=T),
                          has_laburglu_neg_0_7_num=sum(has_laburglu_neg_0_7, na.rm=T),
                          labbloodglu_num_0_7= sum(has_labbloodglu_0_7, na.rm = T),
                          
                          has_laburglu_neg_8_12_num=sum(has_laburglu_neg_8_12, na.rm=T),
                          labbloodglu_num_8_12= sum(has_labbloodglu_8_12, na.rm = T),
                          
                          has_laburglu_neg_13_14_num=sum(has_laburglu_neg_13_14, na.rm=T),
                          
                          has_laburglu_neg_15_17_num=sum(has_laburglu_neg_15_17, na.rm=T),
                          labbloodglu_num_15_17= sum(has_labbloodglu_15_17, na.rm = T),
                          
                          
                          
                          has_laburglu_neg_18_22_num=sum(has_laburglu_neg_18_22, na.rm=T),
                          labbloodglu_num_18_22= sum(has_labbloodglu_18_22, na.rm = T),
                          
                          
                          has_laburglu_neg_23_23_num=sum(has_laburglu_neg_23_23, na.rm=T),
                          labbloodglu_num_23_23= sum(has_labbloodglu_23_23, na.rm = T),
                          
                          has_laburglu_neg_24_28_num=sum(has_laburglu_neg_24_28, na.rm=T),
                          labbloodglu_num_24_28= sum(has_labbloodglu_24_28, na.rm = T),
                          
                          ogct_num_24_28= sum(has_labogct_24_28, na.rm = T),
                          
                          has_laburglu_neg_29_30_num=sum(has_laburglu_neg_29_30, na.rm=T),
                          labbloodglu_num_29_30= sum(has_labbloodglu_29_30, na.rm = T),
                          
                          
                          has_laburglu_neg_31_33_num=sum(has_laburglu_neg_31_33, na.rm=T),
                          labbloodglu_num_31_33= sum(has_labbloodglu_31_33, na.rm = T),
                          
                          
                          has_laburglu_neg_34_38_num=sum(has_laburglu_neg_34_38, na.rm=T),
                          labbloodglu_num_34_38= sum(has_labbloodglu_34_38, na.rm = T),
                          
                          has_laburglu_neg_39_99_num=sum(has_laburglu_neg_39_99, na.rm=T),
                          labbloodglu_num_39_99= sum(has_labbloodglu_39_99, na.rm = T)
                          
                      
                          
                        
                        ),
                        keyby=
                          .(
                            bookyear,
                            str_TRIAL_2_Cluster,
                            bookgestagecat
                          )
                        
                        
                        ]
uglytable

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "trial_2",
                       "LabScreenings_Anemia_GDM.xlsx"))

##### Table for US ######
uglytable <- each_woman[,
                        .(
                          N=.N,
                          
                          us_num_0_7= sum(has_us_0_7, na.rm = T),
                          us_num_8_12= sum(has_us_8_12, na.rm = T),
                          us_num_13_14= sum(has_us_13_14, na.rm = T),
                          us_num_15_17= sum(has_us_15_17, na.rm = T),
                          us_num_18_22= sum(has_us_18_22, na.rm = T),
                          us_num_23_23= sum(has_us_23_23, na.rm = T),
                          us_num_24_28= sum(has_us_24_28, na.rm = T),
                          us_num_29_30= sum(has_us_29_30, na.rm = T),
                          us_num_31_33= sum(has_us_31_33, na.rm = T),
                          us_num_34_38= sum(has_us_34_38, na.rm = T)
                          
                          
                          
                          
                          
                          
                          
                        ),
                        keyby=
                          .(
                            bookyear,
                            str_TRIAL_2_Cluster,
                            bookgestagecat
                          )
                        
                        
                        ]
uglytable

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DROPBOX_RESULTS,
                       "trial_2",
                       "US_screenings.xlsx"))