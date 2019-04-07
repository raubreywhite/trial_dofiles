###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_dataquality", list.files("r_dataquality", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

d <- LoadDataFileFromNetworkPal()
###### SETUP ENDS ######

Analyse_DataQualityVars(d=d,location="wb")
#Analyse_DataQualityVars(d=d,location="gaza")

Analyse_AutoDuplications(d=d,location="wb")
#Analyse_AutoDuplications(d=d,location="gaza")

Analyse_NumberOfBookingsPerMonth(d=d, location="pal")
Analyse_NumberOfBookingsPerMonth(d=d, location="wb")

Bookdate_vs_booklmp(d=d, location="wb")

Bookdate_before_booklmp(d=d, location="wb")


# what you care about
DataCompletion()


# just messing around
long <- LoadDataLongFromNetworkPal()
longwb <- long[ident_gaza==F]

nrow(longwb[angestage>=31 & angestage<=33])
longwb[,anyear:=lubridate::year(andate)]
longwb[,.(
  N=sum(angestage>=24 & angestage<=28,na.rm=T)
),keyby=.(anyear)
]

longwb[,bookyearx:=lubridate::year(as.Date(as.numeric(bookdate),origin="1970-01-01"))]
longwb[ident_dhis2_booking==T &
  bookgestage>=24 &
  bookgestage<=28,.(
  length(unique(bookevent))
),keyby=.(bookyearx)
]
