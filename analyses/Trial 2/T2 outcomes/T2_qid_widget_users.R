###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA=TRUE)

CheckFilesAndVariables(folder="e.reg-intervention",
                       REF_DATE = REF_CLINIC_INTERVENTION_DATE, 
                       CHECK_DATE = CLINIC_INTERVENTION_DATE)



###### SETUP ENDS ######

# load in data 
d <- LoadDataFileFromNetwork()

# defining trial arm

d[, TrialArm:=as.character(NA)]
d[ident_TRIAL_2==T &
    ident_TRIAL_3==F, TrialArm:="SMS"]
d[ident_TRIAL_3==T &
    ident_TRIAL_2==F, TrialArm:="QID"]
d[ident_TRIAL_2_3_Control==T, TrialArm:="Control"]
d[ident_TRIAL_2==T &
    ident_TRIAL_3==T, TrialArm:="QID and SMS"]
xtabs(~d$TrialArm, addNA=T)



DATE <- "2022_02_25"

# structural data for T2 qid widget

users <- as.data.table(readxl::read_excel(file.path(FOLDER_DATA_RAW_GAZA,
                                      sprintf("QID_USERS_GAZA_%s.xlsx", DATE))))

names(users)

setnames(users,"Bookorgunit","bookorgunit")

# fill in nissing values
users[,bookorgunit:=zoo::na.locf(bookorgunit)]
users[,`Bookorgunit High Risk`:=zoo::na.locf(`Bookorgunit High Risk`)]
users[,`Org Name`:=zoo::na.locf(`Org Name`)]
users[,`Trial Arm`:=zoo::na.locf(`Trial Arm`)]



qidcodes <- as.data.table(d[ident_TRIAL_2==T|
                                     ident_TRIAL_3==T|
                                     ident_TRIAL_2_3_Control==T|
                                     ident_TRIAL_2_and_3==T,
                          c("bookorgname",
                            "bookorgcode",
                            "bookorgunit",
                            "str_TRIAL_2_Cluster",
                            "TrialArm")])

qidcodes <- qidcodes[,.(N=.N),
                     keyby=.(bookorgname,
                             bookorgcode,
                             bookorgunit,
                             str_TRIAL_2_Cluster,
                             TrialArm)]
qidcodes[,N:=NULL]


# merge qid codes with user sheet and roles

usercodes <- merge(users,
                   qidcodes,
                   by="bookorgunit",
                   all.x=T)
nrow(usercodes)
nrow(users)

usercodes[TrialArm=="Control", TrialArm:="CONTROL"]
usercodes[TrialArm=="QID and SMS", TrialArm:="SMS+QID"]

nrow(usercodes[`Trial Arm`==TrialArm])

openxlsx::write.xlsx(usercodes,
                     file.path(FOLDER_DATA_RAW_GAZA,
                               "T2_user_uids_gaza.xlsx"))



































