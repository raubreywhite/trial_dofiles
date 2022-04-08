
# extract orgcodes for QID clinics to get usernames #

###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)



###### SETUP ENDS ######


# WB data

d <- LoadDataFileFromNetwork()
nrow(d)


d[,ident_wb:=TRUE]


# Gaza data
g <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                      "full_data_from_r.rds"))
g[,ident_wb:=FALSE]



# append data sets
wbg <- rbind(d,
             g,
             fill=TRUE)




qid <- wbg[(ident_TRIAL_2_and_3==T |
             ident_TRIAL_3==T) &
             ident_dhis2_booking==T, c("bookorgname",
                                       "bookorgdistrict",
               "bookorgcode",
             "bookorgunit",
             "ident_wb")]

qid <- qid[,.(N=.N),
           keyby=.(ident_wb,
                   bookorgdistrict,
                   bookorgname,
                   bookorgunit,
                   bookorgcode)]



# create the workbook    
#dT <- openxlsx::createWorkbook()


# add pages 
#openxlsx::addWorksheet(dT, "bookorgcodes")
# 
# # write data
# openxlsx::writeData(
#   dT,
#   sheet = "phsycian_figures",
#   x = tab,
#   startCol = 1,
#   startRow = 1
# )
# 



openxlsx::write.xlsx(qid[ident_wb==T],
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "QID_orgunit_codes_wb.xlsx"))


openxlsx::write.xlsx(qid[ident_wb==F],
                     file.path(FOLDER_DATA_RESULTS,
                               "T2",
                               "QID_orgunit_codes_gaza.xlsx"))



