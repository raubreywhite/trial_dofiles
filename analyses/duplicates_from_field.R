###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))

fileSources=file.path(getwd(),fileSources)
sapply(fileSources, debugSource)

Setup(IS_GAZA=FALSE)


####LOAD d from Network####
d <- LoadDataFileFromNetwork()

tocheck <- d[motheridno %in% c(855036216,
                               401678289,
                               403581036,
                               854299773 ), 
             c("motheridno",
               "firstname",
               "familyname1",
               "familyname2",
               "fathersname",
               "bookdate",
               "bookorgname",
               "bookorgdistrict",
               "booknum")]

# Name:خديجة كمال عبد الفتاح ابو صبحة 
# ID :401678289
# 
# This name contains 3 files with the same name. We contacted the owner of the file and took the ID number and installed it on the file that contains all the information.
# please delete the files that register in the name and remain the files in ID
# 
# Name: جميلة محمد حسين ابو طبيخ 
# ID : 403581036
# 
# please delete the files that register in the name and remain the files in ID.
# 
# 
# Hanin this file we found in the previous pregnancy case empty and not complete, the case does not accept deleting or editing.
# 
# ID: 854299773
