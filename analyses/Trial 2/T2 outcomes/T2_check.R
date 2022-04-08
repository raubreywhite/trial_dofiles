
###################
# APPEND
###################

FOLDER_DATA_RAW <<- file.path(getwd(),"../data_raw")

FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")

WB <- readRDS(file.path(FOLDER_DATA_CLEAN,
                        "T2_clean",
                        "T2_FINAL_dataset_2021-08-12_WB.rds"))
nrow(WB)



FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")


# make sure correct data for this file before running
Gaza <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                         "T2_clean",
                         "T2_FINAL_dataset_2020-10-01_Gaza.rds"))
nrow(Gaza)


# combine the data sets

fullT2data <-rbind(WB,
                   Gaza,
                   fill=T)

nrow(fullT2data)==nrow(Gaza)+nrow(WB)



# check the trial codes
# GP019999-MCH	335



length(unique(fullT2data$bookorgcode))