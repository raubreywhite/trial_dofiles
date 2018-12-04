###### SETUP STARTS ######

tryCatch({
  setwd("X:/data processing/trial_dofiles")
}, error=function(err){
  setwd("Z:/data processing/trial_dofiles")
})

# load in all the data cleaning code
fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

# load in the specific analyses code
fileSources = file.path("r_pniphabstracts2018", list.files("r_pniphabstracts2018", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup(IS_GAZA = FALSE)

###### SETUP ENDS ######

#d <- LoadDataFileFromNetworkGaza()
d <- LoadDataFileFromNetworkWB()
CreatingFurtherVariablesPNIPH(d)


Analyse_pniph_abstract_2018_cpo(d)
Analyse_clex_abstract_2018_clex(d)
Analyse_pniph_abstract_2018_ppc()
completenbc()
Analyse_pniph_abstract_2018_nbc_services()
