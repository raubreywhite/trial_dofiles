




covidcases <-data.table(readxl::read_excel(file.path(
                        FOLDER_DATA_RAW,
                        "covidpreg.xlsx")))


setnames(covidcases,"idnum","motheridno")


ids <-unique(covidcases$motheridno)[unique(covidcases$motheridno) %in% unique(d$motheridno)]

covidcases[(covidcases$motheridno) %in% (d$motheridno), eReg:=TRUE]

xtabs(~covidcases$eReg, addNA=T)

length(ids)


covidEreg <- merge(covidcases[eReg==T],
                   d[bookdate>"2019-01-01"],
                   by=c("motheridno"),
                   all.x=T)
nrow(covidEreg)
nrow(covidcases[eReg==T])


covidEreg[,covidbooknum:=1:.N, by=motheridno]
xtabs(~covidEreg$covidbooknum, addNA=T)


covidEreg[covidbooknum==2, c("motheridno")]


book <- names(d)[stringr::str_detect(names(d),"^book")]
varsanc <- names(d)[stringr::str_detect(names(d),"^an")]
varsppc <- names(d)[stringr::str_detect(names(d),"^ppc")]
man <-names(d)[stringr::str_detect(names(d),"^man")]
risk <-names(d)[stringr::str_detect(names(d),"^risk")]
cpo <- names(d)[stringr::str_detect(names(d),"^cpo")]
lab <-names(d)[stringr::str_detect(names(d),"^lab")]
us <- names(d)[stringr::str_detect(names(d),"^us")]

covidEreg[deathofwoman==""]

covid <- covidEreg[tolower(deathofwoman)=="yes",c("motheridno",
                      "deathofwoman",
                      book,
                      varsanc,
                      lab, 
                      us,
                      man,
                      risk,
                      cpo,
                      varsppc),with=F]


openxlsx::write.xlsx(covid,
                     file.path(
                       FOLDER_DATA_RESULTS,
                       "covidcases.xlsx"))








