ChiSqTestForMonthlyAvicennaMatching <- function(
  trialArmA,
  trialArmB,
  is_Avicenna_abb_amd_A,
  is_Avicenna_abb_amd_B
){
  
  x <- matrix(c(
    sum(trialArmA)-sum(is_Avicenna_abb_amd_A),
    sum(trialArmB)-sum(is_Avicenna_abb_amd_B),
    sum(is_Avicenna_abb_amd_A),
    sum(is_Avicenna_abb_amd_B)
  ),ncol=2)
  
  returnValue <- 99
  try(returnValue <- chisq.test(x)$p.value, TRUE)
  
  return(returnValue)
  
}

BASE_LINE_STATISTICAL_ANALYSIS <- function(d){

  bookings <- d[bookdate>=as.Date("2017-01-15") & bookdate<=as.Date("2017-09-15"),
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  
  print(bookings)
  bookings[,cohort:="Bookings"]
  print(bookings)

  
  Trial1 <- d[ident_TRIAL_1==TRUE,
                .(
                  numWomen=.N
                ),
                by=.(
                  ident_dhis2_control
                )]
  Trial1[,cohort:="Trial 1"]
  print(Trial1)    
  
  EPD <- d[ident_TRIAL_1==TRUE & 
         isExpectedToHaveDelivered==TRUE,
              .(
                numWomen=.N
              ),
              by=.(
                ident_dhis2_control
              )]
  EPD[,cohort:="Expected to be delivered"]
  print(EPD)    
  
  
  MATCH <- d[ident_TRIAL_1==TRUE & 
             isExpectedToHaveDelivered==TRUE &
               !is.na(ident_avic_any),
           .(
             numWomen=.N
           ),
           by=.(
             ident_dhis2_control
           )]
  MATCH[,cohort:="Matched with Avicenna"]
  print(MATCH)    
  
  
  plotData <- rbind(bookings,
                    Trial1,
                    EPD,
                    MATCH)
  
  print(plotData)
  
  # turn cohort into a factor, with levels in the right order
  plotData[,cohort:=factor(cohort,
                           levels=c(
                             "Bookings",
                             "Trial 1",
                             "Expected to be delivered",
                             "Matched with Avicenna"
                           ))]
  
  plotData[,intOrControl:="Intervention"]
  plotData[ident_dhis2_control==TRUE,intOrControl:="Control"]
  
  print(plotData)
  
  setorder(plotData,cohort,-intOrControl)
  plotData[,cumulative_y:=cumsum(numWomen)-numWomen/2,by=cohort]
  
p <- ggplot(plotData)
p <- p + geom_bar(mapping=aes(x=cohort,y=numWomen,fill=intOrControl),stat="identity",colour="black")
p <- p + geom_text(mapping=aes(x=cohort,y=cumulative_y,label=numWomen))
p <- p + theme_gray(base_size = 16)
p <- p + scale_fill_brewer("",palette="BuPu")
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Number of women in 2017")
p <- p + labs(title="Trial 1")
p <- p + labs(caption=sprintf("Date of data extraction: %s",CLINIC_CONTROL_DATE))
p
ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "random",
  "baselinecohort.png"),
       plot=p,
       height=210,
       width=297,
       units="mm")
  
  
  ######## 

####Denominators

d[,bookyearmonth:=sprintf("%s-%s",lubridate::year(bookdate),formatC(lubridate::month(bookdate),width=2,flag="0"))]

res <- d[,.(
  total=.N,
  mahimasClinics=sum(ident_TRIAL_1,na.rm=T),
  trialArmA=sum(ident_TRIAL_1==T &ident_dhis2_control==T, na.rm=T),
  trialArmB=sum(ident_TRIAL_1==T &ident_dhis2_control==F, na.rm=T),
  is_Avicenna_abb_amd=sum(
    ident_TRIAL_1==T & 
    ident_avic_abb==T & 
    ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb=sum( 
    ident_TRIAL_1==T &
    ident_avic_abb==T, na.rm=T),
  
  is_Avicenna_amd=sum( 
    ident_TRIAL_1==T &
    ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb_amd_A=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==T &
      ident_avic_abb==T & 
      ident_avic_amd==T, na.rm=T),
  
  is_Avicenna_abb_amd_B=sum(
    ident_TRIAL_1==T & 
      ident_dhis2_control==F &
      ident_avic_abb==T & 
      ident_avic_amd==T, na.rm=T)
  
  
  ),by=.(
  bookyearmonth
)]
setorder(res,bookyearmonth)
print(res)


res[,propAvicenna:=is_Avicenna_abb_amd/mahimasClinics]
res[,propAvicennaA:=is_Avicenna_abb_amd_A/trialArmA]
res[,propAvicennaB:=is_Avicenna_abb_amd_B/trialArmB]

res[,monthlyAviccenaPvalue:=
      ChiSqTestForMonthlyAvicennaMatching(
        trialArmA=trialArmA,
        trialArmB=trialArmB,
        is_Avicenna_abb_amd_A=is_Avicenna_abb_amd_A,
        is_Avicenna_abb_amd_B=is_Avicenna_abb_amd_B),
    by=bookyearmonth]

#### OVERALL PVALUE

# number control
sum(res$trialArmA)

# number intervention
sum(res$trialArmB)

# number control matched
sum(res$is_Avicenna_abb_amd_A)

# number intervention
sum(res$is_Avicenna_abb_amd_B)

x <- matrix(c(
  sum(res$trialArmA)-sum(res$is_Avicenna_abb_amd_A),
  sum(res$trialArmB)-sum(res$is_Avicenna_abb_amd_B),
  sum(res$is_Avicenna_abb_amd_A),
  sum(res$is_Avicenna_abb_amd_B)
),ncol=2)

(overallPvalue <- chisq.test(x)$p.value)
res[,totalAviccenaPvalue:=overallPvalue]


openxlsx::write.xlsx(res, file.path(FOLDER_DROPBOX_RESULTS,
                                    "mahima",
                                    "random",
                                    "DENOMINATORS.xlsx"))



}

Analyse_Mahima_Random <- function(d){
  BASE_LINE_STATISTICAL_ANALYSIS(d)
}
