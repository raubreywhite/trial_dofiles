

# how trial 2 data works 

# T2_process_outcomes.R
# T2_Anemia
# T2_Hypertension
# T2_GDM
# Save file after run Anemia, htn,GDM

# Merge with T2_attendance

# Run T2_QIDSMSOutcomes

# append T2 WB and T2 gaza

# calculate outcomes in long format for all four arms

# save copies of WB and Gaza data
# Append the two data sets and send out

# background variable stuff

#Background characteristics  
#•	Women – demographics e.g age, and the following SEP variables
#o	average monthly household incomes (less than 200; 200 – 900; 901 – 1824; 1825 – 3054; and > 3055 Israeli new Shekel#), 
#o	mother’s years of education (<10; 10- 13 years; >13 years), 
#o	age at marriage (less than 20; 21-25; 26- 30; 31- 35; 36- 40; greater than 40 years) 
#o	age at first pregnancy (less than 20; 20-25; 26- 30; 31- 35; 36- 40; greater than 40 years). 
#•	Clinic characteristics   – e.g. lab, UL, level,   
#•	Variables used in the randomization 
#o	phase (time point) of the eRegistry implementation (stratified)
#o	laboratory availability, ultrasound availability and the size of the PHC (constrained)




  

FOLDER_DATA_CLEAN <<- file.path(getwd(),"../data_clean")
 
WB <- readRDS(file.path(FOLDER_DATA_CLEAN,
                                      "T2_clean",
                                      "T2_FINAL_dataset_2020-12-19_WB.rds"))
nrow(WB)


FOLDER_DATA_CLEAN_GAZA <<- file.path(getwd(),"../gaza_data_clean")



Gaza <-readRDS(file.path(FOLDER_DATA_CLEAN_GAZA,
                         "T2_clean",
                         "T2_FINAL_dataset_2020-10-01_Gaza.rds"))
nrow(Gaza)

 
# combine the data sets

fullT2data <-rbind(WB,
                   Gaza,
                   fill=T)

# remove variables from event data that we dont want

# bookdate
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_bookdate")]
fullT2data[,(vars):=NULL]

# ident_WB
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_ident_WB")]
fullT2data[,(vars):=NULL]

# ident_WB
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_ident_schedev_")]
fullT2data[,(vars):=NULL]


# orgname
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_orgname_")]
fullT2data[,(vars):=NULL]


# orgunitcode
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_orgunitcode_")]
fullT2data[,(vars):=NULL]



# orgunitcode
vars <- names(fullT2data)[stringr::str_detect(names(fullT2data),"^evs_bookge_")]
fullT2data[,(vars):=NULL]





nrow(WB)+nrow(Gaza)==nrow(fullT2data)

xtabs(~fullT2data$TrialArm, addNA=T)


#################################################
# save data set
#################################################
# these are the variables we have in the data set

saveRDS(fullT2data,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset.rds"))




#################################################
# save data set with vars we want
#################################################
# these are the variables we have in the data set

varskeep <- c()




saveRDS(outcomes,
        file.path(FOLDER_DATA_CLEAN,
                  "T2_clean",
                  "T2_FINAL_dataset_outcomes.rds"))


#################################################
# Base line characteristics of different Arms
#################################################

library(ggplot2)


# DO ANALYSES HERE
#creating the dataset we want
# TrialArm and dates specified in the data sets

setnames(fullT2data,"TrialArm","prettyExposure")

#data set for analysis is created here so we dont have to reselect 
#women over and over again



##making a table for data we want to analyze from the analysis data set
##for things like parity, make an ugly table because its not a box plot
##or histogram, so better to make a ugly table.
fullT2data[,
       .(
         meanage=mean(age, na.rm=T),
         meanagefirstpreg=mean(agepregnancy, na.rm=TRUE),
         meanavgmonthlyincome= mean(income, na.rm=T),
         meaneducation= mean(education, na.rm=T),
         meanbookweight= mean(bookweight, na.rm=T),
         meanbookheight=mean(bookheight, na.rm=T),
         meanBMI=mean(bookbmi, na.rm=T),
         meabooksystbp= mean(bookbpsyst, na.rm=T),
         meanbookdiastbp=mean(bookbpdiast, na.rm=T),
         meanbookhb= mean(labhb_1, na.rm=T),
         meanincome=mean(income, na.rm=T),
         ProportionBookhbnotmissing=sum(!is.na(labhb_1)),
         ProportionofWeightsis0= mean(bookweight==0, na.rm=T),
         ProportionofWeightsOver100kg= mean(bookweight>100, na.rm=T),
         ProportionofParity= mean(bookparity, na.rm=T),
         MIssingConpara=sum(is.na(conpara)),
         meanbookgestage= mean(bookgestage, na.rm=T),
         ProportionofPrimi=mean(bookprimi, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T),
         meanbookgestage=mean(bookgestage, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T),
         ProportionBookbookhistperi=mean(bookhistperi, na.rm=T),
         ProportionBookbookhistutesur=mean(bookhistutesur, na.rm=T),
         ProportionBookbookhistcs=mean(bookhistcs, na.rm=T),
         ProportionBookbookhistcscompl=mean(bookhistcscompl, na.rm=T),
         ProportionBookbookhistpreterm=mean(bookhistpreterm, na.rm=T),
         ProportionBookbookhistute=mean(bookhistute, na.rm=T),
         ProportionBookbookhistabort=mean(bookhistabort, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T),
         ProportionBookparity=mean(bookparity, na.rm=T)
       ), 
       keyby=.(TrialArm)
       
       ]

#making our plots
###bookweight
# p <- ggplot(fullT2data, aes(x=bookweight))
# p <- p + geom_density()
# p

####################
# bookweight
####################

#from this plot we see that someone was enetered as 6000kg 
#so we want to see where this is happening (in which exposure group)
pval <- t.test(bookweight ~ prettyExposure, data=fullT2data)$p.value

p <- ggplot(fullT2data, aes_string(y="bookweight", x="prettyExposure"))
p <- p + geom_boxplot()
p <- p + geom_boxplot(outlier.color = NA)
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p

####################
# bookbmi
####################

fullT2data[,.(
  meanbookheight=mean(bookheight,na.rm=T),
  medianbookheight=median(bookheight,na.rm=T),
  num0=sum(bookheight==0,na.rm=T)
),
keyby=prettyExposure]

pval <- t.test(bookbmi ~ prettyExposure, data=fullT2data)$p.value

p <- ggplot(fullT2data, aes_string(y="bookbmi", x="prettyExposure"))
p <- p + geom_boxplot()
p <- p + geom_boxplot(outlier.color = NA)
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p

####################
# bookbmi (doing it as a loop)
####################

form <- sprintf("%s ~ prettyExposure", "bookbmi")
form <- glue::glue("{outcome} ~ prettyExposure", outcome="bookbmi")

# 'form' is a string, not a formula. 
# need to turn it into a formula using 'as.formula'
pval <- t.test(as.formula(form), data=fullT2data)$p.value

p <- ggplot(fullT2data, aes_string(y="bookbmi", x="prettyExposure"))
p <- p + geom_boxplot()
p <- p + geom_boxplot(outlier.color = NA)
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p


########add variables we want below in the vector to get loop
for (i in c("bookbmi", 
            "bookweight",
            "bookbpsyst",
            "bookbpdiast",
            "age",
            "income",
            "education",
            "labhb_1")){
  
  form <- sprintf("%s ~ prettyExposure", i)
  form <- glue::glue("{outcome} ~ prettyExposure", outcome=i)
  
  # 'form' is a string, not a formula. 
  # need to turn it into a formula using 'as.formula'
  #we say get(i)!=0 because some of the variables are zero
  #so we want to exclude them from the t.test so we dont get false results
  pval <- t.test(as.formula(form), data=fullT2data[get(i)!=0])$p.value
  
  p <- ggplot(fullT2data, aes_string(y=i, x="prettyExposure"))
  p <- p + geom_boxplot()
  p <- p + geom_boxplot(outlier.color = NA)
  p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
  p
  
  
  ggsave(file.path(
    FOLDER_DROPBOX_RESULTS,
    sprintf("Boxplot_%s.png",i)), 
    plot = p, width = 297, height = 210, units = "mm")
  
  
}


vector1 <- c("a","b","c")
vector2 <- c("f","g","h")

print(vector1)
print(vector2)

for(i in 1:2){
  varThatIWant <- sprintf("vector%s",i)
  print(get(varThatIWant))
}

#Making ugly tables for parity
uglyTable <- fullT2data[,
                    .(
                      N=.N
                      
                    ), 
                    keyby=.(prettyExposure,
                            bookparity,
                            bookhistdm,
                            bookhistgdm,
                            bookhisthtn,
                            bookhistotherch,
                            bookhistcs,
                            bookhistperi,
                            bookprimi,
                            laburglu_1=="POS",
                            laburglu_1=="NEG"
                            
                            
                    )]


#bookprimi
uglyTable[!is.na(bookprimi),denom:=sum(N),by=.(prettyExposure)]
maxYVAL <- max(uglyTable$N)
labelAdjust <- maxYVAL*0.01
#do the other way for as.factor instead better for other purposes
p <- ggplot(uglyTable, aes(x=prettyExposure, y=N/denom, fill=as.factor(bookprimi)))
p <- p + geom_col(alpha=0.75)
#p <- p + geom_text(mapping=bookprimi,aes(y=N+labelAdjust),vjust=0)
p <- p + scale_fill_brewer(palette="Set1")
p <- p + labs(title=" Primi Bookings",
              x="Cohort",
              y= "Proportion of Women")
p <- p + scale_fill_discrete(name="Booked", labels=c("Booked Primi",
                                                     "Not Primi",
                                                     "Missing"))
p <- p + labs(caption=sprintf("Date of data extraction: %s",CLINIC_CONTROL_DATE))
p <- p + labs(fill="Booked")
p <- p + theme(legend.text=element_text(size=10))
p <- p + theme(plot.title = element_text(size = 12, face = "bold"),
               legend.title=element_text(size=10), 
               legend.text=element_text(size=9))
p <- p + theme_gray(20)
p

ggsave(file.path(
  FOLDER_DATA_RESULTS,
  "Number of Primi Bookings.png"), 
  plot = p, width = 297, height = 210, units = "mm")

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "Primi Bookings.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")


########################
##### bookprimi

#Chi-squared for differences in bookprimi
pval <- chisq.test(x=uglyTable$bookprimi, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)


uglyTable[!is.na(bookprimi),denom:=sum(N),by=.(prettyExposure)]

#do the other way for as.factor instead better for other purposes
#Primi
p <- ggplot(uglyTable, aes(x=prettyExposure, y=N, fill=as.factor(bookprimi)))
p <- p + geom_col()
p <- p + scale_fill_brewer(palette="Dark2")
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p

p <- ggplot(uglyTable[!is.na(bookprimi)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookprimi)))
p <- p + geom_col()
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p <- p + labs(title = "Bookprimi",
              y="Proportion of Women",
              fill="bookprimi")
p  


ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookprimi.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")
########################
##### bookparity

#Chi-squared for differences in bookparity
pval <- chisq.test(x=uglyTable$bookparity, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookparity),denom:=sum(N),by=.(prettyExposure)]

#do the other way for as.factor instead better for other purposes
#Parity
p <- ggplot(uglyTable, aes(x=prettyExposure, y=N, fill=as.factor(bookparity)))
p <- p + geom_col()
p <- p + scale_fill_brewer(palette="Dark2")
p <- p + labs(y="Number of Women",
              fill="bookparity",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookparity_with_missing.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

p <- ggplot(uglyTable[!is.na(bookparity)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookparity)))
p <- p + geom_col()
p <- p + scale_fill_brewer(palette="Dark2")
p <- p + labs(y="Proportion of Women",
              fill="bookparity",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookparity.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")


########################
##### bookhistdm

#Chi-squared for differences in bookhistdm
pval <- chisq.test(x=uglyTable$bookhistdm, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookhistdm),denom:=sum(N),by=.(prettyExposure)]

# delete variable
uglyTable[,prettyFill:=NULL]
# create empty variable
uglyTable[,prettyFill:=as.character(NA)]
# set variable to proper values
uglyTable[bookhistdm==0,prettyFill:="Not true"]
uglyTable[bookhistdm==1,prettyFill:="Yes"]
uglyTable[is.na(bookhistdm),prettyFill:="Missing"]
# set the appropriate ordering
uglyTable[,prettyFill:=factor(prettyFill,levels=c("Not true","Yes"))]


p <- ggplot(uglyTable[!is.na(bookhistdm)], aes(x=prettyExposure, y=N, fill=as.factor(bookhistdm)))
p <- p + geom_col()
p <- p + scale_fill_brewer(palette="Dark2")
p <- p + labs(y="Number of Women",
              fill="bookhistdm",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistdm.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")



########################
##### bookhistgdm

#Chi-squared for differences in bookhistgdm
pval <- chisq.test(x=uglyTable$bookhistgdm, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

#bookhistgdm
uglyTable[!is.na(bookhistgdm),denom:=sum(N),by=.(prettyExposure)]

p <- ggplot(uglyTable[!is.na(bookhistgdm)], aes(x=prettyExposure, y=N, fill=as.factor(bookhistgdm)))
p <- p + geom_col()
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p

p <- ggplot(uglyTable[!is.na(bookhistgdm)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookhistgdm)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="bookhistgdm",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistgdm.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

########################
##### bookhistotherch

#Chi-squared for differences in bookhistotherch
pval <- chisq.test(x=uglyTable$bookhistotherch, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookhistotherch),denom:=sum(N),by=.(prettyExposure)]

# raw numbers
p <- ggplot(uglyTable, aes(x=prettyExposure, y=N, fill=as.factor(bookhistotherch)))
p <- p + geom_col()
p <- p + labs(y="Number of Women",
              fill="boookhisther",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistother.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

# percentage
p <- ggplot(uglyTable[!is.na(bookhistotherch)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookhistotherch)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="boookhistotherch",
              caption=sprintf("Chi-squared p-value: %s",pval))
p
ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistother_proportion.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

########################
##### bookhistcs

#Chi-squared for differences in bookhistcs
pval <- chisq.test(x=uglyTable$bookhistcs, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookhistcs),denom:=sum(N),by=.(prettyExposure)]

# raw numbers
p <- ggplot(uglyTable[!is.na(bookhistcs)], aes(x=prettyExposure, y=N, fill=as.factor(bookhistcs)))
p <- p + geom_col()
p <- p + labs(y="Number of Women",
              fill="bookhistcs",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhist.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

# percentage
p <- ggplot(uglyTable[!is.na(bookhistcs)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookhistcs)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="bookhistcs",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhist_proportion.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

########################
##### bookhisthtn

#Chi-squared for differences in bookhisthtn
pval <- chisq.test(x=uglyTable$bookhisthtn, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookhisthtn),denom:=sum(N),by=.(prettyExposure)]

# raw numbers
p <- ggplot(uglyTable[!is.na(bookhisthtn)], aes(x=prettyExposure, y=N, fill=as.factor(bookhisthtn)))
p <- p + geom_col()
p <- p + labs(y="Number of Women",
              fill="bookhisthtn",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhisthtn.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

# percentage
p <- ggplot(uglyTable[!is.na(bookhisthtn)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookhisthtn)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="bookhisthtn",
              caption=sprintf("Chi-squared p-value: %s",pval))
p


ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhisthtn_proportion.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")


########################
##### bookhistperi

#Chi-squared for differences in bookhistperi
pval <- chisq.test(x=uglyTable$bookhistperi, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(bookhistperi),denom:=sum(N),by=.(prettyExposure)]

# raw numbers
p <- ggplot(uglyTable[!is.na(bookhistperi)], aes(x=prettyExposure, y=N, fill=as.factor(bookhistperi)))
p <- p + geom_col()
p <- p + labs(y="Number of Women",
              fill="bookhistperi",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistperi.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

# percentage
p <- ggplot(uglyTable[!is.na(bookhistperi)], aes(x=prettyExposure, y=N/denom, fill=as.factor(bookhistperi)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="bookhistperi",
              caption=sprintf("Chi-squared p-value: %s",pval))
p

ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "bookhistperi_proportion.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")
########################
##### laburglu_1

#Chi-squared for differences in laburglu_1
pval <- chisq.test(x=uglyTable$laburglu_1, y = uglyTable$prettyExposure, correct = TRUE,
                   p = rep(1/length(x), length(x)), rescale.p = FALSE,
                   simulate.p.value = FALSE, B = 2000)$p.value
pval <- round(pval,digits=3)

uglyTable[!is.na(laburglu_1),denom:=sum(N),by=.(prettyExposure)]

# raw data
p <- ggplot(uglyTable[!is.na(laburglu_1)], aes(x=prettyExposure, y=N, fill=as.factor(laburglu_1)))
p <- p + geom_col()
p <- p + labs(y="Number of Women",
              fill="laburglucose",
              caption=sprintf("Chi-squared p-value: %s",pval))
p
ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "laburglucose.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")

# percentages
p <- ggplot(uglyTable[!is.na(laburglu_1)], aes(x=prettyExposure, y=N/denom, fill=as.factor(laburglu_1)))
p <- p + geom_col()
p <- p + labs(y="Proportion of Women",
              fill="laburglucose",
              caption=sprintf("Chi-squared p-value: %s",pval))
p


ggsave(filename=file.path(
  FOLDER_DROPBOX_RESULTS,
  "mahima",
  "trial_1",
  "laburglucose_proportion.png"),
  plot=p,
  height=210,
  width=297,
  units="mm")



###Baseline Charachteristics Tables
#make columns yes, no, and missing
vars_for_columns<-c("prettyExposure",
                    "bookhistdm",
                    "bookhisthtn",
                    "bookhistgdm",
                    "bookhistcs",
                    "bookhistperi",
                    "bookparity",
                    "bookhistutesur",
                    "bookhistcscompl",
                    "bookhistpreterm",
                    "bookhistute",
                    "bookhistabort",
                    "bookhistaph",
                    "bookhistpph"
)


# with=f just formatting with the fuction to make the code run

fullT2data<-d[bookdate >= "2017-01-15"&
            bookdate<="2017-09-15" &
            ident_TRIAL_1==T,
          vars_for_columns,
          with=F
          ]

long <- melt.data.table(fullT2data, id.vars=c(
  "prettyExposure"
),variable.factor = F)



uglytable <- long[,
                  .(
                    not_NA=sum(!is.na(value)),
                    value0=sum(value==0,na.rm=T),
                    value1=sum(value==1, na.rm=TRUE),
                    Missing=sum(is.na(value))
                    
                  ),
                  keyby=.(
                    variable,
                    prettyExposure)
                  ]

openxlsx::write.xlsx(uglytable, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("ObstetricHistory_%s.xlsx", lubridate::today())))

#baseline (character formats)

fullT2data <- d[bookdate >= "2017-01-15"&
              bookdate<="2017-09-15" &
              ident_TRIAL_1==T,]


#bookfetalmove
fullT2data <- fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                    TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
                 
                 keyby=.(bookfetalmove)]

fullT2data <- fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                    TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
                 
                 keyby=.(bookvagbleed)]

#Cleaning Variables

fullT2data[,paracat:=cut(para,
                     breaks=c(0,0.9,4,15),
                     include.lowest=T)]

#checking control for para
fullT2data[,conparacat:=cut(conpara,
                        breaks=c(0,0.9,4,15),
                        include.lowest=T)]


fullT2data <- fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                    TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
                 
                 keyby=.(paracat)]

#check control on its own
fullT2data <- fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                    TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
                 
                 keyby=.(conparacat)]


#new para variable so its calculated
fullT2data[,paraCalculatedAll:=0]
vars <- stringr::str_subset(names(fullT2data), "^prevoutcome_")

#delete prevoutcome_ from it to get the actual number
vars <- stringr::str_remove(vars, "prevoutcome_")


for(i in vars){
  print(i)
  prevoutcome <-sprintf("prevoutcome_%s",i)
  prevdate <- sprintf("prevdate_%s",i)
  fullT2data[!is.na(get(prevoutcome)) & 
           get(prevoutcome)!="" & 
           get(prevoutcome)=="LIVE" |get(prevoutcome)=="STILL" &
           get(prevdate)<bookdate, paraCalculatedAll:=paraCalculatedAll+1]
}

fullT2data[,paraCalculatedAllcat:=cut(paraCalculatedAll,
                                  breaks=c(0,0.9,4,15),
                                  include.lowest=T)]




fullT2data <- fullT2data[,.(ArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                    ArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
                 keyby=.(paraCalculatedAllcat)]

############
fullT2data <- fullT2data[,.(
  bookprimiYes=sum(bookprimi==1, na.rm=TRUE),
  bookprimiNo=sum(bookprimi==0, na.rm=TRUE),
  bookprimiNA=sum(is.na(bookprimi))),
  
  keyby=.(prettyExposure)]





#demoraphic variables
#ageCat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(agecat)]

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("agecats_%s.xlsx", lubridate::today())))

#parity
#paraCat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(paracat)]

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("paracat_%s.xlsx", lubridate::today())))

#avgincomecat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(avgincomecat)]

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("avgincomecat_%s.xlsx", lubridate::today())))

#educat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(educationcat)]
openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("educationcat_%s.xlsx", lubridate::today())))

#agepreg
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(agepregnancycat)]

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("agepregcat_%s.xlsx", lubridate::today())))

#bmicat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(bookbmicat)]
openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("bookbmicat_%s.xlsx", lubridate::today())))





#### Pres at term 36+ #####
fullT2data <- d[ident_TRIAL_1==T,]
fullT2data[,hasan36plusweeks:=FALSE]
#fullT2data[,hasanexampalp36:= FALSE]
vars <- stringr::str_subset(names(fullT2data),"^angestage_")
for (i in vars){
  print(i)
  fullT2data[get(i)>=36 & get(i)<=40, hasan36plusweeks:=TRUE]
  
}

#fetal presentation at term
fullT2data[,presatterm:=as.character(NA)]

vars <- stringr::str_subset(names(fullT2data),"^uspres_")


for (var_pres in vars){
  
  vargestage <-stringr::str_replace(var_pres,"uspres", "usgestage")
  
  fullT2data[hasan36plusweeks==TRUE &
           get(vargestage)>=36 &
           get(vargestage)<=40 &
           !is.na(get(var_pres)) &
           get(var_pres)!="",
         presatterm:=get(var_pres)]
}

fullT2data <- fullT2data[ ,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                     ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                  keyby=.(presatterm)]


### TO DO: Fix this one#####
####anexampalp

#use hasanexampalp36 from previous us, no need to make it again
#fetal presentation at term by anexampalp
fullT2data[,anexampalp36:=as.character(NA)]

vars_gestage <- stringr::str_subset(names(fullT2data),"^angestage_[0-9]+")



for (var_gestage in vars_gestage){
  
  var_exampalp <-stringr::str_replace(var_gestage,"angestage", "anexampalp")
  
  fullT2data[hasan36plusweeks==TRUE &
           get(var_gestage)>=36 &
           get(var_gestage)<=40 &
           !is.na(get(var_exampalp)) &
           get(var_exampalp)!="",
         anexampalp36:=get(var_exampalp)]
}

fullT2data <- fullT2data[ ,.(ArmA=sum(ident_dhis2_control==T, na.rm=T),
                     ArmB=sum(ident_dhis2_control==F, na.rm=T)),
                  keyby=.(anexampalp36)]


openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("PresAtTerm_%s.xlsx", lubridate::today())))


#Hemoglobin at booking visit
## Do not want to include 0 because its missing so include.lowest=F
fullT2data[,booklabhbcat:=cut(booklabhb,
                          breaks=c(0,6.9,8.9,10.9,16,20,200),
                          include.lowest=F)]
#booklabhbcat
fullT2data<-fullT2data[,.(TrialArmA=sum(ident_dhis2_control==T, na.rm=TRUE),
                  TrialArmB=sum(ident_dhis2_control==F, na.rm=TRUE)),
               
               keyby=.(booklabhbcat)]
openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("booklabhbcat_%s.xlsx", lubridate::today())))



# cleaning bookbp
fullT2data[bookbpsyst<60, bookbpsyst:=as.numeric(NA)]
fullT2data[bookbpsyst>170, bookbpsyst:=as.numeric(NA)]
fullT2data[bookbpdiast<40, bookbpdiast:=as.numeric(NA)]


fullT2data[,bookbpsystcat:=cut(bookbpsyst,
                           breaks=c(0,139,149,159,200),
                           include.lowest=T)]
fullT2data[,bookbpdiastcat:=cut(bookbpdiast,
                            breaks=c(0,89,99,109,150),
                            include.lowest=T)]



fullT2data<-fullT2data[,.(NormalHtn=sum(bookbpsystcat=="[0,139]" |
                                  bookbpdiastcat=="[0,89]", na.rm=TRUE),
                  MildHtn=sum(bookbpsystcat=="(139,149]" |
                                bookbpdiastcat=="(89,99]", na.rm=TRUE),
                  ModHtn=sum(bookbpsystcat== "(149,159]" |
                               bookbpdiastcat=="(99,109]", na.rm=TRUE),
                  SevHtn=sum(bookbpsystcat=="(159,200]" |
                               bookbpdiastcat=="(109,150]", na.rm=TRUE),
                  Missing=sum(is.na(bookbpsystcat) & is.na(bookbpdiastcat))),
               
               keyby=.(prettyExposure)]

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("bookHTN_%s.xlsx", lubridate::today())))

fullT2data<-fullT2data[,.(
  TrialArmA=sum(prettyExposure==T, na.rm=TRUE),
  TrialArmB=sum(prettyExposure==F, na.rm=TRUE)),
  
  keyby=.(bookbpsystcat)]
openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("bookbpsystcats_%s.xlsx", lubridate::today())))


fullT2data[,booklaburglu:=as.character(NA)]
fullT2data[abs(as.numeric(bookgestage)-labgestage_1)<=1, booklaburglu:=laburglu_1]
fullT2data<-fullT2data[,.(
  TrialArmA=sum(ident_dhis2_control==T, na.rm=T),
  TrialArmB=sum(ident_dhis2_control==F, na.rm=T)),
  keyby=.(booklaburglu)] 

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("booklaburglu_%s.xlsx", lubridate::today())))

#bookprimi
fullT2data<-fullT2data[,.(
  TrialArmA=sum(ident_dhis2_control==T, na.rm=T),
  TrialArmB=sum(ident_dhis2_control==F, na.rm=T)),
  keyby=.(bookprimi)] 

openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("bookprimi_%s.xlsx", lubridate::today())))

#baseline characteristics
#bookweight
fullT2data[bookweight>140|bookweight<35, bookweight:=as.numeric(NA)]
fullT2data<-fullT2data[,.(missingBookheight=sum(is.na(bookheight)),
                  missingBookweight=sum(is.na(bookweight)),
                  Height0=sum(bookheight==0, na.rm=TRUE),
                  Weight0=sum(bookweight==0, na.rm=TRUE),
                  missingBookbmi=sum(is.na(bookbmi))),
               keyby=.(ident_dhis2_control)]
openxlsx::write.xlsx(fullT2data, 
                     file.path(
                       FOLDER_DATA_RESULTS_WB,
                       "demographics_and_history",
                       sprintf("missing_bookweight__bookheight_bookbmi%s.xlsx", 
                               lubridate::today())))

###########################
# long ANC data for Brian #
###########################

# 
# # trial arms
# d[ident_TRIAL_2_3_Control==T,TrialArm:="Control"]
# 
# d[ident_TRIAL_2==T & 
#     ident_TRIAL_3==F,TrialArm:="SMS only"]
# 
# d[ident_TRIAL_2==F & 
#     ident_TRIAL_3==T,TrialArm:="QID only"]
# 
# d[ident_TRIAL_2==T & 
#     ident_TRIAL_3==T,TrialArm:="SMS and QID"]
# xtabs(~d$TrialArm, addNA=T)
# 
# 
# d[,anevent_0:=bookevent]
# d[,andate_0:=bookdate]
# d[,angestage_0:=bookgestage]
# 
# anc <- names(d)[stringr::str_detect(names(d),"^anevent")]
# ancdate <- names(d)[stringr::str_detect(names(d),"^andate")]
# ancgestage <- names(d)[stringr::str_detect(names(d),"^angestage")]
# 
# longANC <- d[,c("uniqueid","TrialArm",ancgestage,anc, ancdate), with=F]
# 
# l <- melt(longANC, id="uniqueid", measure=patterns("^andate", "^anevent","^angestage","TrialArm"),
#           value.name=c("andate", "anevent","angestage","TrialArm"))
# 
# if(IS_GAZA){
#   
#   openxlsx::write.xlsx(l,
#                        file.path(FOLDER_DATA_CLEAN_GAZA,
#                                  "T2_clean",
#                                  "long_ANC_GAZA.xlsx"))
#   
#   
#   saveRDS(l,file.path(FOLDER_DATA_CLEAN_GAZA,
#                       "T2_clean",
#                       "longANc_Gaza.rds"))
#   
#   
#   
# } else {
#   
#   
#   openxlsx::write.xlsx(l,
#                        file.path(FOLDER_DATA_CLEAN,
#                                  "T2_clean",
#                                  "long_ANC_WB.xlsx"))
#   
#   
#   saveRDS(l,file.path(FOLDER_DATA_CLEAN,
#                       "T2_clean",
#                       "longANc_WB.rds"))
#   
#   
#   
#   
# }










