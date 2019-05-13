###### SETUP STARTS ######

setwd("C:/data processing/trial_dofiles")

fileSources = file.path("r_code", list.files("r_code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

Setup()

FOLDER_DROPBOX_RESULTS <<- file.path(
  FOLDER_DROPBOX,
  "Data management eRegQual",
  "Results_From_PNIPH",
  "WB_Results",
  lubridate::today())
  library(ggplot2)

###### SETUP ENDS ######

d <- LoadDataFileFromNetworkWB()

# DO ANALYSES HERE
#creating the dataset we want
stringr::str_subset(names(d), "^book")
d[ident_dhis2_control==F, prettyExposure:="Trial Arm B"]
d[ident_dhis2_control==T, prettyExposure:="Trial Arm A"]

#data set for analysis is created here so we dont have to reselect 
#women over and over again
smallD <- d[bookdate >= "2017-01-15"&
            bookdate<="2017-09-15" &
            ident_TRIAL_1==T,]


##making a table for data we want to analyze from the analysis data set
##for things like parity, make an ugly table because its not a box plot
##or histogram, so better to make a ugly table.
smallD[,
      .(
        meanage=mean(age, na.rm=T),
        meanavgmonthlyincome= mean(income, na.rm=T),
        meaneducation= mean(education, na.rm=T),
        meanbookweight= mean(bookweight, na.rm=T),
        meanbookheight=mean(bookheight, na.rm=T),
        meanBMI=mean(bookbmi, na.rm=T),
        meabooksystbp= mean(bookbpsyst, na.rm=T),
        meanbookdiastbp=mean(bookbpdiast, na.rm=T),
        meanbookhb= mean(labhb_1, na.rm=T),
        ProportionBookhbnotmissing=sum(!is.na(labhb_1)),
        ProportionofWeightsis0= mean(bookweight==0, na.rm=T),
        ProportionofWeightsOver100kg= mean(bookweight>100, na.rm=T),
        ProportionofParity= mean(bookparity, na.rm=T),
        meanbookgestage= mean(bookgestage, na.rm=T),
        ProportionofPrimi=mean(bookprimi, na.rm=T)
    
      
      ), 
      keyby=.(prettyExposure)
        
      ]

#making our plots
###bookweight
# p <- ggplot(smallD, aes(x=bookweight))
# p <- p + geom_density()
# p

####################
# bookweight
####################

#from this plot we see that someone was enetered as 6000kg 
#so we want to see where this is happening (in which exposure group)
pval <- t.test(bookweight ~ prettyExposure, data=smallD)$p.value

p <- ggplot(smallD, aes_string(y="bookweight", x="prettyExposure"))
p <- p + geom_boxplot()
p <- p + geom_boxplot(outlier.color = NA)
p <- p + labs(caption=sprintf("Chi-squared p-value: %s",pval))
p

####################
# bookbmi
####################

smallD[,.(
  meanbookheight=mean(bookheight,na.rm=T),
  medianbookheight=median(bookheight,na.rm=T),
  num0=sum(bookheight==0,na.rm=T)
),
keyby=prettyExposure]

pval <- t.test(bookbmi ~ prettyExposure, data=smallD)$p.value

p <- ggplot(smallD, aes_string(y="bookbmi", x="prettyExposure"))
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
pval <- t.test(as.formula(form), data=smallD)$p.value

p <- ggplot(smallD, aes_string(y="bookbmi", x="prettyExposure"))
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
  pval <- t.test(as.formula(form), data=smallD[get(i)!=0])$p.value
  
  p <- ggplot(smallD, aes_string(y=i, x="prettyExposure"))
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


#variables that we want to make "uglyTable for because they are yes, no, missing
#
#bookhistdm,
#bookhistgdm,
#bookhisthtn,
#bookhistotherchronic (but must make this unified),
#bookhistcs,
#bookhistperi
#laburglu_1 (these are neg, pos, missing)
#laburglu_1=="POS"
#laburglu_1=="NEG"
#laburglu_1==""
#laburglu_1==NA

#vars <- c(bookhistdm,
          #bookhistgdm,
          #bookhisthtn,
          #bookhistotherch,
          #bookhistcs,
          #bookhistperi
          #laburglu_1 (these are neg, pos, missing)
          #laburglu_1=="POS"
          #laburglu_1=="NEG"
          #laburglu_1==""
          #laburglu_1==NA)



#Making ugly tables for parity
uglyTable <- smallD[,
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

###Counseling IFA
##counseled and if given or not?
d$ancounsifa_1
d$a

###Risks in current pregnancy
unique(d$risktype_1)

#Need gestational ages at visits: then can take the average
d$angestage_1 #(1-18)
d$booklmpknown
str(d$booklmp)
d$andate_10


#Lab urine stick: sugar
d$laburglu_1
#yes or no, pos/neg

#Lab OGCT
#num done + results
unique(d$labogct_1)
d$labother1_1
d$labother3_9
d$labotherres1_4


#fundal height at visits
#$bookfetalmove
#str(d$bookexamfh)
#d$anfetalmove_1
d$anexamsfh_1
d$bookexamfh
d$anexamfh_1

##US
#gestational ages at us
#how many have us?
#gest ages based on these
d$usegaweeks_1
d$usegadays_13

#growth restriction/lga
d$usfh_1
d$uspres_1
d$usiugr_1
d$uslga_1

#gA at referal to clinics


##Man details
d$mantypex_1
d$mandetail_1
d$mantypey_1

#Prev preg outcomes???








