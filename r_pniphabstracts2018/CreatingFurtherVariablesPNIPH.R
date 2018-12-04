

CreatingFurtherVariablesPNIPH <-function(d){
  # abstract 2018
  d[,pniph_agecat:=cut(age,
                       breaks=c(0,20,25,30,35,40,45,50,100),
                       include.lowest=T)]
  d[,pniph_agecat2:=cut(age,
                        breaks=c(0,20,30,40,100),
                        include.lowest=T)]
  d[,pniph_agemarriagecat:=cut(agemarriage,
                               breaks=c(0,20,25,30,35,40,100),
                               include.lowest=T)]
  d[,pniph_agemarriagecat2:=cut(agemarriage,
                                breaks=c(0,20,30,40,100),
                                include.lowest=T)]
  d[,pniph_agepregnancycat:=cut(agepregnancy,
                                breaks=c(0,20,25,30,35,40,100),
                                include.lowest=T)]
  d[,pniph_agepregnancycat2:=cut(agepregnancy,
                                 breaks=c(0,20,30,40,100),
                                 include.lowest=T)]
  
  d[,pniph_educationcat:=cut(education,
                             breaks=c(0,9,13,100),
                             include.lowest=T)]
  
  d[,pniph_educationcat2:=cut(education,
                              breaks=c(0,11,12,16,100),
                              include.lowest=T)]
  levels(d$pniph_educationcat2)
  
  d[,pniph_avgincome := income/members]
  
  d[,pniph_avgincomecat:=cut(pniph_avgincome,
                             breaks=c(0,200,900,1824,3054,100000),
                             include.lowest=T)]
  d[,pniph_incomecat:=cut(income,
                          breaks=c(0,200,900,1824,3054,100000),
                          include.lowest=T)]
  d[,pniph_incomecat2:=cut(income,
                           breaks=c(0,1500,3000,5000,100000),
                           include.lowest=T)]
  
  d[,pniph_householdcat:=cut(members,
                             breaks=c(0,3,5,30),
                             include.lowest=T)]
  
  d[,pniph_nbcdaysatvisit:=as.numeric(nbcdate_1-nbcdateofdelivery_1)]
  
  # number of ppc visits
  d[,pniph_num_ppc_visits:=0]
  vars <- names(d)[stringr::str_detect(names(d),"ppcevent")]
  for(i in vars){
    d[!is.na(get(i)),pniph_num_ppc_visits:=pniph_num_ppc_visits+1]
  }
  xtabs(~d$pniph_num_ppc_visits, addNA=T)
  
  
  #gestage at last anc visit, includes booking visit
  vars_gestages <- c("bookgestage",
                     names(d)[stringr::str_detect(names(d),"^angestage_")]
  )
  
  d[,pniph_gestageatlastvisit:=as.numeric(NA)]
  for(i in vars_gestages){
    d[!is.na(get(i)) & get(i)>0, pniph_gestageatlastvisit:=get(i)]
  }
  d$gestageatlastvisit
  
  ###creating gest age categories at last visit
  d[,pniph_gestageatlastvisit_cats:=cut(pniph_gestageatlastvisit,
                                        breaks=c(0,36,40,42,9999),
                                        include.lowest=T)]
  d$pniph_gestageatlastvisit_cats
  unique(d$gestageatlastvisit)
  
  
  ###make cpo gest age into categories
  d[,pniph_cpogestage_1_cats:=cut(cpogestage_1,
                                  breaks=c(0,25,38,41,99999),
                                  include.lowest=T)]
  
  # Has anemia/hypertension/bleeding
  d[,pniph_risktype_anemia:=F]
  d[,pniph_risktype_chronichypertension:=F]
  d[,pniph_risktype_vaginalbleeding:=F]
  vars <- names(d)[stringr::str_detect(names(d),"^risktype_")]
  for(i in vars){
    d[get(i)%in%c("MildAnemia",
                  "ModerateAnemia",
                  "SevereAnemia"),pniph_risktype_anemia:=T]
    d[get(i)%in%c("ChronicHypertension"),pniph_risktype_chronichypertension:=T]
    d[get(i)%in%c("MildBleeding"),pniph_risktype_mildbleeding:=T]
    
    
  }
  
  
  
  
  
}

