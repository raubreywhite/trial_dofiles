FixObviousMistakes <- function(d){
  #variables in the mangement were more than one kind
  #for example: MildAneTreatment-flkdjfajdifj and MildAneTreatmentfjaldkjfkldj
  #so  we truncated them so they become the same
  
  vars <- stringr::str_subset(names(d),"^mantypey_")
  for(i in vars){
    d[, (i) := stringr::str_replace(string=get(i),
                                          pattern="-[a-zA-Z0-9]*$",
                                          replacement="")
      ]
    unique(d[[i]])
    d[stringr::str_detect(get(i),"^MildAnemiaTreatmentFollowup"),
      (i) := "MildAnemiaTreatmentFollowup"
      ]
    unique(d[[i]])
    d[stringr::str_detect(get(i),"^ModAnemiaTreatmentFollowup"),
      (i) := "ModAnemiaTreatmentFollowup"
      ]
    #print(i)
    #print(unique(d[[i]]))
  }
  
  
  vars <- stringr::str_subset(names(d),"^riskdesy_")
  for(i in vars){
    d[, (i) := stringr::str_replace(string=get(i),
                                    pattern="-[a-zA-Z0-9]*$",
                                    replacement="")
      ]
    unique(d[[i]])
  
    #print(i)
    #print(unique(d[[i]]))
  }
  
  
  # Bookdate_before_booklmp
  #booklmp_original has NO CORRECTIONS MADE TO IT
  # while booklmp has corrections made to it
  d[bookdate<booklmp,ident_bad_bookdate_before_booklmp:=1]
  d[ident_bad_bookdate_before_booklmp==1, ident_bad_all:=1]
  
  d[,booklmp_original:=booklmp]
  d[ident_bad_bookdate_before_booklmp==T & 
      ident_TRIAL_1==T &
      !is.na(usedd_1),
    booklmp:=usedd_1-40*7]
  
  d[ident_bad_bookdate_before_booklmp==T & 
      ident_TRIAL_1==T &
      is.na(usedd_1),
    booklmp:=booklmp-365]
 
  
  #####################
  #####Cleaning bookheight
  
  #missing bookheights for control will show up as empty
  sum(is.na(d$bookheight))
  str(d$bookheight)
  unique(d$bookheight)
  
  #replace all bookheights with absolute values
  d[,bookheight:=abs(bookheight)]
  d[bookheight<2.0,bookheight:=bookheight*100]
  d[bookheight>=10 &
      bookheight<=90,bookheight:=bookheight+100]
  d[bookheight>=1000 & 
      bookheight<2000,bookheight:=bookheight/10]
 d[bookheight>=100000,bookheight:=bookheight/1000]
 
  
}










