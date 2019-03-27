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
  
  # Bookdate_before_booklmp
  #booklmp_original has NO CORRECTIONS MADE TO IT
  # while booklmp has corrections made to it
  d[,booklmp_original:=booklmp]
  d[ident_bad_bookdate_before_booklmp==T & 
      ident_TRIAL_1==T &
      !is.na(usedd_1),
    booklmp:=usedd_1-40*7]
  
  d[ident_bad_bookdate_before_booklmp==T & 
      ident_TRIAL_1==T &
      is.na(usedd_1),
    booklmp:=booklmp-365]
  
  
}










