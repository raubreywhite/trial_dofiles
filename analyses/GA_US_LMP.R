nrow(d[bookyear=="2019"])
nrow(d[bookyear=="2019" &
         !is.na(booklmp)])



vars <- names(d)[stringr::str_detect(names(d),"^usedd_[0-9]+")]
d[,usevent_x:=0]

print(vars)

for(i in vars){
  d[!is.na(get(i)), usevent_x:=usevent_x + 1]
}

dsmall <-d[bookyear=="2019", c("booklmpknown", "booklmp", "usevent_x")]
xtabs(~dsmall$booklmpknown)

nrow(d[bookyear=="2019" &
         is.na(booklmp) &
         !is.na(usevent_x)])

nrow(d[bookyear=="2019" &
         !is.na(booklmp) &
         !is.na(usevent_x)])

nrow(d[bookyear=="2019" &
         is.na(booklmp) &
         is.na(usevent_x)])
