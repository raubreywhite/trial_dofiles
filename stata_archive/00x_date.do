
global CLINIC_INTERVENTION_DATE="2018-03-04"
global CLINIC_CONTROL_DATE="2018-03-18"

global MAX_YEAR=substr("$CLINIC_CONTROL_DATE",1,4)
global MAX_MONTH=substr("$CLINIC_CONTROL_DATE",6,2)

global DATE : di %td_CY-N-D  date("$S_DATE", "DMY")
di "$DATE"
