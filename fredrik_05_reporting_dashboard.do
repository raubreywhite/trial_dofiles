 capture set maxvar 30000
//select 2017 data

cd "X:\data processing\"
use "data_clean/with_indicators.dta", clear



tab bookseenby, mis
2 = doctor
3 = nurse
Booking visit seen by doctor FINAL
Booking visit seen by nurse FINAL
Booking visits above age 40 FINAL
Booking visits below age 16 FINAL
Booking visits between 16 and 40 FINAL



