cd "X:\data processing\"

use "data_clean/IDENT_aviccena_merged_clinical.dta", clear

***/// find the differences between the last ANC visits and booking visits///*

egen lastandate=rowmax(andate*)
format lastandate %td

generate difference_dates_booking_an= lastandate-newbookdatecorrect

summarize difference_dates_booking_an, detail
tab lastandate
tab andate1

