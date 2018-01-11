capture set maxvar 30000

capture cd "Z:\data processing\"
capture cd "X:\data processing\"

forvalues year=2015/$MAX_YEAR {
	di `year'
	foreach month in "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12" {	
		use "~/My Documents/trial_temp_data/IDENT_aviccena_merged_clinical_`year'-`month'.dta", clear


		tab isExpectedToHaveDelivered
		//keep if isExpectedToHaveDelivered==1
		//keep if _n<100

		tab is_aviccena 


		// SINGLE ONES THAT DONT FIT ANYWHERE ELSE



		/// H1

		cap drop H1_*
		gen H1_denominator=0
		gen H1_numerator=0

		replace H1_denominator=1 if ///
			!missing(uniqueid)
			
		replace H1_numerator=1 if ///	
			!missing(bookbpdiast) & bookbpdiast>0 & ///
			!missing(bookbpsyst) & bookbpsyst>0

		/// A1

		cap drop A1_*
		gen A1_denominator=0
		gen A1_numerator=0

		replace A1_denominator=1 if ///
			!missing(uniqueid)
			
		cap drop D1_*
		gen D1_denominator=0
		gen D1_numerator=0

		replace D1_denominator=1 if ///
			!missing(uniqueid)
			
		cap drop D3_*
		gen D3_denominator=0
		gen D3_numerator=0
			
		forvalues X=1/100 {
			capture count if !missing(labdate`X')
			if(_rc!=0){
				continue
				// if there is an error
				// go back to the
				// start of the loop
			}
			
			// A1
			quietly replace A1_numerator=1 if ///
				labdate`X' < newbookdatecorrect + 14 & ///
				labdate`X' >= newbookdatecorrect & ///
				!missing(labhb`X') & labhb`X'>0
			
			// D1
			quietly replace D1_numerator=1 if ///
				labdate`X' < newbookdatecorrect + 14 & ///
				labdate`X' >= newbookdatecorrect & ///
				!missing(laburglu`X')
				
			// D3
			quietly replace D3_numerator=1 if ///
				labdate`X' < newbookdatecorrect + 14 & ///
				labdate`X' >= newbookdatecorrect & ///
				!missing(laburglu`X') & laburglu`X'=="POS"

			quietly replace D3_numerator=1 if ///
				labdate`X' < newbookdatecorrect + 14 & ///
				labdate`X' >= newbookdatecorrect & ///
				!missing(laburglu`X') & laburglu`X'=="POS" & ///
				!missing(labbloodglu`X')
		}


		/*
		MASSIVE ANC_VISIT LOOP
		ALL VARIABLES REGARDING "DID X AT ANC VISIT Y" WILL BE INCLUDED
		IN THIS HUGE LOOP

		THIS WAY WE ONLY HAVE ONE HUGE ANC_VISIT LOOP, ALL AT THE 
		SAME TIME POINTS FOR THE VISIT
		*/

		capture gen anbpdiast0=bookbpdiast
		capture gen anbpsyst0=bookbpsyst
		capture gen angestage=bookgestage
		capture gen angestage0=bookgestage
		capture gen andate0=newbookdatecorrect 
		capture gen anexamsfh0=bookexamsfh
		capture gen anhisthtnsymp0=bookhisthtnsymp

		capture gen usgestage0=bookgestage
		capture gen usdate0=newbookdatecorrect 

		// 1-12 15-17 18-22 24-28 31-33 >36

		forvalues weekIndex=0/13 {
			if(`weekIndex'==0) {
				local startweek=1
				local endweek=12
			}
			else if(`weekIndex'==1){
				local startweek=15
				local endweek=17
			}
			else if(`weekIndex'==2){
				local startweek=18
				local endweek=22
			}
			else if(`weekIndex'==3){
				local startweek=23
				local endweek=23
			}
			else if(`weekIndex'==4){
				local startweek=24
				local endweek=28
			}
			else if(`weekIndex'==5){
				local startweek=29
				local endweek=30
			}
			else if(`weekIndex'==6){
				local startweek=31
				local endweek=33
			}
			else if(`weekIndex'==7){
				local startweek=34
				local endweek=35
			}
			else if(`weekIndex'==8){
				local startweek=36
				local endweek=50
			}
			else if(`weekIndex'==9){
				local startweek=1
				local endweek=50
			} 
			else if(`weekIndex'==10){
				local startweek=16
				local endweek=50
			} 
			else if(`weekIndex'==11){
				local startweek=1
				local endweek=20
			}
			else if(`weekIndex'==12){
				local startweek=20
				local endweek=50
			}
			else if(`weekIndex'==13){
				local startweek=16
				local endweek=20
			}
			
			di "WEEK INDEX"
			di `weekIndex'

			capture drop attends_ANC_`startweek'_`endweek'_weeks
			gen attends_ANC_`startweek'_`endweek'_weeks=0
			
			capture drop F2_numerator_`startweek'_`endweek'_weeks
			capture drop F2_denominator_`startweek'_`endweek'_weeks
			gen F2_numerator_`startweek'_`endweek'_weeks=0
			gen F2_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop F3_numerator_`startweek'_`endweek'_weeks
			capture drop F3_denominator_`startweek'_`endweek'_weeks
			gen F3_numerator_`startweek'_`endweek'_weeks=0
			gen F3_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop F4_numerator_`startweek'_`endweek'_weeks
			capture drop F4_denominator_`startweek'_`endweek'_weeks
			gen F4_numerator_`startweek'_`endweek'_weeks=0
			gen F4_denominator_`startweek'_`endweek'_weeks=0	
			
			capture drop H2_numerator_`startweek'_`endweek'_weeks
			capture drop H2_denominator_`startweek'_`endweek'_weeks
			gen H2_numerator_`startweek'_`endweek'_weeks=0
			gen H2_denominator_`startweek'_`endweek'_weeks=0	
			
			capture drop H3_numerator_`startweek'_`endweek'_weeks
			capture drop H3_denominator_`startweek'_`endweek'_weeks
			gen H3_numerator_`startweek'_`endweek'_weeks=0
			gen H3_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop H4_numerator_`startweek'_`endweek'_weeks
			capture drop H4_denominator_`startweek'_`endweek'_weeks
			gen H4_numerator_`startweek'_`endweek'_weeks=0
			gen H4_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop H5_numerator_`startweek'_`endweek'_weeks
			capture drop H5_denominator_`startweek'_`endweek'_weeks
			gen H5_numerator_`startweek'_`endweek'_weeks=0
			gen H5_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop H6_numerator_`startweek'_`endweek'_weeks
			capture drop H6_denominator_`startweek'_`endweek'_weeks
			gen H6_numerator_`startweek'_`endweek'_weeks=0
			gen H6_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop H7_numerator_`startweek'_`endweek'_weeks
			capture drop H7_denominator_`startweek'_`endweek'_weeks
			gen H7_numerator_`startweek'_`endweek'_weeks=0
			gen H7_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop A2_numerator_`startweek'_`endweek'_weeks
			capture drop A2_denominator_`startweek'_`endweek'_weeks
			gen A2_numerator_`startweek'_`endweek'_weeks=0
			gen A2_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop A3_numerator_`startweek'_`endweek'_weeks
			capture drop A3_denominator_`startweek'_`endweek'_weeks
			gen A3_numerator_`startweek'_`endweek'_weeks=0
			gen A3_denominator_`startweek'_`endweek'_weeks=0
				
			capture drop M1_numerator_`startweek'_`endweek'_weeks
			capture drop M1_denominator_`startweek'_`endweek'_weeks
			gen M1_numerator_`startweek'_`endweek'_weeks=0
			gen M1_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop M2_numerator_`startweek'_`endweek'_weeks
			capture drop M2_denominator_`startweek'_`endweek'_weeks
			gen M2_numerator_`startweek'_`endweek'_weeks=0
			gen M2_denominator_`startweek'_`endweek'_weeks=0
			
			capture drop D2_numerator_`startweek'_`endweek'_weeks
			capture drop D2_denominator_`startweek'_`endweek'_weeks
			gen D2_numerator_`startweek'_`endweek'_weeks=0
			gen D2_denominator_`startweek'_`endweek'_weeks=0
			

			// ANC LOOP
			di "ANC LOOP"
			forvalues X=0/100 {
				capture count if !missing(angestationalage`X')
				if(_rc!=0){
					continue
					// if there is an error
					// go back to the
					// start of the loop
				}
				
				quietly replace attends_ANC_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X')
					
				// F2
				quietly replace F2_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				quietly replace F2_numerator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					!missing(anexamsfh`X') & ///
					anexamsfh`X'!=0
					
				// F3
				quietly replace F3_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(anexamsfh`X') & ///
					!missing(angestage`X') & ///
					((anexamsfh`X' < angestage`X'-2) | (anexamsfh`X' > angestage`X'+2))
					
				// H2
				quietly replace H2_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				quietly replace H2_numerator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					!missing(anbpdiast`X') & anbpdiast`X'!=0 & ///
					!missing(anbpsyst`X') & anbpsyst`X'>0
				
				// H3
				quietly replace H3_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					(anbpsyst`X'>=140 | anbpdiast`X'>=90) & ///
					!missing(anbpsyst`X') & ///
					!missing(anbpdiast`X')
				
				// H4
				quietly replace H4_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					((anbpsyst`X'>=140 & anbpsyst`X'<150) | (anbpdiast`X'>=90 & anbpdiast`X'<100)) & ///
					!missing(anbpsyst`X') & ///
					!missing(anbpdiast`X')
					
				// H5
				quietly replace H5_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					((anbpsyst`X'>=150) | (anbpdiast`X'>=100)) & ///
					!missing(anbpsyst`X') & ///
					!missing(anbpdiast`X')
					
				// H7
				quietly replace H7_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					anhisthtnsymp`X'==1 & ///
					!missing(anhisthtnsymp`X')
				
				// A2
				quietly replace A2_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				// A3
				quietly replace A3_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				// M1
				quietly replace M1_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				quietly replace M1_numerator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					!missing(anexampalp`X') & ///
					anexampalp`X'!="Notchecked" & ///
					anexampalp`X'!="Unknown"
					
				// M2
				quietly replace M2_denominator_`startweek'_`endweek'_weeks=1 if ///
					angestage`X'>=`startweek' &  ///
					angestage`X'<=`endweek' & ///
					!missing(angestage`X') & ///
					!missing(anexampalp`X') & ///
					anexampalp`X'!="Notchecked" & ///
					anexampalp`X'!="Unknown" & ///
					anexampalp`X'!="Cephalic"
					
				// D2
				quietly replace D2_denominator_`startweek'_`endweek'_weeks=attends_ANC_`startweek'_`endweek'_weeks
				
				// ULTRASOUND LOOP INSIDE ANC
				forvalues Y=1/100 {
					capture count if !missing(usdate`Y')
					if(_rc!=0){
						continue
						// if there is an error
						// go back to the
						// start of the loop
					}
					
					// F3
					quietly replace F3_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(F3_denominator_`startweek'_`endweek'_weeks) & ///
						!missing(usdate`Y') & ///
						!missing(andate`X') & ///
						F3_denominator_`startweek'_`endweek'_weeks==1 & ///
						usdate`Y' < andate`X' + 14 & ///
						usdate`Y' >= andate`X'
					
						
				}
				
				// LAB LOOP INSIDE ANC
				forvalues Y=1/100 {
					capture count if !missing(labdate`Y')
					if(_rc!=0){
						continue
						// if there is an error
						// go back to the
						// start of the loop
					}
					
					// H4
					quietly replace H4_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						((anbpsyst`X'>=140 & anbpsyst`X'<150) | (anbpdiast`X'>=90 & anbpdiast`X'<100)) &  ///
						!missing(anbpsyst`X') & ///
						!missing(anbpdiast`X') & ///
						labdate`Y' < andate`X' + 7 & ///
						labdate`Y' >= andate`X' & ///
						!missing(laburpro`Y') & ///
						!missing(andate`X') & ///
						!missing(labdate`Y')
					
					// H6
					quietly replace H6_denominator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						((anbpsyst`X'>=140 & anbpsyst`X'<150) | (anbpdiast`X'>=90 & anbpdiast`X'<100)) & ///
						!missing(anbpsyst`X') & ///
						!missing(anbpdiast`X') & ///
						labdate`Y' < andate`X' + 7 & ///
						labdate`Y' >= andate`X' & ///
						laburpro`Y'>=1 & laburpro`Y'<=3 & ///
						!missing(laburpro`Y') & ///
						!missing(andate`X') & ///
						!missing(labdate`Y')
						
					// A2
					quietly replace A2_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						labdate`Y' < andate`X' + 14 & ///
						labdate`Y' >= andate`X' & ///
						!missing(labhb`Y') & labhb`Y'>0
						
					// A3
					quietly replace A3_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						labdate`Y' < andate`X' + 14 & ///
						labdate`Y' >= andate`X' & ///
						!missing(labhb`Y') & labhb`Y'>=7 & labhb`Y'<11
						
					// D2
					quietly replace D2_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						labdate`Y' < andate`X' + 14 & ///
						labdate`Y' >= andate`X' & ///
						!missing(labbloodglu`Y')
						
					
						
					// MAN LOOP INSIDE LAB INSIDE ANC
					forvalues Z=1/100 {
						capture count if !missing(mandate`Z')
						if(_rc!=0){
							continue
							// if there is an error
							// go back to the
							// start of the loop
						}
						// H6
						quietly replace H6_numerator_`startweek'_`endweek'_weeks=1 if ///
							angestage`X'>=`startweek' &  ///
							angestage`X'<=`endweek' & ///
							!missing(angestage`X') & ///
							((anbpsyst`X'>=140 & anbpsyst`X'<150) | (anbpdiast`X'>=90 & anbpdiast`X'<100)) & ///
							!missing(anbpsyst`X') & ///
							!missing(anbpdiast`X') & ///
							labdate`Y' < andate`X' + 7 & ///
							labdate`Y' >= andate`X' & ///
							laburpro`Y'>=1 & laburpro`Y'<=3 & ///
							!missing(laburpro`Y') & ///
							!missing(andate`X') & ///
							!missing(labdate`Y') & ///
							mandate`Z' < andate`X' + 14 & ///
							mandate`Z' >= andate`X' ///
							& (mantypex`Z'=="RefHosp" | mantypex`Z'== "RefHighRisk" | mantypex`Z'== "RefSpec") & ///
							!missing(mandate`Z') & ///
							!missing(mantypex`Z')
					}
				}
				
				
				// MAN LOOP INSIDE ANC
				forvalues Y=1/100 {
					capture count if !missing(mandate`Y')
					if(_rc!=0){
						continue
						// if there is an error
						// go back to the
						// start of the loop
					}
					// H3
					quietly replace H3_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						(anbpsyst`X'>=140 | anbpdiast`X'>=90) & ///
						!missing(anbpsyst`X') & ///
						!missing(anbpdiast`X') & ///
						mandate`Y' < andate`X' + 14 & ///
						mandate`Y' >= andate`X' ///
						& (mantypex`Y'=="RefHosp" | mantypex`Y'== "RefHighRisk") & ///
						!missing(mandate`Y') & ///
						!missing(mantypex`Y')
						
					// H5
					quietly replace H5_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						((anbpsyst`X'>=150) | (anbpdiast`X'>=100)) & ///
						!missing(anbpsyst`X') & ///
						!missing(anbpdiast`X') & ///
						mandate`Y' < andate`X' + 14 & ///
						mandate`Y' >= andate`X' ///
						& (mantypex`Y'=="RefHosp" | mantypex`Y'== "RefHighRisk" | mantypex`Y'== "RefSpec") & ///
						!missing(mandate`Y') & ///
						!missing(mantypex`Y')
						
					// H7	
					quietly replace H7_numerator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						anhisthtnsymp`X'==1 & ///
						!missing(anhisthtnsymp`X') & ///
						mandate`Y' < andate`X' + 14 & ///
						mandate`Y' >= andate`X' ///
						& (mantypex`Y'=="RefHosp" | mantypex`Y'== "RefHighRisk" | mantypex`Y'== "RefSpec") & ///
						!missing(mandate`Y') & ///
						!missing(mantypex`Y')
						
					// M2
					quietly replace M2_denominator_`startweek'_`endweek'_weeks=1 if ///
						angestage`X'>=`startweek' &  ///
						angestage`X'<=`endweek' & ///
						!missing(angestage`X') & ///
						!missing(anexampalp`X') & ///
						anexampalp`X'!="Notchecked" & ///
						anexampalp`X'!="Unknown" & ///
						anexampalp`X'!="Cephalic" & ///
						mandate`Y' < andate`X' + 14 & ///
						mandate`Y' >= andate`X' ///
						& (mantypex`Y'=="RefHosp") & ///
						!missing(mandate`Y') & ///
						!missing(mantypex`Y')
				}
				
			}
			
			// LAB LOOP LOOP
			di "LAB LOOP"
			forvalues X=1/100 {
				capture count if !missing(labgestage`X')
				if(_rc!=0){
					continue
					// if there is an error
					// go back to the
					// start of the loop
				}
				
				
			}	

			// ULTRASOUND LOOP
			di "ULTRASOUND LOOP"
			forvalues X=1/100 {
				capture count if !missing(usgestage`X')
				if(_rc!=0){
					continue
					// if there is an error
					// go back to the
					// start of the loop
				}
				
				// F2
				foreach usmeasurementvariable in "uscrlmm" "uscrlweeks" "usbpdmm" "usbpdweeks" "usfemurmm" "usfemurweeks" "usacmm" "usacweeks" "usegaweeks" "usegadays" "usedd" {
					quietly replace F2_numerator_`startweek'_`endweek'_weeks=1 if ///
						usgestage`X'>=`startweek' &  ///
						usgestage`X'<=`endweek' & ///
						!missing(`usmeasurementvariable'`X') & ///
						`usmeasurementvariable'`X'!=0 & ///
						!missing(usgestage`X')
				}
				
				quietly replace F2_denominator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(usgestage`X')
					
				// F4
				quietly replace F4_denominator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(usiugr`X') & usiugr`X'==1
				
				quietly replace F4_denominator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(uslga`X')  & uslga`X'==1

				// M1
				quietly replace M1_denominator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(usgestage`X')
				
				quietly replace M1_numerator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(usgestage`X') & ///
					!missing(uspres`X') & ///
					uspres`X'!="Notchecked" & ///
					uspres`X'!="Unknown"

				// M2
				quietly replace M2_denominator_`startweek'_`endweek'_weeks=1 if ///
					usgestage`X'>=`startweek' &  ///
					usgestage`X'<=`endweek' & ///
					!missing(usgestage`X') & ///
					!missing(uspres`X') & ///
					uspres`X'!="Notchecked" & ///
					uspres`X'!="Unknown" & ///
					uspres`X'!="Cephalic"
			
				// MAN LOOP INSIDE ULTRASOUND
				forvalues Y=1/100 {
					capture count if !missing(mandate`Y')
					if(_rc!=0){
						continue
						// if there is an error
						// go back to the
						// start of the loop
					}
					
					quietly replace F4_numerator_`startweek'_`endweek'_weeks=1 if ///
						usgestage`X'>=`startweek' &  ///
						usgestage`X'<=`endweek' & ///
						!missing(mandate`Y') & ///
						!missing(usdate`X') & ///
						F4_denominator_`startweek'_`endweek'_weeks==1 & ///
						mandate`Y' < usdate`X' + 14 & ///
						mandate`Y' >= usdate`X' ///
						& (mantypex`Y'=="RefHosp" | mantypex`Y'== "RefHighRisk")
						
					quietly replace M2_numerator_`startweek'_`endweek'_weeks=1 if ///
						usgestage`X'>=`startweek' &  ///
						usgestage`X'<=`endweek' & ///
						!missing(usgestage`X') & ///
						!missing(uspres`X') & ///
						uspres`X'!="Notchecked" & ///
						uspres`X'!="Unknown" & ///
						uspres`X'!="Cephalic" & ///
						mandate`Y' < usdate`X' + 14 & ///
						mandate`Y' >= usdate`X' ///
						& (mantypex`Y'=="RefHosp")
				}
			}
		}


		///////
		// LAB LOOP
		cap drop A4_*
		gen A4_denominator=0
		gen A4_numerator=0

		cap drop A5_*
		gen A5_denominator=0
		gen A5_numerator=0

		cap drop D4_*
		gen D4_denominator=0
		gen D4_numerator=0

		cap drop D5_*
		gen D5_denominator=0
		gen D5_numerator=0

		cap drop D6_*
		gen D6_denominator=0
		gen D6_numerator=0

		forvalues X=1/100 {
			capture count if !missing(labdate`X')
			if(_rc!=0){
				continue
				// if there is an error
				// go back to the
				// start of the loop
			}
			
			// A4
			quietly replace A4_denominator=1 if ///
				!missing(labhb`X') & labhb`X'>=7 & labhb`X'<11
				
			// A5
			quietly replace A5_denominator=1 if ///
				!missing(labhb`X') & labhb`X'>0 & labhb`X'<7
				
			// D4
			quietly replace D4_denominator=1 if ///
				!missing(labgestage`X') & labgestage`X' < 24 & ///
				!missing(labbloodglu`X') & labbloodglu`X'>=140
				
			// D5
			quietly replace D5_denominator=1 if ///
				!missing(labgestage`X') & labgestage`X' >= 24 & ///
				!missing(labbloodglu`X') & labbloodglu`X'>=105 & labbloodglu`X'<=140
				
			// D6
			quietly replace D6_denominator=1 if ///
				!missing(labgestage`X') & labgestage`X' >= 24 & ///
				!missing(labogct`X') & labogct`X'> 140
				
			// LAB LOOP INSIDE LAB
			forvalues Y=1/100 {
				capture count if !missing(labdate`Y')
				if(_rc!=0){
					continue
					// if there is an error
					// go back to the
					// start of the loop
				}	
				
				// D5
				quietly replace D5_numerator=1 if ///
					!missing(labgestage`X') & labgestage`X' >= 24 & ///
					!missing(labbloodglu`X') & labbloodglu`X'>=105 & labbloodglu`X'<=140 & ///
					labdate`Y' < labdate`X' + 14 & ///
					labdate`Y' >= labdate`X' & ///
					!missing(labogct`Y') & labogct`Y'>0 & ///
					!missing(labdate`Y')
			}
						
			// MAN LOOP INSIDE LAB
			forvalues Y=1/100 {
				capture count if !missing(mandate`Y')
				if(_rc!=0){
					continue
					// if there is an error
					// go back to the
					// start of the loop
				}
				
				// A4
				quietly replace A4_numerator=1 if ///
					!missing(labhb`X') & labhb`X'>=7 & labhb`X'<11 & ///
					mandate`Y' < labdate`X' + 14 & ///
					mandate`Y' >= labdate`X' ///
					& (mantypex`Y'== "RefHighRisk") & ///
					!missing(mandate`Y') & ///
					!missing(mantypex`Y')
					
				// A5
				quietly replace A5_numerator=1 if ///
					!missing(labhb`X') & labhb`X'>0 & labhb`X'<7 & ///
					mandate`Y' < labdate`X' + 14 & ///
					mandate`Y' >= labdate`X' ///
					& (mantypex`Y'== "RefHosp") & ///
					!missing(mandate`Y') & ///
					!missing(mantypex`Y')
					
				// D4
				quietly replace D4_numerator=1 if ///
					!missing(labgestage`X') & labgestage`X' < 24 & ///
					!missing(labbloodglu`X') & labbloodglu`X'>=140 & ///
					mandate`Y' < labdate`X' + 14 & ///
					mandate`Y' >= labdate`X' ///
					& (mantypex`Y'== "RefHighRisk") & ///
					!missing(mandate`Y') & ///
					!missing(mantypex`Y')
					
				// D6
				quietly replace D6_denominator=1 if ///
					!missing(labgestage`X') & labgestage`X' >= 24 & ///
					!missing(labogct`X') & labogct`X'> 140 & ///
					mandate`Y' < labdate`X' + 14 & ///
					mandate`Y' >= labdate`X' ///
					& (mantypex`Y'== "RefHighRisk" | mantypex`Y'== "RefDiabetes" | mantypex`Y'== "RefHosp" | mantypex`Y'== "RefSpec") & ///
					!missing(mandate`Y') & ///
					!missing(mantypex`Y')
					
			}
		}
		save "data_clean/with_indicators_`year'-`month'.dta", replace
	}
}

