tab is_clinical_int
tab is_demof_not_greenf_or_bookf
tab is_trial_arm_a


//find demof in mahima clinics to then get the total of trial a and trial b together
//arm a doesnt change because its control, we are controlling the files that go into
//the system 

***find the number of trial clinics
tab is_clinical_int is_mahima_clinics_trial_1
 
***finds the total of arm a and arm b combines***
tab is_mahima_clinics_trial_1 is_demof_not_greenf_or_bookf

****Hospital stuff****
tab is_aviccena
tab is_hbo
tab is_hospital_birth_outcome 
tab is_aviccena is_trial_arm_a
tab is_aviccena is_trial_arm_b

tab is_trial_arm_a if is_mahima_clinics_trial_1==1 & missing( is_demof_not_greenf_or_bookf)
tab is_trial_arm_b if is_mahima_clinics_trial_1==1 & missing( is_demof_not_greenf_or_bookf)

