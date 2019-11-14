CompareRefToNew <- function(xref, xnew){
  fail <- FALSE
  # reference files missing in new export
  trouble <- xref[!xref %in% xnew]
  if(length(trouble)>0){
    cat(crayon::red(glue::glue("  \u2716 Missing in new: {trouble}\n\n")))
    fail <- TRUE
  }
  
  # new export files
  trouble <- xnew[!xnew %in% xref]
  if(length(trouble)>0){
    cat(crayon::red(glue::glue("  \u2716 Something new: {trouble}\n\n")))
    fail <- TRUE
  }
  if(!fail) cat(crayon::green("  \u2713 Works! :-)\n\n"))
  
  return(fail)
}

# CheckFilesAndVariables <- function(folder="e.reg-intervention"){
#   
#   total_fail <- FALSE
#   
#   ref_files <- fs::dir_ls(fs::path(FOLDER_DATA_RAW,
#                                    folder,
#                                    REF_CLINIC_INTERVENTION_DATE))
#   
#   new_files <- fs::dir_ls(fs::path(FOLDER_DATA_RAW,
#                                    folder,
#                                    CLINIC_INTERVENTION_DATE))
#   
#   
#   # reference files missing in new export
#   print("Checking for missing files")
#   total_fail <- total_fail | CompareRefToNew(xref=fs::path_file(ref_files), xnew=fs::path_file(new_files))
#   
#   shared_files <- fs::path_file(new_files)[fs::path_file(new_files) %in% fs::path_file(ref_files)]
#   
#   for(i in shared_files){
#     print(glue::glue("\n\nChecking for different variables in {i}"))
#     ref_f <- fread(fs::path(FOLDER_DATA_RAW,
#                             folder,
#                           REF_CLINIC_INTERVENTION_DATE,
#                           i))
#     
#     new_f <- fread(fs::path(FOLDER_DATA_RAW,
#                             folder,
#                           CLINIC_INTERVENTION_DATE,
#                           i))
#     
#     total_fail <- total_fail | CompareRefToNew(xref=names(ref_f), xnew=names(new_f))
#     
#   }
#   
#   if(total_fail){
#     for(i in 1:10) cat(crayon::red("  \u2713 FINAL CONCLUSION: FAILED :-(  \n\n"))
#     stop("FAILED!!!")
#   } else {
#     for(i in 1:10) cat(crayon::green("  \u2713 FINAL CONCLUSION: IT ALL WORKS! :-)\n\n"))
#   }
# }
# 
CheckFilesAndVariables <- function(folder="e.reg-intervention", 
                                   REF_DATE, 
                                   CHECK_DATE){
  total_fail <- FALSE  
  ref_files <- fs::dir_ls(fs::path(FOLDER_DATA_RAW,
                                   folder,
                                   REF_DATE)) 
  
  new_files <- fs::dir_ls(fs::path(FOLDER_DATA_RAW,
                                   folder,
                                   CHECK_DATE))  
                                  
                                   
# reference files missing in new export
  print("Checking for missing files")
  
  total_fail <- total_fail | CompareRefToNew(xref=fs::path_file(ref_files), xnew=fs::path_file(new_files))  
                                 
 shared_files <- fs::path_file(new_files)[fs::path_file(new_files) %in% fs::path_file(ref_files)]
 for(i in shared_files){
                        print(glue::glue("\n\nChecking for different variables in {i}"))
                        ref_f <- fread(fs::path(FOLDER_DATA_RAW,
                                                folder,
                                                REF_DATE,i))    
                                                                                                         new_f <- fread(fs::path(FOLDER_DATA_RAW,
                          folder,
                          CHECK_DATE,i))    
                                                                                                        total_fail <- total_fail | CompareRefToNew(xref=names(ref_f),
                                                                                                                 xnew=names(new_f))}
          if(total_fail){
                          for(i in 1:10) 
          cat(crayon::red("  \u2713 FINAL CONCLUSION: FAILED :-(  \n\n"))
           stop("FAILED!!!")} else {
                   for(i in 1:10) 
                   cat(crayon::green("  \u2713 FINAL CONCLUSION: IT ALL WORKS! :-)\n\n"))
                                                                                                         }
}



