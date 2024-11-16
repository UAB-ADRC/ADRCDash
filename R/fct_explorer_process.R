#' Process the dataframe used for the covariate explorer
#'
#' Updated version that uses df_slicer
#'
#' @param df 
#' @param study 
#' @param vis_type 
#' @param study_restrict 
#' @param dict 
#'
#' @return dataframe
#' @noRd
#' @author Chad Murchison
explorer_process <- function(df, study, vis_type, study_restrict, dict = explorer_slice){
  
  #Select dataframe based on study
  df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
  
  #First pull the slicing call from the dictionary
  slice_call <- dict[["df_action"]][[which(dict[["visit_type"]] == vis_type)]]
  
  #Check if we're restricting the study, need to add an argument if so
  if(study_restrict == TRUE){
    slice_call <- rlang::call_modify(slice_call, study_curr = study)
  }
  
  #Slice the dataframe according to the call whether it's been processed or not
  df <- eval(slice_call)

  return(df)
}
