#'
#'A collection of outdated functions largely kept for code persistence e.g. using tidyverse
#'
#'@noRd
#'





#This is the old version of slicer which used the tidyverse; we use data.table now instead
#Function to split a dataframe using group_by
#In this context, can return the first row, all rows, or most recent row
#Can also return the first NACC event (e.g. either IV or IV_redo)

df_slicer_tidy <- function(df, slice_type, drop_min = FALSE,
                           group_col = "adc_sub_id", 
                           dict = slice_dict, event_col = "redcap_event_name", 
                           study_curr = NULL,
                           default_event_1 = "iv_arm_1", default_event_2 = "iv_pre_arm_1"){
  
  #Make sure the proper slicer is being used
  if(is.null(slice_type)) stop("Please select a valid slice, see `slice_dict` for details")
  
  #Note, using dplyr function
  
  #Modularization update - for the first / last slices drop any row without a valid date in the minimum date set (prescreen and a1_form_dt)
  
  
  
  #Initial processing based on arguments - drop min dates or restrict to study timeframe
  df_out <- df
  #if(drop_min == TRUE) df_out <- df_out[!rowSums(is.na(df_out[,colnames(df_out) %in% dict$min_fields])),]
  
  #Restricts study if needed
  if(!is.null(study_curr)){
    start_dt <- study_choices[["start_dt"]][which(study_choices[["name"]] == study_curr)]
    study_date_field <- study_choices[["expl_restrict_date_field"]][which(study_choices[["name"]] == study_curr)]
    df_out <- df_out[!is.na(df_out[[study_date_field]]) & df_out[[study_date_field]] > as.Date(start_dt),]
  }
  
  
  
  #Return first row, regardless of occurrence
  if(slice_type == "first"){
    df_out <- dplyr::slice(dplyr::group_by(df_out, !!as.symbol(group_col)), 1)
    #df_out[, .I[1], by = group_col]
  }
  
  #Return last row
  if(slice_type == "last"){
    df_out <- dplyr::slice(dplyr::group_by(df_out, !!as.symbol(group_col)), dplyr::n())
    #df_out[, .I[.N], by = group_col]
  }
  
  #First NACC has also been updated since the "iv_pre" events are pushed to the end
  #Return first NACC row
  if(slice_type == "first NACC"){
    
    #First drop all rows not in the default events
    df_out <- df[df[[event_col]] %in% c(default_event_1),]
    
    #Find the ids with the second event (iv_redo)
    #ids_with_redos <- df_out[[group_col]][df_out[[event_col]] %in% default_event_2]
    
    #Drop their first event
    #df_out <- df_out[-which(df_out[[group_col]] %in% ids_with_redos & df_out[[event_col]] %in% default_event_1),]
  }
  
  #For longitudinal, return anything with two or more rows
  if(slice_type == "longitudinal"){
    group_tab <- table(df_out[[group_col]]); drop_group <- names(group_tab[group_tab==1])
    df_out <- df_out[df_out[[group_col]] %not_in% drop_group,]
  }
  
  #As default, return the original dataset
  if(slice_type == "all" || slice_type %not_in% dict[["slice_type"]]) return(df)
  
  return(df_out)
}




