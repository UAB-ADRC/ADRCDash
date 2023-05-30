#' Functions that do the processing on the dataframe used for the enrollment plots
#' 
#' Our new structure accommodates columns with different levels of involvement
#' This requires better use of list structure
#' 
#' @param df 
#' @param start_dt 
#' @param end_dt 
#' @param study 
#' @param compon_var 
#' @param group_var 
#' @param date_adjust 
#' @param follow_only 
#' @param dict 
#'
#' @author Chad Murchison
#' 
#' @noRd
compon_process <- function(df, start_dt, end_dt, study, compon_var, group_var, date_adjust = TRUE, follow_only = TRUE, dict = data_dict_component){
    
    #Return null if no components or groups are selected
    if(length(compon_var) == 0 || length(group_var) == 0) return(NULL)
  
    #Only use enrolled participants we're currently following; can adjust enroll_types in the dictionary
    df <- df[!is.na(df[[dict[["enroll_field"]]]]),]  
    if(follow_only == TRUE) df <- df[df[[dict[["enroll_field"]]]] %in% dict[["follow_only"]],]
    
    #Build the date off the appropriate enroll date built on step 8
    date_field <- study_choices[["date_field"]][which(study == study_choices[["name"]])]
    
    #Adjust by reactive date if desired (controlled by boolean, defaults to FALSE for display purposes)
    if(date_adjust == TRUE) df <- start_end_date_filter(df, date_field, start_dt, end_dt)
    
    #Select dataframe based on study
    df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
    
    #Slice the data frame - return first visit
    #Generally not needed since we're stilling relying on the registry 
    df <- df_slicer(df, slice_type = "first")
    
    #Return the NULL plot if needed
    if(nrow(df) == 0) return(default_plot)
    
    
    #Build parameters from dictionary, specifically the list of components and the annotation for column names and plots
    
    #Get the variable names, their annotations, and table column names based on the sidebar selections
    compon_var <- compon_var[order(match(compon_var, dict[["new_name"]]))]   #First order based on the dictionary
    compon_annot <- dict[["comp_annot"]][which(dict[["new_name"]] %in% compon_var)]
    compon_data_cols <- dict[["data_col"]][which(dict[["new_name"]] %in% compon_var)] #Then select the data column names
   
    #Recast Radio factors to reduced factor levels as needed using the modified flatten_radio (previously only collapsed to binary variables)
    rebin_set <- dict[["radio_to_bin"]][which(dict[["new_name"]] %in% compon_var)]
    for(ii in seq_along(compon_data_cols)){
      if(rebin_set[ii] == TRUE) df[[compon_data_cols[ii]]] <- flatten_radio(df[[compon_data_cols[ii]]], .var = compon_annot[ii])
    }
    
    #Build the row names and get the max number of levels (would be 1 for check boxes or dependent on radio button levels
    compon_rows <- build_compon_annot(compon_var)
    
    #Finally build out the table-esque structure that's used for eventual plotting and table generation
    compon_df <- lapply(group_var, function(.var){
      if(dict[["covar_collapse"]][which(dict[["covar_list"]] == .var)] == TRUE) df[[.var]] <- forcats::fct_drop(df[[.var]])
      multi_var_df_maker(df, .var, df_rownames = compon_rows)
    })
    
    # assign groups as an attribute
    # this gets passed to the plotting functions for setting the titles
    attributes(compon_df) <- list(groups = group_var)
    
    return(compon_df)
}
