#' Functions that do the processing on the dataframe used for the completion component plots
#'  
#' @param df 
#' @param study 
#' @param start_dt 
#' @param end_dt 
#' @param visit_type 
#' @param enroll_field 
#' @param valid_enroll_levels 
#' @param dict 
#'
#' @author Chad Murchison
#' 
#' @noRd
completion_process <- function(df, study, start_dt, end_dt, visit_type, enroll_field = "enroll", valid_enroll_levels = c(1), dict = data_dict_completion){
    
    #We need to carry over some of the study / date processing that's no longer being done by `enroll_process_initial`
    if(!data.table::is.data.table(df)) df <- data.table::data.table(df)
    
    #First pull the specific study
    df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
    
    #Determine the specific study's start point and reference field for dates
    ref_date_field <- study_choices[["date_field"]][which(study_choices[["name"]] == study)]
    start_dt <- max(as.Date(start_dt), as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
    df <- start_end_date_filter_alt(df, date_field = ref_date_field, start_date = start_dt, end_date = end_dt, na_filter = TRUE)
    
    #Since completion is visit specific, we also make sure we don't have any visits outside the restricted time using visit_date_field
    restrict_date_field <- study_choices[["visit_date_field"]][which(study_choices[["name"]] == study)]
    df <- start_end_date_filter_alt(df, date_field = restrict_date_field, start_date = start_dt, end_date = end_dt, na_filter = TRUE)
    #This does impact some of the BVal visits outside the 9/1/2020 window but is largely not an issue
    
    
    #The rest proceeds as usual
    
    #Pull the most recent visit - this will need to be adjusted because of CSF, just a stop gap for now
    #In either case, we can use the slice_dict entry for the completion processing
    df <- df_slicer(df, slice_type = slice_dict[["completion_slice"]][["slice"]][which(slice_dict[["completion_slice"]][["type"]] == visit_type)])
   
    #Only use enrolled participants - add levels to valid_enroll_levels but defaults to "Currently Following" only
    df <- df[!is.na(df[[enroll_field]]) & df[[enroll_field]] %in% levels(df[[enroll_field]])[valid_enroll_levels],]
    
    #Step through each of the three types of completion plots in the data_dictionary
    for(entry in seq_along(dict)){
      
      #Get the current entry
      dict_curr <- dict[[entry]]
    
      #Begin the processing to flatten the completion fields
      for(ii in seq_along(dict_curr[["data_col"]])){
      
        #Get the recast parameters from the dictionary 
        #Because of multiple reduction fields, unlist commands have to be applied specifically for the shipped components (entry 3)
        .new <- dict_curr[["label_col"]][ii]
        .var <- dict_curr[["data_col"]][ii]
        .reduc <- dict_curr[["reduc"]][ii]
        .field <- unlist(dict_curr[["reduc_fields"]][ii])
        .radio_to_check <- dict_curr[["radio_to_check"]][ii]
        .reduc_vals <- unlist(dict_curr[["reduc_vals"]][ii]) #Bit lazy but this passes NULL if no entry in the dictionary exists
        
      
        #Make the recast variables and add to df
        if(length(df[[.var]])>0){
          df[[.new]] <- make_completion_var(df[[.var]], .reduc, df[,...field], .radio_to_check, .reduc_vals)
        }else df[[.new]] <- NA
      
      }
      
    }
    
    #The rest of the processing occurs in the plot call to avoid having a reactive dataframe with date filtering
    
    return(df)
    
  }



#The function that makes the new binary variable


#' @noRd
#' @describeIn completion_process Creates new binary variable
#' @description Creates new binary variable 
#' Uses dictionary to 
#' 1) recast NA's as 0 
#' 2) flatten a multi-factor to a binary (and turn NA's to 0) 
#' 3) filter on another field to add NA's a reduce the count
#' Option 3 is for things like PET and CSF that not everyone is doing
#' Recast has also been expanded to allow for filtering on multiple fields
make_completion_var <- function(.var, .recast, .reduc_field, .radio, .reduc_val = NULL){
  if(is.na(.recast) || .radio == TRUE){ .var[is.na(.var)] <- 0
  } else if(.recast == "flat"){ .var <- flatten_factor(.var)
  } 
  
  #Recast now includes an option on whether multiple fields should be used
  if(!is.na(.recast) && .recast == "reduc"){
    if(is.null(.reduc_val)){ .var <- reduc_factor(.var, .reduc_field)
    } else .var <- reduc_factor(.var, .reduc_field, .vals = .reduc_val)
  }
   
  return(.var)
}  

#' Create process plot
#' 
#' Finalizes the df processing within the plot call to limit the number of reactive elements
#'
#' @param df 
#' @param dict 
#'
#' @noRd
#' @author Chad Murchison
completion_process_plot <- function(df, dict){
    
    #Initialize
    df_out <- df
    
    #Restrict to the current components
    df_out <- df_out[,dict[["label_col"]][dict[["label_col"]] %in% colnames(df_out)], with=FALSE]
    
    #Make it long and drop any NA
    df_out <- data.table::melt(df_out, measure.vars = dict[["label_col"]], variable.name = "Component", value.name = "Completers")
    df_out <- df_out[!is.na(df_out$Completers),]
    
    #Recast component and completers as a factor
    df_out$Component <- forcats::fct_relevel(df_out$Component, dict[["label_col"]])
    df_out$Completers <- factor(df_out$Completers, levels=c(1,3,2,0), labels=c("Completed", "Partial Completion", "Unable to Complete", "Pending/Unknown"))
    
    #Get counts and percentages
    df_out <- df_out[,.N, by = c("Component", "Completers")]
    df_out[, Perc := .SD[["N"]]/sum(.SD[["N"]]), by="Component"]

    return(df_out)
}
