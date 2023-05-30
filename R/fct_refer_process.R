#'
#' Building out the referral data for plotting and tabulation
#' This is designed to be Global in nature, we'll save reactives for the plotting and tables
#' We may eventually roll this into the main processing function for uploading to the database
#'
#' @noRd
#'




refer_process <- function(df, dict = refer_details_dict){
  
  #Initialize the dataset
  df_out <- df
  
  #Do a min field check on referral date
  df_out <- redcap_drop_invalid_rows(df_out, .type = "referral")
  
  #Step through and recast all types and subtypes to their factors
  #A single factor call for the main typing
  df_out[[dict[["type"]][["field"]]]] <- factor(df_out[[dict[["type"]][["field"]]]], 
                                                levels = seq_along(dict[["type"]][["labels"]]), labels = dict[["type"]][["labels"]])
  
  #An lapply for the sub_types
  #For works just as well but change it up a bit for scope
  invisible(lapply(dict[["sub_type"]][["recast_col"]], function(.col){
    
    #Get the dictionary entry
    .entry <- gsub(dict[["gsub_string"]], "", .col)
    #Recast the factor
    df_out[[.col]] <<- factor(df_out[[.col]],
                              levels = seq_along(dict[[.entry]][["labels"]]), labels = dict[[.entry]][["labels"]])
    return(NULL)
    })
  )
  
  #With levels created coalesce the subtype vectors after coercing from factor to character
  df_out$refer_subtype <- data.table::fcoalesce(df_out[,lapply(.SD, as.character), .SDcols = dict[["sub_type"]][["coalesce"]]])
  
  
  #Copy the main referral type and recast it so all pre-P20 participants are cast as "Prior ADC Participants"
  #We'll just use the entry for the full cohort and the P20 threshold date, need to hard code something anyway, either here or the arguments
  .date_field <- study_choices[["date_field"]][3]
  .date_thresh <- study_choices[["start_dt"]][1]
  
  df_out$refer_type_p20 <- df_out$refer_type
  df_out$refer_type_p20[df_out[["P20 Registrant"]]==1 & !is.na(df_out[[.date_field]]) & 
                          df_out[[.date_field]] < as.Date(.date_thresh)] <- dict$type$recast
  
  #Finally, we coallesce some variables (enroll-elig-refer / screen_decline_reasn-refer_details) for table printing
  #These used during some refer table building later, see dict_refer_details
  df_out[["coalesced_enroll"]] <- data.table::fcoalesce(lapply(df_out[,.(enroll, elig, refer)], as.character))
  
  #A small catch for prior declines that were set to Undecided for research interest during initial registry creation
  df_out[["coalesced_enroll"]][is.na(df_out[["coalesced_enroll"]]) & df_out[["regist_research_yn"]]==2 & 
                                 df_out[["contact_status"]]==9 & df_out[["screen_eligibility"]]==3] <- "Prior Decline"
  df_out[["coalesced_details"]] <- data.table::fcoalesce(lapply(df_out[,.(screen_decline_reasn, refer_details)], as.character))
  
  
  
  return(df_out)
  
  
}


