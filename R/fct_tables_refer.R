#' Functions for making the more refined by-year referral table
#' 
#' The function for the full table has been rolled into the plotting function for referrals using a toggle
#' The first is designed to display all referrals in a single list
#' Very similar to what is found in the explorer tab, although simpler since only counts are use
#'
#' @param df 
#' @param source_curr 
#' @param group_curr 
#' @param study 
#' @param start_dt 
#' @param end_dt 
#' @param date_field 
#' @param restrict_on_study 
#' @param date_adjust 
#' @param dict 
#'
#' @noRd
#' @author Chad Murchison
#' @return reactable table
make_table_refer_by_yr <- function(df, source_curr, group_curr, 
                                   study, start_dt, end_dt,
                                   
                                   date_field = "refer_dt",
                                   restrict_on_study = FALSE, date_adjust = TRUE,
                                   
                                   dict = refer_details_dict){
  
  
  
  ##
  #Date thresholding and processing of P20 participants
  ##
  
  #Start the spinner
  # spin_update$show()
  
  #Return the NULL plot if needed
  if(length(source_curr) == 0){
    # spin_update$hide()
    return(return_default_plot())
  }
  
  #We don't consider anyone with a ".bad" contact_status entry i.e. anyone we've been unable to contact
  df <- df[!is.na(df$status) & df$status != status_dict[[".bad"]],]
  
  #Adds a catch to determine whether we recast the pre-P20 participants or not
  #In either case, rename the main referral_type to "Referral Source"
  if(study == "Full UAB ADRC Cohort"){ colnames(df)[colnames(df) == "refer_type"] <- "Referral Source"
  
  #For pre-P20 we use the recast referral
  } else {
    colnames(df)[colnames(df) == "refer_type_p20"] <- "Referral Source"
    
    #We also shift early referral dates to the study start date just so we don't drop them
    date_thresh <- study_choices[["start_dt"]][which(study_choices[["name"]] == study)]
    
    df[[date_field]][df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]]==1 & 
                     !is.na(df[[date_field]]) &
                     df[[date_field]] < as.Date(date_thresh)] <- date_thresh
  }
  
  
  #Initialize devpar_curr since it's static for the referral plot
  depvar_curr <- "Count"
  
  #Build out the main referral types that will be iterated through
  df <- df[df[["Referral Source"]] %in% source_curr,]
  df[["Referral Source"]] <- forcats::fct_drop(df[["Referral Source"]])
  indvar_curr_set <- c("type", names(dict[["sub_type"]][["plot_source_map"]][which(dict[["sub_type"]][["plot_source_map"]] %in% source_curr)]))
  
  #We use the study variable for splitting the stack to get enrolled and not-enrolled
  stack_split <- study_choices[["var"]][which(study_choices[["name"]] == study)]
  df[[stack_split]] <- factor(df[[stack_split]], levels=c(0,1), labels = c("Not Enrolled", "Enrolled"))
  
  
  #Finally filter based on the date field
  #If we're restricting to the study window, take the max of the input$start_dt and the start date of whatever study we're considering
  if(restrict_on_study == TRUE)  start_dt <- max(as.Date(start_dt), as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
  if(date_adjust == TRUE) df <- start_end_date_filter(df, date_field, start_dt, end_dt)
  
  
  
  ##
  #Iterating through the referral sets for both tables and plots
  ##
  
  tables_by_year_out <- lapply(indvar_curr_set, function(.indvar){
    
    #First run data frame processing that is needed for both plots and tables
    
    #Get the column names in the dataframe for indvar used by both tables and plots
    indvar_curr <- dict[[.indvar]][["field"]]
    if(.indvar == "type") indvar_curr <- "Referral Source"
    
    #Filter dataframe to not have NA's in group or indvar, also drop levels as needed
    df <- df[!is.na(df[[indvar_curr]]) & !is.na(df[[group_curr]]),]
    df[[indvar_curr]] <- forcats::fct_drop(df[[indvar_curr]])
    df[[group_curr]] <- forcats::fct_drop(df[[group_curr]])
    
    #First pull the years to get the appropriate tabs
    years_curr <- sort(unique(lubridate::year(df[[date_field]])))
    
    #Set the environment so the dictionary can recognize the .group entry in the dictionary
    .env <- environment()
    
    #Make a call to `create_referral_details_enroll` to create the individual tables / tabs based on years
    .tabs <- purrr::map(years_curr, ~tabPanel(.x, reactable::renderReactable(create_referral_details(.x, df, .dict_env = .env))))
    
    #Create the first wrapper set of tabs
    do.call(bs4Dash::tabsetPanel, append(.tabs, list(type="tabs")))
  })
  
  #Usual tab-building for type of referral
  names(tables_by_year_out) <- c(dict$type$plot_field, dict[["sub_type"]][["plot_source_map"]][which(dict[["sub_type"]][["plot_source_map"]] %in% source_curr)])
  .tabs <- purrr::map(names(tables_by_year_out), ~tabPanel(.x, renderUI(tables_by_year_out[[.x]])))
  
  #Return the final UI
  do.call(bs4Dash::tabsetPanel, append(.tabs, list(type="tabs")))
  
}
  

create_referral_details <- function(.filter, df, .dict_env = parent.env(),
                                    year_field = "refer_dt", #status_field = "refer",
                                    dict = refer_details_dict){
  
  #Initialize contact date
  df[[year_field]] <- lubridate::as_date(df[[year_field]])
  
  #Set the boundaries for the current year
  year_start <- lubridate::as_date(paste0(.filter,"-01-01"))
  year_end   <- lubridate::as_date(paste0(.filter,"-12-31"))
  
  #Subset the dataframe to return said year and set dictionary
  df <- df[df[[year_field]] >= year_start & df[[year_field]] <= year_end,]
  dict <- dict[["table_cast"]][["enroll"]]
  
  #Extract the relevant columns, reorder as necessary, and rename - using appropriate refer_details_dict
  #We do need to make sure we call the current environment since the binding is going to be global and .group won't be found
  dict_colnames <- sapply(dict[["data_col"]], eval, envir = .dict_env)
  df_out <- as.data.frame(df)[,colnames(df) %in% dict_colnames]
  df_out <- df_out[, match(dict_colnames, colnames(df_out))]
  colnames(df_out) <- sapply(dict[["new_name"]], eval, envir = .dict_env)
  
  # create reactable html
  html_out <- reactable::reactable(
    df_out, 
    rownames = FALSE,
    highlight = TRUE,
    # bordered = TRUE,
    minRows = 15,
    defaultPageSize = 15
  )
  
  return(html_out)
}
