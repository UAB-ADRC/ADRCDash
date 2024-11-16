#' Functions that do the initial processing on the REDCap data
#'  
#' 
#' STEP 1
#' This is set as its own function since it takes so long
#' Should be replaced by a `DBI` call at some point
#' source function is currently commented out for testing
#' 
#' @noRd
#' 



##
#Part 1 - the read in functions from REDCap
##

#Readin function for the registry object

registry_read_in <- function(synth = FALSE, use_spinner = TRUE, dict = redcap_dict, use_redcap_factors = FALSE){
  
  #Display spinner
  if(use_spinner == TRUE) spin_redcap$show()
  
  if(synth == FALSE){
  
  #Read in the registry from REDCap
  redcap_token <- Sys.getenv("REDCAP_API_NEW")
  #redcap_conn <- redcapAPI::redcapConnection(url="https://redcap.dom.uab.edu/api/", token=redcap_token)
  #redcap_curr <- redcapAPI::exportRecords(redcap_conn, factors=use_redcap_factors)
  redcap_curr <- REDCapR::redcap_read(redcap_uri = "https://redcap.dom.uab.edu/api/", token = redcap_token)$data
  } else{
    redcap_curr <- redcap_synth
  }
  
  #Coerce to data.table
  redcap_curr <- data.table::as.data.table(redcap_curr)
  
  #The very first step is building a registry key based on the record ID
  redcap_curr$regist_key <- paste0("UAB", sprintf("%06d", as.numeric(redcap_curr[[dict[["redcap_key"]]]])))
  
  #Drop any participants that are missing the minimum requirement of contact status
  redcap_curr <- redcap_drop_invalid_rows(redcap_curr, .type = "registry")
  
  return(redcap_curr)
}





#Readin function for visit based projects

visit_read_in <- function(token, synth = FALSE, dict = redcap_dict, subtable_dict = NULL, use_redcap_factors = FALSE, ...){
  
  if(synth == FALSE){
  
  #1 - Read in the NACC dataset
  visit_token <- Sys.getenv(token)
  #visit_conn <- redcapAPI::redcapConnection(url="https://redcap.dom.uab.edu/api/", token=visit_token)
  #visit_curr <- redcapAPI::exportRecords(visit_conn, factors = use_redcap_factors)
  visit_curr <- REDCapR::redcap_read(redcap_uri = "https://redcap.dom.uab.edu/api/", token = visit_token)$data
  
  #Coerce to data.table for populating
  visit_curr <- data.table::as.data.table(visit_curr)
  
   
  #2- Extract subtable if needed (for example, inventory and M1 from )
  if(!is.null(subtable_dict)){
    #INPUT LATER - Currently a second call is made to REDCAP_NACC_API
  } else{
    subtable_curr <- NULL
  }
  
  
  #3 - Fill down subject_data event
  
  #First get the column names associated specifically with "subject_data_arm_1" by checking which columns are all NA's
  visit_subj_data <- visit_curr[visit_curr[[dict[["event_col"]]]] == dict[["subj_event"]],]
  subj_data_cols_to_drop <- which(colSums(is.na(visit_subj_data)) == nrow(visit_subj_data))
  
  #Assuming it's not length 0, drop those columns
  if(length(subj_data_cols_to_drop) > 0){
    subj_data_cols <- colnames(visit_subj_data)[-subj_data_cols_to_drop]
  } else {
    subj_data_cols <- colnames(visit_subj_data)
  }
  
  #Fill down the subject_info rows - this is all done by reference within fill_down_rows so we technically don't need to assign it
  visit_curr <- fill_down_rows(visit_curr, dict = subj_data_cols, fill_key = dict[["redcap_key"]])
  
  } else{
    visit_curr <- data.table::as.data.table(nacc_synth)
    subtable_curr <- NULL
  }
  
  
  #4 - Run redcap_drop_invalid_rows which checks on the A1 date
  #This drops the subject data which never has a min_field data and any visit_info events missing from the min_field dictionary
  #We pass ... to make use of certain projects e.g. min_fields differs between nacc visits and neuroimaging visits
  visit_curr <- redcap_drop_invalid_rows(visit_curr, ...)
  
  #Finally, reorder the data frames
  data.table::setorderv(visit_curr, cols = c(dict[["adrc_key"]], dict[["visit_col"]]))
  
  
  
  #Return a list with each processed table which can be called as needed
  return(list(visits = visit_curr, subtable = subtable_curr))
}









##
#Part 2 - Read-in helpers 
##



#' Filter out rows that don't meet minimum requirements
#' We're trying to be a bit cleaner now though since the registry and study are two different entities
#' Specifically, the slice dictionary now has to index min_fields off the type of dictionary via .type
#' 
#' @noRd

redcap_drop_invalid_rows <- function(df, .type, dict = min_fields, all_cols = TRUE, ...){

  #Simplest approach is to just pull a temp data frame and index off of it
  df_temp <- df[,colnames(df) %in% dict[[.type]],with=FALSE]
  
  #Option 1 is to make sure there's a value in ALL columns
  if(all_cols == TRUE){
    #Cast as a matrix and evaluate which rows have no columns equal to NA
    .idx <- which(rowSums(is.na(df_temp)) == 0)
  
  #Other option is to make sure at least 1 column is filled (e.g. at least one neuroimage PET type)
  } else{
    #We do need to do this by compliment though
    .idx <- c(1:nrow(df_temp))
    .idx_neg <- which(rowSums(is.na(df_temp)) == ncol(df_temp))
    if(length(.idx_neg) > 0) .idx <- .idx[-.idx_neg]
  }
  
  #Use this index to return the parsed data frame
  df[.idx,]
}



#' Fill empty cells downward, mainly used to fill in subject level information in a visit column
#' We can use this on a number of fronts since dict is just expecting a vector of column names
#' We also use last-observation carried forward but can change if needed
#' We make extensive use of data.table's set by reference for efficiency
#' Unfortunately, it has needed some adjustments since the current version of data.table only works on integer/double columns
#' 
#' 
#' @noRd

fill_down_rows <- function(df, dict, .type = "locf", fill_key = redcap_dict[["adrc_key"]]){
  
  #Our new version just uses a wrapper around setnafill from data.table
  if(!data.table::is.data.table(df)) df <- data.table::as.data.table(df)
  
  #Update dictionary to only contain columns in the data.table and drop the fill_key
  dict <- dict[dict %in% colnames(df)]
  dict <- dict[dict != fill_key]
  
  
  #This is the workaround which takes characters and sets them to factors to use in the numeric representation
  #After that nafill is called and then the integers are replaced with the character levels in a for loop
  #Coerce everything to a factor to start
  df[, (dict) := lapply(.SD, factor), .SDcols = dict]
  #Pull the levels for back indexing later
  levels_set <- sapply(dict, function(xx){ levels(df[[xx]])}, simplify = FALSE)
  #Coerce all those factors into their integers
  df[, (dict) := lapply(.SD, as.integer), .SDcols = dict]
  #Run the setnafill
  df[, (dict) := lapply(.SD, data.table::nafill, .type), by = fill_key, .SDcols = dict]
  #Step through the columns and replace the integers for the factor levels
  for(col in dict) data.table::set(df, NULL, col, levels_set[[col]][df[[col]]])

  
  #In a perfect world, this is the only call that would be needed
  # df[, (dict) := lapply(.SD, data.table::na.fill, .type), by = fill_key, .SDcols = dict]
  
  return(df)
}






#' Filtering our specific test cases
#'
#' @noRd

redcap_filter_test_cases <- function(df, adc_id_column = redcap_dict[["adrc_key"]], test_cases = c("001", paste0("0", c(54:56,70,88))), use_spinner = TRUE){
  
  #Update the spinner
  if(use_spinner == TRUE) spin_redcap$update(html = spinner_fade_w_message("Processing REDCap Data"))
  
  #Set test cases based on function arguments
  adc_test_cases <- paste0("ADC", test_cases)
  
  #Filter out test_cases
  df <- dplyr::filter(df, !!(as.symbol(adc_id_column)) %not_in% adc_test_cases)
  
  return(df)
  
}
































##
#Part 3 - Adjusting some variables for shiny engagement
##




#' PART 3a
#' Recast factors using data dictionary, drop NA levels
#' Note this is done after adding the interactions (see add_interact under utils_misc_func.R)
#'
#' @noRd

recast_factors <- function(df, dict=data_dict){

  #For each entry in the data dictionary
  for(ii in seq_along(1:length(dict[["data_col"]]))){
    
    #Assuming the column exists
    if(dict[["data_col"]][ii] %in% colnames(df)){

      #Add the new factor
      df[[dict[["new_name"]][ii]]] <-
        factor(df[[dict[["data_col"]][[ii]]]],
               levels = dict[["col_levels"]][[ii]],
               labels = dict[["col_labels"]][[ii]])

      #Drop unused levels if necessary
      if(dict[["drop_levels"]][[ii]]==TRUE){
        df[[dict[["new_name"]][ii]]] <- forcats::fct_drop(df[[dict[["new_name"]][ii]]])
      }
    }
  }

  return(df)
}



#' PART 3b
#' Recast specific factors to continuous variables, dropping certain levels
#' This is used for things like years of educatin or CDR
#'
#' @noRd

fact_to_cont <- function(df, dict=data_dict_fact_to_cont){

  #For each entry in the data dictionary
  for(ii in seq_along(1:length(dict[["data_col"]]))){
    
    #Assuming the column exists
    if(dict[["data_col"]][ii] %in% colnames(df)){

      #Recast the factor to a continuous
      df[[dict[["new_name"]][ii]]] <- as.numeric(as.character(df[[dict[["data_col"]][ii]]]))

      #Recast any unknown levels or undesired values to NA
      bad_vals <- unique(c(dict[["cast_to_na"]][ii], dict[["levels_exclude"]][ii]))
      df[[dict[["new_name"]][ii]]][df[[dict[["new_name"]][ii]]] %in% bad_vals] <- NA
    }
  }


  return(df)
}



#' PART 3c
#' Rename continuous factors to match sidebar, make sure it's numeric
#' This currently isn't used but we're keeping it here just in case
#'
#' @noRd

rename_continuous <- function(df, dict=data_dict_cont){
  for(ii in seq_along(1:length(dict[["data_col"]]))){
    if(dict[["data_col"]][ii] %in% colnames(df)){
      df[[dict[["new_name"]][ii]]] <- df[[dict[["data_col"]][ii]]]
      df[[dict[["new_name"]][ii]]] <- as.numeric(as.character(df[[dict[["new_name"]][ii]]]))
    }
  }

  return(df)

}





##
#PART 4 - Making Age
##


#' STEP 4
#'
#' Making age relative to an ending date
#' We like to have some options for this since it's not something REDCap readily updates dynamically
#'
#' @noRd

make_age <- function(df, end_dt=NULL, dob_var = "regist_dob", yr_var = "birthyr", mo_var = "birthmo"){

  #Default to today's date for current age if end_dt is NULL
  if(is.null(end_dt)){
    date_curr <- lubridate::today()
    
  #Otherwise use end_dt  
  } else{
    date_curr <- df[[end_dt]]
  }
  
  #If a date variable exists, just take the difference using interval and dividing by years(1) using a modulo to round down %/%
  if(dob_var %in% colnames(df)) {
    age_curr <- lubridate::interval(df[[dob_var]], date_curr) %/% lubridate::years(1)
  
  #Otherwise we need to use a combination of month and year  
  } else{
    
    #Get the current / end_dt year and month
    yr_curr <- lubridate::year(date_curr)
    mo_curr <- lubridate::month(date_curr)
    
    #Get a month shift if necessary
    mo_adjust <- as.numeric(mo_curr < df[[mo_var]]) * (-1)
    
    #Then calculate the age
    age_curr <- yr_curr - df[[yr_var]] + mo_adjust
  }
  
  return(age_curr)
}






##
#Part 5 - Making some participant level variables
##





#' 5a - Participant variable by querying dates
#' This function checks dates to build participant level variables
#'
#' Structure is overall very similar to the trunc_by_date_new function although rather than cull the dataset, it builds a vector
#' We can also vary date_fields 
#'
#' @noRd

make_var_by_date <- function(df, date_thresh, id_col = redcap_dict[["adrc_key"]],
                             date_fields = c("adc_clin_core_dt", "adc_prep20_dt", "a1_form_dt"), set_all_dates = TRUE){
  #Initialize a vector of 0's
  nacc <- rep(0, nrow(df))

  #Step through the date_fields, anyone who has a valid date is marked as an appropriate participant
  for(ii in seq_along(date_fields)){
    
    #Make sure the date field exists
    if(date_fields[ii] %in% colnames(df)){

      #Start by identifying the IDs that match the date threshold
      id_idx <- which(!is.na(df[[date_fields[ii]]]) & df[[date_fields[ii]]] >= date_thresh)

      #Then recast the output vector to 1 for those IDs
      nacc[id_idx] <- 1
    }
  }

  #Update the the participant variable to cover all instances of the ID
  #We do include a toggle since this may need to by dynamic for visit (e.g. merging pre-P20 into NACC cohort)
  #Also check if the values id_col appears more than once in which case we'll need to populate everything accordingly
  #If this is done, it may need to be updated later i.e. rerun this function
  if(set_all_dates == TRUE && length(stats::na.omit(df[[id_col]])) > length(unique(df[[id_col]]))){
    df_temp <- data.frame(id = df[[id_col]], nacc = nacc)
    df_temp$nacc[df_temp$id %in% unique(df_temp$id[df_temp$nacc == 1])] <- 1
    nacc <- df_temp$nacc
  }

  return(nacc)

}


#' 5b - The ADRC enrollment date
#'
#' Consolidating the adc_preP20_dt and adc_clin_core_dt to get a global ADRC enrollment date
#' A little contrived but we use a temporary data frame to use data.table and assign by reference
#'
#' @noRd
#' 
consolidate_dt <- function(df, dt_fields = c("adc_clin_core_dt", "adc_prep20_dt"), .by = redcap_dict[["adrc_key"]], .min = TRUE){
  
  #Build a temp data frame
  df_temp <- df[, .SD, .SDcols = c(.by, dt_fields)]
  
  #initialize the date vector output
  df_temp$dt_out <- as.Date(NA)
  
  #Build out the variable
  #Rows are subset
  if(.min == TRUE){
    df_temp[!is.na(df_temp[[.by]]) & rowSums(is.na(df_temp[,..dt_fields])) < length(dt_fields), dt_out := min(stats::na.omit(sapply(.SD, as.Date))), by = .by, .SDcols = dt_fields]
  } else{
    df_temp[!is.na(df_temp[[.by]]) & rowSums(is.na(df_temp[,..dt_fields])) < length(dt_fields), dt_out := max(stats::na.omit(sapply(.SD, as.POSIXct))), by = .by, .SDcols = dt_fields]
  }
  
  return(df_temp$dt_out)
    
}






















