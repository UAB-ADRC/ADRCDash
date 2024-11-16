### move this to the R package? ###


#' Load the core application data
#' 
#' The output of this should include the two core dataframes for the application: `data_curr` and `regist_curr`. See also fct_initial_redcap_process.R and test-data_format.R.
#' 
#' The function is called at the top of `app_server()`.
#' 
#' Most/all of this data processing will be moved to the data pipeline work. The app will pull down the processed tables from the SQL DB.
#'
#' @noRd
#'
#' @return a list of two dataframes
load_data <- function(){
  
  # TODO: depending on how the final data is pulled it, may need shiny::reactivePoll
  
  #Read-in the registry
  regist_curr <- registry_read_in(synth = TRUE, use_spinner = FALSE)
  
  #Read in NACC visits; also includes biomarker inventory
  nacc_curr_list <- visit_read_in(token = "REDCAP_NACC_API_NEW", subtable_dict = NULL, .type = "nacc", synth=TRUE)
  nacc_curr <- nacc_curr_list[["visits"]]
  invent_curr <- nacc_curr[["subtable"]]
  rm(nacc_curr_list)
  
  #Read in neuroimaging
  # neuroimag_curr_list <- visit_read_in(token = "REDCAP_NEUROIMAGE_API", .type = "neuroimage", all_cols = FALSE)
  # neuroimag_curr <- neuroimag_curr_list[["visits"]]
  # rm(neuroimag_curr_list)
  
  #Registry and Visit Processing
  
  #Dropping test cases - currently only ADC001
  regist_curr <- redcap_filter_test_cases(regist_curr, test_cases = c("001"), use_spinner = FALSE)
  
  #Validate age both for now and for A1 date
  regist_curr[["Age"]] <- make_age(regist_curr)
  nacc_curr[["A1 Age"]] <- make_age(nacc_curr, end_dt = min_fields$nacc)
  
  #Add interactions within interact_dict (currently only race and sex)
  regist_curr <- add_interact(regist_curr, itx_dict = interact_dict)
  # nacc_curr <- add_interact(nacc_curr, itx_dict = interact_dict)  #No interactions currently in nacc_curr
  
  #Recast some factors to better play with Shiny
  regist_curr <- recast_factors(regist_curr)
  nacc_curr <- recast_factors(nacc_curr)
  
  #Some additional processing, could probably be dropped or handled more effectively
  nacc_curr <- fact_to_cont(nacc_curr)
  # regist_curr <- rename_continuous(regist_curr)   #Only works with old cs_age, not used anymore
  
  
  #These next two sections could probably be condensed into something more elegant but it's working well enough for now
  
  #Set study level variables
  #We still use this function although it really only needs to handle the NACC / UAB ADRC participants and validate P20 registrants
  #Other studies can use the enrollment fields from the Additional Studies instrument as needed although a function may need to collapse conditions
  regist_curr[["NACC Participant"]] <- make_var_by_date(regist_curr, date_thresh = "2020-09-01")
  regist_curr[["ADRC Participant"]] <- make_var_by_date(regist_curr, date_thresh = "2018-01-01")
  regist_curr[["P20 Registrant"]] <- make_var_by_date(regist_curr, date_thresh = "2020-09-01", date_fields = c("contact_dt", "refer_dt", "screen_dt", "adc_clin_core_dt"))
  
  #The ADRC enrollment date doesn't exists as a consolidated field so we just made that here
  regist_curr[["adrc_enroll_dt"]] <- consolidate_dt(regist_curr)
  
  
  #A couple of dataset merges
  #We suppress warnings since data.table doesn't like us using := setting by reference; some of the function based modifications we make earlier lead to R based copies
  suppressWarnings(nacc_curr[, "Most Recent Visit" := date_find(.SD, dt_set = "nacc_visit"), by = eval(redcap_dict[["adrc_key"]])])
  suppressWarnings(regist_curr[nacc_curr, on = eval(redcap_dict[["adrc_key"]]), `:=` ("Most Recent Visit" = `Most Recent Visit`)])
  
  #Do a similar merge for most recent AD syndromal stage
  nacc_last <- df_slicer(nacc_curr, "last")
  suppressWarnings(regist_curr[nacc_last, on = eval(redcap_dict[["adrc_key"]]), `:=` ("AD Syndromal Stage" = `AD Syndromal Stage`)])
  
  
  #We call enroll_process_initial here in order to build out enrollment variables prior to the merge
  regist_curr <- enroll_process_initial(regist_curr)
  
  #Similarly, add in slight modifications on referral processing
  regist_curr <- refer_process(regist_curr)
  
  
  #At this point we have all the processing needed to upload to the database
  #Anything beyond this is some sort of processing that's done for tabulation or visualization
  
  #For convenience, we merge the registry data onto the nacc visit data
  #We do a previous merge step where we drop invalid registrants i.e. never contacted or insufficient info
  data_curr <- nacc_curr[regist_curr[!is.na(regist_curr$status),], on = eval(redcap_dict[["adrc_key"]])]
  
  data_out <- list(data_curr = data_curr, regist_curr = regist_curr)
  validate_data(data_out)
  
  return(data_out)
}


validate_data <- function(.data){
  
  if (!isTRUE(is.list(.data) && length(.data) == 2)){
    cli::cli_abort('Data must be a list of length 2')
  }
  
  if (!isTRUE(setequal(names(.data), c('data_curr', 'regist_curr')))){
    cli::cli_abort('Data must have names `data_curr` and `regist_curr`')
  }
  
  validate_data_data(.data$data_curr)
  validate_data_regist(.data$regist_curr)
  
  return(invisible(TRUE))
}

validate_data_data <- function(.data_data){
  
  if (!isTRUE(inherits(.data_data, 'data.frame'))){
    cli::cli_abort('`data_curr` must be a data.frame')
  }
  
  # TODO: additional checks
  
  return(invisible(TRUE))
}

validate_data_regist <- function(.data_regist){
  
  if (!isTRUE(inherits(.data_regist, 'data.frame'))){
    cli::cli_abort('`regist_curr` must be a data.frame')
  }
  
  # TODO: additional checks
  
  return(invisible(TRUE))
}
