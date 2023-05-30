#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @noRd
#' 
app_server <- function( input, output, session ) {
  

  # data --------------------------------------------------------------------

  # DATA PROCESSING ON BASE FRAME - See fct_initial_redcap_process.R for details
  
  # Reading in all the data
  loaded_data <- local({
  
    #Read-in the registry
    regist_curr <- registry_read_in(synth = FALSE, use_spinner = FALSE)
    
    #Read in NACC visits; also includes biomarker inventory
    nacc_curr_list <- visit_read_in(token = "REDCAP_NACC_API_NEW", subtable_dict = NULL, .type = "nacc", synth=FALSE)
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

    data_curr$Race[data_curr$Race=="Other"] <- "White"
    regist_curr$Race[regist_curr$Race=="Other"] <- "White"
    
    return(list(data_curr = data_curr, regist_curr = regist_curr))
  })

  # this is the only data used by the server
  data_curr <- loaded_data$data_curr
  regist_curr <- loaded_data$regist_curr
  rm(loaded_data)

  # hide loading spinner
  Sys.sleep(2) # prevent flashing if load is too fast
  waiter::waiter_hide()
  # spin_redcap$hide()
  

  # explorer tab ------------------------------------------------------------

  # et dataframes for all data and truncated by date
  data_curr_explorer <- reactive(
    explorer_process(
      data_curr,
      study = input$study_select,
      vis_type = input$visit_curr,
      study_restrict = input$expl_study_restrict
    )
  )

  # Call to make the reactive plot
  output$plot_curr_explorer_echarts <- echarts4r::renderEcharts4r(
    make_plot_explorer(
      data_curr_explorer(),
      indvar_curr = input$indvar_curr,
      depvar_curr = input$depvar_curr,
      group_curr = input$group_curr,
      vis_type = input$visit_curr,
      use_echarts = TRUE
    )
  )
  
  # Call to make the reactive table
  output$table_curr_explorer <- reactable::renderReactable(
    reactable::reactable(
      make_table_explorer(
        data_curr_explorer(),
        indvar_curr = input$indvar_curr,
        depvar_curr = input$depvar_curr,
        group_curr = input$group_curr,
        vis_type = input$visit_curr
      ),
      highlight = TRUE
    )
  )
  
  
  # As the operational tab is expanded, it will probably need to be recast as a reactive dataframe
  # Don't want to do it with the date truncation since it's a lot faster to just make a new df, date adjustment is another matter
  
  
  # enrollment tab ----------------------------------------------------------

  # Enrollment
  # At this stage, data_curr_enroll still has all rows, the only difference is refer/elig/enroll have been parsed according to study
  
  # We now merge the data.table that's been processed for enrollment onto the visit data
  
  # Call to make the reactive flow plot
  output$enroll_flow_plot_echart <- renderUI(
    make_plot_enroll_flow(
      data_curr,
      study = input$study_select,
      date_adjust = TRUE,
      start_dt = input$start_date,
      end_dt = input$end_date,
      use_echarts = TRUE
    )
  )
  
  
  # Call to make the reactive rate plot
  data_curr_rate <- reactive(
    enroll_process_rate(
      regist_curr, 
      start_dt = input$start_date, 
      end_dt = input$end_date, 
      study = input$study_select
    )
  )
  output$enroll_rate_plot_all <- echarts4r::renderEcharts4r(
    make_plot_enroll_rate_all(
      data_curr_rate(), 
      use_echarts = TRUE
    )
  )
  output$enroll_rate_plot_race <- echarts4r::renderEcharts4r(
    make_plot_enroll_rate_race(
      data_curr_rate(), 
      use_echarts = TRUE
    )
  )
  
  
  # referral tab ------------------------------------------------------------
  
  # Call to make the reactive plot
  # output$plot_refer <- renderUI(
  #   make_object_refer(
  #     regist_curr, 
  #     study = input$study_select, 
  #     group_curr = input$refer_group_curr,
  #     source_curr = input$refer_source_curr, 
  #     restrict_on_study = input$refer_restrict,
  #     start_dt = input$start_date, 
  #     end_dt = input$end_date, 
  #     make_plot = TRUE
  #   )
  # )
  output$plot_refer_ec <- renderUI(
    make_object_refer(
      regist_curr, 
      study = input$study_select, 
      group_curr = input$refer_group_curr,
      source_curr = input$refer_source_curr, 
      restrict_on_study = input$refer_restrict,
      start_dt = input$start_date, 
      end_dt = input$end_date, 
      make_plot = TRUE,
      use_echarts = TRUE
    )
  )
  
  # Call to make the primary reactive table
  output$table_refer_full <- renderUI(
    make_object_refer(
      regist_curr, 
      study = input$study_select, 
      group_curr = input$refer_group_curr,
      source_curr = input$refer_source_curr, 
      restrict_on_study = input$refer_restrict,
      start_dt = input$start_date, 
      end_dt = input$end_date, 
      make_plot = FALSE
    )
  )

  # A call for the split tables
  output$table_refer_by_yr <- renderUI(
    make_table_refer_by_yr(
      regist_curr,
      study = input$study_select,
      group_curr = input$refer_group_curr,
      source_curr = input$refer_source_curr,
      restrict_on_study = input$refer_restrict,
      start_dt = input$start_date,
      end_dt = input$end_date
    )
  )
  
  
  # study component involvement ---------------------------------------------

  # Make data frames for building tables / plots (need to make specific call for truncation)
  data_curr_compon <- reactive(
    compon_process(
      regist_curr, 
      start_dt = input$start_date, 
      end_dt = input$end_date, 
      study = input$study_select,
      compon_var = input$compon_curr, 
      group_var = input$compon_group_curr
    )
  )

  # Build the plots
  output$compon_plot_ec <- renderUI(
    make_plot_compon_ec(
      data_curr_compon()
    )
  )

  # Build the table
  output$compon_table <- reactable::renderReactable(
    reactable::reactable(
      make_table_compon(
        data_curr_compon()
      ),
      rownames = FALSE,
      highlight = TRUE
    )
  )
      
  
  # component completion ----------------------------------------------------

  # update UI box titles based on dropdown
  observeEvent(input$completion_vis_data, {
    bs4Dash::updateBox(
      "complete_table_box",
      action = "update",
      options = list(title = h2(input$completion_vis_data))
    )
    bs4Dash::updateBox(
      "complete_plot_box",
      action = "update",
      options = list(title = h2(input$completion_vis_data))
    )
  })
  
  # Make data frame
  data_curr_completion <- reactive(
    completion_process(
      data_curr, 
      start_dt = input$start_date, 
      end_dt = input$end_date,
      study = input$study_select, 
      visit_type = input$completion_vis_curr
    )
  )

  # Make the plot
  # Note this has an extra processing step inside it to avoid making a reactive dataframe in the previous step
  output$visit_comp_echarts <- echarts4r::renderEcharts4r({
    # render plot based on user input
    
    # get data type
    dict_entry <- switch(
      input$completion_vis_data,
      `UDS Visit Components` = 'visit',
      `Collected Biomarkers and Imaging` = 'collect',
      `Shipped Biospecimens` = 'shipped'
    )
    if (is.null(dict_entry)) cli::cli_warn('Invalid dropdown value for type')
    
    # make plot
    e_chart <- make_plot_completion(
      data_curr_completion(),
      dict_entry = dict_entry,
      study = input$study_select,
      start_dt = input$start_date,
      end_dt = input$end_date,
      use_echarts = TRUE
    )
    
    return(e_chart)
  })
  
  # tables
  output$visit_comp_table <- reactable::renderReactable({
    # render table based on user input
    
    # get data type
    dict_entry <- switch(
      input$completion_vis_data,
      `UDS Visit Components` = 'visit',
      `Collected Biomarkers and Imaging` = 'collect',
      `Shipped Biospecimens` = 'shipped'
    )
    if (is.null(dict_entry)) cli::cli_warn('Invalid dropdown value for type')
    
    # create table
    completion_table <- make_plot_completion(
      data_curr_completion(), 
      dict_entry = dict_entry, 
      study = input$study_select,
      start_dt = input$start_date, 
      end_dt = input$end_date, 
      return_table = TRUE
    )

    # convert to reactable
    colnames(completion_table)[1] <- ' '
    react_table <- reactable::reactable(
      completion_table,
      columns = list(` ` = reactable::colDef(minWidth = 150)),
      highlight = TRUE
      # minRows = 4
      # bordered = TRUE
    )
    
    return(react_table)
  })


  # biospecimen inventory ---------------------------------------------------

  # #inventory_process used to make a unique API call, this isn't necessary but future processing may be required
  # #For example, we currently only consider sample in aggregate
  # data_curr_inventory <- inventory_process(data_curr)

  # Process the tables reactive (return a list of tables)
  inventory_tables_list <- reactive({
    
    # create table
    inv_table <- make_table_inventory(
      data_curr, 
      study = input$study_select, 
      group_var = input$inventory_group
    )

    # change first column name if no grouping vars
    if (is.null(input$inventory_group)){
      colnames(inv_table[["sum"]])[1] <- ' '
      colnames(inv_table[["mean"]])[1] <- ' '
    }
    
    return(inv_table)
  })

  # Render the tables
  output$biospecimen_table_sum <- reactable::renderReactable({
    table_sum <- inventory_tables_list()[["sum"]]
    reactable::reactable(
      table_sum,
      rownames = FALSE,
      highlight = TRUE,
      minRows = 2
    )
  })
  output$biospecimen_table_mean <- reactable::renderReactable({
    table_mean <- inventory_tables_list()[["mean"]]
    reactable::reactable(
      table_mean,
      rownames = FALSE,
      highlight = TRUE,
      minRows = 2
    )
  })
  

  # other -------------------------------------------------------------------

  # show warning popup if user is on mobile
  observeEvent(shinybrowser::is_device_mobile(), {
    if(isTRUE(shinybrowser::is_device_mobile())){
      ADRCDashHelper::show_popup_mobile(session) 
    }
  })
  
}
