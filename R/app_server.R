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

  # this is the only data used by the server
  loaded_data <- load_data()
  data_curr <- loaded_data$data_curr
  regist_curr <- loaded_data$regist_curr
  rm(loaded_data)
  

  # explorer tab ------------------------------------------------------------

  # data explorer module
  mod_data_explorer_server(
    id = 'data_explorer', 
    data_curr = data_curr, 
    study = reactive(input$study_select)
  )
  
  # # et dataframes for all data and truncated by date
  # data_curr_explorer <- reactive(
  #   explorer_process(
  #     data_curr,
  #     study = input$study_select,
  #     vis_type = input$visit_curr,
  #     study_restrict = input$expl_study_restrict
  #   )
  # )
  # 
  # # Call to make the reactive plot
  # output$plot_curr_explorer_echarts <- echarts4r::renderEcharts4r(
  #   make_plot_explorer(
  #     data_curr_explorer(),
  #     indvar_curr = input$indvar_curr,
  #     depvar_curr = input$depvar_curr,
  #     group_curr = input$group_curr,
  #     vis_type = input$visit_curr,
  #     use_echarts = TRUE
  #   )
  # )
  # 
  # # Call to make the reactive table
  # output$table_curr_explorer <- reactable::renderReactable(
  #   reactable::reactable(
  #     make_table_explorer(
  #       data_curr_explorer(),
  #       indvar_curr = input$indvar_curr,
  #       depvar_curr = input$depvar_curr,
  #       group_curr = input$group_curr,
  #       vis_type = input$visit_curr
  #     ),
  #     highlight = TRUE
  #   )
  # )
  
  
  # As the operational tab is expanded, it will probably need to be recast as a reactive dataframe
  # Don't want to do it with the date truncation since it's a lot faster to just make a new df, date adjustment is another matter
  
  
  # enrollment tab ----------------------------------------------------------

  # Enrollment
  # At this stage, data_curr_enroll still has all rows, the only difference is refer/elig/enroll have been parsed according to study
  
  # We now merge the data.table that's been processed for enrollment onto the visit data
  
  #We're going to try tracking the calendar as an observable event
  #If the start date hasn't changed from load, we'll just use the start date from the dictionary
  .study_date_initial <- reactiveVal(TRUE)
  observeEvent(input$start_date, {
    if(input$start_date != min_date_all){
      .study_date_initial(FALSE)
    }
  }, ignoreInit=TRUE)
  
  
  # Call to make the reactive flow plot
  output$enroll_flow_plot_echart <- renderUI(
    make_plot_enroll_flow(
      data_curr,
      study = input$study_select,
      date_adjust = .study_date_initial(),
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
      study = input$study_select,
      date_adjust = .study_date_initial(),
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
  
  # render these objects even when hidden
  # outputOptions(output, 'table_refer_full', suspendWhenHidden = FALSE)
  
  
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
