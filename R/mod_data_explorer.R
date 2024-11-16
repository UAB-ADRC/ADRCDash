### TODO: this should be moved to the helpers package ###

#' data_explorer UI Function
#'
#' @description A shiny Module for the Data Explorer tab
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @author Joseph Marlo, \email{support@landeranalytics.com}
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::box(
        width = 4,
        title = 'Inputs',
        
        selectInput(inputId = ns("group_curr"),
                    label = "Group of Interest",
                    choices = group_choices),
        selectInput(inputId = ns("indvar_curr"),
                    label = "X-axis Category",
                    choices = xvar_choices),
        selectInput(inputId = ns("depvar_curr"),
                    label = "Y-axis Variable",
                    choices = yvar_choices),
        selectInput(inputId = ns("visit_curr"),
                    label = "Select visit",
                    choices = visit_choices),  
        shinyWidgets::prettyCheckbox(
          inputId = ns("expl_study_restrict"),
          label = "Restrict to Study Timeframe",
          icon = icon("check"),
          shape = "curve"
        )
      ),
      
      bs4Dash::box(
        width = 8,
        title = "Results",
        align = "center" , 
        reactable::reactableOutput(
          outputId = ns("table_curr_explorer")
        )
      )
    ),
    
    shiny::fluidRow(
      bs4Dash::box(
        width = 12,
        title = "Covariate Explorer Plot",
        echarts4r::echarts4rOutput(
          ns("plot_curr_explorer_echarts"), 
          height = "500px"
        )
      )
    )
 
  )
}
    
#' data_explorer Server Functions
#'
#' @noRd 
mod_data_explorer_server <- function(id, data_curr, study){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # validate data_curr object
    if (!isTRUE(is.data.frame(data_curr))) cli::cli_abort('`data_curr` must be a data.frame')
    
    # validate study object
    is_reactive <- tryCatch(is.reactive(study), error = function(e) FALSE)
    if (!isTRUE(is_reactive)) cli::cli_abort('`study` object must be a reactive. Wrap the study argument in the module call: e.g. `reactive(input$study_select)`')
    
    # get dataframes for all data and truncated by date
    data_curr_explorer <- reactive(
      explorer_process(
        data_curr,
        study = study(), #input$study_select,
        vis_type = input$visit_curr,
        study_restrict = input$expl_study_restrict
      )
    )
    
    # If there is no group, then treat it as the same when it matches indvar
    # This is preferred because many of the underlying functions already account for
      # indvar == group_var logic. E.g. using a jitter plot vs grouped jitter plot
    group_curr <- reactive({
      ifelse(
        input$group_curr == "None",
        input$indvar_curr,
        input$group_curr
      )
    })
    
    # Call to make the reactive plot
    output$plot_curr_explorer_echarts <- echarts4r::renderEcharts4r(
      make_plot_explorer(
        data_curr_explorer(),
        indvar_curr = input$indvar_curr,
        depvar_curr = input$depvar_curr,
        group_curr = group_curr(),
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
          group_curr = group_curr(),
          vis_type = input$visit_curr
        ),
        highlight = TRUE
      )
    )
 
  })
}
