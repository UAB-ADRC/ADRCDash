#' The application User-Interface
#' 
#' 
#' UI is defined with header, left sidebar and body
#' Sidebar has three options: "Home", "Data Explorer", "Operational Dashboard"
#' 
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @import data.table
#'
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources - adds CSS (www) and Images (img)
    golem_add_external_resources(),
    
    # initial load spinner
    waiter::waiterShowOnLoad(
      color = ADRCDashHelper::color_palette$green_dark,
      html = shiny::tagList(
        tags$img(src = 'www/logo-watermark.png',
                 style = 'width: 200px; margin-bottom: 40px',
                 alt = 'UAB logo'),
        br(),
        waiter::spin_ellipsis()
      )
    ),
    
    # TODO: remove when ready
    ADRCDashHelper::add_beta_ribbon(),
    
    # start of dashboard
    bs4Dash::dashboardPage(
      title = "ADRC Data Visualization",
      fullscreen = TRUE,
      dark = NULL,
      
      header = bs4Dash::dashboardHeader(
        title = tags$a(
          href = 'https://www.uab.edu/',
          target = "_blank",
          tags$img(
            src = 'www/logo-ADC.png',
            width = "100%",
            style = "padding: 8% 10% 2% 10%", 
            alt = 'UAB logo'
          )
        )
      ),
      
      body = bs4Dash::dashboardBody(
        
        bs4Dash::tabItems(

          # Home tab - splash image
          bs4Dash::tabItem(
            tabName = "home_tab", 
            
            htmltools::tags$div(
              id = 'landing-page',
              htmltools::tags$img(src = "img/splash.png"),
              htmltools::tags$p(
                "This is the visualization dashboard for UAB's Alzheimer's Disease Research Center. Navigation is on the left-hand side and consists of a data exploration tab and operation dashboard. The exploration tab allows you to select covariates of interest and can be subset by sex and race. The operational dash further details participant workflows and engagement."
              )
            )
          ),
          
          # Data Explorer tab - consists of 1 fluidRow for a plotly element, another for a table output
          bs4Dash::tabItem(
            tabName = "explorer_output",
            
            shiny::fluidRow(

              bs4Dash::box(
                width = 4,
                title = 'Inputs',
                
                selectInput(inputId = "group_curr",
                            label = "Group of Interest",
                            choices = group_choices),
                selectInput(inputId = "indvar_curr",
                            label = "X-axis Category",
                            choices = xvar_choices),
                selectInput(inputId = "depvar_curr",
                            label = "Y-axis Variable",
                            choices = yvar_choices),
                selectInput(inputId = "visit_curr",
                            label = "Select visit",
                            choices = visit_choices),  
                shinyWidgets::prettyCheckbox(
                  inputId = "expl_study_restrict",
                  label = "Study Timeframe Only",
                  icon = icon("check"),
                  shape = "curve"
                )
              ),
              
              bs4Dash::box(
                width = 8,
                title = "Results",
                align = "center" , 
                reactable::reactableOutput(
                  outputId = "table_curr_explorer"
                )
              )
            ),
            
            shiny::fluidRow(
              bs4Dash::box(
                width = 12,
                title = "Covariate Explorer Plot",
                echarts4r::echarts4rOutput("plot_curr_explorer_echarts", height = "500px")
              )
            )
          ),
          
          # Operational tab 1 - Enrollment status - 1st fluidRow for plot of enrollment flow; 2nd fluidRow for enrollment rates over time
          bs4Dash::tabItem(
            tabName = "enrollment_status",
            
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = 'Enrollment Flow',
                # plotOutput("enroll_flow_plot", height = "500px")
                # TODO: this doesn't render in shiny https://stackoverflow.com/questions/73891585/echarts4r-ee-arrange-not-displaying-in-shiny
                # echarts4r::echarts4rOutput(outputId = 'enroll_flow_plot_echart', height = '500px')
                uiOutput(outputId = 'enroll_flow_plot_echart')
              )
            ),
            
            fluidRow(
              bs4Dash::box(
                width = 6,
                title = 'Enrollment Rate',
                # plotly::plotlyOutput("enroll_rate_plot_all", height = "600px")
                echarts4r::echarts4rOutput("enroll_rate_plot_all", height = "500px")
              ),
              bs4Dash::box(
                width = 6,
                title = 'Enrollment Rate by Race',
                # plotly::plotlyOutput("enroll_rate_plot_race", height = "600px")
                echarts4r::echarts4rOutput("enroll_rate_plot_race", height = "500px")
              )
            )
          ),
          
          # Operational tab 2 - Referral source - Similar layout as Explorer tab for counts
          bs4Dash::tabItem(
            tabName = "referral_output",
            
            fluidRow(
              bs4Dash::box(
                width = 4,
                title = 'Inputs',
                
                selectInput(
                  inputId = "refer_group_curr", 
                  label = "Referral Subset Group",
                  choices = group_choices, 
                  selected = "Race"
                ),
                shinyWidgets::pickerInput(
                  inputId = "refer_source_curr", 
                  label = "Referral Sources Selection",
                  choices = refer_choices, 
                  selected = refer_choices,
                  options = list(`actions-box` = TRUE, size = "auto",
                                 `selected-text-format` = "count", `count-selected-text` = "{0} selected"),
                  multiple = TRUE
                ),
                shinyWidgets::prettyCheckbox(
                  inputId = "refer_restrict", 
                  label = "Restrict to Study Timeframe",
                  icon = icon("check"),
                  shape = "curve"
                )
              )
            ),
            
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = "Referral Source",
                # height = '800px',
                uiOutput("plot_refer_ec")
              )
            ),
            fluidRow(
              bs4Dash::box(
                width = 12,
                style = 'min-height: 425px;',
                title = "Referral Source - Full Cohort", 
                tableOutput("table_refer_full")
              ),
              bs4Dash::box(
                width = 12,
                style = 'min-height: 750px;',
                title = "Referral Source - By Year", 
                uiOutput("table_refer_by_yr")
              )
            )
          ),
          
          # Operational tab 3 - Study Component Involvement - fluidRow for a plot; fluidRow for table
          # This has been expanded as a drop down to generalize to a variety of components although we still only use the main demographics
          bs4Dash::tabItem(
            tabName = "compon_status",
            
            fluidRow(
              bs4Dash::box(
                width = 4,
                title = 'Inputs',
                
                shinyWidgets::pickerInput(
                  inputId = "compon_group_curr", 
                  label = htmltools::tagList("Enrollment Groups", htmltools::h6(htmltools::tags$em("Select up to 3"))),
                  choices = compon_group_choices, 
                  selected = compon_group_choices[1],
                  options = list(`actions-box` = TRUE, size = "auto", `max-options-group` = 3,
                                 `selected-text-format` = "count", `count-selected-text` = "{0} selected"),
                  multiple = TRUE
                ),
                shinyWidgets::pickerInput(
                  inputId = "compon_curr", 
                  label = htmltools::tagList("Data Collection", htmltools::h6(htmltools::tags$em("Select up to 4"))),
                  choices = compon_choices, 
                  selected = compon_choices[1],
                  options = list(`actions-box` = TRUE, size = "auto", `max-options-group` = 4,
                                 `selected-text-format` = "count", `count-selected-text` = "{0} selected"),
                  multiple = TRUE
                )
              ),
              bs4Dash::box(
                width = 8,
                title = 'Component Table',
                reactable::reactableOutput(outputId = "compon_table")
              )
            ),
            
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = 'Component Plot',
                uiOutput(outputId = 'compon_plot_ec')
              )
            )
          ),
          
          # Operational tab 4 - Component completion with 1 fluidRow element for the plot
          bs4Dash::tabItem(
            tabName = "component_completion",
            
            fluidRow(
              bs4Dash::box(
                width = 4,
                title = 'Inputs',
                style = 'min-height: 240px',
                selectInput(
                  inputId = "completion_vis_curr", 
                  label = "Visit Selection",
                  choices = complete_visits_select, 
                  selected = "Most Recent Visit"
                ),
                selectInput(
                  inputId = "completion_vis_data", 
                  label = "Select Type",
                  choices = list(
                    "UDS Visit Components", 
                    "Collected Biomarkers and Imaging", 
                    "Shipped Biospecimens"
                  ),
                  selected = 1
                )
              ),
              bs4Dash::box(
                width = 8,
                id = 'complete_table_box',
                title = '...awaiting title',
                style = 'min-height: 240px',
                reactable::reactableOutput(outputId = "visit_comp_table")
              )
            ),
            
            fluidRow(
              bs4Dash::box(
                width = 12,
                id = 'complete_plot_box',
                title = '...awaiting title',
                echarts4r::echarts4rOutput(
                  outputId = "visit_comp_echarts",
                  height = '500px'
                )
              )
            )
          ),
          
          # Operational tab 5 - Inventories
          bs4Dash::tabItem(
            tabName = "biospecimen_inventory",
            
            fluidRow(
              bs4Dash::box(
                width = 4,
                title = 'Inputs',
                shinyWidgets::pickerInput(
                  inputId = "inventory_group", 
                  label = "Select inventory subsets",
                  choices = inventory_group_select,
                  options = list(
                    `actions-box` = TRUE, 
                    size = "auto",
                    `select-text-format` = "count", 
                    `count-selected-text` = "{0} selected"
                  ),
                  multiple = TRUE)
              )
            ),
            
            fluidRow(
              bs4Dash::box(
                width = 12,
                title = "Inventory Totals {REVIEW NAME}",
                reactable::reactableOutput(outputId = "biospecimen_table_sum")
              ),
              bs4Dash::box(
                width = 12,
                title = "Inventory Totals {REVIEW NAME}",
                reactable::reactableOutput(outputId = "biospecimen_table_mean")
              )
            )
          )
        )
      ),
      
      sidebar = bs4Dash::dashboardSidebar(
        id = 'sideBarMenu',
        skin = 'dark',
        width = "275px",
        
        bs4Dash::sidebarMenu(
          
          # study select input at top of page
          shinyWidgets::pickerInput(
            inputId = "study_select",
            label = "Study",
            choices = study_choices[["name"]],
            multiple = FALSE,
            selected = "ADRC Cohort"
          ),
          
          htmltools::hr(style = "height: 1px; background-color: #c9d1c5; margin: 1em;"),
        
          # Submenu 1 - Home tab
          bs4Dash::menuItem(
            text = "Home",
            tabName = "home_tab",
            selected = TRUE,
            icon = icon("home", lib = "glyphicon")
          ),
          
          # Submenu 2 - Data Explorer with plot and companion table
          # Has three inputs, grouping of plot (used as group aes), depvar (for Y-axis) and indvar (for X-axis)
          bs4Dash::menuItem(
            text = "Data Explorer", 
            tabName = "explorer_output",
            icon = icon("wpexplorer")
          ),
          
          # Submenu 3 - Operational dashboard, subsetable by min date and max date as inputs
          bs4Dash::menuItem(
            text = "Operational Dash",
            tabName = "operational_dash",
            icon = icon("table"), 

            # Date inputs for all three operational menu options
            dateInput(
              inputId = "start_date",
              width = "100%",
              label = "Start Date",
              min = min_date_all,
              value = min_date_all,
              format = "mm/dd/yyyy"
            ),
            dateInput(
              inputId = "end_date",
              width = "100%",
              label = "End Date",
              min = min_date_all,
              value = lubridate::today(),
              format = "mm/dd/yyyy"
            ), 

            # Four operational options - enrollment, referral, Brain/CSF status and Completion of biomarkers
            bs4Dash::menuItem(
              text = "Enrollment", 
              tabName = "enrollment_status"
            ), 

            # Referrals are fairly complex, uses two types of drop down menus
            bs4Dash::menuItem(
              text = "Referral Source", 
              tabName = "referral_output"
            ), 

            # Component enrollment
            bs4Dash::menuItem(
              text = "Component Enrollment Status", 
              tabName = "compon_status"
            ),

            # Component completion
            bs4Dash::menuItem(
              text = "Visit Component Completion", 
              tabName = "component_completion"
            ),

            # Biospecimen Inventories
            bs4Dash::menuItem(
              text = "Biospecimen Inventories", 
              tabName = "biospecimen_inventory"
            )

          )
        )
        
      ),
      
      # footer
      footer = bs4Dash::dashboardFooter(
        fixed = TRUE,
        left = htmltools::a(
          href = "https://www.uab.edu/",
          target = "_blank",
          htmltools::HTML(glue::glue("&copy; UAB {format(lubridate::today(), '%Y')}"))
        )
      )
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  #Resource path for CSS
  add_resource_path(
    'www', app_sys('app/www/')
  )
  
  #Resource path for Images (e.g. logo in header)
  add_resource_path(
    'img', system.file('app/img/', package = 'ADRCDash')
  )
  
  #Head tags including favicon
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ADRC Data Visualization'
    ),
    
    # Add here other external resources
    # Initialize use of waiter
    waiter::use_waiter(),
    
    # enable shinybrowser
    shinybrowser::detect()
  )
}
