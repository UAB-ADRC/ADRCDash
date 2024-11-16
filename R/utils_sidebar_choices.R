#' These are the choices used in the sidebar of the visualization dash
#' 
#' 
#' @noRd

#Data Exploration Choices
group_choices <- c("None", "AD Syndromal Stage", "Race", "Sex", "Race and Sex")
xvar_choices <- c("AD Numeric Stage", "AD Syndromal Stage", "Global CDR", "Race")
yvar_choices <- c("Count", "Age", "Education", "Global CDR")
visit_choices <- c("Baseline", "Most Recent", "Longitudinal")

#Operational Dash Choices
min_date_all <- "2018-01-01"
min_date_trunc <- "2020-01-01"

refer_choices = c("Clinician Referral", "Community Outreach", "Study Referral", 
                  "Self-Referral", "Prior ADC Subject", "Unknown Source")

#Some sets used for component enrollment
compon_choices = c("Neuroimaging", "CSF / Lumbar", "Brain Donation Interest")
compon_group_choices = c("Race", "Sex", "AD Syndromal Stage")

#Selection of visits
complete_visits_select = c("Most Recent Visit", "All Visits")

#Selection of marginals for inventory
inventory_group_select <- c("Race", "Sex", "AD Syndromal Stage")


#Axis label dictionary
expl_xvar_dict <- list(xvar = c("AD Numeric Stage", "AD Syndromal Stage", "Global CDR", "Race", "Visit"),
                       axis_name = c("Numeric Staging", "Syndromal Staging", "Global CDR", "Race", "Visit"))

expl_yvar_dict <- list(yvar = c("Age", "Global CDR"),
                       axis_name = c("Age", "Global CDR"))


expl_group_dict <- list(group_var = c("AD Syndromal Stage", "Race", "Sex", "Race and Sex"))
expl_group_dict[["title_curr"]] <- paste("Grouped by", expl_group_dict[["group_var"]])
                        


#Default plot

text_default <- paste("Parameter set out of bounds\n",
                      "Please reset and try again")

default_plot <- 
  ggplot2::ggplot() + 
  ggplot2::annotate("text", x = 4, y = 25, size = 8, label = text_default) + 
  ggplot2::theme_void()

default_plotly <- plotly::ggplotly(default_plot)

default_echarts <- data.frame(text = 'test', x = 2, y = 2) |> 
  echarts4r::e_charts(x) |> 
  echarts4r::e_title(text_default)

default_table <- data.frame(V1 = text_default); colnames(default_table) <- ""