#' The various spinners and functions used to indicate loading
#' 
#' Deprecated. Use spin_facing_circles and attach a message. Uses <span> to style the message font nested inside a <div> to set the size
#' 
#' @noRd
#' @return html
#' @author Chad Murchison
spinner_fade_w_message <- function(message){
  tagList(
    waiter::spin_fading_circles(),
    div(
      span(
        message, 
        style = "color: white; background: #144B39; font-size: 36px; padding: 2em; border-radius: 0.1em;"
      ), 
      style = "width=200px; margin: auto"
    )
  )
}

#' @noRd
#' @describeIn spinner_fade_w_message Deprecated. An example spinner used when REDCap first loads 
spin_redcap <- waiter::Waiter$new(html = spinner_fade_w_message("Loading Visits from REDCap"))

#' @noRd
#' @describeIn spinner_fade_w_message Deprecated. Spinner for plots and tables.
spin_update <- waiter::Waiter$new(
  html = spinner_fade_w_message("Updating Plots and Tables"),
  id=c("plot_curr_explorer", "table_curr_explorer", "enroll_flow_plot", "enroll_rate_plot_all", "enroll_rate_plot_race",
       "plot_refer", "table_refer_full", "table_refer_by_yr","compon_plot", "compon_table", "component_plot"))
