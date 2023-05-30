#' From the family of function for plotting
#'  
#' This handles the brain donation and csf plots
#' Could be further generalized to handle any of the lists from dual_var_table_maker
#' 
#' @param df 
#' @param dict 
#'
#' @noRd
#' @return ggplot
#' @author Chad Murchison
make_plot_compon <- function(df, dict = data_dict_component){
  
  # spin_update$show()
  
  #Return the NULL plot if needed
  if(ggplot2::is.ggplot(df) || is.null(df)) {
    # spin_update$hide()
    return(default_plot)
  }
  
  
  #Iterate over each dataframe in the passed list
  compon_plots <- lapply(df, function(.df){
      
      #Do some filtering, the dictionary indicates which rows to drop
      #On the off chance a row isn't dropped,
      .row_filter <- grep(dict[["row_filter"]], rownames(.df))
      if(length(.row_filter) > 0) .df <- .df[-.row_filter,]
      
      #We don't worry about columns though since there's always a _prop string
      .df <- .df[, -grep("_prop", colnames(.df))]
      
      #Pivot to a long data set so ggplot can accurately plot
      .df <- tidyr::pivot_longer(.df, cols = !grouping)
      
      #Set the breaks on the counts
      y_breaks <- seq(0, 500, 5)
      if(max(.df$value) < 11) y_breaks <- seq(0, 100, 2)
      
      #Prep the fill colors
      fill_length <- max(length(unique(.df[["grouping"]])), 3)
      fill_curr <- RColorBrewer::brewer.pal(fill_length, "Set2")
      
      #Orient name appropriately
      .df$name <- factor(.df$name, levels=unique(.df$name), labels=unique(.df$name))
      
      #Make the current plot
      plot_curr <- ggplot2::ggplot(data = .df, ggplot2::aes(x = name, y = value)) + 
        ggplot2::geom_bar(
          ggplot2::aes(fill = grouping), 
          color = "black", 
          stat = "identity", 
          position = ggplot2::position_dodge()
        ) + 
        ggplot2::scale_y_continuous(breaks = y_breaks) + 
        ggplot2::scale_x_discrete() + 
        ggplot2::scale_fill_manual(
          values = fill_curr, 
          labels = unique(.df$grouping)
        ) +
        gg_themes$compon + 
        ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(size=2, linetype=0)))
      
      return(plot_curr)
    })
  
  #Pass the plots to gridExtra
  plot_cols <- length(compon_plots)
  compon_plots <- do.call(gridExtra::grid.arrange, c(compon_plots, ncol = plot_cols))
  
  # spin_update$hide()
  
  return(compon_plots)
}

#' Create the echarts component enrollment plots 
#'
#' @param df 
#' @param dict 
#' 
#' @author Chad Murchison, \email{cfmurch@uab.edu}
#'
#' @return html containing the echarts output
#' @noRd
make_plot_compon_ec <- function(df, dict = data_dict_component){

  # extract titles
  plot_titles <- attributes(df)$groups
    
  # function to create a sigle bar chart
  make_bar_chart <- function(.data, title_text){
    # prep the data
    df_prepped <- plot_compon_prep_data(.data, dict = dict)
    
    # set title
    plot_title <- glue::glue("Enrollment Group: {title_text}")
    
    # create the single bar chart
    ec_plot <- df_prepped |> 
      tibble::tibble() |> 
      tidyr::pivot_longer(!grouping) |> 
      dplyr::group_by(grouping) |> 
      echarts4r::e_charts(name) |> 
      echarts4r::e_bar(value) |> 
      echarts4r::e_aria(
        enabled = TRUE, 
        decal = list(show = TRUE)
      ) |>
      echarts4r::e_theme("walden") |>
      echarts4r::e_tooltip('axis') |> 
      echarts4r::e_axis_labels(y = "Participant Count") |>
      echarts4r::e_title(plot_title) |>
      echarts4r::e_legend(right = 10) |>
      echarts4r::e_grid(left = "60px") |> 
      echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green) |> 
      echarts4r::e_group("component_enroll") |> # this enables grouping of tooltip across plots
      echarts4r::e_connect_group("component_enroll") # this enables grouping of tooltip across plots
    
    html_out <- htmltools::tagList(ec_plot, htmltools::br())
    
    return(html_out)
  }
  
  # make all the plots
  ec_plots <- purrr::map2(df, plot_titles, make_bar_chart)
  
  return(ec_plots)
}

#' @noRd
#' @describeIn make_plot_compon_ec Prep the data for plotting
plot_compon_prep_data <- function(df, dict = data_dict_completion){
  # Do some filtering, the dictionary indicates which rows to drop
  # On the off chance a row isn't dropped,
  .row_filter <- grep(dict[["row_filter"]], rownames(df))
  if(length(.row_filter) > 0) df <- df[-.row_filter,]
  
  #We don't worry about columns though since there's always a _prop string
  df <- df[, -grep("_prop", colnames(df))]
  
  return(df)
}
