#' Create the explorer plot
#'  
#' This handles the main explorer plot
#' 
#' @param df data.frame
#' @param indvar_curr Current independent variable
#' @param depvar_curr Current dependent variable
#' @param group_curr Grouping variable
#' @param vis_type Type of visualization. One of c('Baseline', 'Most Recent', 'Longitudinal')
#' @param group_col Grouping column in dataframe. Defaults to 'adc_sub_id'
#' @param use_echarts TRUE/FALSE
#' 
#' @author Chad Murchison, Joseph Marlo, \email{support@landeranalytics.com}
#' @noRd
#' 
#' @return echarts or ggplot object
#' 
#' @examples 
#' # See app_server.R
make_plot_explorer <- function(df, indvar_curr, depvar_curr, group_curr, vis_type, group_col = "adc_sub_id", use_echarts){
  
  if(nrow(df) == 0 || is.null(df) || is.null(dim(df))) return(return_default_plot(use_echarts = use_echarts))

  ##
  #Initial Processing
  ##
  
  #Get the column names in the dataframe for indvar, group and non-count depvar
  indvar_plot <- back_ticker(indvar_curr)
  group_plot <- back_ticker(group_curr)
  
  #Check if depvar and invdar are the same
  if(depvar_curr == indvar_curr){
    df[[paste0(depvar_curr, "_dep")]] <- df[[depvar_curr]]
    depvar_curr <- paste0(depvar_curr, "_dep")
  }
  
  #Change the indvar_plot variable if longitudinal data is being used
  if(vis_type == "Longitudinal"){
    df_dt <- data.table::data.table(df)
    df_dt[, Visit := seq(.N), by = group_col]
    df[["Visit"]] <- df_dt$Visit
    indvar_curr <- "Visit"
    indvar_plot <- back_ticker(indvar_curr)
  }
  
  #Prep the x-axis and main title
  x_title <- expl_xvar_dict[["axis_name"]][which(expl_xvar_dict[["xvar"]]==indvar_curr)]
  title_curr <- expl_group_dict[["title_curr"]][which(expl_group_dict[["group_var"]]==group_curr)]
  title_curr <- ifelse(group_curr == indvar_curr, indvar_curr, title_curr)
  
  #Also prep the group colors for fill/color
  fill_length <- max(length(levels(df[[group_curr]])), 3)
  fill_curr <- RColorBrewer::brewer.pal(fill_length, "Dark2")
  
  #Filter dataframe to not have NA's in group or indvar
  df <- df[!is.na(df[[indvar_curr]]) & !is.na(df[[group_curr]]),]
  
  #Also drop all unused levels as needed
  .currs <- c(indvar_curr, depvar_curr, group_curr)
  for(ii in seq_along(.currs)){
    if(is.factor(df[[.currs[ii]]])) df[[.currs[ii]]] <- forcats::fct_drop(df[[.currs[ii]]])
  }
  
  #If indvar is numeric, recast to factor if five or less unique values, otherwise bin into 5 groups
  if(!is.factor(df[[indvar_curr]]) && indvar_curr != "Visit"){
    if(length(unique(df[[indvar_curr]])) < 6){ df[[indvar_curr]] <- factor(df[[indvar_curr]])
    } else df[[indvar_curr]] <- ggplot2::cut_number(df[[indvar_curr]], n=5)
  }
  
  
  ##
  #Contextual processing
  ##
  
  #Do some additional processing if the Count-based bar plots are run, mainly breaks on the y-axis
  if(depvar_curr=="Count") {
    
    #Process breaks
    n_curr <- nrow(df)
    round_curr <- n_curr * 0.05
    round_curr <- round(round_curr, -(nchar(trunc(round_curr))-1))
    if(round_curr < 1) round_curr <- 1
    breaks_ceil <- ceiling(n_curr*2)
    breaks_curr <- seq(0, breaks_ceil, round_curr)
    
    #Set the title
    y_title <- "Participant Counts"

        
  #Otherwise, do some processing for a continuous metric  
  } else {
    #Title and axis process
    df[[depvar_curr]] <- as.numeric(unclass(df[[depvar_curr]]))
    
    depvar_plot <- back_ticker(depvar_curr)
    y_title <- expl_yvar_dict[["axis name"]][which(expl_yvar_dict[["yvar"]]==depvar_curr)]
    
    #Set jitter_dodge and dodge positions
    posit_data <- position_jitterdodge(dodge.width=0.35, jitter.width=0.15, jitter.height=0)
    posit_group <- position_dodge(width=0.35)
  }
  
  
  ##
  #Make the plot
  ##
  
  #First the two cross-sectional designs
  
  #Make one plot if providing count summary
  if(depvar_curr == "Count"){
    
    #Adjust the x-axis for counts depending on whether longitudinal data is being used or not
    if(vis_type == "Longitudinal"){
      df[["Visit"]] <- factor(df[["Visit"]], levels = sort(unique(df[["Visit"]])), labels = paste0("Visit ", sort(unique(df[["Visit"]]))))
    } 
    

    # echarts -----------------------------------------------------------------
    
    if (isTRUE(use_echarts)){
      
      indvar_plot <- untick(indvar_plot)
      group_plot <- untick(group_plot)
      
      if (isTRUE(group_plot == indvar_plot)){
        # create echarts bar plot
        ec_plot <- plot_explorer_bar_ec(df, indvar_plot, title_curr)
      } else {
        # create grouped echarts bar plot
        ec_plot <- plot_explorer_bar_grouped_ec(df, indvar_plot, group_plot, title_curr)
      }

      return(ec_plot)
    }
    
    # -------------------------------------------------------------------------

    plot_explorer <- ggplot(data = df, aes_string(x = indvar_plot)) + 
      geom_bar(aes_string(fill = group_plot), position = position_dodge(), color = "black") + 
      scale_fill_manual(values = fill_curr, drop = FALSE) + 
      scale_y_continuous(breaks = breaks_curr, name = y_title) + 
      scale_x_discrete(name = x_title, drop = FALSE) + 
      ggtitle(title_curr) + 
      gg_themes$explorer
  
  # Or a plot for continuous outcomes - use mult=2 for +/- 1sd 
  } else if(vis_type != "Longitudinal") {
    
    df <- df[!is.na(df[[depvar_curr]]),]
    

    # echarts -----------------------------------------------------------------

    if (isTRUE(use_echarts)){
      
      indvar_plot <- untick(indvar_plot)
      group_plot <- untick(group_plot)
      depvar_plot <- untick(depvar_plot)
      
      if (isTRUE(indvar_plot == group_plot)){
        # create echarts jitter plot
        ec_plot <- plot_explorer_jitter_ec(df, indvar_plot, depvar_plot, title_curr)
      } else {
        # create grouped echarts jitter plot
        ec_plot <- plot_explorer_jitter_grouped_ec(df, indvar_plot, depvar_plot, group_plot, title_curr)
      }

      # spin_update$hide()
      
      return(ec_plot)
    }
    
    # -------------------------------------------------------------------------
    
    plot_explorer <- 
      ggplot(data=df, aes_string(x = indvar_plot, y = depvar_plot)) + 
      geom_point(aes_string(color = group_plot), position = posit_data, size=3) +
      stat_summary(aes_string(group = group_plot), fun = mean, geom = "point", shape = 45, size = 18, position = posit_group) + 
      #geom_errorbar(aes_string(group = group_plot), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), position = posit_group, width = 0.2, size = 1.25, alpha = 0.45, na.rm=TRUE) +
      
      scale_color_manual(values = fill_curr, drop = FALSE) + 
      scale_y_continuous(name = y_title) + 
      scale_x_discrete(name = x_title, drop = FALSE) + 
      
      ggtitle(title_curr) + 
      gg_themes$explorer
    
    
  # Finally we have the longitudinal set-up  
  } else{
    
    df <- df[!is.na(df[[depvar_curr]]),]

    if (isTRUE(use_echarts)){
      
      indvar_plot <- untick(indvar_plot)
      depvar_plot <- untick(depvar_plot)
      group_plot <- untick(group_plot)

      # echarts plot
      ec_plot <- plot_explorer_longitudinal_ec(df, indvar_plot, depvar_plot, group_plot, title_curr)
  
      return(ec_plot)
    }

    plot_explorer <- 
      ggplot(data=df, aes_string(x = indvar_plot, y = depvar_plot)) + 
      geom_point(aes_string(color = group_plot), position = posit_data, size=3) +
      #geom_errorbar(aes_string(group = group_plot), stat = "summary", fun.data = "mean_sdl", fun.args = list(mult = 1), position = posit_group, width = 0.2, size = 1.25, alpha = 0.45) + 
      
      stat_summary(aes_string(group = group_plot), fun = mean, fun.args = list(na.rm = TRUE), geom = "point", shape = 45, size = 18, position = posit_group) + 
      stat_summary(aes_string(group = group_plot, color = group_plot), fun = mean, fun.args = list(na.rm = TRUE), geom = "line", size = 1.5, alpha = 0.45, position = posit_group) + 
      
      scale_color_manual(values = fill_curr, drop = FALSE) + 
      scale_y_continuous(name = y_title) + 
      scale_x_continuous(name = x_title, breaks = c(1:max(df[["Visit"]])), labels = paste0("Visit ", c(1:max(df[["Visit"]])))) + 
      
      ggtitle(title_curr) + 
      gg_themes$explorer
  }
  
  
  #Cast the plot to a plotly object and drop the legend if desired
  plot_explorer <- plotly::ggplotly(plot_explorer)
  plot_explorer <- plotly::layout(plot_explorer, legend = list(x=0.0, y=-.15, orientation = 'h', xanchor="left", yanchor="top", font=list(size=20)),
                                            hoverlabel = list(font=list(size=18)))
  
  #Catch to replace NA in stat_summary calls with actual measures from y, uses stringi to do vectorized replacement
  for(ii in seq_along(plot_explorer$x$data)){
    if(length(grep("NA$", plot_explorer$x$data[[ii]]$text)) > 0){
      .decimal_curr <- do.call(max, lapply(df[[depvar_curr]], .decimals)) + 1
      plot_explorer$x$data[[ii]]$text<- 
      stringi::stri_replace_all_regex(plot_explorer$x$data[[ii]]$text,
                                      paste0("(",depvar_curr, ":\\s+).*$"),
                                      paste0("$1", round(plot_explorer$x$data[[ii]]$y, .decimal_curr)))
    }
  }
  
  
  # #plot_explorer <- plot_explorer %>% layout(showlegend = FALSE)
  # htmlwidgets::saveWidget(plot_explorer, "index.html")
  
  #Hide the spinner and return the plot
  # spin_update$hide()
  return(plot_explorer)
}


#' Internal echarts plotting function used within plot_explorer
#'
#' @param df a dataframe
#' @param indvar_plot Independent variable
#' @param group_plot Grouping variable
#' @param title_curr Title of plot
#' 
#' @author Joseph Marlo, \email{support@landeranalytics.com}
#' @noRd
#'
#' @return echarts
#' @describeIn plot_explorer_bar_ec Bar chart
plot_explorer_bar_ec <- function(df, indvar_plot, title_curr){
  
  # create counts of the data
  counts_for_plot <- df |> 
    dplyr::group_by(!!dplyr::sym(indvar_plot)) |> 
    dplyr::tally()

  # create echart
  ec_plot <- counts_for_plot |>
    echarts4r::e_charts_(indvar_plot) |>
    echarts4r::e_bar(n) |> 
    echarts4r::e_aria(
      enabled = TRUE, 
      decal = list(show = TRUE)
    ) |>
    echarts4r::e_theme("walden") |>
    echarts4r::e_tooltip('axis') |>
    echarts4r::e_x_axis(axisLabel = list(rotate = -20)) |> 
    echarts4r::e_axis_labels(y = "Participant Count") |>
    echarts4r::e_title(title_curr) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_grid(left = "60px") |> 
    echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green)
  
  return(ec_plot)
}

#' @noRd
#' @describeIn plot_explorer_bar_ec Grouped bar chart
plot_explorer_bar_grouped_ec <- function(df, indvar_plot, group_plot, title_curr){
  
  # create counts of the data
  counts_for_plot <- df |> 
    dplyr::group_by(!!dplyr::sym(indvar_plot), 
                    !!dplyr::sym(group_plot)) |> 
    dplyr::tally()
  
  # make implicit missing combinations explicit and fill with 0s
  counts_for_plot <- counts_for_plot |> 
    dplyr::ungroup() |> 
    tidyr::complete(!!dplyr::sym(indvar_plot), !!dplyr::sym(group_plot), fill = list(n = 0))
    
  
  # create echart
  ec_plot <- counts_for_plot |>
    dplyr::group_by(!!dplyr::sym(group_plot),
                    !!dplyr::sym(indvar_plot)) |> 
    echarts4r::e_charts_(indvar_plot, fill = group_plot) |>
    echarts4r::e_bar(n, barGap = '-70%') |> 
    echarts4r::e_aria(
      enabled = TRUE, 
      decal = list(show = TRUE)
    ) |>
    echarts4r::e_theme("walden") |>
    echarts4r::e_tooltip('axis') |>
    echarts4r::e_x_axis(axisLabel = list(rotate = -20)) |> 
    echarts4r::e_axis_labels(y = "Participant Count") |>
    echarts4r::e_title(title_curr) |>
    echarts4r::e_legend(right = 10) |>
    echarts4r::e_grid(left = "60px") |> 
    echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green)
  
  return(ec_plot)
}

#' @noRd
#' @describeIn plot_explorer_bar_ec Jitter plot
plot_explorer_jitter_ec <- function(df, indvar_plot, depvar_plot, title_curr){

  # create tooltip that shows the mean
  tooltip <- htmlwidgets::JS(
    "function(params){
    
              // get mean and point values
              let seriesMean = (Math.round(params[1].value[1]*10)/10).toFixed(1);
              let value = (Math.round(params[0].value[1]*10)/10).toFixed(1);
              
              // get and parse the upper and lower error bar values
              let errors;
              try {
                errors = params[1].data.name.split(',');
              } catch (e) {
                errors = ['', ''];
              }
              let errorMax = errors[0];
              let errorMin = errors[1];
              
              // finalize string to show to user
              let myLabel = '<strong>' + params[0].seriesName + 
                            '</strong><br>+1 SD: ' + errorMax +
                            '<br>Mean: ' + seriesMean +
                            '<br>-1 SD: ' + errorMin +
                            '<br>' +
                            '<br>Value: ' + value
              return(myLabel)   }"
  )
  
  # create new numeric x axis so we can jitter it
  if (!isTRUE(is.factor(df[[indvar_plot]]))) cli::cli_abort('Independent variable must be a factor')
  df$jittered_x <- jitter(as.numeric(df[[indvar_plot]]))
  
  # create x-axis formatter that turns numerics into labels from the pre-jittered variable
  label_array <- paste0('"', levels(df[[indvar_plot]]), '"', collapse = ', ')
  x_axis_formatter <- htmlwidgets::JS(
    glue::glue(
      .open = "<",
      .close = ">",
      "function(index){ return([<label_array>][index-1])}"
    )
  )
  
  # prep the scatter data
  ec_data <- df |>
    dplyr::group_by(!!dplyr::sym(indvar_plot)) |> 
    dplyr::mutate(
      mean = mean(!!dplyr::sym(depvar_plot)),
      error_max = mean + sd(!!dplyr::sym(depvar_plot)),
      error_min = mean - sd(!!dplyr::sym(depvar_plot)),
      error_combined = paste0(round(error_max, 1), ",", round(error_min, 1))
    ) |> 
    dplyr::select(
      !!dplyr::sym(indvar_plot), 
      !!dplyr::sym(depvar_plot), 
      mean, 
      jittered_x, 
      error_max, 
      error_min,
      error_combined
    )
  
  # prep data for the error bars
  ec_error_data <- ec_data |> 
    dplyr::ungroup() |> 
    dplyr::distinct(!!dplyr::sym(indvar_plot), error_max, error_min) |> 
    dplyr::arrange(!!dplyr::sym(indvar_plot)) |> 
    dplyr::mutate(jittered_x = dplyr::row_number()) |> 
    tidyr::pivot_longer(c(error_max, error_min)) |> 
    dplyr::group_by(!!dplyr::sym(indvar_plot))
  
  # create echart
  ec_plot <- ec_data |> 
    echarts4r::e_charts(jittered_x) |> 
    echarts4r::e_scatter_(
      depvar_plot,
      symbol_size = 9,
      itemStyle = list(opacity = 0.4)
    ) |>
    echarts4r::e_line(
      mean,
      bind = error_combined, 
      symbol = 'none',
      lineStyle = list(width = 7)
    ) |>
    echarts4r::e_data(data = ec_error_data) |> 
    echarts4r::e_line(value, symbol = 'rect') |> 
    echarts4r::e_aria(
      enabled = TRUE, 
      decal = list(show = TRUE)
    ) |>
    echarts4r::e_theme("walden") |> 
    echarts4r::e_tooltip('axis', formatter = tooltip) |>
    echarts4r::e_x_axis(
      axisLabel = list(rotate = -20),
      formatter = x_axis_formatter
    ) |>
    echarts4r::e_y_axis(min = y_min(df[[depvar_plot]])) |>
    echarts4r::e_axis_labels(y = depvar_plot) |>
    echarts4r::e_title(title_curr) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_grid(left = "60px") |> 
    echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green)
  
  return(ec_plot)
}

#' @noRd
#' @describeIn plot_explorer_bar_ec Groupped jitter plot
plot_explorer_jitter_grouped_ec <- function(df, indvar_plot, depvar_plot, group_plot, title_curr){
  
  # create new numeric x axis so we can jitter it
  if (!isTRUE(is.factor(df[[indvar_plot]]))) cli::cli_abort('Independent variable must be a factor')
  between_subgroup_separation <- 0.15
  within_group_jitter <- 0.05
  scale_between <- function(x, min_val = -between_subgroup_separation, max_val = between_subgroup_separation) {
    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)
    scaled_x <- min_val + (max_val - min_val) * ((x - x_min) / (x_max - x_min))
    return(scaled_x)
  }

  # for each subgroup, shift it within its parent group and apply jitter
  df <- df |> 
    dplyr::select(
      !!dplyr::sym(indvar_plot),
      !!dplyr::sym(group_plot),
      !!dplyr::sym(depvar_plot),
    ) |> 
    dplyr::mutate(raw_x = as.numeric(!!dplyr::sym(indvar_plot))) |> 
    dplyr::group_by(!!dplyr::sym(group_plot)) |> 
    dplyr::mutate(subgroup = dplyr::cur_group_id()) |> 
    dplyr::group_by(!!dplyr::sym(indvar_plot)) |> 
    dplyr::mutate(
      jittered_x = jitter(raw_x + scale_between(subgroup), amount = within_group_jitter)
    ) |>
    dplyr::select(-raw_x, -subgroup) |> 
    dplyr::ungroup()

  # create x-axis formatter that turns numerics into labels from the pre-jittered variable
  label_array <- paste0('"', levels(df[[indvar_plot]]), '"', collapse = ', ')
  x_axis_formatter <- htmlwidgets::JS(
    glue::glue(
      .open = "<",
      .close = ">",
      "function(index){ return([<label_array>][index-1])}"
    )
  )
  
  # tooltip
  tooltip <- htmlwidgets::JS(
    "function(params){ tmp = params;
    
        function replaceNaN(x) {
          return isNaN(x) ? '' : x
        }
        function calculateMean(arr) {
          const sum = arr.reduce((acc, val) => acc + val, 0);
          return sum / arr.length;
        }
    
        let value = (Math.round(params[0].value[1]*10)/10).toFixed(1);
        let name = params[0].seriesName
        
        // get and parse the upper and lower error bar values
        let errors;
        try {
          errors = params[0].data.name.split(',');
        } catch (e) {
            errors = ['', ''];
        }
        let errorMax = (Math.round(errors[0]*10)/10).toFixed(1);
        let errorMin = (Math.round(errors[1]*10)/10).toFixed(1);
        
        // get the mean; this only works b/c +/- SD is symmetrical
        let seriesMean = calculateMean([+errorMin, +errorMax])
        seriesMean = (Math.round(seriesMean*10)/10).toFixed(1);
        
        // catch NaN values
        value = replaceNaN(value)
        errorMax = replaceNaN(errorMax)
        errorMin = replaceNaN(errorMin)
        seriesMean = replaceNaN(seriesMean)
        
        // finalize string to show to user
        let myLabel = '<strong>' + name + 
                      '</strong><br>+1 SD: ' + errorMax +
                      '<br>Mean: ' + seriesMean +
                      '<br>-1 SD: ' + errorMin +
                      '<br>' +
                      '<br>Value: ' + value
    return(myLabel)   }"
  )

  # prep the scatter data
  ec_data <- df |>
    dplyr::group_by(!!dplyr::sym(group_plot),
                    !!dplyr::sym(indvar_plot)) |>
    dplyr::mutate(
      mean = mean(!!dplyr::sym(depvar_plot)),
      error_max = mean + sd(!!dplyr::sym(depvar_plot)),
      error_min = mean - sd(!!dplyr::sym(depvar_plot)),
      error_combined = paste0(round(error_max, 1), ",", round(error_min, 1))
    ) |> 
    dplyr::select(
      !!dplyr::sym(group_plot),
      !!dplyr::sym(indvar_plot),
      !!dplyr::sym(depvar_plot),
      jittered_x,
      mean,
      error_max, 
      error_min,
      error_combined
    )
  
  # prep data for the error bars
  ec_error_data <- ec_data |> 
    dplyr::ungroup() |> 
    dplyr::group_by(!!dplyr::sym(indvar_plot), !!dplyr::sym(group_plot)) |> 
    dplyr::mutate(jittered_x = mean(jittered_x)) |> 
    dplyr::distinct(!!dplyr::sym(indvar_plot), !!dplyr::sym(group_plot), jittered_x, error_max, error_min) |> 
    tidyr::pivot_longer(c(error_max, error_min)) |> 
    dplyr::group_by(!!dplyr::sym(group_plot), 
                    !!dplyr::sym(indvar_plot)) |> 
    dplyr::mutate(mean = mean(value))
  
  # make the plot
  ec_plot <- ec_data |> 
    echarts4r::e_charts(jittered_x) |> 
    echarts4r::e_scatter_(
      depvar_plot, 
      bind = 'error_combined',
      symbol_size = 9,
      itemStyle = list(opacity = 0.4)
    ) |> 
    echarts4r::e_data(data = ec_error_data) |> 
    echarts4r::e_line(value, symbol = 'rect') |> 
    echarts4r::e_line(mean) |> 
    echarts4r::e_aria(
      enabled = TRUE, 
      decal = list(show = TRUE)
    ) |>
    echarts4r::e_theme("walden") |> 
    echarts4r::e_tooltip(trigger = 'axis', formatter = tooltip) |>
    echarts4r::e_x_axis(
      axisLabel = list(rotate = -20),
      formatter = x_axis_formatter
    ) |>
    echarts4r::e_y_axis(min = y_min(df[[depvar_plot]])) |>
    echarts4r::e_axis_labels(y = depvar_plot) |>
    echarts4r::e_title(title_curr) |>
    echarts4r::e_legend(right = 10) |>
    echarts4r::e_grid(left = "60px") |> 
    echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green)
  
  return(ec_plot)
}

#' @noRd
#' @describeIn plot_explorer_bar_ec Longitudinal scatter plot
plot_explorer_longitudinal_ec <- function(df, indvar_plot, depvar_plot, group_plot, title_curr){

  # create new numeric x axis so we can jitter it
  df$jittered_x <- jitter(as.numeric(df[[indvar_plot]]))
  
  # create tooltip that shows the mean
  tooltip <- htmlwidgets::JS(
    "
    function(params){
      let seriesMean = Math.round(params[1].value[1]*10)/10;
      let myLabel = '<strong>' + params[0].seriesName + 
                    '</strong><br>Mean: ' + seriesMean +
                    '<br>Value: ' + params[0].value[1]
      return(myLabel)
    }"
  )
  
  # create x-axis formatter that turns numerics into labels from the pre-jittered variable
  label_array <- paste0('"', 'Visit ', sort(unique(df[[indvar_plot]])), '"', collapse = ', ')
  x_axis_formatter <- htmlwidgets::JS(
    glue::glue(
      .open = "<",
      .close = ">",
      "function(index){ return([<label_array>][index-1])}"
    )
  )
  
  # create echart
  ec_plot <- df |>
    dplyr::group_by(!!dplyr::sym(group_plot),
                    !!dplyr::sym(indvar_plot)) |>
    dplyr::mutate(mean = mean(!!dplyr::sym(depvar_plot))) |>
    dplyr::group_by(!!dplyr::sym(group_plot)) |>
    echarts4r::e_charts(jittered_x) |>
    echarts4r::e_scatter_(depvar_plot,
                          symbol_size = 9,
                          itemStyle = list(opacity = 0.6)) |>
    echarts4r::e_line(mean,
                      symbol = 'none',
                      lineStyle = list(width = 7)) |>
    echarts4r::e_aria(
      enabled = TRUE, 
      decal = list(show = TRUE)
    ) |>
    echarts4r::e_theme("walden") |>
    echarts4r::e_tooltip('axis', formatter = tooltip) |>
    echarts4r::e_x_axis(
      axisLabel = list(rotate = -20),
      formatter = x_axis_formatter
    ) |>
    echarts4r::e_y_axis(min = y_min(df[[depvar_plot]])) |>
    echarts4r::e_axis_labels(y = depvar_plot) |>
    echarts4r::e_title(title_curr) |>
    echarts4r::e_legend(right = 10) |>
    echarts4r::e_grid(left = "60px") |>
    echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green)
  
  return(ec_plot)
}

#' @noRd
#' @describeIn plot_explorer_bar_ec Calculate y axis min with buffer for plotting
y_min <- function(x) {
  ymin <- min(x)
  if (ymin > 0) ymin <- ymin * 0.9
  if (ymin == 0) ymin <- -0.25
  if (ymin < 0) ymin <- ymin * 1.1
  ymin <- round(ymin * 10) / 10
  return(ymin)
}
