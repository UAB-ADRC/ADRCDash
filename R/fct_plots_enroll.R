
#' Create enrollment plot or table
#'
#' @param df_orig 
#' @param start_dt 
#' @param end_dt 
#' @param study 
#' @param date_adjust 
#' @param dict 
#' @param study_start_trunc_field 
#' @param use_echarts 
#'
#' @noRd
#' @return table, ggplot, or echart depending on argument
#' @author Chad Murchison, Joseph Marlo, \email{support@landeranalytics.com}
make_plot_enroll_flow <- function(df_orig, start_dt, end_dt, study, date_adjust = FALSE, dict = enroll_flow_dict,
                                  study_start_trunc_field = "screen_dt", use_echarts = FALSE){
  
  df <- df_orig
  
  #We now filter from the study choice dictionary instead of from a fixed list of arguments
  #Filter on the end date, use the alternate filter function (allows for NA's and uses filter by omission)
  #As a new update we're only adjusting start_dt_new instead
  
  #Identify the field we're referring to
  date_field <- study_choices[["date_field"]][which(study_choices[["name"]] == study)]
    
  #Right now the entry for date_field from the dictionary is only one, but we keep the for in case it ever becomes a vector
  for(ii in seq_along(date_field)){
    #Get the current study-specific date field
    ref_field <- date_field[ii]
      
    #Identify the max date based on date_adjust
    if(isTRUE(date_adjust)){
      start_dt_new = max(as.Date(start_dt), as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
    } else {
      start_dt_new = as.Date(start_dt)
    }
      
    #Call the filtering function
    df <- start_end_date_filter_alt(df, date_field = ref_field, start_date = start_dt_new, end_date = end_dt)
    
    #Finally, drop anyone missing a value for the current date field whose prescreen is outside the range
    trunc_id_drop <- which(is.na(df[[ref_field]]) & df[[study_start_trunc_field]] < as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
    if(length(trunc_id_drop) > 0){
      df <- df[-trunc_id_drop,]
    }
  }
  
  
  #Return the NULL plot if needed
  if(nrow(df) == 0){
    # spin_update$hide()
    return(default_plot)
  } 
  
  #Make a dummy factor of 1's
  df$dummy <- factor(1)
  
  #Start the spinner
  # spin_update$show()

  # calculate y limit maximum to use across all echarts plots
  y_limit_max <- purrr::map(as.character(dict[["var"]]), function(.var){
    p_data <- df[!is.na(df[[.var]]),] |> tibble::tibble()
    
    # count the data
    p_data <- p_data |> 
      dplyr::group_by(!!sym(.var)) |> 
      dplyr::tally() |> 
      tidyr::pivot_longer(-n)
    
    return(p_data$n)
  }) |> 
    unlist() |> 
    max()
    
  #Iterate over the flow variables in the enrollment dictionary to get a plot for each
  enroll_plots <- lapply(as.character(dict[["var"]]), function(.var){
      
      #Inclusion of study filtering check
      if(dict[["study_filter"]][which(dict[["var"]]==.var)] == TRUE) {
        #Select dataframe based on study
        df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
      }
      
      #Slice to get the first valid visit
      df <- data.table::as.data.table(df_slicer(df, slice_type = "first"))
      # print(as.data.frame(df[,colnames(df) %in% c("adc_sub_id", as.character(dict[["var"]]), as.character(study_choices[["var"]][c(1,3)]),"prescreen_dt", "a1_form_dt")]))
       
      #Plot processing for text annotations and the main title (from dictionary)
      counts_annot <- enroll_annotation(df[[.var]])
      title_curr <- paste0(dict[["title"]][which(dict[["var"]]==.var)], " N=", sum(counts_annot$Freq))
      
      #More processing for fill colors and labels
      fill_length <- max(length(levels(df[[.var]])), 3)
      fill_curr <- RColorBrewer::brewer.pal(fill_length, "Set2")
      labels_curr <- paste0(as.character(levels(df[[.var]])), "    ")  #Add some extra space for buffer
      
      #Now make the plot
      if (isTRUE(use_echarts)){
        p_data <- df[!is.na(df[[.var]]),] |> tibble::tibble()
        
        # count the data
        p_data <- p_data |> 
          dplyr::group_by(!!sym(.var)) |> 
          dplyr::tally() |> 
          tidyr::pivot_longer(-n)
        
        # echarts bar chart
        e_chart <- p_data |> 
          dplyr::group_by(value) |> 
          echarts4r::e_charts(
            name,
            height = "150px"
          ) |> 
          echarts4r::e_bar(n, stack = 'grp') |> 
          echarts4r::e_y_axis(max = y_limit_max) |> 
          echarts4r::e_flip_coords() |> 
          echarts4r::e_aria(
            enabled = TRUE, 
            decal = list(show = TRUE)
          ) |>
          echarts4r::e_theme("walden") |>
          echarts4r::e_labels(
            position = 'inside',
            fontWeight = 'bold',
            fontSize = 20
          ) |> 
          echarts4r::e_grid(top = "30") |> 
          echarts4r::e_tooltip() |>
          echarts4r::e_title(title_curr) |> 
          echarts4r::e_legend(right = 10) |> 
          echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green_dark)
        
        return(e_chart)
      } else {
        
        plot_curr <- ggplot() + 
          
          #The stacked bar
          geom_bar(data = df[!is.na(df[[.var]]),], aes_string(x = "dummy", fill = .var), position = position_stack(reverse = TRUE)) + 
          
          #The annotations
          geom_text(data = counts_annot, aes(x = 1, y = posit, label = Freq), size = 18) + coord_flip() + 
          
          #The scales
          ggtitle(label = title_curr) + 
          scale_y_continuous(breaks = seq(0, 500, 5)) + 
          scale_fill_manual(drop = FALSE, values = fill_curr, labels = labels_curr) + 
          gg_themes$enroll_flow
        
        return(plot_curr)
      }
      
    })
  
  if (isTRUE(use_echarts)) return(enroll_plots)
  
  #Pass the plots to gridExtra
  plot_rows <- length(enroll_plots)
  enroll_plots <- do.call(gridExtra::grid.arrange, c(enroll_plots, nrow = plot_rows))
  
  # spin_update$hide()
  # print(cbind(unique(df$adc_sub_id[df$enroll=="Currently Following" & !is.na(df$enroll)]), 1))
  return(enroll_plots)
}


#' Create enrollment rate plots for all
#'
#' @param df_orig 
#' @param use_echarts boolean
#'
#' @noRd
#' @return ggplot, or echart depending on argument
#' @author Chad Murchison, Joseph Marlo, \email{support@landeranalytics.com}
make_plot_enroll_rate_all <- function(df_orig, use_echarts){
  
  df <- df_orig
  
  #Start the spinner
  # spin_update$show()
  
  #Some renaming and labelling for plotly on the x-axis
  df$Date <- as.numeric(df$dt_plot)
  df[["Current Enrollment"]] <- df$count_all
  dt_labels = as.character(levels(df$dt_plot))
  dt_labels_axis <- label_subset(dt_labels, length_curr = 4)
  
  #Build the target enrollment df off the potentially truncated dataset
  df_tar <- build_target_enroll_df(df, .cols = "count_p30", .labs = "105 per year")
  
  #Some similar relabelling for the y-axis, finding the max between the target and current enrollment, and then building the sequence breaks
  max_count <- max(df[["Current Enrollment"]], df_tar[["Target Enrollment"]], 10)
  count_breaks <- ifelse(max_count <= 100, 10, 
                         ifelse(max_count <= 200, 20,
                                ifelse(max_count <= 500, 50,
                                       ifelse(max_count <= 1000, 100, 200))))
  
  if (isTRUE(use_echarts)){
    df$ideal <- df_tar[df_tar$Target != "50 per year",]$`Target Enrollment`
    df$date_label <- dt_labels
    ec_plot <- df |> 
      echarts4r::e_charts(x = date_label) |> 
      echarts4r::e_line_(
        sym('Current Enrollment'),
        name = 'Current Enrollment',
        color = colorspace::desaturate(ADRCDashHelper::color_palette$gold, amount = -0.5),
        symbolSize = 5
      ) |> 
      echarts4r::e_line(
        ideal,
        name = 'Target Enrollment (105 per yr)',
        color = ADRCDashHelper::color_palette$grey_dark, 
        showSymbol = FALSE, 
        symbolSize = 5,
        lineStyle = list(type = "dashed")
      ) |>
      echarts4r::e_aria(
        enabled = TRUE
        # decal = list(show = TRUE)
      ) |>
      echarts4r::e_theme("walden") |>
      echarts4r::e_tooltip(trigger = 'axis') |>
      echarts4r::e_axis_labels(y = "Participant Count") |>
      echarts4r::e_title('All Subject Enrollment') |>
      echarts4r::e_legend(bottom = 10) |>
      echarts4r::e_grid(left = "60px") |> 
      echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green_dark)
    
    # spin_update$hide()
    
    return(ec_plot)
  }
  
  rate_all <- ggplot() +
      geom_line(data = df, aes(x = Date, y = `Current Enrollment`), color = "black", size = 2) + 
      geom_line(data = df_tar[df_tar$Target != "50 per year",], aes(x = Date, y = `Target Enrollment`, color = Target, linetype = Target), size = 3) + 
    
      #scale_linetype_manual(values=c("dashed", "dotted"), labels=c("50 per year", "100 per year")) + 
      #scale_color_manual(values=c("red", "blue"), labels=c("50 per year", "100 per year")) + 
      scale_linetype_manual(values=c("dashed"), labels=c("105 per year")) + 
      scale_color_manual(values=c("red"), labels=c("105 per year")) + 
      
      ggtitle("All Subject Enrollment") + 
      scale_x_continuous(breaks = c(1:length(dt_labels)), labels = dt_labels_axis, name = "Date") + 
      scale_y_continuous(breaks = seq(0,10000,count_breaks), name = "Participant Count") + 
    
      gg_themes$enroll_rate + 
      guides(linetype = guide_legend(override.aes = list(alpha = 1, size = 3, linetype = "solid"))) 
  
  rate_all <- plotly::ggplotly(rate_all, tooltip = c("y", "x", "colour"))
  rate_all <- rate_all %>% plotly::layout(legend = list(x = 0.15, y = 0.85, xanchor = "left", yanchor = "top", font = list(size = 20)),
                                  hoverlabel = list(font = list(size = 18)),
                                  hoverdistance = 100)
  
  
  #Update the hovertext from plotly_build text directly, an issue since the x-axis is a numeric continuous and doesn't take the labels
  #This approach easier than trying to build a custom text object in the dataframes
  rate_all <- plotly::plotly_build(rate_all)
  for(ii in seq_along(rate_all$x$data)){
    rate_all$x$data[[ii]]$text <- 
      stringi::stri_replace_all_regex(rate_all$x$data[[ii]]$text, "^Date:(\\s+\\d+)", paste0("Date: ", dt_labels))
  }
  
  # spin_update$hide()
  
  return(rate_all)
}




#' Create enrollment plot for race
#'
#' @param df_orig 
#' @param use_echarts boolean
#'
#' @noRd
#' @return ggplot, or echart depending on argument
#' @author Chad Murchison, Joseph Marlo
make_plot_enroll_rate_race <- function(df_orig, use_echarts){
  
  df <- df_orig
  
  #Start the spinner
  # spin_update$show()
  
  #Some renamining and labelling for plotly
  #Doing rounding here since hoverformat doesn't work off ggplotly
  df$Date <- as.numeric(df$dt_plot)
  df[["Monthly Proportion"]] <- round(df$count_race_per_month_prop, 3)
  df[["Total Proportion"]] <- round(df$count_race_prop, 3)
  
  dt_labels = as.character(levels(df$dt_plot))
  dt_labels_axis <- label_subset(dt_labels, length_curr = 4)
  
  df2 <- data.frame(V1 = c(0.4))
  colnames(df2) <- "Target Rate"
  
  if (isTRUE(use_echarts)){
    
    # df$ideal <- df_tar[df_tar$Target != "100 per year",]$`Target Enrollment`
    df$date_label <- dt_labels
    df$target_rate <- df2$`Target Rate`
   
    ec_plot <- df |> 
      echarts4r::e_charts(x = date_label) |> 
      echarts4r::e_bar_(
        sym('Monthly Proportion'),
        name = 'Monthly Proportion',
        color = colorspace::lighten(ADRCDashHelper::color_palette$grey_light, amount = 0.5)
      ) |>
      echarts4r::e_line_(
        sym('Total Proportion'),
        name = 'Total Proportion',
        color = colorspace::desaturate(ADRCDashHelper::color_palette$gold, amount = -0.5),
        symbolSize = 5
      ) |> 
      echarts4r::e_line(
        target_rate,
        name = 'Target Rate',
        color = ADRCDashHelper::color_palette$grey_dark, 
        showSymbol = FALSE, 
        lineStyle = list(type = "dashed")
      ) |> 
      echarts4r::e_aria(
        enabled = TRUE
        # decal = list(show = TRUE)
      ) |>
      echarts4r::e_theme("walden") |>
      echarts4r::e_tooltip(trigger = 'axis') |>
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = 0)) |>
      echarts4r::e_axis_labels(y = "% AA Enrollment") |>
      echarts4r::e_title('Proportion African American Enrollment') |>
      echarts4r::e_legend(bottom = 10) |>
      echarts4r::e_grid(left = "60px") |> 
      echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green_dark)
    
    # spin_update$hide()
    
    return(ec_plot) 
  }
  
  rate_race <- ggplot(data=df) + 
    geom_bar(aes(x=Date, y=`Monthly Proportion`), stat="identity", alpha=0.5) + 
    geom_line(aes(x=Date, y=`Total Proportion`), color="black", size=3) + 
    geom_hline(data=df2, aes(yintercept=`Target Rate`), color="red", size=2, lty="dashed") + 
    ggtitle("Proportion African American Enrollment") + 
    scale_x_continuous(breaks=c(1:length(dt_labels)), labels=dt_labels_axis, name="Date") + 
    scale_y_continuous(breaks=seq(0,1,.1), name = "% African American Enrollment") + 
    gg_themes$enroll_rate
  
  
  rate_race <- plotly::ggplotly(rate_race)
  rate_race <- rate_race %>% plotly::layout(yaxis = list(hoverformat = ".3f"),
                                    hoverlabel = list(font = list(size = 18)),
                                    hoverdistance = 50)
  
  
  
  #Same label update for rate_race as seen in rate_all
  rate_race <- plotly::plotly_build(rate_race)
  for(ii in seq_along(rate_race$x$data)){
    rate_race$x$data[[ii]]$text <- 
      stringi::stri_replace_all_regex(rate_race$x$data[[ii]]$text, "^Date:(\\s+\\d+)", paste0("Date: ", dt_labels))
  }
  
  
  # spin_update$hide()
  return(rate_race)
}
