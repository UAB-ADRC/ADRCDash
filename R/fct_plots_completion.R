#' From the family of function for plotting
#'  
#' This handles the plot of completed components
#' We also include a call for tables here since we essentially just replicate the df_plot call
#' 
#' @param df 
#' @param dict_entry 
#' @param start_dt 
#' @param end_dt 
#' @param study 
#' @param date_field 
#' @param date_adjust 
#' @param dict 
#' @param return_table 
#' @param use_echarts 
#'
#' @noRd
#' @return table, ggplot, or echart depending on argument
#' @author Chad Murchison
make_plot_completion <- function(
    df, 
    dict_entry,
    start_dt, 
    end_dt, 
    study,
    date_field = "a1_form_dt", 
    date_adjust = TRUE, 
    dict = data_dict_completion,
    return_table = FALSE,
    use_echarts = FALSE
  ){
  
  #Usual processing by the toggle and the date
  #This could probably be cast to a function but this is only the 2nd time its been used in this format

  #Select dataframe based on study
  df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
  
  #Adjust by reactive date if desired (controlled by boolean, defaults to FALSE for display purposes)
  if(date_adjust == TRUE) df <- start_end_date_filter(df, date_field, start_dt, end_dt)
  
  #Identify which dictionary entry to be using from data_dict_completion
  dict <- dict[[dict_entry]]
  
  #Return the NULL plot if needed
  if(nrow(df) == 0){
    # spin_update$hide()
    return(default_plot)
  } 
  
  #Run the final processing on the plot after date filtering
  df_plot <- completion_process_plot(df, dict)
  df_plot <- df_plot[!is.na(df_plot$Completers),]

  
  #At this stage we split between returning a plot or returning a table 
  
  #First, we build the plot
  if(return_table == FALSE){
  
    #Get the number of levels in the Completers variable to determine if a purple bar for "unable to complete" is needed
    color_idx <- seq_along(unique(df_plot$Completers))
    color_idx <- c(color_idx[-2], 2)
    bar_cols_curr <- RColorBrewer::brewer.pal(length(color_idx), "Dark2")[color_idx]
    
    
    if (isTRUE(use_echarts)){

      ec_plot <- df_plot |> 
          dplyr::left_join(tibble::tibble(
            Component = dict$label_col,
            full_label = dict$plot_name
          ),
          by = 'Component') |>
        dplyr::group_by(Completers) |> 
        echarts4r::e_chart(full_label) |> 
        echarts4r::e_bar(Perc, bind = N, stack = 'grp') |> 
        echarts4r::e_aria(
          enabled = TRUE, 
          decal = list(show = TRUE)
        ) |>
        echarts4r::e_theme("walden") |>
        echarts4r::e_labels(
          position = 'inside',
          fontWeight = 'bold',
          fontSize = 16,
          formatter = htmlwidgets::JS(
            # hide label if number is less than 10
          "function(params) {
            if(params.name < 10) {
              return '';
            } else {
              return 'N=' + params.name;
            }
          }"
          )) |> 
        echarts4r::e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.seriesName + 
                                        '</strong><br />Percent: ' + parseFloat(params.value[1] * 100).toFixed(0)+'%' + 
                                        '<br />N: ' +  params.name)   }  ")) |>
        echarts4r::e_x_axis(axisLabel = list(rotate = -40)) |> 
        echarts4r::e_y_axis(max = 1,
                            formatter = echarts4r::e_axis_formatter("percent", digits = 0)) |>
        echarts4r::e_axis_labels(x = 'Component', y = "Percent") |>
        # echarts4r::e_title(title_curr) |>
        # echarts4r::e_legend(bottom = 10) |>
        echarts4r::e_grid(left = "60px")
      
      return(ec_plot)
    }
      
    #Build the plot
    completion_plots <- 
      ggplot(data = df_plot) + 
    
      geom_bar(aes(x = Component, y = Perc, fill = Completers), stat="identity", position = position_fill(reverse = TRUE)) +
      geom_text(aes(x = Component, y = Perc, label = N, group = Completers), size = 10, position = position_stack_repel(vjust = 0.5, reverse = TRUE)) + 
    
      scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) + 
      scale_x_discrete(labels = dict[["plot_name"]]) + 
      scale_fill_manual(values = bar_cols_curr) + 
    
      gg_themes$completion
  
    return(completion_plots)
    
  
  #Otherwise we build a table
  } else {
    
    #Build the output string
    df_plot$string <- paste0(df_plot$N, " (", round(df_plot$Perc, 3), ")")
    
    #We also use the plot names from the dictionary
    df_plot$colnames <- dict[["plot_name"]][match(df_plot$Component, dict[["label_col"]])]
    
    #We get the 2D table for display by using dcast to pivot long-to-wide
    df_out <- data.table::dcast(df_plot[,c("Completers", "string", "colnames")], formula = Completers ~ colnames, value.var = "string")
    
    #Fill and NA's with 0
    df_out[is.na(df_out)] <- "-"
    colnames(df_out)[1] <- ""
    
    return(df_out)
  }
   
}  
