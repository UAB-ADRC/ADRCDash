#' From the family of function for plotting
#'  
#' 
#' This handles the main referal plot as well as the aggregate table
#' 
#' Table making only requires a small call so it's rolled in to the post-processing
#' 
#' Note it is very similar in structure to the general bar plots in `make_plot_explorer`
#' 
#' Main change being that indvar_curr will always be "Referral Source" / "prescreen_refer"
#'  and depvar_curr will always be "count"
#'  
#' Primary adaptation is the inclusion of stacking to distinguish between enrollment
#'  before and after funding; this is accomplished by using facets on the plot
#' 
#' Includes additional function to allow for assessment of participants who have declined
#' This requires using different `group_curr` names though
#' 
#' @param df 
#' @param source_curr 
#' @param group_curr 
#' @param study 
#' @param start_dt 
#' @param end_dt 
#' @param make_plot 
#' @param date_col 
#' @param restrict_on_study 
#' @param date_adjust 
#' @param dict 
#' @param use_echarts boolean
#'
#' @author Chad Murchison
#' @noRd
#' 
#' @return list of tabset panels
make_object_refer <- function(
    df, 
    source_curr, 
    group_curr, 
    study, 
    start_dt, 
    end_dt,
    make_plot, #Toggle to determine if table or plot is made
    date_col = "refer_dt",
    restrict_on_study = FALSE, 
    date_adjust = TRUE,
    dict = refer_details_dict,
    use_echarts = FALSE
  ){
  
  ##
  #Date thresholding and processing of P20 participants
  ##
  
  #Start the spinner
  # spin_update$show()
  
  #Return the NULL plot if needed
  if(length(source_curr) == 0){
	  # spin_update$hide()
	  return(return_default_plot())
  }
  
  #We don't consider anyone with a ".bad" contact_status entry i.e. anyone we've been unable to contact
  df <- df[!is.na(df$status) & df$status != status_dict[[".bad"]],]
  
  #Adds a catch to determine whether we recast the pre-P20 participants or not
  #In either case, rename the main referral_type to "Referral Source"
  if(study == "Full UAB ADRC Cohort"){ colnames(df)[colnames(df) == "refer_type"] <- "Referral Source"
  
  #For pre-P20 we use the recast referral
  } else {
    colnames(df)[colnames(df) == "refer_type_p20"] <- "Referral Source"
    
    #We also shift early referral dates to the study start date just so we don't drop them
    date_thresh <- study_choices[["start_dt"]][which(study_choices[["name"]] == study)]
    
    df[[date_col]][df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]]==1 & 
                     !is.na(df[[date_col]]) &
                   df[[date_col]] < as.Date(date_thresh)] <- date_thresh
  }
  
  
  #Initialize devpar_curr since it's static for the referral plot
  depvar_curr <- "Count"
  
  #Build out the main referral types that will be iterated through
  df <- df[df[["Referral Source"]] %in% source_curr,]
  df[["Referral Source"]] <- forcats::fct_drop(df[["Referral Source"]])
  indvar_curr_set <- c("type", names(dict[["sub_type"]][["plot_source_map"]][which(dict[["sub_type"]][["plot_source_map"]] %in% source_curr)]))
  
  #We use the study variable for splitting the stack to get enrolled and not-enrolled
  stack_split <- study_choices[["var"]][which(study_choices[["name"]] == study)]
  df[[stack_split]] <- factor(df[[stack_split]], levels=c(0,1), labels = c("Not Enrolled", "Enrolled"))
  
  
  #Finally filter based on the date field
  #If we're restricting to the study window, take the max of the input$start_dt and the start date of whatever study we're considering
  if(restrict_on_study == TRUE)  start_dt <- max(as.Date(start_dt), as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
  if(date_adjust == TRUE) df <- start_end_date_filter(df, date_col, start_dt, end_dt)
  
  
  ##
  #Iterating through the referral sets for both tables and plots
  ##
  refer_viz_out <- lapply(indvar_curr_set, function(.indvar){
    
    #First run data frame processing that is needed for both plots and tables
    
    #Get the column names in the dataframe for indvar used by both tables and plots
    indvar_curr <- dict[[.indvar]][["field"]]
    if(.indvar == "type") indvar_curr <- "Referral Source"
    
    #Filter dataframe to not have NA's in group or indvar, also drop levels as needed
    df <- df[!is.na(df[[indvar_curr]]) & !is.na(df[[group_curr]]),]
    df[[indvar_curr]] <- forcats::fct_drop(df[[indvar_curr]])
    df[[group_curr]] <- forcats::fct_drop(df[[group_curr]])
    
    
    #If making the summary table...
    if(make_plot == FALSE){
      
      #Make one table for Enrolled, another for not Enrolled
      table_set <- rev(levels(df[[stack_split]]))
        
      tables_out <- lapply(table_set, function(.set){
        #Filter the table
        df <- df[df[[stack_split]] == .set,]
        
        #Final catch to return the NULL table if needed
        if(nrow(df) == 0) return(default_table)
        
        #Otherwise call the appropriate table maker function
        tab_refer <- count_table_maker(df, indvar_curr, group_curr)
        #Rename the column name for outreach as necessary
        colnames(tab_refer)[1] <- dict[[.indvar]][["plot_field"]]
        return(tab_refer)
        
      })
      
      #Build the tables into a tab set
      names(tables_out) <- table_set
      
      # Build out the tabesetPanel
      .tabs <- purrr::map(names(tables_out), ~tabPanel(
          .x,
          reactable::reactable(
            tables_out[[.x]],
            rownames = FALSE,
            highlight = TRUE,
            minRows = 7
          )
        )
      )
      do.call(bs4Dash::tabsetPanel, append(.tabs, list(type="tabs")))

    #Otherwise we make the primary plotting object  
    } else{
      
      ##
      #Variable and title processing
      ##
  
      #Prep the backticked names for plotting
      indvar_plot <- back_ticker(indvar_curr)
      group_plot <- back_ticker(group_curr)
      stack_split_plot <- back_ticker(stack_split)
  
      #Prep the x-axis and main title
      x_title <- expl_xvar_dict[["axis_name"]][which(expl_xvar_dict[["xvar"]]==indvar_curr)]
      title_curr <- expl_group_dict[["title_curr"]][which(expl_group_dict[["group_var"]]==group_curr)]
  
      if (isTRUE(use_echarts)){

        html_grid <- make_plot_refer_ec(
          df,
          indvar_plot = indvar_plot,
          group_curr = group_curr,
          stack_split_plot = stack_split_plot
        )
        
        return(html_grid)
      } else {
       
        #Also prep the group colors for fill/color
        fill_length <- max(length(levels(df[[group_curr]])), 3)
        fill_curr <- RColorBrewer::brewer.pal(fill_length, "Set1")
        #In case we ever want to use a different color set, can check the old plot_labels_dict for an example
        #fill_curr <- RColorBrewer::brewer.pal(fill_length, plot_labels_dict[["colorset"]][which(plot_labels_dict[["declined"]]==declined_subj)])
        
        #Another catch to return the NULL plot if needed
        if(nrow(df) == 0) return(return_default_plot())
        
        ##
        #Plot Processing
        ##
        #Do some additional processing since Count-based bar plots are run, mainly breaks on the x-axis
        
        #Process breaks
        n_curr <- nrow(df)
        round_curr <- n_curr * 0.05
        round_curr <- round(round_curr, -(nchar(trunc(round_curr))-1))
        if(round_curr < 1) round_curr <- 1
        breaks_ceil <- ceiling(n_curr*2)
        breaks_curr <- seq(0, breaks_ceil, round_curr)
        
        #Set the title
        y_title <- paste0("Referral Counts - ", dict[[.indvar]][["plot_field"]])
        
        #Select the plot labels
        labels_curr <- dict[[.indvar]][["plot_labels"]]
        levels_curr <- dict[[.indvar]][["labels"]]
        levels_curr_idx <- which(levels_curr %in% df[[indvar_curr]])
        labels_curr <- labels_curr[levels_curr_idx]
        names(labels_curr) <- levels_curr[levels_curr_idx]
        
        
        ##
        #Make the plot
        ##
        
        #Make one plot if providing count summary
        plot_refer <- ggplot2::ggplot(
          data = df, 
          ggplot2::aes_string(x = group_plot)) +
          ggplot2::geom_bar(
            ggplot2::aes_string(fill = stack_split_plot), 
            position = ggplot2::position_stack(), 
            color = "black"
          ) +
          ggplot2::scale_fill_manual(values = fill_curr, drop = FALSE) +
          ggplot2::scale_y_continuous(breaks = breaks_curr, name = y_title) +
          ggplot2::scale_x_discrete(name = title_curr, drop = FALSE) +
          ggplot2::ggtitle("") +
          ggplot2::facet_wrap(
            stats::as.formula(paste("~", indvar_plot)), 
            nrow = 1, 
            labeller = ggplot2::as_labeller(labels_curr)
          ) +
          gg_themes$refer
        
        #Cast the plot to a plotly object and drop the legend if desired
        plot_refer <- plotly::ggplotly(plot_refer,
                                       height=700)
        plot_refer <- plot_refer %>% plotly::layout(legend = list(x=0.0, y=-.15, orientation = 'h', xanchor="left", yanchor="top", font=list(size=20)),
                                                    hoverlabel = list(font=list(size=18)),
                                                    margin = list(l = 75, t = 75))
        #plot_refer <- plot_refer %>% layout(showlegend = FALSE)
        
        plot_refer$x$layout$annotations[[1]]$y <- -0.10
        
        return(plot_refer)
      }
    }
  })

  names(refer_viz_out) <- c(dict$type$plot_field, dict[["sub_type"]][["plot_source_map"]][which(dict[["sub_type"]][["plot_source_map"]] %in% source_curr)])
  
  # build out the tabsetPanel  
  if(make_plot == TRUE){
    if (isTRUE(use_echarts)){
      .tabs <- purrr::map(names(refer_viz_out), ~tabPanel(.x, refer_viz_out[[.x]]))
    } else {
      .tabs <- purrr::map(names(refer_viz_out), ~tabPanel(.x, plotly::renderPlotly(refer_viz_out[[.x]]))) 
    }
  } else{
    .tabs <- purrr::map(names(refer_viz_out), ~tabPanel(.x, renderUI(refer_viz_out[[.x]])))
  }
  
  
  return(do.call(bs4Dash::tabsetPanel, append(.tabs, list(type="tabs"))))
}


#' Create a grid of stacked bar charts
#'
#' @param .data 
#' @param indvar_plot 
#' @param group_curr 
#' @param stack_split_plot 
#' 
#' @noRd
#' @author Chad Murchison, \email{cfmurch@uab.edu}
#'
#' @return html containing echarts
make_plot_refer_ec <- function(.data, indvar_plot, group_curr, stack_split_plot){

  # this is the ideal method: https://echarts.apache.org/examples/en/editor.html?c=bar-stack
  # but does not seem to be possible with echarts4r
  # note that individual charts with 0 counts for a category will not show that category
  
  # set labs
  x_lab <- NULL #title_curr
  y_lab <- "Referral Counts"
  
  # make frequency counts of the data
  df_counts <- .data |>
    dplyr::group_by(
      !!rlang::sym(untick(indvar_plot)),
      !!rlang::sym(untick(group_curr)),
      !!rlang::sym(untick(stack_split_plot))
    ) |>
    dplyr::tally(name = 'n')
  
  # global y max for consistent y scale across bar charts
  y_limit <- df_counts |> 
    dplyr::group_by(
      !!rlang::sym(untick(indvar_plot)),
      !!rlang::sym(untick(group_curr))) |> 
    dplyr::summarize(n = sum(n)) |> 
    dplyr::pull(n) |> 
    max()
  
  # split the data into a list so it can be mapped over
  df_counts_split <- df_counts |> 
    dplyr::group_by(!!rlang::sym(untick(indvar_plot))) |> 
    dplyr::group_split()
  
  # function to create a single stacked bar chart
  make_single_chart <- function(.data){

    # error handling for empty data.frames
    if (!ADRCDashHelper::is_truthy(.data)){
      cli::cli_warn("Data into make_single_chart is empty")
      return(NULL)
    }
    
    # derive plot title from data
    plot_title <- .data[[untick(indvar_plot)]][1] |> as.character()
    
    # relevel the data so Enrolled is first so its the bottom stack
    # TODO: this may break if the stack_split_plot var changes or values change
    data_cleaned <- .data |> 
      dplyr::mutate(stack_var = relevel(!!sym(untick(stack_split_plot)), ref = 'Enrolled'))
    
    # replace NAs with "Unknown"
    # TODO: review
    if (any(is.na(data_cleaned$stack_var))){
      cli::cli_warn("NAs detected in referral data. Recoding as 'Unknown'")
    } 
    data_cleaned <- data_cleaned |>
      dplyr::mutate(stack_var = dplyr::if_else(is.na(stack_var), 'Unknown', stack_var))
    
    # assign colors based on values
    vals_in_data <- data_cleaned$stack_var |> as.character() |> unique() |> sort()
    color_pal <- c(
      "Enrolled" = ADRCDashHelper::color_palette$gold, 
      "Not Enrolled" = colorspace::adjust_transparency(ADRCDashHelper::color_palette$gold, alpha = 0.35), 
      "Unknown" = colorspace::adjust_transparency(ADRCDashHelper::color_palette$grey_light, alpha = 0.4)
    )
    colors_to_use <- as.vector(color_pal[vals_in_data])
    
    # make stacked bar chart
    ec_plot <- data_cleaned |>
      dplyr::group_by(stack_var) |> #!!rlang::sym(untick(stack_split_plot))) |>
      echarts4r::e_charts_(
        untick(group_curr),
        height = "300px"
      ) |>
      echarts4r::e_bar(n, stack = 'group1') |>
      echarts4r::e_color(color = colors_to_use) |> 
      echarts4r::e_aria(
        enabled = TRUE, 
        decal = list(show = TRUE)
      ) |> 
      echarts4r::e_y_axis(max = y_limit) |> 
      echarts4r::e_axis_labels(
        x = x_lab,
        y = y_lab
      ) |>
      echarts4r::e_tooltip('axis') |>
      echarts4r::e_title(plot_title) |>
      echarts4r::e_legend(right = 10) |>
      echarts4r::e_grid(left = "60px") |>
      echarts4r::e_show_loading(color = ADRCDashHelper::color_palette$green) |> 
      echarts4r::e_group("referral") |> # this enables grouping of tooltip across plots
      echarts4r::e_connect_group("referral") # this enables grouping of tooltip across plots
    
    # place into a bootstrap col
    html_out <- shiny::column(width = 6, ec_plot)
    
    return(html_out)
  }
  
  # make all the plots
  ec_plots <- purrr::map(df_counts_split, make_single_chart)

  # place into a bootstrap row
  html_grid <- shiny::fluidRow(ec_plots, style = "margin: 10px;")
  
  return(html_grid)
}


#' DEPRECATED This was the previous referral plot when there was only a flat column and a split
#' 
#' Note it is very similar in structure to the general bar plots in make_plot_explorer
#' 
#' Main change being that indvar_curr will always be "Referral Source" / "prescreen_refer"
#'  and depvar_curr will always be "count"
#'  
#' Primary adaptation is the inclusion of stacking to distinguish between enrollment
#'  before and after funding; this is accomplished by using facets on the plot
#' 
#' Includes additional function to allow for assessment of participants who have declined
#' This requires using different `group_curr` names though
#' 
#' @noRd
#' @author Chad Murchison
make_plot_refer_old <- function(df, group_curr, source_curr, study, start_dt, end_dt,
                            declined_subj = FALSE, date_adjust = TRUE,
                            split_date_field = "prescreen_dt",
                            stack_split = "Contact Time Point",
                            split_decline = "refer",
                            label_dict = plot_labels_dict){


  #Start the spinner
  # spin_update$show()

  #Return the NULL plot if needed
  if(length(source_curr) == 0) return(return_default_plot())

  #Begin by filtering the date using the study start date
  filter_date_field <- study_choices[["date_field"]][which(study_choices[["name"]] == study)]
  if(date_adjust == TRUE) df <- start_end_date_filter(df, filter_date_field, start_dt, end_dt)

  #Initialize indvar_curr and devpar_curr since they are static for the referral plot
  indvar_curr <- "Referral Source"
  depvar_curr <- "Count"

  #Filter the dataset on the selected referral sources
  df <- df[df[[indvar_curr]] %in% source_curr,]

  #If focusing on declined participants, subset the dataset accordingly
  if(declined_subj == TRUE){
    stack_split <- "Declined or Unknown"
    df <- filter_decline(df, group_old = group_curr)
    if(!is.data.frame(df)) return(return_default_plot())
  #Otherwise, select dataframe based on study
  } else {
    df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
  }

  #Slice to get the first visit
  df <- df_slicer(df, slice_type = "first")

  ##
  #Initial Processing
  ##

  #Get the column names in the dataframe for indvar, group and non-count depvar
  indvar_plot <- back_ticker(indvar_curr)
  group_plot <- back_ticker(group_curr)
  stack_split_plot <- back_ticker(stack_split)

  #Prep the x-axis and main title
  x_title <- expl_xvar_dict[["axis_name"]][which(expl_xvar_dict[["xvar"]]==indvar_curr)]
  title_curr <- expl_group_dict[["title_curr"]][which(expl_group_dict[["group_var"]]==group_curr)]

  #Also prep the group colors for fill/color
  fill_length <- max(length(levels(df[[group_curr]])), 3)
  fill_curr <- RColorBrewer::brewer.pal(fill_length, label_dict[["colorset"]][which(label_dict[["declined"]]==declined_subj)])

  #Filter dataframe to not have NA's in group or indvar, also drop levels as needed
  df <- df[!is.na(df[[indvar_curr]]) & !is.na(df[[group_curr]]),]
  df[[indvar_curr]] <- forcats::fct_drop(df[[indvar_curr]])
  df[[group_curr]] <- forcats::fct_drop(df[[group_curr]])

  #Another catch to return the NULL plot if needed
  if(nrow(df) == 0) return(return_default_plot())



  ##
  #Adding split variable for pre/post funding / process splits and positioning
  ##

  #Set the split_date_threshold based on the start of the study (always split on prescreen_dt thought)
  split_date_threshold <- study_choices[["start_dt"]][which(study == study_choices[["name"]])]

  #This first portion is used since combining stacking and dodging is not a viable option
  #Instead we only use stacking, set the `group_curr` as the x-axis and use facets to split up the sources

  #First create a variable for the stacking
  if(declined_subj == FALSE){
    df[[stack_split]] <- dplyr::case_when(lubridate::as_date(df[[split_date_field]]) >= lubridate::as_date(split_date_threshold) ~ 0,
                                                lubridate::as_date(df[[split_date_field]]) <  lubridate::as_date(split_date_threshold) ~ 1)
    df[[stack_split]] <- factor(df[[stack_split]], levels = c(0,1),
                                      labels=c("Post-Funding Initial Contact", "Contacted Prior to Funding"))
  } else {
    df[[stack_split]] <- factor(df[[split_decline]])
  }


  #This was a prior attempt to manually define bar split and shifts but stacking didn't work
  #Stacking would need to be done manually since the bars otherwise just lay on top of each other
  #This code is retained just in case we ever return to it

  ##Create a list that will hold dataframes for each of the group_curr splits
  #df_plot <- list()
  #group_curr_levels <- levels(df[[group_curr]])

  #for(ii in seq_along(1:length(group_curr_levels))){
  #  df_plot[[ii]] <- df[df[[group_curr]] == group_curr_levels[ii],]
  #}

  ##Determine the width and position adjustment of the bars based on the number of levels
  #bar_width <- bar_width_shift_calc(length(group_curr_levels), return_width = TRUE)
  #bar_shift <- bar_width_shift_calc(length(group_curr_levels), return_width = FALSE)



  ##
  #Additional Processing
  ##

  #Do some additional processing if the Count-based bar plots are run, mainly breaks on the x-axis

  #Process breaks
  n_curr <- nrow(df)
  round_curr <- n_curr * 0.05
  round_curr <- round(round_curr, -(nchar(trunc(round_curr))-1))
  if(round_curr < 1) round_curr <- 1
  breaks_ceil <- ceiling(n_curr*2)
  breaks_curr <- seq(0, breaks_ceil, round_curr)

  #Set the title
  y_title <- "Referral Counts"

  #Select the plot labels
  labels_curr <- label_dict[["plot_labels"]][[which(label_dict[["var_curr"]]==indvar_curr)]]
  levels_curr <- label_dict[["df_levels"]][[which(label_dict[["var_curr"]]==indvar_curr)]]
  levels_curr_idx <- which(levels_curr %in% df[[indvar_curr]])
  labels_curr <- labels_curr[levels_curr_idx]
  names(labels_curr) <- levels_curr[levels_curr_idx]


  ##
  #Make the plot
  ##

  #Make one plot if providing count summary
  plot_refer <-
    ggplot(data=df, aes_string(x = group_plot)) +
    geom_bar(aes_string(fill = stack_split_plot), position = position_stack(), color = "black") +

    scale_fill_manual(values = fill_curr, drop = FALSE) +
    scale_y_continuous(breaks = breaks_curr, name = y_title) +
    scale_x_discrete(name = title_curr, drop = FALSE) +

    ggtitle("") +
    facet_wrap(stats::as.formula(paste("~", indvar_plot)), nrow=1,
               labeller = as_labeller(labels_curr)) +
    gg_themes$refer

  #Cast the plot to a plotly object and drop the legend if desired
  plot_refer <- plotly::ggplotly(plot_refer)
  plot_refer <- plot_refer %>% plotly::layout(legend = list(x=0.0, y=-.15, orientation = 'h', xanchor="left", yanchor="top", font=list(size=20)),
                                      hoverlabel = list(font=list(size=18)),
                                      margin = list(l = 75, t = 75))
  #plot_refer <- plot_refer %>% layout(showlegend = FALSE)

  plot_refer$x$layout$annotations[[1]]$y <- -0.10

  #Hide the spinner and return the plot
  # spin_update$hide()
  return(plot_refer)
}
