#' Miscellaneous functions for app_server.R
#' 
#' @import ggplot2
#' 


# functions on dataframes -------------------------------------------------

#' @title Truncate data.frame by date
#'
#' @description Deprecated. Date function that uses the toggle checkmark
#'
#' @param df 
#' @param date_field 
#' @param threshold 
#' @param event_field 
#' @param secondary_event 
#' @param date_field_secondary 
#' @param threshold_secondary 
#' 
#' @seealso trunc_by_date_new
#' @noRd
#'
#' @return a data.frame
trunc_by_date <- function(df, date_field, threshold, 
                          event_field = "redcap_event_name", secondary_event = NULL, date_field_secondary = NULL, threshold_secondary = NULL){
  
  if(not_null(secondary_event)){
    .idx <- which(df[[date_field]] > threshold)
    .idx2 <- which(not_na(df[[date_field_secondary]]) & df[[date_field_secondary]] > threshold_secondary)
    df_new <- df[unique(sort(c(.idx, .idx2))),]
  } else {
    df_new <- df[not_na(df[[date_field]]) & df[[date_field]]>threshold,]
  }
  
  return(df_new)
}

#' Truncate data.frame by date
#'
#' @description New version of `trunc_by_date` that generalizes better.
#'
#' @param df 
#' @param date_field 
#' @param threshold 
#' 
#' @noRd
#'
#' @return a data.frame
trunc_by_date_new <- function(df, date_field, threshold){
  
  #Make sure the threshold is either as long as date_field or is only 1
  if(length(threshold) > 1 && length(threshold) != length(date_field)) stop("Threshold must either be length 1 or as long as vector of date_fields")
  
  #You'll always want to start with the first threshold regardless
  thresh_curr <- threshold[1]
  
  #Iterate over the fields for thresholding
  idx_retain <- lapply(seq_along(date_field), function(ii){
    
    #Using the current date_field
    date_field_curr <- date_field[ii]
    
    #Get the next theshold if necessary
    if(length(threshold) > 1) thresh_curr <- threshold[ii]
    
    #Get the index of the current date field according to the threshold
    which(!is.na(df[[date_field_curr]]) & df[[date_field_curr]] >= thresh_curr)
  })
  
  #Collapse to a vector for indexing, using only the unique indices and sorting as needed
  idx_retain <- sort(unique(do.call(c, idx_retain)))
  
  #Return the indexed data frame
  return(df[idx_retain,])
}



#Date function that uses the start and end dates to filter

start_end_date_filter <- function(df, date_field, start_date, end_date){
  df <- df[!is.na(df[[date_field]]),]
  df <- df[df[[date_field]] >= as.Date(start_date) & df[[date_field]] <= as.Date(end_date),]
  return(df)
}

#Alternative filter I'm working on for the enrollment flow
start_end_date_filter_alt <- function(df, date_field, start_date, end_date, na_filter = FALSE){
  if(na_filter == TRUE) df <- df[!is.na(df[[date_field]]),]
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  
  #We filter by omission, find the indices for the dates outside start/end range
  if(is.null(start_date)){
    .idx <- which(!is.na(df[[date_field]]) & df[[date_field]] > end_date)
  } else if(is.null(end_date)){
    .idx <- which(!is.na(df[[date_field]]) & df[[date_field]] < start_date)
  } else  .idx <- which(!is.na(df[[date_field]]) & (df[[date_field]] < start_date | df[[date_field]] > end_date))
  
  #Drop the dates outside the range
  if(length(.idx!=0)) df <- df[-.idx,]
  return(df)
}



#Make a dataframe with two targets based on a specific covariate
#Should eventually generalize this to take a list for .tar

dual_var_df_maker <- function(df, .covar, .tar1, .tar2, tab_rownames, perc = FALSE){
  
  #Make a table with counts
  .tab1 <- rbind(table(df[[.covar]]), table(df[[.tar1]], df[[.covar]]), table(df[[.tar2]], df[[.covar]]))
  
  #Make a table with proportions
  .tab2 <- rbind(prop.table(table(df[[.covar]])), prop.table(table(df[[.tar1]], df[[.covar]]),margin=2), prop.table(table(df[[.tar2]], df[[.covar]]), margin=2))
  .tab2 <- round(.tab2, 4)*100
  
  #Cast them both to dataframes
  .tab1 <- data.frame(.tab1, check.names = FALSE); .tab2 <- data.frame(.tab2, check.names = FALSE)
  colnames(.tab2) <- paste0(colnames(.tab2), "_prop")
  
  #Some formatting is using percentages
  if(isTRUE(perc)){
    .tab2 <- apply(.tab2, c(1,2), function(xx){
      xx <- sprintf("%#.2f", xx)
      xx <- paste0(xx, "%")})}
  
  #Final formatting for rownames and grouping
  .tab_out <- data.frame(.tab1, .tab2, check.names = FALSE)
  rownames(.tab_out) <- tab_rownames
  .tab_out$grouping <- factor(rownames(.tab_out), levels=tab_rownames, labels=tab_rownames)
  
  return(.tab_out)
}


#Updated version that relies on indexing from the dictionary based on df_rownames
#Since df_rownames is named by comp_annot in the dictionary, we can index off that the build everything


multi_var_df_maker <- function(df, .covar, df_rownames, dict= data_dict_component, perc = FALSE){
  
  #First get the indexing we'll be using
  .idx <- which(dict[["comp_annot"]] %in% names(df_rownames))
  
  #Step through the indexingto start building the tables
  #max_levels is not built annotation by annotation based on the the length of build_table_cols in the dictionary
  #We cast df[[.col]] as a factor just in case everyone is or is not involved 
  .tab1_list <- lapply(.idx, function(ii){
    
    #Get the current annotation and column name
    .annot <- dict[["comp_annot"]][ii]
    .col <- dict[["data_col"]][ii]
    
    #Build the factor 
    #We get the levels based on the ordering within the "build_order" entry of the dictionary; max_levels no longer_required
    #We may want to drop non-used levels though which is another reason to fully cast as a factor now
    .var_factor <- factor(df[[.col]], levels = dict[["build_order"]][[.annot]], labels = df_rownames[[.annot]])
    #forcats::fct_drop(.var_factor)
    
    #Build the initial table
    .tab_curr <- table(.var_factor, df[[.covar]])
    
    #Add to margin as a Total column and return
    .tab_curr <- addmargins(.tab_curr, 2, sum)
    colnames(.tab_curr)[ncol(.tab_curr)] <- "Total"
    return(.tab_curr)
  })
  #Get a table for the covariate under consideration and add a margin to it as well
  .tab1_totals <- table(df[[.covar]])
  .tab1_totals <- addmargins(.tab1_totals, 1, sum)
  names(.tab1_totals)[length(.tab1_totals)] <- "Total"
  
  #Bind the everything together by row to get the same final .tab1 as in dual
  .tab1 <- rbind(.tab1_totals, do.call(rbind, .tab1_list))
  
  #Do the same step through for the proportions
  .tab2_list <- lapply(.idx, function(ii){
    
    #Get the current annotation and column name
    .annot <- dict[["comp_annot"]][ii]
    .col <- dict[["data_col"]][ii]
    
    #Again, build the factor before making the table
    #The main benefit is creating the rownames we'll be using now instead of trying to assign them later
    .var_factor <- factor(df[[.col]], levels = dict[["build_order"]][[.annot]], labels = df_rownames[[.annot]])
    #.var_factor <- forcats::fct_drop(.var_factor)
    
    #Get the proportions margined on the columns i.e. within the covariate
    .tab_curr <- prop.table(table(.var_factor, df[[.covar]]),margin=2)
    
    #Do the same for the factor across all levels of the covariate and return the bound table
    .tab_curr <- cbind(.tab_curr, prop.table(table(.var_factor)))
    colnames(.tab_curr)[ncol(.tab_curr)] <- "Total"
    return(.tab_curr)
  })
  
  #Again, we need to account for the proportions of the covariate
  .tab2_totals <- prop.table(table(df[[.covar]]))
  .tab2_totals <- addmargins(.tab2_totals, 1, sum)
  names(.tab2_totals)[length(.tab2_totals)] <- "Total"#Again, we set our levels according to the build order so it alligns with build_names and build_table_cols ordering
  
  #Again, we set our levels according to the build order so it aligns with build_names and build_table_cols ordering
  .tab2 <- rbind(.tab2_totals, do.call(rbind, .tab2_list))
  .tab2 <- round(.tab2, 4)*100
  
  
  #Cast them both to dataframes
  .tab1 <- data.frame(.tab1, check.names = FALSE); .tab2 <- data.frame(.tab2, check.names = FALSE)
  colnames(.tab2) <- paste0(colnames(.tab2), "_prop")
  
  
  
  #Some formatting if using percentages
  if(perc==T){
    .tab2 <- apply(.tab2, c(1,2), function(xx){
      xx <- sprintf("%#.2f", xx)
      xx <- paste0(xx, "%")})}
  
  
  #Final formatting for the unnamed rownames at the top
  .tab_out <- data.frame(.tab1, .tab2, check.names = FALSE)
  rownames(.tab_out)[seq_along(df_rownames[[1]])] <- df_rownames[[1]]
  .tab_out$grouping <- factor(rownames(.tab_out), levels=rownames(.tab_out), labels=rownames(.tab_out))
  
  
  return(.tab_out)
}



#Some small functions to build out column and plot names for components (e.g. Yes BD or No/Maybe BD)
#We try to use it for both initial df building and for prepping tables for display
build_compon_annot <- function(compon_set, build_table = FALSE, dict = data_dict_component){
 
  if(build_table == FALSE){
    #First get the annotation set based on the selected components
    annot_set <- dict[["comp_annot"]][which(dict[["new_name"]] %in% compon_set)]
    
    #Next build it out for the dataframe for the components we're annotating
    rows_out <- lapply(annot_set, function(xx){paste(dict[["build_names"]][[xx]], xx, sep = " ")})
    names(rows_out) <- annot_set
    
    #Finally, append the "Total" name to the list
    rows_out <- append(dict[["all_tables_rows"]], rows_out)

  #Otherwise prep the column set for the displayed table (including handling the blanked columns)
  } else{
    #We can just use the compon_set since it's already gone through the annotation process and we extract it from the built table in fct_tables_compon
    annot_set <- compon_set
    
    #Rows out is similar except we don't need a list structure but we do need to prepend a moving blank
    #Previously we stepped through the annotation set but we had to use ii <<- ii+1 since we initialized ii <- 0 outside the sapply; this is cleaner
    rows_out <- sapply(seq_along(annot_set), function(ii){
      .annot <- annot_set[[ii]]
      c(paste0("Blank", ii), paste(dict[["build_table_cols"]][[.annot]], .annot, sep = " "))
      })
    
    #We do want to unlist this version to return a vector after adding in the table "V1" and "Total" colnames
    rows_out <- c(dict[["all_tables_cols"]], unlist(rows_out))
    
  }
  
 
  return(rows_out)
}


#' Use the `decline_dict` to specifically filter a dataframe on participants who have declined or are unknown
#'
#' @param df 
#' @param group_old 
#' @param dict 
#'
#' @return a data.frame
#' @noRd
#'
#' @examples
#' # See fct_plots_refer.R
filter_decline <- function(df, group_old, dict = decline_dict){
  
  #Identify the new group column if necessary, return default if needed, otherwise recast the group
  if(group_old %in% dict[["old_group"]]){
    group_new <- dict[["new_group"]][which(dict[["old_group"]]==group_old)]
    if(length(group_new)==0) return("default")
    df[[group_old]] <- df[[group_new]]
    df[[group_old]] <- forcats::fct_drop(df[[group_old]])
  }
  
  #Subset appropriately
  df <- df[df[[dict[["subset_col"]]]] %in% dict[["subset_lvls"]],]
  return(df)
}


#' Split a dataframe using group_by
#'
#' In this context, can return the first row, all rows, or most recent row. This is the new version cleaned up with new redcap design.
#'
#' @param df 
#' @param slice_type 
#' @param drop_min 
#' @param by_col 
#' @param dict 
#' @param study_curr 
#'
#' @return a data.frame
#' @noRd
#'
#' @examples
#' # See app_server.R
df_slicer <- function(df, slice_type, drop_min = NULL,
                      by_col = redcap_dict[["adrc_key"]],
                      dict = min_fields, study_curr = NULL){
  
  #If by_col doesn't have duplicates there's no need to slice, just return the data frame
  if(length(na.omit(df[[by_col]])) == length(unique(na.omit(df[[by_col]])))) return(df)
  
  #Part 1 - argument processing, drop_min dates and study restriction is desired
  #This needs some work since min_fields, slice_dict, and study_choices don't really coincide well anymore
  
  #drop_min still serves the same purpose - return a row sum only if all fields in min_field are not NA 
  #But now we surround the row sum portion (i.e. the row filtering) by parenthesis because of calling scope in data.table
  df_out <- as.data.table(df)
  if(!is.null(drop_min)) df_out <- df_out[(!rowSums(is.na(df_out[,colnames(df_out) %in% dict[[drop_min]],with=FALSE]))),]
  
  if(!is.null(study_curr)){
    start_dt <- study_choices[["start_dt"]][which(study_choices[["name"]] == study_curr)]
    study_date_field <- study_choices[["visit_date_field"]][which(study_choices[["name"]] == study_curr)]
    df_out <- df_out[!is.na(df_out[[study_date_field]]) & df_out[[study_date_field]] > as.Date(start_dt),]
  }
  
  
  #Part 2 - Slicing the dataframe by pulling an index using data.table
  
  #Return first row, regardless of occurrence
  if(slice_type == "first"){
    idx_out <- df_out[, .I[1], by = by_col]
  }
  
  #Return last row
  if(slice_type == "last"){
    idx_out <- df_out[, .I[.N], by = by_col]
  }
  
  #For longitudinal, return anything with two or more rows
  if(slice_type == "longitudinal"){
    idx_out <- df_out[, .I[nrow(.SD) > 1], by = by_col]
  }
  
  #As default, return the original dataset
  if(slice_type == "all" || slice_type %not_in% slice_dict[["slice_type"]]) return(df)
  
  #Return the indexed data.tabled (don't forget V1 is the index since we always use the by call)
  return(df_out[idx_out$V1,])
}


#' Find the most recent date from the list of fields
#'
#' Adaptation to work with the newer min_fields directory. 
#' 
#' This does not subset the dataframe; requires either a by from data.table or group_by from dplyr
#'
#' @param df 
#' @param dt_set 
#' @param dict 
#' @param id_col 
#' @param force_date_set 
#' @param FUN 
#'
#' @return a date object
#' @noRd
#'
#' @examples
#' # See app_server.R
date_find <- function(df, dt_set = NULL, dict = min_fields,
                     id_col = redcap_dict[["adrc_key"]], force_date_set = NULL,
                     FUN = max){
  
  #First check whether to use the minimum date_set or all regex dates (those ending in 'dt')
  if(!is.null(dt_set)){
    if(dt_set %not_in% names(dict)) {
      stop("Date set not found in min_fields dictionary; use default of `dt_set = NULL` for all dates, check dictionary entries, or give vector of dates to `force_date_set`")
    }
    date_set <- dict[[dt_set]]
  } else{
    date_set <- grep("_dt$", colnames(df), value = TRUE)
  }
  
  #Also allow for a predefined set of date fields if desired
  if(!is.null(force_date_set)) {
    date_set <- force_date_set
    if(length(grep(date_set, colnames(df))) == 0) stop("Date sets not found in data frame, please check names")
  }
  
  #Get the full date_set as a vector, drop NAs
  #We know it's going to be a data.table
  dates_curr <- do.call(c, df[,colnames(df) %in% date_set, with = FALSE])
  dates_curr <- dates_curr[!is.na(dates_curr)]
  
  #Make sure at least one value is returned
  if(length(dates_curr) == 0) {
    warning("No dates returned, returning NA, check date set and data frame")
    return(NA_real_)
  }
  
  #Find the date based on the function, first make sure the function actually exists
  
  #if(!exists(substitute(FUN))) stop("Seeker function not found, use default of max?")   
  #This is giving a very odd error specifically in Shiny Server
  
  date_seek <- FUN(dates_curr)
  date_seek <- unique(date_seek)
  
  #Final sanity check, should only be one date
  if(!(length(date_seek) == 1)) stop("More than one date found, check dataframe")
  
  #Return the targeted date
  return(unique(date_seek))
}


# functions on strings and numbers ----------------------------------------

#' Backticker
#' 
#' Surround a string with backticks
#' 
#' @noRd
back_ticker <- function(x){gsub("^(.*)$", "`\\1`", x)}
untick <- function(x) stringr::str_remove_all(x, '`')


#' Decimals
#' 
#' Get decimals on a value (or vector) for rounding
#' sub drops trailing zeros
#' strsplit splits on the decimal, uses fixed=TRUE to avoid regex
#' nchar extracts the appropriate list entry 
#' 
#' @noRd
#' 
.decimals <- function(xx){
  if(abs(xx - round(xx)) > .Machine$double.eps^0.5){
    nchar(strsplit(sub("0+$", "", as.character(xx)), ".", fixed=TRUE)[[1]][2])
  } else 0
    
}


# functions on data vectors -----------------------------------------------

#' cdfer
#'
#' Generic function that takes a vector and returns descriptive statistics
#' 
#' @noRd
#' 
cdfer <- function(xx, yy){
  length.curr <- length(na.omit(xx[[yy]]))
  mean.curr <- mean(na.omit(xx[[yy]]))
  sd.curr <- sd(na.omit(xx[[yy]])); sem.curr <- sd.curr/sqrt(length.curr)
  median.curr <- stats::median(na.omit(xx[[yy]]))
  mad.curr <- stats::mad(na.omit(xx[[yy]]))
  max.curr <- max(na.omit(xx[[yy]])); min.curr <- min(na.omit(xx[[yy]]))
  cdf.curr <- data.frame(length=length.curr, mean=mean.curr, sd=sd.curr, sem=sem.curr, median=median.curr, mad=mad.curr, min=min.curr, max=max.curr)
  cdf.curr <- purrr::modify(cdf.curr, as.numeric)
  return(cdf.curr)}


add_interact <- function(df, itx_dict = interact_dict){
  for(i in seq_along(1:length(itx_dict[["name"]]))){
    .new <- itx_dict[["name"]][i]
    .v1 <- itx_dict[["var1"]][i]
    .v2 <- itx_dict[["var2"]][i]
    df[[.new]] <- factor(interaction(df[[.v1]], df[[.v2]], sep="_"))
  }
  return(df)
}


#Flatten a multi-level factors to a binary variable
#We have multiple versions 
# flatten_factor is designed to set NA's to 0 and non-NA's to 1


flatten_factor <- function(xx){
  xx <- as.numeric(xx)
  xx[!is.na(xx)] <- 1
  xx[is.na(xx)] <- 0
  return(xx)    
}


# flatten_radio is a generalized approach but requires a dictionary to account for a variety of options
#See data_dict_component for a variety of utilizations

flatten_radio <- function(xx, make_zero = TRUE, na_shift = TRUE, dict = data_dict_component, .var = NULL){
  
  #Check if the variable xx has a specific remapping
  if(!is.null(.var) && .var %in% names(dict[["recast_radio"]])){
    
    for(ii in seq_along(dict[["recast_radio"]][[.var]][["map_from"]])){
      
      #Get the mappings
      .from <- dict[["recast_radio"]][[.var]][["map_from"]][ii]
      .to <- dict[["recast_radio"]][[.var]][["map_to"]][ii]
      
      #Some if / else nesting since we have both NA and the "any" options to consider
      #Better to use this than ifelse or dplyr's if_else since we're doing a variety of subsetting assignments
      if(is.na(.from)) {xx[is.na(xx)] <- .to
      } else if(.from == "any"){ xx[!is.na(xx)] <- .to
        } else xx[!is.na(xx) & xx==.from] <- .to
    }
    
  #If we don't have a dictionary entry (which we usually will at this stage) we can do the normal type of collapsing 
  #For best practice these arguments should really be part of the dictionary too
  } else{
  
    #Make numeric, relevel to 0 if needed based on the minimum value of the numeric (this would usually be one from a factor)
    xx <- as.numeric(xx)
    if(min(abs(na.omit(xx))) != 0) xx <- xx - min(abs(na.omit(xx)))
  
    #Case one (make_zero == FALSE) set non-zero to 1
    if(make_zero == FALSE){ xx[!is.na(xx) & xx != 0] <- 1
    
    #Otherwise set non-zero values to 0
    } else{
      xx[!is.na(xx) & xx != 1] <- 0
    }
  
    #Finally, set NA's to 0 if desired based on na_shift
    if(na_shift == TRUE) xx[is.na(xx)] <- 0
  }
  
  return(xx)
}


#A way to set NA's from a set instead of singular calls like na_if
#Adapted to work over multiple fields, requires multiple .vals options

reduc_factor <- function(.var, .field, .vals = 1){
  
  #Be sure to cast the filtering field as a dataframe in case only a vector is passed
  .field <- as.data.frame(.field)
  
  for(ii in seq_along(.field)){
    
    #Pull the current field we're reducing on
    field_curr <- .field[[ii]]
    
    #Get the values we're using to reduce if there's more than one
    if(length(.vals)>1) {vals_curr <- .vals[ii]
    } else vals_curr <- .vals
    
    #Identify which values match the filter and cast them to NA
    x <- which(is.na(field_curr) | (!is.na(field_curr) & field_curr %not_in% .vals))
    .var[x] <- NA
  }
  return(.var)
}


# table functions ---------------------------------------------------------

#Return the default table
return_default_table <- function(){
  # spin_update$hide()
  return(default_table)
}


#Make a table for frequency counts
count_table_maker <- function(df, .indvar, .group){
  
  #Make a frequency table, cast wide with group as the columns
  .tab <- as.data.frame(table(df[[.indvar]], df[[.group]]))
  .tab <- tidyr::pivot_wider(.tab, names_from = Var2, values_from = Freq)
  
  #Add totals for columns (across the groups) and rows (for all subjects)
  .tab$Var1 <- forcats::fct_expand(.tab$Var1, "Total")
  .tab$`All Subjects` <- apply(.tab[,-1], 1, sum)
  .tab <- rbind(.tab, c("Total", apply(.tab[,-1], 2, sum)))
  colnames(.tab)[1] <- .indvar
  
  #Replace 0's with blanks for readability except for the upper right cell which may be a 0 as a factor level
  .tab[.tab==0] <- NA
  if(is.na(.tab[1,1])) .tab[1,1] <- "0"
  return(.tab)
}



#Make a table for covariates
covar_table_maker <- function(df, .depvar, .indvar, .group){
  
  #Cast the strings as symbols
  .depvar_sym <- as.symbol(.depvar)
  .indvar_sym <- as.symbol(.indvar)
  .group_sym <- as.symbol(.group)
  
  #Drop NA's to help with group_by parsing
  df_cdf <- tidyr::drop_na(df, tidyselect::any_of(c(.depvar, .indvar, .group)))
  
  #Recast depvar as numeric if needed
  if(!is.numeric(df_cdf[[.depvar]])) df_cdf[[.depvar]] <- as.numeric(as.character(unclass(df_cdf[[.depvar]])))
  
  
  #Make the summary frame on the groups and the total - dependent on whether indvar and group are the same
  if(.indvar == .group){ 
    # .tab_cdf <- 
    #   df_cdf %>%
    #   dplyr::group_by(!!.indvar_sym) %>%
    #   dplyr::do(cdfer(., .depvar))
    .tab_cdf <- df_cdf[,cdfer(.SD, .depvar), by=.indvar]
    .tab_all <- cdfer(df_cdf, .depvar)
    
  } else {
    
    # .tab_cdf <- 
    #   df_cdf %>%
    #   dplyr::group_by(!!.indvar_sym, !!.group_sym) %>%
    #   dplyr::do(cdfer(., .depvar)) %>%
    #   dplyr::ungroup()
    .tab_cdf <- df_cdf[,cdfer(.SD, .depvar), by=c(.indvar, .group)]
    .tab_all <- df_cdf[,cdfer(.SD, .depvar), by=.group]
    
    # .tab_all <- 
    #   df_cdf %>%
    #   group_by(!!.group_sym) %>%
    #   dplyr::do(cdfer(., .depvar))
  }
  .tab_all <- data.frame(rep("Total", nrow(.tab_all)), .tab_all, check.names = FALSE)
  colnames(.tab_all)[1] <- .indvar
  
  #Processing the summary table to get the display measure (mean/sem/N as a string named Value)
  .tab_cdf[[.indvar]] <- forcats::fct_expand(.tab_cdf[[.indvar]], "Total")
  .tab_cdf <- rbind(.tab_cdf, .tab_all)
  .tab_cdf$Value <- paste0(signif(.tab_cdf$mean, 2), " {", signif(.tab_cdf$sem, 3), "}, N=", .tab_cdf$length)
  .tab_cdf <- .tab_cdf[, colnames(.tab_cdf) %in% c(.indvar, .group, "Value"), with=FALSE]
  
  
  #Some extra processing when .group and .indvar differ; involves recast summary table against .indvar
  if(.indvar != .group) {
    #.tab_print <- tidyr::pivot_wider(.tab_cdf, names_from = !!.group_sym, values_from = Value)
    .tab_print <- data.table::dcast(.tab_cdf, formula = paste("...~", back_ticker(.group)), value.var = "Value")
    
    #Need to recast .tab_all against .indvar for row margins
    .tab_all <- df_cdf[,cdfer(.SD, .depvar),by=.indvar]
    # .tab_all <- 
    #   df_cdf %>%
    #   group_by(!!.indvar_sym) %>%
    #   dplyr::do(cdfer(., .depvar))
    .tab_all$Value <- paste0(signif(.tab_all$mean, 2), " {", signif(.tab_all$sem, 3), "}, N=", .tab_all$length)
    
    #Add a final column to .tab_print and fill it with the new Value metric
    .tab_print$`All Subjects` <- NA
    .tab_print$`All Subjects`[as.character(.tab_print[[.indvar]]) %in% as.character(.tab_all[[.indvar]])] <- .tab_all$Value
    
    #And the final cell
    .tab_all <- cdfer(df_cdf, .depvar)
    .tab_print$`All Subjects`[.tab_print[[.indvar]] == "Total"] <- paste0(signif(.tab_all$mean, 2), " {", signif(.tab_all$sem, 3), "}, N=", .tab_all$length)
    
  } else .tab_print <- .tab_cdf
  
  return(.tab_print)
}


# plot functions ----------------------------------------------------------

#' Return the default plot
#' @noRd
#' 
return_default_plot <- function(.plotly = TRUE){
  # spin_update$hide()  
  if(.plotly == TRUE){ return(default_plotly)
  } else return(default_plot)
}

#' gg_color_hue
#'
#' Legacy function designed to mimic the default colors of ggplot2. Largely replaced by `rcolorbrewer`
#' 
#' Assign the standard ggplot2 colors based on a length n
#' This has largely been abandoned in favor of RColorBrewer and Dark2
#' 
#' @noRd
#' 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  grDevices::hcl(h=hues, l=65, c=100)[1:n]}



#A table for annotating the enrollment plot with test - gets counts of factors
enroll_annotation <- function(.var){
  
  #Get the counts from a table and cumulative sum
  .counts <- data.frame(table(.var))
  .counts$posit <- cumsum(.counts$Freq)
  
  #Use those counts to get the mid-points for positioning
  .counts$posit[-1] <- .counts$posit[-nrow(.counts)] + .counts$Freq[-1]/2
  .counts$posit[1] <- .counts$posit[1]/2
  
  #Drop rows with 0
  .counts <- .counts[.counts$Freq>0,]
  
  return(.counts)
}


#Subset labels to a certain length
label_subset <- function(.labs, length_curr= 5 ){
  
  #Determine how many label entries you need to skip between printed labels
  #e.g. div_curr of 2 means we'll blank out every other label
  div_curr <- floor(length(.labs)/length_curr) 
  
  #If we're not skipping any, just return the labels
  if(div_curr<2) return(.labs)
  
  #Figure out which we're retaining
  .idx <- seq(1, length(.labs), div_curr)
  
  #Blank out everything that isn't in .idx
  .labs[-.idx] <- ""
  
  return(.labs)
}


#Calculate how to shift a bar plot based on the numbers of groups

bar_width_shift_calc <- function(n_grp, return_width = TRUE){
  #Make sure it's an integer
  if(n_grp %% 1 != 0) stop("Number of bar plot groups must be an integer")
  
  #Calculate the width, return if necessary
  width_curr <- 0.95 / n_grp
  if(return_width == TRUE) return(width_curr)
  
  #For shifts, first calculate the half-width
  half_width_curr <- width_curr / 2
  
  #Get the positions for the two most inner bars
  pos_curr <- c(-half_width_curr, half_width_curr)
  rem_n_grp <- n_grp - 2
  
  #Add a zero if there's an odd number
  if(n_grp %% 2 == 1){ 
    pos_curr <- c(pos_curr[1], 0, pos_curr[2])
    rem_n_grp <- n_grp - 3
  }
  
  #If there are remaining levels (i.e. greater than 3), add them to the vector
  if(rem_n_grp > 0){
    
    #We only need to work with half since we stick them on the beginning and end
    rem_n_grp <- rem_n_grp / 2 
    for(ii in seq_along(1:rem_n_grp)){
      
      #Subtract / add the bar width to the first / last element of pos_curr and combine
      pos_curr <- c(pos_curr[1] - width_curr, 
                    pos_curr, 
                    pos_curr[length(pos_curr)] + width_curr)
    }
  }
  
  #Return the final position set
  return(pos_curr)
}


#' Text repel for ggplot2
#' 
#' @description I believe this is taken from the ggrepel package.
#' 
#' https://stackoverflow.com/questions/50059193/jitter-text-labels-with-position-stack
#' 
#' @noRd
position_stack_repel <- function(vjust = 1, reverse = FALSE, offset = 1) {
  ggproto(NULL, PositionStackRepel, vjust = vjust, reverse = reverse,
          offset = offset)
}
PositionStackRepel <- ggplot2::ggproto("PositionStackRepel", ggplot2::PositionStack,
                              type = NULL,
                              vjust = 1,
                              fill = FALSE,
                              reverse = FALSE,
                              offset = 1,
                              
                              setup_params = function(self, data) {
                                list(
                                  var = self$var %||% ggplot2:::stack_var(data),
                                  fill = self$fill,
                                  vjust = self$vjust,
                                  reverse = self$reverse,
                                  offset = self$offset
                                )
                              },
                              
                              setup_data = function(self, data, params) {
                                data <- PositionStack$setup_data(data, params)
                                data <- data[order(data$x), ]
                                data$to_repel <- unlist(by(data, data$x, function(x) {
                                  sapply(seq(nrow(x)), function(i) {
                                    (x$y[i]) / sum(x$y) < 0.1 & (
                                      (if (i != 1) (x$y[i-1] / sum(x$y)) < 0.1 else FALSE) | (
                                        if (i != nrow(x)) (x$y[i+1] / sum(x$y)) < 0.1 else FALSE))
                                  })
                                }))
                                data
                              },
                              
                              compute_panel = function(data, params, scales) {
                                data <- PositionStack$compute_panel(data, params, scales)
                                data[data$to_repel, "x"] <- unlist(
                                  by(data[data$to_repel, ], data[data$to_repel, ]$x, 
                                     function(x) seq(x$x[1] - 0.3, x$x[1] + 0.3, length.out = nrow(x))))
                                data
                              }
)


#' Custom ggplot2 themes
#' 
#' @noRd
#' @import ggplot2
#' @example 
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + gg_themes$explorer
gg_themes <- local({
  
  # base theme
  theme_base <-  ggplot2::theme_bw()
  
  # explorer theme
  theme_explorer <- theme_base +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 24)) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 12))
  
  # enroll flow theme
  theme_enroll_flow <- theme_base +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 20)
    ) + 
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 18)
    )
  
  # enroll rate theme
  theme_enroll_rate <- theme_base +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 24),
      axis.title.x = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 18, color = "black")
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 28)) +
    ggplot2::theme(
      legend.position = c(0.15, 0.85),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 18),
      legend.key.width = grid::unit(3, "line")
    )
  
  # compon theme
  theme_compon <- theme_base +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 20),
      legend.text = ggplot2::element_text(size = 18)
    ) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  
  # completion theme
  theme_completion <- theme_base +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(vjust = 0.7, size = 20),
      axis.text.y = ggplot2::element_text(size = 20)
    ) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 18)
    )
  
  # refer theme
  theme_refer <- theme_base +
    ggplot2::theme(plot.margin = grid::unit(c(5.5, 5.5, 5.5, 8.5), "points")) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 24)) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 14)
    ) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) +
    ggplot2::theme(
      panel.spacing = grid::unit(0, "in"),
      strip.text.x = ggplot2::element_text(size = 20),
      strip.background = ggplot2::element_blank()
    )
  
  # one list to manage all of theme
  themes <- list(
    explorer = theme_explorer,
    enroll_flow = theme_enroll_flow,
    enroll_rate = theme_enroll_rate,
    compon = theme_compon,
    completion = theme_completion,
    refer = theme_refer
  )
  
  return(themes)
})


# other -------------------------------------------------------------------

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      '...field',
      '..dt_fields',
      '.enroll_curr',
      'AD Syndromal Stage',
      'Completers',
      'Component',
      'count_all',
      'count_race',
      'Current Enrollment',
      'Date',
      'date_label',
      'dt_out',
      'dt_plot',
      'dt_plot_posix',
      'elig',
      'enroll',
      'Freq',
      'full_label',
      'i.count_all',
      'i.count_race',
      'ideal',
      'jittered_x',
      'Monthly Proportion',
      'Most Recent',
      'Most Recent Visit',
      'n',
      'N',
      'nacc_synth',
      "name",
      'Perc',
      'plot_labels_dict',
      'posit',
      'redcap_synth',
      'refer',
      'refer_details',
      'screen_decline_reasn',
      'Target',
      'Target Enrollment',
      'Target Rate',
      'target_rate',
      'Total Proportion',
      "value",
      'Var2',
      'Visit'
    )
  )
}
