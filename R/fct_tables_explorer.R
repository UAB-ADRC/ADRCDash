#' From the family of functions for tables
#'  
#' This handles the main explorer table
#' 
#' @param df 
#' @param indvar_curr 
#' @param depvar_curr 
#' @param group_curr 
#' @param vis_type 
#' @param group_col 
#'
#' @noRd
#' @author Chad Murchison
#' @return dataframe
make_table_explorer <- function(df, indvar_curr, depvar_curr, group_curr, vis_type, group_col = "adc_sub_id"){
  
  
  if(nrow(df) == 0 || is.null(df) || is.null(dim(df))) return(default_table)
  
  #Start the spinner
  # spin_update$show()
  
  #Drop any unused levels in group_curr
  .currs <- c(indvar_curr, depvar_curr, group_curr)
  for(ii in seq_along(.currs)){
    if(is.factor(df[[.currs[ii]]])) df[[.currs[ii]]] <- forcats::fct_drop(df[[.currs[ii]]])
  }
  
  #Check if depvar and invdar are the same
  if(depvar_curr == indvar_curr){
    df[[paste0(depvar_curr, "_dep")]] <- df[[depvar_curr]]
    depvar_curr <- paste0(depvar_curr, "_dep")
  }

  #Get the column names in the dataframe for indvar, group and non-count depvar
  indvar_tab <- back_ticker(indvar_curr)
  group_tab <- back_ticker(group_curr)
  
  #Change the indvar_plot variable if longitudinal data is being used
  if(vis_type == "Longitudinal"){
    df_dt <- data.table::data.table(df)
    df_dt[, Visit := seq(.N), by = group_col]
    df[["Visit"]] <- df_dt$Visit
    df[["Visit"]] <- factor(df[["Visit"]], levels = sort(unique(df[["Visit"]])), labels = paste0("Visit ", sort(unique(df[["Visit"]]))))
    indvar_curr <- "Visit"
  }
  
  #If indvar is numeric, recast to factor if five or less unique values, otherwise bin into 5 groups
  if(!is.factor(df[[indvar_curr]]) && indvar_curr != "Visit"){
    if(length(unique(df[[indvar_curr]])) < 6){ df[[indvar_curr]] <- factor(df[[indvar_curr]])
    } else df[[indvar_curr]] <- ggplot2::cut_number(df[[indvar_curr]], n=5)
  }
  
  
  #Call the appropriate table maker function
  if(depvar_curr=="Count") { tab_explorer <- count_table_maker(df, indvar_curr, group_curr)
  } else tab_explorer <- covar_table_maker(df, depvar_curr, indvar_curr, group_curr)
  
  # spin_update$hide()
  
  return(tab_explorer)
}

