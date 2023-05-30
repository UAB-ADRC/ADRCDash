
#' Function to build the inventory tables as subset by demographics
#'
#' @param df 
#' @param study 
#' @param group_var 
#' @param dict 
#' 
#' @author Chad Murchison
#'
#' @noRd
make_table_inventory <- function(df, study, group_var = NULL, dict = data_dict_inventory){
  
  #Select dataframe based on study
  df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]][is.na(df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]])] <- 0
  df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
  

  #Slice the data frame - return first visit
  #We have to slice since the inventories are currently in aggregate
  df <- df_slicer(df, slice_type = "first")
  
  #Return the NULL tables if needed
  if(nrow(df) == 0) return(list(sum = default_table, mean = default_table))
  
  #Coerce to a data.table to easily subset according to the group variables
  df_tab <- data.table::data.table(df)
  
  #Make sure all count variables are numeric
  df_tab[,dict[["data_col"]] := lapply(.SD, as.numeric), .SDcols = dict[["data_col"]]]
  
  #Apply over the entire table if no group subsetting was passed
  if(is.null(group_var)){
    tab_sum <- df_tab[,make_tab_sum(.SD),]
    tab_mean <- df_tab[,make_tab_mean(.SD),]
    
    #Add a header cell
    tab_sum <- cbind.data.frame("Full cohort", tab_sum)
    tab_mean <- cbind.data.frame("Full cohort", tab_mean)
    colnames(tab_sum) <- colnames(tab_mean) <- c("", dict[["biospec_names"]])
    
  #Otherwise, build off the marginals  
  } else{
     
    #Build the tables
    tab_sum <- df_tab[,make_tab_sum(.SD), by = group_var]
    tab_mean <- df_tab[,make_tab_mean(.SD), by = group_var]
    
    #Since we might have some marginals that are NA (e.g. AD Stage) we check for all 0's in the tab_sum we can index off of to drop those rows
    .idx_drop <- rowSums(tab_sum[,-c(1:length(group_var)),with = FALSE])
    .idx_drop <- which(.idx_drop == 0)
    if(length(.idx_drop) > 0){
      tab_sum <- tab_sum[-.idx_drop,]
      tab_mean <- tab_mean[-.idx_drop,]
    }
    
    #Finally, we just sort on the group_vars - note this uses setorderv since we're still using data.table
    #This does have the benefit of sorting on the factor levels
    tab_sum <- data.table::setorderv(tab_sum, group_var)[]
    tab_mean <- data.table::setorderv(tab_mean, group_var)
    
    #Column names from the dictionary
    colnames(tab_sum)[-c(1:length(group_var))] <- colnames(tab_mean)[-c(1:length(group_var))] <- dict[["biospec_names"]]
    
  }
  
  #Return the list
  return(list(sum = tab_sum, mean = tab_mean))
}

#' @noRd
#' @describeIn make_table_inventory Create the table inventory - mean
make_tab_mean <- function(.df, dict = data_dict_inventory){
  
  #The means, calculate and replace NaN with a "-"
  .mean <- colMeans(.df[,colnames(.df) %in% dict[["data_col"]], with = FALSE], na.rm = TRUE)
  .mean <- sprintf("%.2f", .mean)
  .mean <- gsub("NaN", "-", .mean)
  
  #The length
  .length <- sapply(.df[,colnames(.df) %in% dict[["data_col"]], with = FALSE], function(xx){length(xx[!is.na(xx)])})
  
  #Pasting together
  .out <- paste0(.mean, " (N = ", .length, ")")
  
  return(as.data.frame(t(.out)))
}

#' @noRd
#' @describeIn make_table_inventory Create the table inventory - sum
make_tab_sum <- function(.df, dict = data_dict_inventory){
  
  #Easy sum and formatting
  .out <- colSums(.df[,colnames(.df) %in% dict[["data_col"]], with = FALSE], na.rm = TRUE)
  #.out <- sprintf("%.0f", .out)
  #.out <- gsub("NaN", "-", .out)
  
  return(as.data.frame(t(.out)))
}
