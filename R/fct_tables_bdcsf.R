#' From the family of function for tables
#'  
#' This handles the brain donation and csf tables
#' Like plots, this could be further generalized to handle any of the lists from dual_var_table_maker
#' 
#' @param df 
#' @param dict 
#'
#' @noRd
#' @author Chad Murchison
#' @return dataframe
make_table_compon <- function(df, dict = data_dict_component){
  
  # spin_update$show()
  
  if(is.null(df)){
    # spin_update$hide()
    return(default_table)
  }
  
  #Iterate over each dataframe in the passed list
  compon_table <- lapply(df, function(.df){
      
      #Iterate over the non-prop column
      cols_curr <- colnames(.df)[-grep("_prop|grouping", colnames(.df))]
      
      #Initial table processing over the columns
      .tab_out <- lapply(cols_curr, function(.col){
        
        #Add some spacing for anything less than 2 digits, they get made to characters anyway
        if(min(.df[[.col]]) < 10){
          .ltt <- which(.df[[.col]] < 10) #less than ten
          .df[[.col]] <- as.character(.df[[.col]])
          .df[[.col]][.ltt] <- paste0(" ", .df[[.col]][.ltt])
        }
        
        #Take on the proportions as a string for everything
        str_curr <- paste0(.df[[.col]], " (", .df[[paste0(.col, "_prop")]], ")")
        return(str_curr)
      })
      
      #Final processing on table, adds a row of NA's for spacing
      .tab_out <- do.call(rbind, .tab_out)
      .tab_out <- rbind(.tab_out, rep(NA, ncol(.tab_out)))
      
      rownames(.tab_out) <- c(cols_curr, NA)
      colnames(.tab_out) <- rownames(.df)
      
      
      return(.tab_out)
    })
  
  #Compile the list into a single table, we do in two steps so we can extract the unprocessed row names to paste into V1 later
  #It's a little inefficient but check.rows doesn't work as an argument
  compon_table <- do.call(rbind, compon_table)
  compon_row_names <- rownames(compon_table)[-nrow(compon_table)]
  compon_table <- data.frame(compon_table, check.names = FALSE)
  compon_table <- compon_table[-nrow(compon_table),] #We drop the last row since it's just the NA padding
   
  #Building the final display table, including interleaving blank columns
  
  #Start by getting enough columns of .blank
  .blank_df <- matrix(NA, nrow = nrow(compon_table), ncol = length(grep(dict[["grep_string"]], colnames(compon_table))))
  .blank_df <- as.data.frame(.blank_df)
  colnames(.blank_df) <- paste0("Blank", seq_len(ncol(.blank_df)))
  
  #Make the final component set, first build the table then sort the column names
  compon_table <- data.frame(V1 = compon_row_names, compon_table, .blank_df, check.names = FALSE)
  
  #We build the column names by first extracting the annotations (e.g. BD or CSF)
  annot_set <- grep(dict[["grep_string"]], colnames(compon_table), value = TRUE)
  annot_set <- gsub(dict[["grep_string"]], "", annot_set)
  annot_set <- build_compon_annot(annot_set, build_table = TRUE)
  names(annot_set) <- NULL
  compon_table <- dplyr::select(compon_table, tidyselect::all_of(annot_set))
  
  #Final display processing to drop periods and null out NAs
  compon_table$V1 <- gsub("\\.", " ", compon_table$V1)
  compon_table$V1[grep("^NA", compon_table$V1)] <- ""
  compon_table[is.na(compon_table)] <- ""
  colnames(compon_table)[grep("V1|Blank", colnames(compon_table))] <- " "
  
  
  # spin_update$hide()
  
  return(compon_table)
}
  
