#'  Some specific builder functions used in the enroll tab
#'  Used for building target enrollment vectors and dataframes
#'  Also includes a label builder for enrollment
#'
#'
#'



# Create a vector of target enrollment - used in enroll_process
#' @noRd
#' 
target_enroll <- function(n, dt_list, floor = FALSE){
  
  upper_lim <- length(dt_list)
  
  if(floor == TRUE){
    incr <- floor(n/12)
    seq(0, incr*upper_lim, by=incr)[-1]
  
  } else {
    incr <- n/12
    round(seq(0, incr*upper_lim, by=incr)[-1], 2)
  }
}



#Build a dataframe for target enrollment - used in make_plot_enroll_rate_all
build_target_enroll_df <- function(df, date_col = "Date",
                                   .cols = c("count_50", "count_105"), .labs = c("50 per year", "105 per year")){
  
  #Build the df using the Date metric from df as a filler
  tar_df <- df[,.SD,.SDcols = c(date_col, .cols)]
  
  #Cast long, set the Target to a factor
  tar_df <- data.table::melt(tar_df, measure.vars = .cols, variable.name = "Target", value.name = "Target Enrollment")
  tar_df$Target <- factor(tar_df$Target, levels = .cols, labels = .labs)
  
  return(tar_df)
}


