#' Functions that do the processing on the dataframe used for the enrollment plots
#'  
#' @param df 
#' @param adc_id_column 
#' @param regist_field 
#' @param status_field 
#' @param refer_field 
#' @param elig_field 
#'
#' @noRd
#' @author Chad Murchison
#' @return dataframe
enroll_process_initial <- function(df, adc_id_column = redcap_dict[["adrc_key"]], 
                                   regist_field = "regist_research_yn", status_field = "contact_status",
                                   refer_field = "screen_eligibility", elig_field = "cons_enroll"){
  
  
  #This has been reduced significantly since it now relies solely on the registry data table
  #Things like future date filtering are no longer required
  
  
  #Set some dedicated fields for the plots
  df$status <- df[[status_field]]
  df$regist <- df[[regist_field]]
  df$refer <- df[[refer_field]]
  df$elig <- df[[elig_field]]
  df$enroll <- df[[elig_field]]
  

  #We'll eventually want this as part of the referal tab
  # # 0 - Filter P20 registrants and get basic contact status
  # #We have an initial process step where if we're not explicitly looking at the full cohort, we only work with the P20 registrants
  # if(study != "Full UAB ADRC Cohort"){
  #   df <- df[df[["P20 Registrant"]]==1,]
  # } 
  
  
  # 0 - Contact status adjust for anyone who we've been unable to add to the registry; we use a dummy variable to set all downstream variables to NA later
  df$status[is.na(df$status)] <- status_dict[["na_fill"]]
  df$status <- factor(df$status, levels = status_dict[["levels"]], labels = status_dict[["labels"]])
  df$status[df$status %in% status_dict[["enroll_flow_drop"]]] <- status_dict[[".bad"]]
  
  
  # 1 - Registry participants interested in research, NA's pass to a new "Unknown"
  df$regist[is.na(df$regist)] <- regist_dict[["na_fill"]]
  df$regist <- factor(df$regist, levels=regist_dict[["levels"]], labels=regist_dict[["labels"]])
  
  #A final catch recasts the registration status of non-enrolled initial creation participants to "Interested in Research"
  df$regist[df$status == "Initial Creation" & df$regist == "Undecided" & !is.na(df$elig) & !is.na(df$status)] <- "Interested in Research"
  
  
  
  # 2 - Referred participants
  
  #Simply use the eligibility status as refer, replace NA's with unknown level and map from dictionary
  df$refer[is.na(df$refer)] <- refer_dict[["na_fill"]]
  
  #Pass the proper labeling
  df$refer <- factor(df$refer, levels=refer_dict[["levels"]], labels=refer_dict[["labels"]])
  
  #Drop everyone who is not interested in research from the registry
  df$refer[is.na(df$regist) | df$regist %not_in% regist_dict[["retained"]]] <- NA
  
  
  
  
  # 3 - Eligible & Willing
  
  #Get rid of anyone not eligible and willing from the referral field
  df$elig[is.na(df$elig)] <- elig_dict[["na_fill"]]
  df$elig[is.na(df$refer) | df$refer != levels(df$refer)[1]] <- NA
  
  #Map from dictionary
  df$elig[df$elig %in% elig_dict[["map_from"]]] <- elig_dict[["map_to"]]
  df$elig <- factor(df$elig, levels=elig_dict[["levels"]], labels=elig_dict[["labels"]])
  
  
  # 4 - Enrolled
  
  #Only focus on those enrolled
  df$enroll[is.na(df$elig) | df$elig != levels(df$elig)[1]] <- NA
  
  
  #Map milestone deaths from dictionary just in case
  df$enroll[!is.na(df[[enroll_dict[["death_field"]]]]) & df[[enroll_dict[["death_field"]]]]==1] <- enroll_dict[["death_level"]]
  
  
  #Update the loss to follow-up based on most recent date
  #This was originally based on having a visit data frame and may not be especially applicable from a pure registry perspective
  #df_dat <- as.data.table(df)
  #df_dat[, "new_enroll" := enroll_loss_check(.SD), by = adc_id_column]  #Use eval since adc_id_col is a variable
  #df$enroll <- df_dat$new_enroll; rm(df_dat)
  
  
  #Update enrollment factor and return
  df$enroll <- factor(df$enroll, levels=enroll_dict[["levels"]], labels=enroll_dict[["labels"]])
  df$enroll <- forcats::fct_drop(df$enroll)
  
  #Set everything flagged as being bad within status as -888
  df[df$status == "-888",c("status", "regist", "refer", "elig", "enroll")] <- NA
  
  
  return(df)
}

#' @noRd
#' @describeIn enroll_process_initial Enroll variable to check for loss to follow-up
enroll_loss_check <- function(df, targ_diff = 1365, diff_units = "days", ...){
  
  #Make sure there's only one type of enrollment for the participant
  if(length(unique(df[[.enroll_curr]]))>1) stop("More than one enrollment type found, check upstream code")
  
  #Make sure we're only using enrollment variables
  if(unique(df[[.enroll_curr]]) %not_in% enroll_dict[["enroll_level"]]) return(df[[.enroll_curr]])
  
  #Find the max date for the current subject
  date_max <- date_find(df)
  
  #Calculate differene in time
  date_diff <- difftime(lubridate::today(), date_max, units = diff_units)
  
  #If more than the threshold has passed, replace the enroll field
  if(date_diff > targ_diff) {return(rep(enroll_dict[["loss_level"]], nrow(df)))
  } else return(df[[.enroll_curr]])
  
}



#' Process data for enrollment plot
#' 
#' Processing for the rates of enrollment, currently using A1 date, should ultimately use enrollment date and a reactive for ref_date
#'
#' @param df 
#' @param start_dt 
#' @param end_dt 
#' @param study 
#' @param date_adjust 
#' @param enroll_field 
#' @param race_field 
#' @param ref_race 
#' @param yr_start 
#' @param mo_start 
#' @param dt_sep 
#'
#' @noRd
#' @author Chad Murchison
#' @return dataframe
enroll_process_rate <- function(df, start_dt, end_dt, study, date_adjust = TRUE, enroll_field = "enroll",
                                race_field = "Race", ref_race = "Black / AA",
                                yr_start = 2018, mo_start = 1, dt_sep = "-"){
  
  ##
  #Initialization processing and reactive date adjustments
  ##
  
  #We're currently doing some collapsing on year, specifically to only use the last two digits
  #If you want to go to four number years, just comment out the spots that have "gsub(^..)" i.e. yr_start, yr_curr, yr_list
  
  #Add a dummy variable to be able to subset the entire dataset if needed (e.g. Full UAB ADRC Cohort)
  df$dummy <- 1
  
  #We also make sure we only track rates for enrolled individuals
  #The major conflict is "Unable to Complete" would be included since they have consenting dates but aren't actually "enrolled"
  df <- df[!is.na(df[[enroll_field]]),]
  
  #First determine the starting window
  if(date_adjust == TRUE) {
    
    #Determine the specific study's start point and reference field for dates
    ref_date_field <- study_choices[["date_field"]][which(study_choices[["name"]] == study)]
    start_dt <- max(as.Date(start_dt), as.Date(study_choices[["start_dt"]][which(study_choices[["name"]] == study)]))
    
    #Making the reference, yr_start and mo_start dates more dynamic with the reactive context
    ref_date <- start_dt
    yr_start = lubridate::year(ref_date); mo_start = lubridate::month(ref_date)
    
    
    #Run the filter and build the window
    df <- start_end_date_filter_alt(df, date_field = ref_date_field, start_date = start_dt, end_date = end_dt, na_filter = TRUE)
    if(lubridate::year(start_dt) > yr_start){ 
      yr_start <- lubridate::year(start_dt)
      mo_start <- lubridate::month(start_dt)
    } else if(lubridate::month(start_dt) > mo_start) mo_start <- lubridate::month(start_dt)
    
  }
  yr_start <- gsub("^..", "", yr_start)
  dt_start <- paste(mo_start, yr_start, sep = dt_sep)
  
  #Select dataframe based on study
  df <- df[df[[study_choices[["var"]][which(study_choices[["name"]] == study)]]] == 1,]
  
  #Slice to get the first visit
  df <- as.data.table(df_slicer(df, slice_type = "first"))
  
  
  #Then determine the ending window
  
  if(date_adjust == TRUE) {
    mo_curr <- lubridate::month(end_dt)
    yr_curr <- lubridate::year(end_dt)
  } else {
    mo_curr <- lubridate::month(lubridate::today())
    yr_curr <- lubridate::year(lubridate::today())
  }
  yr_curr <- gsub("^(..)", "", yr_curr)
  
  #With the window established, start to build the labels list for the x-axis
  mo_list <- c(1:12)
  yr_list <- c(yr_start:yr_curr)
  
  #We use a gsub to only return the last two digits of the year
  dt_cap <- paste(mo_curr, yr_curr, sep = dt_sep)
  
  
  
  #Cast the month/year combinations as levels for a factor and cap based on dt_start
  dt_levels <- expand.grid(mo_list, yr_list)
  dt_levels <- do.call(paste, c(dt_levels, sep=dt_sep))
  dt_levels <- dt_levels[which(dt_levels == dt_start) : which(dt_levels == dt_cap)]
  
  
  
  ##
  #Process to compile within months
  ##
  
  #Initial process step to filter on date (using non-reactive for now), floor the date to focus on months, and make a factor
  dt_field <- study_choices[["date_field"]][which(study_choices[["name"]] == study)]
  if(!(lubridate::is.Date(df[[dt_field]]))) df[[dt_field]] <- as.Date(df[[dt_field]])
  
  #Drop all NA's from the reference date field
  df_dates <- df[!is.na(df[[dt_field]]),]
  #Only take dates after the reference date
  df_dates <- df_dates[df_dates[[dt_field]] > ref_date,]
  
  #Floor the month and then paste the month-year combination to align to the plot; end by making it a factor
  df_dates[, "dt_plot_posix" := lubridate::floor_date(df_dates[[dt_field]], unit = "month")]
  df_dates[, "dt_plot" := paste(lubridate::month(dt_plot_posix), lubridate::year(dt_plot_posix), sep = dt_sep)]
  df_dates$dt_plot <- gsub("..(..)$", "\\1", df_dates$dt_plot)
  df_dates$dt_plot <- factor(df_dates$dt_plot, levels = dt_levels, labels = dt_levels)
  
  
  #Reduce the df_dates object to only have counts for each month and then subset by race
  #First sort by the dt_plot and then by race
  df_reduc <- df_dates[order(dt_plot, df_dates[[race_field]]),]
  
  #Counts per month for all participants
  df_reduc[, "count_all" := .N, by = "dt_plot"]
  #Counts per month by race
  df_reduc[, "count_race" := .N, by = c("dt_plot", race_field)]
  
  
  
  ##
  #Processing for final dataframe
  ##
  
  #Initialize dataframe with two target enrollments, 50 and 100
  df_plot <- 
    data.table::data.table(dt_plot = factor(dt_levels, levels=dt_levels, labels=dt_levels),
                           count_50 = target_enroll(50, dt_levels),
                           count_100 = target_enroll(100, dt_levels))
  
  
  #Use left-join to combine with the df_reduc
  #Use data.table merge in place for both types for efficiency but need to add in count_all and count_race separately
  df_plot[df_reduc, on = "dt_plot", `:=` (count_all = i.count_all)]
  df_plot[df_reduc[df_reduc[[race_field]] == ref_race,], on = "dt_plot", `:=` (count_race = i.count_race)]
  
  #Replace any NA's with 0
  setnafill(df_plot, type="const", fill=0, cols=c("count_all", "count_race"))
  
  #Replace with the cumulative sum for all and get per month for eventual race proportions
  df_plot[, `:=` (count_all_per_month = count_all, count_all = cumsum(count_all))]
   
  #Do the same for race but also calculate the proportions also by month
  df_plot[, `:=` (count_race_per_month = count_race, count_race = cumsum(count_race))]
  df_plot$count_race_per_month_prop <- df_plot$count_race_per_month / df_plot$count_all_per_month
  df_plot$count_race_prop <- df_plot$count_race / df_plot$count_all

  #Zero out any proportion that isn't a number to avoid dividing by zero
  df_plot$count_race_prop[is.nan(df_plot$count_race_prop)] <- 0
  df_plot$count_race_per_month_prop[is.nan(df_plot$count_race_per_month_prop)] <- 0
  
  
  
  
  
  # #The prior version which used tidyverse; we keep to make note of the double bang etc.
  # 
  # #Make some symbols for unquoting
  # dt_field <- as.symbol(ref_date_field)
  # race_field <- as.symbol(ref_race_field)
  # 
  # #First process step to filter on date (using non-reactive for now), floor the date to focus on months, and make a factor
  # if(!(lubridate::is.Date(df[[dt_field]]))) df[[dt_field]] <- as.Date(df[[dt_field]])
  # df_dates <- 
  #   df[!is.na(df[[ref_date_field]]),] %>%
  #   dplyr::filter(!!dt_field > ref_date) %>%
  #   dplyr::mutate(dt_plot_posix = lubridate::floor_date(!!dt_field, unit="month"),
  #          dt_plot = paste(lubridate::month(dt_plot_posix), lubridate::year(dt_plot_posix), sep = dt_sep)) %>%
  #   dplyr::mutate(dt_plot = factor(dt_plot, levels = dt_levels, labels = dt_levels))
  #
  # 
  # #Reduce the df_dates object to only have counts for each month and then subset by race
  # df_reduc <- 
  #   df_dates %>%
  #   dplyr::arrange(dt_plot, !!race_field) %>%
  #   
  #   #Counts per month - all subjects 
  #   dplyr::group_by(dt_plot) %>%
  #   dplyr::mutate(count_all = dplyr::n()) %>%
  #   
  #   #Counts per month - by race
  #   dplyr::group_by(dt_plot, !!race_field) %>% 
  #   dplyr::mutate(count_race = dplyr::n()) %>%
  #   
  #   #First row only
  #   dplyr::group_by(dt_plot, !!race_field) %>%
  #   dplyr::filter(dplyr::row_number() == 1) %>%
  #   dplyr::ungroup(.)
  #   
  #   
  # 
  # ##
  # #Processing for final dataframe
  # ##
  # 
  # #Initialize dataframe with two target enrollments, 50 and 100
  # df_plot <- 
  #   data.frame(dt_plot = factor(dt_levels, levels=dt_levels, labels=dt_levels),
  #              count_50 = target_enroll(50, dt_levels),
  #              count_100 = target_enroll(100, dt_levels))
  # 
  # 
  # #Use left-join to combine with the df_reduc, take the first row to avoid doubling, then mutate to get cumulative sums
  # df_plot <- 
  #   dplyr::left_join(df_plot, dplyr::select(df_reduc, dt_plot, count_all), by="dt_plot") %>%
  #   dplyr::left_join(., dplyr::select(df_reduc[df_reduc[[race_field]]==ref_race,], dt_plot, count_race), by="dt_plot") %>%
  #   dplyr::group_by(dt_plot) %>%
  #   dplyr::filter(dplyr::row_number() == 1) %>%
  #   dplyr::ungroup(.) %>%
  #   
  #   dplyr::mutate(count_all = tidyr::replace_na(count_all, 0),
  #          count_all_per_month = count_all,
  #          count_all = cumsum(count_all)) %>%
  #   
  #   #Race also has proportions calculated
  #   dplyr::mutate(count_race = tidyr::replace_na(count_race, 0),
  #          count_race_per_month = count_race,
  #          count_race = cumsum(count_race),
  #          count_race_per_month_prop = count_race_per_month / count_all_per_month,
  #          count_race_prop = count_race / count_all)
  # 
  # #Zero out any proportion that isn't a number to avoid dividing by zero
  # df_plot$count_race_per_month_prop[is.nan(df_plot$count_race_per_month_prop)] <- 0
  
  
  
  return(df_plot)
  
}
