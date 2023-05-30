#' Functions that do the  processing for the inventory
#'  
#' 
#' This is currently no longer needed since inventories are part of the nacc_visit pulls now
#' This may change in the future if we start to process by date instead of in aggregate
#'
#'




# inventory_process <- function(df, adc_id_column = "adc_sub_id",
#                               dict = data_dict_inventory, invent_forms = c("subject_info", "biospec")){
# 
# 
#   #We start by calling the specific inventory REDCap project
#   invent_token <- Sys.getenv("REDCAP_NACC_API")
#   invent.conn <- redcapAPI::redcapConnection(url="https://redcap.dom.uab.edu/api/", token=invent_token)
#   invent_curr <- redcapAPI::exportRecords(invent.conn, factors=F, forms = invent_forms)
# 
#   #Some column cleanup based on the dictionary, only want sample counts
#   invent_curr <- invent_curr[,-which(colnames(invent_curr) %in% dict[["dropped_fields"]])]
#   invent_curr <- invent_curr[,-grep(dict[["bulk_drop_string"]], colnames(invent_curr))]
# 
#   #Drop any rows that are brain donation specific
#   invent_curr <- invent_curr[-grep("ADC1\\d\\d\\d", invent_curr[[adc_id_column]]),]
# 
#   #Merge the two sets
#   df_invent <- merge(df, invent_curr, by = adc_id_column, all = TRUE)
# 
# 
#   #We end here and keep the reactive to the table generation
#   return(df_invent)
# 
# }


