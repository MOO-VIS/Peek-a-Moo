intake_warnings <- function(df, cuttoff){
  convert_date_col(df) %>%
    mutate(date = as.Date(date)) %>%
    group_by(Cow, date) %>%
    summarise(intake = sum(Intake)) %>%
    filter(intake < cuttoff) %>%
    mutate(message = paste(Cow, ": ", intake))
}

#' Combine additional warnings with original warnings
#'
#' @param insentec Dataframe containing all insentec tables
#' @param food_cuttoff Cuttoff for feed to trigger warning
#' @param water_cuttoff Cuttoff for water to trigger warning
#'
#' @return named list containing error names and messages
combine_warnings <- function(insentec, food_cuttoff = 0, water_cuttoff = 0){
  
  #food_warnings <- intake_warnings(insentec[["Cleaned_feeding_original_data"]], food_cuttoff)
  #drink_warnings <- intake_warnings(insentec[["Cleaned_drinking_original_data"]], water_cuttoff)
  
  # get the latest warnings
  all_warnings <- insentec[["Insentec warning"]] %>%
    mutate(date = as.Date(date)) %>%
    arrange(desc(date))
  all_warnings
  
}

#' Generate warning message list
#'
#' @param warning_df Dataframe containing all warnings
#'
#' @return named list containing error names and messages
parse_warnings <- function(warning_df){
  
  # get the latest warnings
  latest_warning_df <- warning_df %>%
    filter(date == max(date))
  
  # warning types to notify
  warning_types <- list(
    "No feed/water after 6pm: " = latest_warning_df[["no_show_after_6pm_cows"]],
    "No feed visit Cows: " = latest_warning_df[["cows_no_visit_to_feed_bin"]],
    "Double detection bin: " = latest_warning_df[["double_bin_detection_bin"]],
    "High intake bin: " = latest_warning_df[["double_cow_detection_bin"]],
    "Negative intake bin: " = latest_warning_df[["negative_intake_bin"]]
  )
  
  # filter out empty warnings
  warning_types[warning_types != ""]
}

#' Generate warning notifications and dropdown
#'
#' @param raw_df Dataframe containing all warnings
#' @param food_cuttoff Cuttoff for feed to trigger warning
#' @param water_cuttoff Cuttoff for water to trigger warning
#'
#' @return dropdownMenu with warning messages
get_warning_dropdown <- function(warning_df){
  
  warning_types <- parse_warnings(warning_df)
  
  # format warning messages
  warning_names <- lapply(
    seq_along(warning_types), 
    function(x){
      paste(names(warning_types[x]), ": ", warning_types[x])
    }
  ) 
  # create notification items list
  warnings_list <- warning_names %>%
    lapply(
      function(x){
        notificationItem(
          text = x,
          icon = icon("exclamation-triangle"),
          status = "warning"
        )
      }
    )
  
  # return dropdown menu with notification items
  dropdownMenu(
    type = "notifications", badgeStatus = "warning",
    .list = warnings_list
  )
}