get_warnings <- function(warning_df, food_cuttoff, water_cuttoff){
  
  # get the latest warnings
  latest_warning_df <- warning_df %>%
    mutate(date = as.Date(date)) %>%
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
  warning_types <- warning_types[warning_types != ""]
  
  # format warning messages
  lapply(
    seq_along(warning_types), 
    function(x){
      paste(names(warning_types[x]), ": ", warning_types[x])
    }
  ) 
  
}

#' Generate warning notifications and dropdown
#'
#' @param warning_df Dataframe containing warnings
#' @param food_cuttoff Cuttoff for feed to trigger warning
#' @param water_cuttoff Cuttoff for water to trigger warning
#'
#' @return dropdownMenu with warning messages
get_warning_dropdown <- function(warning_df, food_cuttoff = 0, water_cuttoff = 0){
  
  warning_names <- get_warnings(warning_df, food_cuttoff, water_cuttoff)
  
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