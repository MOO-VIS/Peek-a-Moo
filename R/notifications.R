get_warnings <- function(df){
  
  latest_row <- df %>%
    mutate(date = as.Date(date)) %>%
    filter(date == max(date))
  
  warnings_list <- list()
  for(i in 2:ncol(latest_row)) { 
    column_names <- colnames(latest_row)
    value <- latest_row[ , i]
    if(value != ""){
      warnings_list <- append(
        warnings_list, 
        list(
          notificationItem(
            text = paste0(column_names[i], ": ", value),
            icon = icon("exclamation-triangle"),
            status = "warning"
          )
        )
      )
    }
  }
  dropdownMenu(
    type = "notifications", badgeStatus = "warning",
    .list = warnings_list
  )
}