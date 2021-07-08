
#' Filter by user input date range
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param date_range A list containing two dates - start and end date
#'
#' @return Filtered dataframe with only dates within selected range
filter_date_range <- function(df, col, date_range){
  df %>%
    filter({{col}} >= date_range[[1]]) %>%
    filter({{col}} <= date_range[[2]])
}

#' Filter by user input cow selection
#'
#' @param df The dataframe to filter
#' @param col The cow column
#' @param cow_selection A list of cows Ids to display
#'
#' @return Filtered dataframe with only selected cows
filter_cows <- function(df, col, cow_selection){
  
  df %>%
    filter({{col}} %in% cow_selection)
}

#' Filter data by user input and add summary rows
#'
#' @param df Dataframe to filter and adjust
#' @param agg_type A string representing aggregation type, day/month/year
#' @param cow_selection A list of cow Ids to display
#' @param date_range A list containing two dates - start and end date
#'
#' @return Filtered dataframe with additional stats
process_range_data <- function(df, agg_type, cow_selection, date_range){
  
  # convert Cow col to character to add summary stats later and filter by date
  df <- df %>%
    mutate(Cow = as.character(Cow)) %>%
    filter_date_range(date, date_range)
  
  # calculate summary stats
  herd_average <- df %>%
    mutate(date = lubridate::floor_date(date, agg_type)) %>%
    group_by(date) %>%
    summarise(.,
              across(where(is.character), ~"Herd Average"),
              across(where(is.numeric), mean, na.rm=TRUE))
  
  # filter by cow and add summary rows
  df %>%
    filter_cows(Cow, cow_selection) %>%
    group_by(Cow, date) %>%
    summarise(.,
              across(where(is.character), ~"Herd Average"),
              across(where(is.numeric), mean, na.rm=TRUE)) %>%
    bind_rows(herd_average) 
}

#' Generate the plot and data tabs for time range plots
#'
#' @param df The dataframe containing data to be displayed
#' @param y_col The column of interest
#' @param show_average Boolean whether to show average line per cow
#'
#' @return NULL
cow_date_range_plot <- function(df, y_col, show_average){
  plt <- df %>%
    ggplot(aes(x = date, y = {{y_col}}, colour = `Cow`)) +
    geom_line() +
    theme(legend.position = "bottom")
  
  if(show_average){
    plt <- plt + 
      stat_smooth(
        method = "lm", 
        formula = y~1,
        se = FALSE,
        fullrange = TRUE,
        size = 0.5,
        linetype = "dashed"
      )
  }
  
  plt %>%
    ggplotly() %>%
    layout(legend = list(orientation = "h", y = -0.2))
}