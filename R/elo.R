#' Filtered elo data. 
#'
#' @param x The dataframe with new column names i.e the output of THI_analysis function
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id The interested cow
#'
#' @return A filtered data. 
elo_df <- function(x, start_date, end_date, cow_id = NULL) {
  df <- x %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date) & Cow == cow_id) %>%
    select(-present)
}

#' Make an interactive plot of ELO data and return both plot and filtered data. 
#'
#' @param x The dataframe with new column names i.e the output of THI_analysis function
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#' @param cow_id The interested cow
#'
#' @return An interactive plotly plot. 
plot_elo <- function(x, start_date, end_date, cow_id = NULL) {
  df <- elo_df(x, start_date, end_date, cow_id)
  
  plot <- ggplot(df, aes(x = Date, y = Elo)) +
    geom_line(color = "red") +
    scale_x_date(date_labels = "%d-%B-%y") +
    labs(x = 'Date', 
         y = 'Elo Rating',
         title = paste0('Daily Dominance Score of Cow ', cow_id)) +
    ylim(min(x$Elo), max(x$Elo)) +
    theme_classic() + theme(legend.position = "none")
  
  ggplotly(plot)
}