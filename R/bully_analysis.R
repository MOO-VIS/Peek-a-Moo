#' Combine the dataframes of the replacement behaviour based of their dates.
#'
#' @param df The insentec dataframe after indexing the 'Replacement behaviour by date' list
#'
#' @return Combined dataframe
make_analysis_df <- function(df) {
  df <- bind_rows(df, .id = "date")
}

#' Carry out the analysis and create new columns for the desired replacement 
#' behaviour.
#' @param df The combined dataframe i.e the output of make_analysis_df function
#'
#' @return A dataframe with 4 new columns after undertaking the required analysis
actor_reactor_analysis <- function(df) {
  
  df %>%
    group_by(Actor_cow, date) %>%
    mutate(actor_cow_freq = n(),
           unique_reactor_cows_kicked = length(unique(Reactor_cow))) %>%
    ungroup() %>%
    group_by(Reactor_cow, date) %>%
    mutate(reactor_cow_freq = n(), 
           unique_actor_cows_kicking = length(unique(Actor_cow)))
}

#' Make an interactive plot of the actor reactor analysis. 
#'
#' @param df The dataframe with new columns i.e the output of actor_reactor_analysis function
#' @param cow_id An interger value used to represent the cow in which the user is interested. 
#' @param start_date A character value in the format 'YYYY-MM-DD', that represents the start date of the analysis
#' @param end_date A character value in the format 'YYYY-MM-DD', that represents the end date of the analysis
#'
#' @return An interactive plotly plot. 
plot_bully_analysis <- function(df, cow_id, start_date, end_date) {
  
  ### Make plotting data 
  cow_act_df <- df %>%
    filter(Actor_cow == cow_id) %>%
    filter(date >= start_date & date <= end_date)
  
  cow_reac_df <- df %>%
    filter(Reactor_cow == cow_id) %>%
    filter(date >= start_date & date <= end_date)
  
  cow_act_df$date <- ymd(cow_act_df$date)
  cow_reac_df$date <- ymd(cow_reac_df$date)
  
  
  ### Make plot
  colours <- c("Being Actor" = "steelblue", "Unique Cows kicked" = "red",
               "Being Reactor" = "yellow", "Unique cows kicking" = "green")
  
  analysis_plot <- ggplot(cow_act_df, aes(x = date)) +
    geom_line(aes(y = actor_cow_freq, color = "Being Actor")) +
    geom_line(aes(y = unique_reactor_cows_kicked, color = "Unique Cows kicked")) +
    geom_point(shape = 2, aes(y = actor_cow_freq, color = "Being Actor")) +
    geom_point(shape = 12, aes(y = unique_reactor_cows_kicked, color = "Unique Cows kicked")) +
    
    geom_line(data = cow_reac_df, aes(y = reactor_cow_freq, color = "Being Reactor")) +
    geom_line(data = cow_reac_df, aes(y = unique_actor_cows_kicking, color = "Unique cows kicking")) +
    geom_point(shape = 18, data = cow_reac_df, aes(y = reactor_cow_freq, color = "Being Reactor")) +
    geom_point(shape = 8, data = cow_reac_df, aes(y = unique_actor_cows_kicking, color = "Unique cows kicking")) +
    scale_x_date(date_labels = "%d-%B-%y") +
    labs(x = 'Date', y = 'Frequency', color = "Legend") +
    scale_color_manual(values = colours) #+
  #theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(analysis_plot)
}