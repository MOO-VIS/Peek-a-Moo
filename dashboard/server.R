library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
source(here("R/network.R"))

shinyServer(function(input, output, session) {

  #' Filter by user input date range
  #'
  #' @param df The dataframe to filter
  #' @param col The date column
  #'
  #' @return Filtered dataframe with only dates within selected range
  filter_date_range <- function(df, col){
    df %>%
      filter({{col}} >= input$date_range[[1]]) %>%
      filter({{col}} <= input$date_range[[2]])
  }

  #' Filter by user input cow selection
  #'
  #' @param df The dataframe to filter
  #' @param col The cow column
  #'
  #' @return Filtered dataframe with only selected cows
  filter_cows <- function(df, col){

    df %>%
      filter({{col}} %in% input$cow_selection)
  }

  #' Filter data by user input and add summary rows
  #'
  #' @param df Dataframe to filter and adjust
  #'
  #' @return Filtered dataframe with additional stats
  process_range_data <- function(df){

    # convert Cow col to character to add summary stats later and filter by date
    df <- df %>%
      mutate(Cow = as.character(Cow)) %>%
      filter_date_range(date)

    # calculate summary stats
    herd_average <- df %>%
      group_by(date) %>%
      summarise(.,
                across(where(is.character), ~"Herd Average"),
                across(where(is.numeric), mean, na.rm=TRUE))

    # filter by cow and add summary rows
    df %>%
      filter_cows(Cow) %>%
      bind_rows(herd_average) %>%
      mutate(date = lubridate::floor_date(date, input$agg_type)) %>%
      group_by(Cow, date) %>%
      summarise(.,
                across(where(is.character), ~"Herd Average"),
                across(where(is.numeric), mean, na.rm=TRUE))
  }

  #' Generate the plot and data tabs for time range plots
  #'
  #' @param df The dataframe containing data to be displayed
  #' @param y_col The column of interest
  #' @param var_name The name of the UI output variable
  #'
  #' @return NULL
  cow_date_range_plot <- function(df, y_col, var_name){

    # filter table
    df <- process_range_data(df)

    # generate table
    output[[paste0(var_name, "_table")]] <- DT::renderDataTable(
      df,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        dom = 'Bftip',
        buttons = c("csv")
      )
    )

    # generate plot
    output[[paste0(var_name, "_plot")]] <- renderPlotly({
      plt <- df %>%
        ggplot(aes(x = date, y = {{y_col}}, colour = `Cow`)) +
        geom_line() +
        theme(legend.position = "bottom")
      
      if(input$show_average){
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
    })
  }

  # add plots and tables to the UI
  hobo <- dashboard_full_analysis[["HOBO"]]
  insentec <- dashboard_full_analysis[["Insentec"]]

  standing_bout_df <- hobo[["lying_standing_summary_by_date"]]
  feed_drink_df <- insentec[["Feeding and drinking analysis"]]
  
  # helper function for dataframes without dates in a single column
  convert_date_col <- function(df){
    enframe(df, name = "date") %>%
      mutate(date = as.Date(date)) %>%
      unnest(value)
  }
  non_nutritive_df <- convert_date_col(insentec[["non_nutritive_visits"]])
  feeding_together_df <- convert_date_col(insentec[["average number of feeding buddies"]])

  # update cow selection
  observe({
    cow_choices <- filter_date_range(feed_drink_df, date) %>%
      select("Cow") %>%
      unique()
    colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")

    updatePickerInput(
      session = session,
      inputId = "cow_selection",
      choices = cow_choices
    )
  })

  # render network
  g <- .make_tidygraph(hobo$`paired lying total time`)
  output$network <- visNetwork::renderVisNetwork({
    plot_network(g)
  })

  # render plots
  observe({
    cow_date_range_plot(
      feed_drink_df,
      `Feeding_Duration(s)`,
      "feed"
    )
    
    cow_date_range_plot(
      feed_drink_df,
      `Drinking_Duration(s)`,
      "drink"
    ) 
  
    cow_date_range_plot(
      standing_bout_df,
      `standing_time(seconds)`,
      "standing_time"
    ) 
    
    cow_date_range_plot(

      standing_bout_df,
      `standing_bout`,
      "standing_bout"
    )
    
    cow_date_range_plot(
      non_nutritive_df,
      `number_of_non_nutritive_visits`,
      "non_nutritive"
    )
    
    cow_date_range_plot(
      feeding_together_df,
      `average_num_other_cows_feeding_together`,
      "feeding_together"
    )

  })
})
